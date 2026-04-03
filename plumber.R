# plumber.R
# Stripe webhook receiver
# Runs on web server port 8766

library(plumber)
library(DBI)
library(RPostgres)
library(jsonlite)
library(digest)

# ============================================
# Helpers
# ============================================

make_con <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("DB_NAME"),
    host     = Sys.getenv("DB_HOST"),
    port     = as.integer(Sys.getenv("DB_PORT")),
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
}

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !is.na(a) && nzchar(as.character(a))) a else b
}

verify_stripe_signature <- function(payload, sig_header, secret) {
  
  if (is.null(sig_header) || !nzchar(sig_header)) {
    message("No signature header")
    return(FALSE)
  }
  if (is.null(secret) || !nzchar(secret)) {
    message("No webhook secret set")
    return(FALSE)
  }
  
  parts     <- strsplit(sig_header, ",")[[1]]
  ts_parts  <- parts[grepl("^t=",  parts)]
  sig_parts <- parts[grepl("^v1=", parts)]
  
  if (length(ts_parts) == 0 || length(sig_parts) == 0) {
    message("Could not parse signature header: ", sig_header)
    return(FALSE)
  }
  
  timestamp  <- sub("^t=",  "", ts_parts[1])
  signatures <- sub("^v1=", "", sig_parts)
  
  ts <- suppressWarnings(as.numeric(timestamp))
  if (is.na(ts) || abs(as.numeric(Sys.time()) - ts) > 300) {
    message("Webhook timestamp too old or invalid: ", timestamp)
    return(FALSE)
  }
  
  signed_payload <- paste0(timestamp, ".", payload)
  expected_sig   <- digest::hmac(
    key    = secret,
    object = signed_payload,
    algo   = "sha256",
    raw    = FALSE
  )
  
  matched <- any(signatures == expected_sig)
  if (!matched) message("Signature mismatch")
  matched
}

#' Map amount paid to subscription name
#' Amount is in dollars (already divided by 100)
map_from_amount <- function(amount_paid) {
  
  if (is.null(amount_paid) || is.na(amount_paid)) {
    message("No amount — defaulting to Starter")
    return("Starter")
  }
  
  message("Mapping amount: $", amount_paid)
  
  # Annual payments (full year charged at once)
  # Starter annual: $99 x 12 = $1,188
  # Pro annual:     $349 x 12 = $4,188
  if (amount_paid >= 4000) return("Pro")       # Pro annual
  if (amount_paid >= 1000) return("Starter")   # Starter annual
  
  # Monthly payments
  if (amount_paid >= 449)  return("Pro")       # Pro monthly
  if (amount_paid >= 129)  return("Starter")   # Starter monthly
  
  # Below $129 — default to Starter
  message("Amount below known thresholds — defaulting to Starter")
  "Starter"
}

# ============================================
# Webhook endpoint
# ============================================

#* @post /stripe-webhook
function(req, res) {
  
  message("\n=== Stripe webhook: ", format(Sys.time()), " ===")
  
  payload        <- req$postBody
  sig_header     <- req$HTTP_STRIPE_SIGNATURE
  webhook_secret <- Sys.getenv("STRIPE_WEBHOOK_SECRET")
  
  # ── Verify signature ────────────────────────────────────────────────
  if (!verify_stripe_signature(payload, sig_header, webhook_secret)) {
    message("✗ Signature verification failed")
    res$status <- 400
    return(list(error = "Invalid signature"))
  }
  
  message("✓ Signature verified")
  
  # ── Parse event ─────────────────────────────────────────────────────
  event <- tryCatch(
    fromJSON(payload, simplifyVector = FALSE),
    error = function(e) {
      message("✗ JSON parse error: ", e$message)
      NULL
    }
  )
  
  if (is.null(event)) {
    res$status <- 400
    return(list(error = "Invalid JSON"))
  }
  
  message("Event type: ", event$type)
  
  # ============================================
  # checkout.session.completed
  # ============================================
  
  if (event$type == "checkout.session.completed") {
    
    session        <- event$data$object
    customer_email <- tryCatch(
      session$customer_details$email,
      error = function(e) NULL
    )
    amount_paid    <- tryCatch(
      (session$amount_total %||% 0) / 100,
      error = function(e) 0
    )
    stripe_sess_id <- session$id
    
    # Try client_reference_id first — most reliable link to our user
    client_ref <- tryCatch(
      session$client_reference_id,
      error = function(e) NULL
    )
    
    message("Customer email: ", customer_email %||% "(none)")
    message("Client ref ID: ", client_ref %||% "(none)")
    message("Amount:        $", amount_paid)
    
    sub_name <- map_from_amount(amount_paid)
    message("Tier:          ", sub_name)
    
    con <- tryCatch(make_con(), error = function(e) {
      message("DB connection failed: ", e$message)
      NULL
    })
    if (is.null(con)) {
      res$status <- 500
      return(list(error = "DB unavailable"))
    }
    on.exit(dbDisconnect(con), add = TRUE)
    
    tryCatch({
      dbBegin(con)
      
      # 1. Record purchase
      dbExecute(con, "
      INSERT INTO user_purchases
        (email, access_level, amount, purchase_date,
         stripe_session_id, is_active)
      VALUES ($1, $2, $3, NOW(), $4, TRUE)
      ON CONFLICT (stripe_session_id) DO NOTHING",
                params = list(customer_email %||% "unknown",
                              sub_name, amount_paid, stripe_sess_id))
      message("✓ user_purchases written")
      
      # 2. Find user — prefer client_reference_id over email
      user <- NULL
      
      # Try login_id from client_reference_id first
      if (!is.null(client_ref) && nzchar(client_ref %||% "")) {
        login_id_ref <- suppressWarnings(as.integer(client_ref))
        if (!is.na(login_id_ref)) {
          user <- dbGetQuery(con,
                             "SELECT login_id, email FROM dim_user WHERE login_id = $1",
                             params = list(login_id_ref))
          if (nrow(user) > 0) {
            message("✓ User found via client_reference_id: login_id ",
                    user$login_id[1])
          }
        }
      }
      
      # Fall back to email match
      if (is.null(user) || nrow(user) == 0) {
        if (!is.null(customer_email) && nzchar(customer_email %||% "")) {
          user <- dbGetQuery(con,
                             "SELECT login_id, email FROM dim_user WHERE email = $1",
                             params = list(customer_email))
          if (nrow(user) > 0) {
            message("✓ User found via email: ", customer_email)
          }
        }
      }
      
      # Still not found — create new user from email
      if (is.null(user) || nrow(user) == 0) {
        if (!is.null(customer_email) && nzchar(customer_email %||% "")) {
          temp_hash <- digest::digest(
            paste0(customer_email, as.numeric(Sys.time())),
            algo = "sha256"
          )
          user <- dbGetQuery(con,
                             "INSERT INTO dim_user
             (date_added, email, password_hash, onboarding_complete)
           VALUES (CURRENT_DATE, $1, $2, FALSE)
           RETURNING login_id, email",
                             params = list(customer_email, temp_hash))
          message("✓ New user created: ", customer_email,
                  " (login_id: ", user$login_id[1], ")")
        } else {
          stop("Cannot identify user — no client_reference_id or email")
        }
      }
      
      login_id <- user$login_id[1]
      
      # Update email if it changed (user paid with different email)
      if (!is.null(customer_email) && nzchar(customer_email %||% "") &&
          !is.null(user$email) && user$email != customer_email) {
        message("Note: payment email (", customer_email,
                ") differs from account email (", user$email, ") — keeping account email")
      }
      
      # 3. Get subscription level ID
      sub_row <- dbGetQuery(con,
                            "SELECT subscription_level_id FROM dim_subscription
       WHERE subscription_name = $1 LIMIT 1",
                            params = list(sub_name))
      
      if (nrow(sub_row) == 0) stop(paste("Subscription not found:", sub_name))
      sub_level_id <- sub_row$subscription_level_id[1]
      
      # 4. Close existing subscriptions
      rows_closed <- dbExecute(con, "
      UPDATE fact_user_sub_level
      SET date_valid_to = CURRENT_DATE - 1
      WHERE login_id = $1 AND date_valid_to >= CURRENT_DATE",
                               params = list(login_id))
      message("  Closed ", rows_closed, " existing subscription(s)")
      
      # 5. Insert new subscription
      dbExecute(con, "
      INSERT INTO fact_user_sub_level
        (login_id, subscription_level_id, date_valid_from, date_valid_to)
      VALUES ($1, $2, CURRENT_DATE, '2099-12-31')
      ON CONFLICT DO NOTHING",
                params = list(login_id, sub_level_id))
      
      dbCommit(con)
      message("✓ Subscription set: login_id ", login_id, " → ", sub_name)
      
    }, error = function(e) {
      dbRollback(con)
      message("✗ DB error: ", e$message)
      res$status <- 500
      return(list(error = e$message))
    })
  }
  
  # ============================================
  # customer.subscription.deleted
  # ============================================
  
  if (event$type == "customer.subscription.deleted") {
    
    customer_email <- tryCatch(
      event$data$object$customer_email,
      error = function(e) NULL
    )
    
    message("Cancellation for: ", customer_email %||% "(unknown)")
    
    if (!is.null(customer_email) && nzchar(customer_email %||% "")) {
      
      con <- tryCatch(make_con(), error = function(e) NULL)
      if (is.null(con)) {
        res$status <- 500
        return(list(error = "DB unavailable"))
      }
      on.exit(dbDisconnect(con), add = TRUE)
      
      user <- dbGetQuery(con,
                         "SELECT login_id FROM dim_user WHERE email = $1",
                         params = list(customer_email))
      
      if (nrow(user) > 0) {
        login_id <- user$login_id[1]
        
        free_sub <- dbGetQuery(con,
                               "SELECT subscription_level_id FROM dim_subscription
           WHERE subscription_name = 'Free' LIMIT 1")
        
        if (nrow(free_sub) > 0) {
          
          # Close active subscriptions
          dbExecute(con, "
            UPDATE fact_user_sub_level
            SET date_valid_to = CURRENT_DATE
            WHERE login_id = $1
              AND date_valid_to >= CURRENT_DATE",
                    params = list(login_id))
          
          # Downgrade to free
          dbExecute(con, "
            INSERT INTO fact_user_sub_level
              (login_id, subscription_level_id,
               date_valid_from, date_valid_to)
            VALUES ($1, $2, CURRENT_DATE, '2099-12-31')",
                    params = list(login_id, free_sub$subscription_level_id[1]))
          
          # Mark purchases inactive
          dbExecute(con, "
            UPDATE user_purchases
            SET is_active = FALSE
            WHERE email = $1",
                    params = list(customer_email))
          
          message("✓ Downgraded to Free: ", customer_email)
        }
      } else {
        message("User not found for cancellation: ", customer_email)
      }
    }
  }
  
  list(status = "ok")
}