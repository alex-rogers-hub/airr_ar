create_pro_account <- function(email, password) {
  
  # ── Validate inputs ────────────────────────────────────────────────
  if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}$", email)) {
    cat("✗ Invalid email address\n")
    return(invisible(NULL))
  }
  
  if (nchar(password) < 6) {
    cat("✗ Password must be at least 6 characters\n")
    return(invisible(NULL))
  }
  
  # ── Check if already exists ────────────────────────────────────────
  existing <- dbGetQuery(pool,
                         "SELECT login_id, email FROM dim_user WHERE email = $1",
                         params = list(tolower(trimws(email))))
  
  if (nrow(existing) > 0) {
    cat("! User already exists (login_id:", existing$login_id[1], ")\n")
    cat("  Updating to Pro subscription...\n")
    login_id <- existing$login_id[1]
  } else {
    # ── Create new user ──────────────────────────────────────────────
    password_hash <- digest::digest(password, algo = "sha256")
    
    new_user <- dbGetQuery(pool,
                           "INSERT INTO dim_user 
         (date_added, email, password_hash, onboarding_complete)
       VALUES (CURRENT_DATE, $1, $2, FALSE)
       RETURNING login_id",
                           params = list(tolower(trimws(email)), password_hash))
    
    login_id <- new_user$login_id[1]
    cat("✓ User created (login_id:", login_id, ")\n")
  }
  
  # ── Get Pro subscription level ID ─────────────────────────────────
  pro_sub <- dbGetQuery(pool,
                        "SELECT subscription_level_id FROM dim_subscription
     WHERE subscription_name = 'Pro' LIMIT 1")
  
  if (nrow(pro_sub) == 0) {
    cat("✗ Pro subscription tier not found in dim_subscription\n")
    return(invisible(NULL))
  }
  
  pro_level_id <- pro_sub$subscription_level_id[1]
  
  # ── Close any existing subscriptions ──────────────────────────────
  closed <- dbExecute(pool,
                      "UPDATE fact_user_sub_level
     SET date_valid_to = CURRENT_DATE - 1
     WHERE login_id = $1
       AND date_valid_to >= CURRENT_DATE",
                      params = list(login_id))
  
  if (closed > 0) cat("  Closed", closed, "existing subscription(s)\n")
  
  # ── Insert Pro subscription ────────────────────────────────────────
  dbExecute(pool,
            "INSERT INTO fact_user_sub_level
       (login_id, subscription_level_id, date_valid_from, date_valid_to)
     VALUES ($1, $2, CURRENT_DATE, '2099-12-31')
     ON CONFLICT DO NOTHING",
            params = list(login_id, pro_level_id))
  
  cat("✓ Pro subscription assigned\n")
  
  # ── Verify ────────────────────────────────────────────────────────
  result <- dbGetQuery(pool,
                       "SELECT 
       u.login_id,
       u.email,
       u.onboarding_complete,
       ds.subscription_name,
       fus.date_valid_from,
       fus.date_valid_to
     FROM dim_user u
     JOIN fact_user_sub_level fus ON fus.login_id = u.login_id
     JOIN dim_subscription ds 
       ON ds.subscription_level_id = fus.subscription_level_id
     WHERE u.login_id = $1
       AND fus.date_valid_to >= CURRENT_DATE",
                       params = list(login_id))
  
  cat("\n=== Account created successfully ===\n")
  cat("  Login ID:     ", result$login_id[1], "\n")
  cat("  Email:        ", result$email[1], "\n")
  cat("  Password:     ", password, "\n")
  cat("  Subscription: ", result$subscription_name[1], "\n")
  cat("  Valid until:  ", format(result$date_valid_to[1]), "\n")
  cat("  Onboarding:   ", 
      if (result$onboarding_complete[1]) "complete" else "pending", "\n")
  cat("\nShare these credentials with the user.\n")
  cat("They will complete brand setup on first login.\n")
  
  return(invisible(list(
    login_id     = login_id,
    email        = result$email[1],
    password     = password,
    subscription = result$subscription_name[1]
  )))
}

# create_pro_account('rentrup@airrscore.com', 'REntrup123!')
# create_pro_account('pro@airrscore.com', 'Pro_airr_123!')
