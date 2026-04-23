#' Create a linked account for an existing user
#' 
#' @param primary_email     Email of the main account to link to
#' @param link_number       Which link number (1, 2, 3...) — determines email prefix
#' @param password          Password for the new linked account
#' @param n_competitors     Competitor slots for this account
#' @param n_prompts         Prompt slots for this account
#' @param n_personas        Persona slots for this account
#' @param brand_name        Optional — pre-set the brand name
#'
create_linked_account <- function(primary_email,
                                  link_number   = 1,
                                  password      = NULL,
                                  n_competitors = 10,
                                  n_prompts     = 10,
                                  n_personas    = 3,
                                  account_name   = NULL, 
                                  brand_name    = NULL) {
  
  # ── Find primary account ─────────────────────────────────────────────
  primary <- dbGetQuery(pool,
                        "SELECT login_id, email FROM dim_user WHERE email = $1",
                        params = list(tolower(trimws(primary_email))))
  
  if (nrow(primary) == 0) {
    cat(sprintf("✗ Primary account not found: %s\n", primary_email))
    return(invisible(NULL))
  }
  
  primary_login_id <- primary$login_id[1]
  cat(sprintf("✓ Primary account found: %s (login_id: %d)\n",
              primary_email, primary_login_id))
  
  # ── Build linked email address ────────────────────────────────────────
  # Split primary email at @ and prepend link prefix
  # Clean the primary email before splitting
  primary_email_clean <- tolower(trimws(primary_email))
  email_parts  <- strsplit(primary_email, "@")[[1]]
  # Remove any whitespace from both parts before concatenating
  local_part  <- trimws(email_parts[1])
  domain_part <- trimws(email_parts[2])
  
  linked_email <- sprintf("link%d.%s@%s", link_number, local_part, domain_part)
  
  # Final safety clean — remove any remaining whitespace
  linked_email <- gsub("\\s+", "", linked_email)
  
  cat(sprintf("  Linked email: %s\n", linked_email))
  
  # Check it doesn't already exist
  existing <- dbGetQuery(pool,
                         "SELECT login_id FROM dim_user WHERE email = $1",
                         params = list(linked_email))
  
  if (nrow(existing) > 0) {
    cat(sprintf("! Linked account already exists (login_id: %d)\n",
                existing$login_id[1]))
    linked_login_id <- existing$login_id[1]
  } else {
    # ── Create the linked user account ─────────────────────────────────
    if (is.null(password)) {
      # Generate a random password if none supplied
      password <- paste0(
        sample(c(LETTERS, letters, 0:9, "!", "@", "#"), 12, replace = TRUE),
        collapse = "")
    }
    
    password_hash <- digest::digest(password, algo = "sha256")
    
    new_user <- dbGetQuery(pool,
                           "INSERT INTO dim_user
       (date_added, email, password_hash, onboarding_complete,
        is_linked_account, primary_login_id, account_name)
     VALUES (CURRENT_DATE, $1, $2, FALSE, TRUE, $3, $4)
     RETURNING login_id",
                           params = list(linked_email, password_hash, primary_login_id,
                                         account_name %||% linked_email))
    
    linked_login_id <- new_user$login_id[1]
    cat(sprintf("✓ Linked user created (login_id: %d)\n", linked_login_id))
  }
  
  # ── Determine subscription — Enterprise base + extra slots if needed ──
  ent_sub <- dbGetQuery(pool,
                        "SELECT subscription_level_id, num_competitors_included,
            num_prompts_included, num_personas_included
     FROM dim_subscription
     WHERE subscription_name = 'Enterprise' LIMIT 1")
  
  if (nrow(ent_sub) == 0) {
    cat("✗ Enterprise subscription tier not found\n")
    return(invisible(NULL))
  }
  
  sub_level_id      <- ent_sub$subscription_level_id[1]
  base_competitors  <- ent_sub$num_competitors_included[1]
  base_prompts      <- ent_sub$num_prompts_included[1]
  base_personas     <- ent_sub$num_personas_included[1]
  
  extra_competitors <- max(0, n_competitors - base_competitors)
  extra_prompts     <- max(0, n_prompts     - base_prompts)
  extra_personas    <- max(0, n_personas    - base_personas)
  
  # Close any existing subscriptions
  dbExecute(pool,
            "UPDATE fact_user_sub_level
     SET date_valid_to = CURRENT_DATE - 1
     WHERE login_id = $1 AND date_valid_to >= CURRENT_DATE",
            params = list(linked_login_id))
  
  # Insert subscription with extras
  dbExecute(pool,
            "INSERT INTO fact_user_sub_level
       (login_id, subscription_level_id, date_valid_from, date_valid_to,
        extra_competitors_added, extra_prompts_added, extra_personas_added)
     VALUES ($1, $2, CURRENT_DATE, '2099-12-31', $3, $4, $5)
     ON CONFLICT DO NOTHING",
            params = list(linked_login_id, sub_level_id,
                          extra_competitors, extra_prompts, extra_personas))
  
  cat(sprintf("✓ Subscription set: Enterprise + %d extra competitors, ",
              extra_competitors))
  cat(sprintf("%d extra prompts, %d extra personas\n",
              extra_prompts, extra_personas))
  
  # ── Summary ───────────────────────────────────────────────────────────
  cat("\n=== Linked account created ===\n")
  cat(sprintf("  Primary account:  %s (login_id: %d)\n",
              primary_email, primary_login_id))
  cat(sprintf("  Linked email:     %s\n", linked_email))
  cat(sprintf("  Linked login_id:  %d\n", linked_login_id))
  cat(sprintf("  Password:         %s\n", password))
  cat(sprintf("  Competitors:      %d (%d base + %d extra)\n",
              n_competitors, base_competitors, extra_competitors))
  cat(sprintf("  Prompts:          %d (%d base + %d extra)\n",
              n_prompts, base_prompts, extra_prompts))
  cat(sprintf("  Personas:         %d (%d base + %d extra)\n",
              n_personas, base_personas, extra_personas))
  cat("\nUser completes brand setup on first login.\n")
  
  return(invisible(list(
    primary_login_id = primary_login_id,
    primary_email    = primary_email,
    linked_login_id  = linked_login_id,
    linked_email     = linked_email,
    password         = password,
    n_competitors    = n_competitors,
    n_prompts        = n_prompts,
    n_personas       = n_personas
  )))
}


# EXAMPLES ----------------

# Create first linked account for a user
# create_linked_account(
#   primary_email = "	kate.shaw@living-group.com",
#   link_number   = 1,
#   password      = "LinkedAccount1LG!",
#   n_competitors = 30,
#   n_prompts     = 2,
#   n_personas    = 2
# )
# 
# create_linked_account(
#   primary_email = "	kate.shaw@living-group.com",
#   link_number   = 2,
#   password      = "LinkedAccount2LG!",
#   n_competitors = 10,
#   n_prompts     = 2,
#   n_personas    = 2,
#   account_name   = 'Top 10 US Alternative Asset Managers'
# )



# View all linked accounts for a user
# primary <- dbGetQuery(pool,
#                       "SELECT login_id FROM dim_user WHERE email = 'client@company.com'")
# 
# get_linked_accounts(primary$login_id[1])