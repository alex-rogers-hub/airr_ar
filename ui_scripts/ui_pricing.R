ui_pricing <- function(billing_cycle = "annual",
                       prefill_email    = "",
                       prefill_login_id = "") {
  
  starter_features <- list(
    "AiRR Score with full 4P breakdown",
    "Daily score tracking",
    "5 competitors (AI-recommended)",
    "1 prompt",
    "1 persona",
    "30-day historical charts",
    "ChatGPT coverage"
  )
  
  pro_features <- list(
    "Everything in Starter",
    "10 competitors",
    "10 prompts",
    "3 personas",
    "CSV & API export",
    "90-day historical charts",
    "ChatGPT coverage"
  )
  
  enterprise_features <- list(
    "Everything in Pro",
    "Unlimited competitors",
    "Unlimited prompts & personas",
    "Multi-LLM coverage",
    "Executive Report Builder",
    "Full historical data",
    "Dedicated account manager",
    "White-glove onboarding",
    "Custom integrations",
    "SSO",
    "Priority support with SLA"
  )
  
  make_feature <- function(text) {
    div(
      style = "display: flex; align-items: flex-start; gap: 8px;
               font-size: 13px; color: #4a5568; padding: 4px 0;",
      icon("check", style = "color: #27AE60; font-size: 11px;
                              flex-shrink: 0; margin-top: 2px;"),
      tags$span(text)
    )
  }
  
  make_plan_card <- function(name, monthly_price, annual_price,
                             monthly_link, annual_link,
                             features, is_popular = FALSE,
                             is_enterprise = FALSE,
                             accent_color = "#667eea",
                             prefill_email = "",
                             prefill_login_id = "") {
    
    price <- if (billing_cycle == "annual") annual_price else monthly_price
    
    # Build link with optional email prefill
    base_link <- if (billing_cycle == "annual") annual_link else monthly_link
    
    link <- if (!is_enterprise) {
      params <- c()
      if (nzchar(prefill_email)) {
        params <- c(params, paste0("prefilled_email=",
                                   URLencode(prefill_email, reserved = TRUE)))
      }
      if (nzchar(prefill_login_id)) {
        params <- c(params, paste0("client_reference_id=", prefill_login_id))
      }
      if (length(params) > 0) {
        paste0(base_link, "?", paste(params, collapse = "&"))
      } else {
        base_link
      }
    } else {
      base_link
    }
    
    div(
      style = paste0(
        "flex: 1; border-radius: 16px; padding: 28px 24px; ",
        "display: flex; flex-direction: column; position: relative; ",
        if (is_popular) {
          paste0("border: 2px solid ", accent_color, "; ",
                 "box-shadow: 0 8px 30px rgba(102,126,234,0.15);")
        } else {
          "border: 1px solid #e2e8f0;"
        }
      ),
      
      # Popular badge
      if (is_popular) {
        div(
          style = paste0(
            "position: absolute; top: -14px; left: 50%; ",
            "transform: translateX(-50%); ",
            "background: ", accent_color, "; color: white; ",
            "font-size: 11px; font-weight: 700; padding: 4px 16px; ",
            "border-radius: 20px; white-space: nowrap; letter-spacing: 0.5px;"
          ),
          "MOST POPULAR"
        )
      },
      
      # Plan name
      div(
        style = "margin-bottom: 6px;",
        tags$span(
          style = "font-size: 18px; font-weight: 700; color: #2d3748;",
          name
        )
      ),
      
      # Founding member badge
      div(
        style = "margin-bottom: 16px;",
        tags$span(
          style = "background: rgba(212,168,67,0.12);
                   color: #B8922E; font-size: 11px; font-weight: 600;
                   padding: 3px 10px; border-radius: 20px;",
          icon("star", style = "font-size: 9px; margin-right: 3px;"),
          "Founding Member Rate"
        )
      ),
      
      # Price
      if (!is_enterprise) {
        div(
          style = "margin-bottom: 6px;",
          tags$span(
            style = paste0("font-size: 42px; font-weight: 800; color: ",
                           accent_color, ";"),
            paste0("$", price)
          ),
          tags$span(
            style = "font-size: 13px; color: #a0aec0;",
            "/mo"
          )
        )
      } else {
        div(
          style = "margin-bottom: 6px;",
          tags$span(
            style = "font-size: 36px; font-weight: 800; color: #2d3748;",
            "Custom"
          )
        )
      },
      
      # Billing note
      if (!is_enterprise) {
        div(
          style = "font-size: 11px; color: #a0aec0; margin-bottom: 20px;",
          if (billing_cycle == "annual") {
            paste0("billed annually ($", price * 12, "/yr)")
          } else {
            "billed monthly"
          }
        )
      } else {
        div(
          style = "margin-bottom: 20px;",
          tags$span(style = "font-size: 12px; color: #718096;",
                    "Tailored to your organisation")
        )
      },
      
      # CTA button
      tags$a(
        href   = link,
        target = "_blank",
        style  = paste0(
          "display: block; text-align: center; padding: 12px; ",
          "border-radius: 10px; font-weight: 700; font-size: 14px; ",
          "text-decoration: none; margin-bottom: 24px; ",
          "transition: all 0.2s ease; ",
          if (is_popular) {
            paste0("background: ", accent_color, "; color: white;")
          } else {
            paste0("background: white; color: ", accent_color, "; ",
                   "border: 2px solid ", accent_color, ";")
          }
        ),
        if (is_enterprise) "Contact Sales" else "Get Started"
      ),
      
      # Divider
      div(style = "height: 1px; background: #f0f0f0; margin-bottom: 20px;"),
      
      # Features
      div(
        style = "flex: 1;",
        lapply(features, make_feature)
      )
    )
  }
  
  div(
    style = "max-width: 1000px; margin: 0 auto; padding: 20px;",
    
    # Heading
    div(
      style = "text-align: center; margin-bottom: 32px;",
      h2(style = "font-size: 28px; font-weight: 800; color: #1A1A1A;
                  margin-bottom: 8px;",
         "Choose your plan")
    ),
    
    # Billing toggle
    div(
      style = "display: flex; justify-content: center;
               align-items: center; gap: 12px; margin-bottom: 32px;",
      tags$span(
        style = paste0("font-size: 13px; font-weight: 600; ",
                       if (billing_cycle == "monthly") "color: #2d3748;"
                       else "color: #a0aec0;"),
        "Monthly"
      ),
      div(
        style = "position: relative; width: 48px; height: 26px;
                 background: #667eea; border-radius: 13px; cursor: pointer;",
        onclick = "Shiny.setInputValue('pricing_billing_toggle',
                   Math.random(), {priority: 'event'})",
        div(
          style = paste0(
            "position: absolute; top: 3px; width: 20px; height: 20px; ",
            "background: white; border-radius: 50%; ",
            "transition: left 0.2s ease; ",
            if (billing_cycle == "annual") "left: 25px;" else "left: 3px;"
          )
        )
      ),
      tags$span(
        style = paste0("font-size: 13px; font-weight: 600; ",
                       if (billing_cycle == "annual") "color: #2d3748;"
                       else "color: #a0aec0;"),
        "Annual"
      ),
      if (billing_cycle == "annual") {
        tags$span(
          style = "background: rgba(39,174,96,0.1); color: #27AE60;
                   font-size: 11px; font-weight: 700; padding: 3px 10px;
                   border-radius: 20px;",
          "Save ~23%"
        )
      }
    ),
    
    # Plan cards
    div(
      style = "display: flex; gap: 20px; align-items: stretch;",
      
      make_plan_card(
        name          = "Starter",
        monthly_price = 129,
        annual_price  = 99,
        monthly_link  = "https://buy.stripe.com/dRmeVfep5cSv0zq9Q9gEg04",
        annual_link   = "https://buy.stripe.com/00wbJ36WD2dRci86DXgEg03",
        features      = starter_features,
        accent_color  = "#3498DB",
        prefill_email    = prefill_email,
        prefill_login_id = prefill_login_id
      ),
      
      make_plan_card(
        name          = "Pro",
        monthly_price = 449,
        annual_price  = 349,
        monthly_link  = "https://buy.stripe.com/5kQ3cx3Kr8Cfci85zTgEg01",
        annual_link   = "https://buy.stripe.com/28E9AV6WD8Cfaa01jDgEg00",
        features      = pro_features,
        is_popular    = TRUE,
        accent_color  = "#667eea",
        prefill_email    = prefill_email,
        prefill_login_id = prefill_login_id
      ),
      
      make_plan_card(
        name          = "Enterprise",
        monthly_price = NULL,
        annual_price  = NULL,
        monthly_link  = "https://airrscore.com/pricing",
        annual_link   = "https://airrscore.com/pricing",
        features      = enterprise_features,
        is_enterprise = TRUE,
        accent_color  = "#D4A843",
        prefill_email    = prefill_email,
        prefill_login_id = prefill_login_id
      )
    ),
    
    # Footer
    div(
      style = "text-align: center; margin-top: 24px;
               font-size: 12px; color: #a0aec0;",
      icon("lock", style = "margin-right: 4px;"),
      "Secure payments via Stripe. Cancel anytime."
    )
  )
}