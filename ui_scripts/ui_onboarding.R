ui_onboarding <- function() {
  div(
    id = "onboarding_container",
    style = "max-width: 620px; margin: 40px auto;",
    
    # Progress bar
    uiOutput("onboarding_progress_bar"),
    
    br(),
    
    # Step panels
    uiOutput("onboarding_step_ui")
  )
}
