# ============================================
# Profile
# ============================================

output$profile_info <- renderUI({
  req(rv$logged_in)
  
  tagList(
    div(
      style = "text-align: center; padding: 20px;",
      icon("user-circle", class = "fa-5x", style = "color: #667eea;"),
      h3(style = "margin-top: 20px;", rv$brand_name),
      p(style = "color: #7f8c8d;", rv$email)
    ),
    hr(),
    div(
      style = "padding: 15px;",
      h4("Account Details"),
      p(strong("User ID: "), rv$login_id),
      p(strong("Brand: "), rv$brand_name),
      p(strong("Email: "), rv$email),
      p(strong("Member Since: "), format(Sys.Date(), "%B %Y"))
    )
  )
})