# ============================================================
# tabs/teamTab.R
# ============================================================

team_tab <- tabPanel(
  "Team",
  fluidPage(
    tags$h2("Meet the Team"),
    tags$p("This project was developed by:"),
    
    fluidRow(
      
      # Repeat this column block for each team member
      column(width = 3,
             tags$div(class = "team-card",
                      tags$img(src = "team/member1.jpg",  # place photos in www/team/
                               class = "team-photo",
                               alt  = "Team Member 1"),
                      tags$h4("Name"),          # <-- UPDATE
                      tags$p("Role / Title"),   # <-- UPDATE
                      tags$p("Affiliation"),    # <-- UPDATE
                      tags$a(href = "mailto:email@example.com", "email@example.com")
             )
      )
      
      # Add more columns here for additional team members
      
    ),
    
    tags$hr(),
    tags$h3("Acknowledgements"),
    tags$p("[Funding sources, advisors, data providers, etc.]")   # <-- UPDATE
  )
)