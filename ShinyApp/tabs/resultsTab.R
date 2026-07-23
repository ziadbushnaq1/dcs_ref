# ============================================================
# tabs/resultsTab.R
# ============================================================
results_tab <- tabPanel(
  "Results",
  fluidPage(
    tags$h2("Results"),
    
    tabsetPanel(
      
      tabPanel(
        "Economy",
        tags$h3("Economy Results"),
        uiOutput("economy_hypotheses_output"),
        tags$hr(),
        uiOutput("economy_model_equation"),
        tags$hr(),
        
        
        ######Event study plots 
        
        tags$h4("Event Study Explorer"),
        tags$p(
          "Event-study paths show the estimated effect in each year relative to the county's ",
          "first data center opening. Flat, near-zero estimates before year 0 support the ",
          "parallel-trends assumption; the path after 0 is the dynamic treatment effect. ",
          "Pick an outcome, control group, and specification to view its plot."
        ),
        selectInput("es_outcome", "Outcome", choices = NULL, width = "380px"),
        uiOutput("es_controls_ui"),
        imageOutput("economy_event_study_plot", height = "470px"),
        tags$hr(),
        
        #-------------- results reading 
        tags$h4("Results at a Glance (Forest Plot)"),
        tags$div(
          class = "reading-guide",
          style = "background:#f6f8fa; border-left:4px solid #0056b3; padding:12px 16px; border-radius:4px; margin-bottom:14px;",
          tags$p(
            "Each point is the overall ATT (estimated effect) for one outcome under one ",
            "specification and control group, and the horizontal bar is its 95% confidence ",
            "interval. The dashed line at 0 marks \"no effect\": a point whose interval crosses ",
            "0 is not statistically significant (grey), while a point whose interval falls ",
            "entirely to one side is significant at the 5% level (green). Points to the right of ",
            "0 are positive effects, points to the left are negative. Hover over any point to see ",
            "its exact ATT, confidence interval, and p-value. Use the checkboxes to add or remove ",
            "outcome groups, control groups, and specifications."
          )
        ),
        fluidRow(
          column(4, checkboxGroupInput("forest_sources", "Outcome groups",
                                       choices  = c("Main outcomes", "Sector employment", "Lights"),
                                       selected = "Main outcomes")),
          column(4, checkboxGroupInput("forest_control", "Control group",
                                       choices  = c("Never Treated", "Not Yet Treated"),
                                       selected = c("Never Treated", "Not Yet Treated"))),
          column(4, checkboxGroupInput("forest_spec", "Specification",
                                       choices  = c("No Covariates", "Lagged ACS + Race Controls"),
                                       selected = c("No Covariates", "Lagged ACS + Race Controls")))
        ),
        tags$div(
          style = "position:relative;",
          plotOutput("economy_forest_plot", height = "760px",
                     hover = hoverOpts("forest_hover", delay = 80, delayType = "debounce")),
          uiOutput("forest_hover_info")
        ),
        tags$hr(),
        
        #----------
        
        tags$h4("Overall Difference-in-Differences Results"),
        tags$p(
          "Overall ATT estimates for the CBP business outcomes and ACS socioeconomic outcomes. "
        ),
        # NOTE: results_-prefixed id (bound to the same builder as the Report
        # tab's economy_results_summary_table). Do NOT reuse the Report tab id
        # here — a Shiny output can only render into one element.
        DT::DTOutput("results_economy_results_summary_table"),
        tags$hr(),
        
        tags$h4("Employment by Sector Results"),
        tags$p(
          "Overall ATT estimates for ACS sector employment, shown across the available specifications and control groups. "
        ),
        DT::DTOutput("results_economy_sector_results_table"),
        tags$hr(),
        
        tags$h4("Artificial Light at Night Results"),
        tags$p(
          "Overall ATT estimates for all county-level VIIRS artificial-light-at-night outcomes. "
        ),
        # This id is unique to the Results tab, so it keeps its original name.
        DT::DTOutput("economy_lights_results_table"),
        tags$hr(),
        
        uiOutput("economy_interpretation_output")
      ),
      
      tabPanel("Heat Results",
               tags$h3("Surface Temperature Effects of Data Centers"),
               tags$p(class = "subtitle", textOutput("heat_subtitle")),
               tags$hr(),
               
               tags$h4("Headline Results"),
               tags$p("Each row answers one question about the effect of data centers
                       on nearby land surface temperature."),
               DT::DTOutput("heat_poster_tbl"),
               tags$br(),
               tags$em(textOutput("heat_footnote_txt")),
               tags$hr(),
               
               tags$h4("All Specifications"),
               tags$div(
                 class = "reading-guide",
                 style = "background:#f6f8fa; border-left:4px solid #0056b3; padding:12px 16px; border-radius:4px; margin-bottom:14px;",
                 tags$p(
                   "Each row is one model. ", tags$b("Operational"), " is the effect once the
                    facility is running; ", tags$b("Construction"), " is the effect during the
                    three years of site work beforehand. Estimates are in degrees Celsius per
                    treating facility within 600m, measured against control pixels 1000\u20131500m
                    away. Rows differ only in the fixed effects, the standard-error clustering,
                    and which nearby facilities disqualify a control pixel \u2014 the estimate
                    holding steady across all of them is the robustness result. Use the filter
                    boxes to narrow, or click a column header to sort."
                 )
               ),
               DT::DTOutput("heat_models_tbl"),
               tags$hr(),
               
               tags$h4("Seasonal Breakdown"),
               tags$p("Daytime surface warming is driven by solar loading, so the effect is
                       strongest in summer and near zero in winter."),
               DT::DTOutput("heat_seasonal_tbl"),
               tags$hr(),
               
               tags$details(
                 tags$summary(tags$b("What do these terms mean?")),
                 tags$br(),
                 DT::DTOutput("heat_glossary_tbl")
               )
      )
    )
  )
)