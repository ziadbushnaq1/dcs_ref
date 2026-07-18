# ============================================================
# tabs/reportTab.R
# ============================================================
# Report Tab
#
# Structure:
# Report
# └── tabsetPanel
#     ├── Economy Report  
#     └── Heat Report     
#
# Server-backed tables (defined in app.R):
#   - economy_outcomes_definitions_table
#   - economy_methods_table
#   - economy_results_summary_table
#   - economy_sector_results_table
# ============================================================


# ============================================================
# Helper: Report Section
# ============================================================

report_section <- function(title, sections) {
  
  tabPanel(
    
    title,
    
    fluidPage(
      
      tags$br(),
      
      fluidRow(
        column(
          width = 12,
          
          downloadButton(
            "downloadReport",
            "Download PDF Report",
            class = "btn-primary"
          )
        )
      ),
      
      tags$hr(),
      
      tags$div(
        class = "report-body",
        
        sections
      )
    )
  )
}


# ============================================================
# ECONOMY REPORT
# ============================================================

economy_report <- report_section(
  
  "Economy Report",
  
  tagList(
    
    tags$h2("Economic Analysis"),
    
    # ----------------------------------------------------
    # 1. Introduction
    # ----------------------------------------------------
    tags$h3("1. Introduction"),
    tags$p(
      "Data centers are often promoted as engines of local economic development: they ",
      "contribute fiscally, drive infrastructure investment, and, although they employ ",
      "relatively few workers for their physical size, those workers are well paid. The ",
      "average employee in Virginia data centers earned roughly $134,308 in 2020, more ",
      "than double the state's average private-sector wage (Mullin 2023)."
    ),
    tags$p(
      "That prosperity, however, does not appear to reach the broader community. Ngata et ",
      "al. (2025) project that Loudoun County residents will face an electricity bill ",
      "increase of about $37 per month by 2040 — meaning nearby residents may pay more for ",
      "electricity rather than benefit from the development. Data centers are energy ",
      "intensive: their share of national electricity demand grew from just under 2% in ",
      "2018 to about 4.4% in 2023, and could reach 6.7%–12% by 2028 (Kahrl et al. 2025). ",
      "Meeting that demand requires new transmission lines and substations, and regulatory ",
      "gaps in cost allocation have allowed utilities to pass much of this cost to ",
      "residential ratepayers — over $4.3 billion across seven PJM Interconnection states ",
      "in 2024 alone (Kahrl et al. 2025)."
    ),
    tags$p(
      "These systemic costs extend beyond the counties that host the facilities. West ",
      "Virginia ratepayers are projected to bear over $440 million in infrastructure ",
      "upgrade costs for projects built primarily to serve Virginia data centers, and ",
      "analysis by Energy and Environmental Economics (E3, 2026) attributes roughly half of ",
      "recent PJM capacity price spikes to data-center-driven load growth. Muller (2026) ",
      "estimates that operational data centers consumed about 250 TWh of electricity in ",
      "2025, generating approximately $25 billion in gross external damages from air ",
      "pollutants and greenhouse gas emissions, concentrated in Virginia and Texas."
    ),
    tags$p(
      "Prior work documents the fiscal contributions of data centers and the small number ",
      "of high-wage jobs they create, but it remains unclear how these facilities affect ",
      "the economic well-being of residents who do not work for them, and of the area more ",
      "broadly. This report uses a staggered difference-in-differences design to estimate ",
      "how a county's economy changes after its first data center opens."
    ),
    
    # ----------------------------------------------------
    # 2. Data
    # ----------------------------------------------------
    tags$h3("2. Data"),
    tags$p(
      "County economic outcomes come from the Census County Business Patterns (CBP), the ",
      "American Community Survey (ACS) 5-year estimates, and County Health income ",
      "measures. The table below defines each regression outcome with its source, units, ",
      "and interpretation."
    ),
    tags$h4("Outcome Definitions"),
    tags$p("Click an outcome for full details, interpretation guidance, and source information."),
    DT::DTOutput("economy_outcomes_definitions_table"),
    
    tags$h4("Sample Composition"),
    tags$p("County counts for the analysis sample and the excluded groups."),
    uiOutput("economy_sample_composition"),
    tags$br(),
    DT::DTOutput("economy_cohort_table"),
    
    # ----------------------------------------------------
    # 3. Methods
    # ----------------------------------------------------
    tags$h3("3. Methods"),
    
    tags$h4("Hypotheses"),
    tags$p("For every outcome, the analysis tests the same pair of hypotheses:"),
    tags$ul(
      tags$li(
        tags$strong("Null hypothesis (H", tags$sub("0"), "): "),
        "a data center opening has ", tags$strong("no effect"), " on the county's economic outcome."
      ),
      tags$li(
        tags$strong("Alternative hypothesis (H", tags$sub("1"), "): "),
        "a data center opening has ", tags$strong("an economic effect"), " on the outcome ",
        "(the effect is different from zero)."
      )
    ),
    
    tags$p(
      "The economic analysis uses a staggered difference-in-differences design ",
      "(Callaway and Sant'Anna) to estimate whether counties experience measurable ",
      "economic changes after their first operational data center opens. Counties whose ",
      "first operational data center opened between 2009 and 2023 are treated; counties ",
      "with no operational data center serve as controls. Counties with only pre-2007 ",
      "data centers or missing opening years are excluded from the analysis sample."
    ),
    
    tags$p(
      "Treatment timing is the year of the first data center opening, recoded so that ",
      "2009 = period 1, 2010 = period 2, and so on. Never-treated counties are assigned ",
      "group 0. Estimation produces group-time average treatment effects, aggregated to ",
      "an overall ATT and to an event-study (dynamic) path."
    ),
    
    uiOutput("report_equation"),
    
    tags$p("Where the terms are defined as follows:"),
    tags$ul(
      tags$li(tags$strong("ATT(g, t) "), "— the average treatment effect at time ", tags$em("t"),
              " for the cohort of counties first treated in period ", tags$em("g"),
              " (the group-time average treatment effect)."),
      tags$li(tags$strong("Y", tags$sub("t"), "(g) "), "— the outcome at time ", tags$em("t"),
              " for a county treated starting in period ", tags$em("g"), "."),
      tags$li(tags$strong("Y", tags$sub("t"), "(0) "), "— the counterfactual outcome at time ",
              tags$em("t"), " for that same county had it never been treated."),
      tags$li(tags$strong("G", tags$sub("i"), " "), "— the period in which county ", tags$em("i"),
              " received its first data center (its treatment cohort); ", tags$em("E[·]"),
              " denotes the average over counties in that cohort."),
      tags$li(tags$strong("Y", tags$sub("it"), " "), "— the outcome for county ", tags$em("i"),
              " in year ", tags$em("t"), "."),
      tags$li(tags$strong("α", tags$sub("i"), " "), "— county fixed effects, absorbing fixed ",
              "differences between counties."),
      tags$li(tags$strong("λ", tags$sub("t"), " "), "— year fixed effects, absorbing shocks ",
              "common to all counties in a given year."),
      tags$li(tags$strong("D", tags$sub("it"), " "), "— the treatment indicator (equal to 1 once ",
              "county ", tags$em("i"), " has an operational data center in year ", tags$em("t"),
              "); ", tags$strong("β"), " is its coefficient, the effect of treatment."),
      tags$li(tags$strong("X", tags$sub("it"), " "), "— the vector of county covariates, entered ",
              "with a one-year lag (see below); ", tags$strong("γ"), " is their coefficient vector."),
      tags$li(tags$strong("ε", tags$sub("it"), " "), "— the error term, clustered by county.")
    ),
    
    tags$p(
      "Outcomes span three families: CBP business outcomes (employment, real annual ",
      "payroll, establishments), ACS socioeconomic outcomes (unemployment and ",
      "labor-force participation rates, plus log income and population measures), and ",
      "ACS sector employment across 13 industry sectors. CBP business outcomes and all ",
      "income and population measures are log-transformed; the unemployment and ",
      "labor-force participation rates are estimated in levels (percentage points)."
    ),
    
    tags$p(
      "The ACS-controlled specifications add county covariates entered with a ",
      tags$strong("one-year lag"), " (the vector ", tags$strong("X", tags$sub("it")),
      " above, evaluated at ", tags$em("t"), " − 1): log population, log median household ",
      "income, percent with a bachelor's degree or higher, percent high-school graduate, ",
      "and racial composition shares (white, Black, Hispanic, Asian, and American ",
      "Indian/Alaska Native). Lagging pairs each year's outcome with the county's ",
      tags$em("pre-period"), " characteristics, so the controls are not themselves affected ",
      "by that year's treatment (avoiding \"bad-control\" bias)."
    ),
    tags$p(
      "The lag has one implication worth noting for the study window. Because each ",
      "covariate is taken from the prior year, the first year of ACS data (2009) has no ",
      "lagged value to draw on and drops out of the controlled models. So while the raw ",
      "regression window is 2009–2023, the ", tags$strong("ACS-controlled"), " specifications ",
      "effectively estimate over ", tags$strong("2010–2023"), ". The uncontrolled baseline ",
      "(Section 9), which uses no covariates, retains the full 2009–2023 window."
    ),
    tags$p(
      "Sector employment is analyzed using ACS sector employment (DP03_0033-0045) rather ",
      "than CBP NAICS, in two specifications — without covariates and with the lagged ",
      "covariates — each estimated against both never-treated and not-yet-treated control ",
      "groups."
    ),
    
    # --- Methods specification table temporarily removed ---------------------
    # Commented out to avoid repeating the outcome list already shown in the
    # Outcome Definitions table above. The methods table carried the model
    # specs (control group, covariates, transformation, years, status). To
    # restore: uncomment the two lines below AND the matching
    # output$economy_methods_table render block in app.R (server).
    # tags$h4("Economic Model Specification Table"),
    # DT::DTOutput("economy_methods_table"),
    
    # ----------------------------------------------------
    # 4. Results
    # ----------------------------------------------------
    tags$h3("4. Results"),
    tags$p(
      "The tables below report the overall average treatment effect (ATT) for each ",
      "outcome. Before reading them, the guide below explains what each column means and ",
      "how to judge whether an estimate supports the null or the alternative hypothesis."
    ),
    
    tags$h4("How to Read the Results Table"),
    tags$div(
      class = "reading-guide",
      style = "background:#f6f8fa; border-left:4px solid #0056b3; padding:12px 16px; border-radius:4px;",
      
      tags$p(
        tags$strong("ATT (the estimate). "),
        "The average effect of a data center opening on the outcome. For logged outcomes ",
        "(employment, payroll, establishments, income, population) an ATT of 0.05 means ",
        "roughly a 5% change. For rate outcomes (unemployment, labor-force participation) ",
        "an ATT of 0.05 means a 0.05 percentage-point change — not 5%. A positive ATT ",
        "(shown in green) means an increase; a negative ATT (red) means a decrease."
      ),
      
      tags$p(
        tags$strong("p-value. "),
        "The probability of seeing an estimate at least this large if the null hypothesis ",
        "of ", tags$em("no effect"), " were true. A small p-value is evidence against ",
        "\"no effect.\" The Result column translates the p-value into plain language:"
      ),
      tags$ul(
        tags$li(
          tags$strong("Strong evidence"), " (p < 0.01): very unlikely to be a fluke; strong ",
          "evidence of a real economic effect."
        ),
        tags$li(
          tags$strong("Statistically significant"), " (p < 0.05): by the conventional ",
          "threshold, we reject the null of no effect and conclude there is an effect."
        ),
        tags$li(
          tags$strong("Marginally significant"), " (p < 0.10): suggestive but weaker; it ",
          "does not clear the usual 5% bar, so treat it as a hint rather than a conclusion."
        ),
        tags$li(
          tags$strong("Not significant"), " (p ≥ 0.10): the data cannot distinguish the ",
          "effect from zero; we do not reject the null of no effect."
        )
      ),
      
      tags$p(
        tags$strong("The 95% confidence interval and the role of 0. "),
        "The interval (95% CI Lower to 95% CI Upper) is the range of effect sizes ",
        "consistent with the data. The key thing to check is whether the interval ",
        tags$strong("contains 0"), ". If it does, then \"no effect\" is a plausible value, ",
        "and the estimate is not statistically significant at the 5% level. If the interval ",
        "lies entirely above or entirely below 0, the effect is significant at 5% and we ",
        "reject the null. The p-value and the interval always agree on this: p < 0.05 ",
        "corresponds to an interval that excludes 0."
      ),
      
      tags$p(
        tags$strong("A note of caution. "),
        "Statistical significance is not the same as practical importance — a tiny effect ",
        "can be significant in a large sample. And some estimates rest on small numbers of ",
        "treated counties (especially the covariate-controlled sector results), so read ",
        "those as suggestive rather than definitive."
      )
    ),
    
    tags$p(
      "Overall ATT estimates for the CBP business outcomes (ACS-controlled) and the ACS ",
      "socioeconomic outcomes:"
    ),
    DT::DTOutput("economy_results_summary_table"),
    
    tags$h4("Sector Employment Results"),
    tags$p(
      "Overall ATT estimates for ACS sector employment, shown for both specifications ",
      "(no covariates, and lagged ACS + race controls) and both control groups. Use the ",
      "Specification and Control Group filters to compare. Sector treatment cohorts are ",
      "small, so the covariate-controlled estimates in particular should be read as ",
      "suggestive rather than precise."
    ),
    DT::DTOutput("economy_sector_results_table"),
    
    # ----------------------------------------------------
    # 5. Discussion
    # ----------------------------------------------------
    tags$h3("5. Discussion"),
    tags$p("[Interpret economic findings.]"),
    
    # ----------------------------------------------------
    # 6. Conclusion
    # ----------------------------------------------------
    tags$h3("6. Conclusion"),
    tags$p("[Summarize economic conclusions.]"),
    
    tags$h3("References"),
    tags$p(tags$em("Full bibliographic details to be completed.")),
    tags$ul(
      tags$li("Mullin (2023) — data center employment and wages in Virginia."),
      tags$li("Ngata et al. (2025) — residential electricity cost impacts near data centers (Loudoun County, VA)."),
      tags$li("Kahrl, F. et al. (2025) — data center electricity demand growth and PJM transmission cost allocation."),
      tags$li("Energy and Environmental Economics (E3) (2026) — drivers of PJM capacity price increases."),
      tags$li("Muller, N. Z. (2026) — gross external damages from data center electricity consumption.")
    )
  )
)


# ============================================================
# HEAT REPORT
# ============================================================

heat_report <- report_section(
  
  "Heat Report",
  
  tagList(
    
    tags$h2("Heat & Environmental Analysis"),
    
    # ----------------------------------------------------
    # 1. Introduction
    # ----------------------------------------------------
    tags$h3("1. Introduction"),
    tags$p(
      "..."
    ),
    
    
    # ----------------------------------------------------
    # 2. Data
    # ----------------------------------------------------
    tags$h3("2. Data"),
    tags$p("[To be added as the heat/environmental analysis is developed]"),
    
    # ----------------------------------------------------
    # 3. Methods
    # ----------------------------------------------------
    tags$h3("3. Methods"),
    tags$p("[To be added]"),
    
    # ----------------------------------------------------
    # 4. Results
    # ----------------------------------------------------
    tags$h3("4. Results"),
    tags$p("[To be added once the heat/environmental analysis is complete.]"),
    
    # ----------------------------------------------------
    # 5. Discussion
    # ----------------------------------------------------
    tags$h3("5. Discussion"),
    tags$p("[To be added]"),
    
    # ----------------------------------------------------
    # 6. Conclusion
    # ----------------------------------------------------
    tags$h3("6. Conclusion"),
    tags$p("[Summarize conclusions.]"),
    
    tags$h3("References"),
    tags$p(tags$em("Full bibliographic details to be completed.")),
    tags$ul(
      tags$li("...")
    )
  )
)


# ============================================================
# MAIN REPORT TAB
# ============================================================

report_tab <- tabPanel(
  
  "Report",
  
  tags$div(
    
    class = "tab-header",
    
    tags$h1("Project Reports"),
    
    tags$p(
      class = "subtitle",
      
      paste(
        "Explore detailed reports covering",
        "economic impacts, and heat-related analysis."
      )
    )
  ),
  
  tabsetPanel(
    
    id = "report_subtabs",
    
    type = "tabs",
    
    economy_report,
    
    heat_report
  )
)

# ============================================================
# SERVER ADDITIONS NEEDED:
# ============================================================
#
# output$downloadReport <- downloadHandler(
#   filename = function() paste0("report_", Sys.Date(), ".pdf"),
#   content  = function(file) {
#     rmarkdown::render("report/report.Rmd",
#                       output_format = "pdf_document",
#                       output_file   = file,
#                       envir         = new.env(parent = globalenv()))
#   }
# )