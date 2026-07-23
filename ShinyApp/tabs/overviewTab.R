# ============================================================
# tabs/overviewTab.R
# ============================================================
# Overview Tab
#
# Landing page for the app. Sections:
#   1. Background            (project framing, unchanged)
#   2. What This Project Studies
#   3. Research Gaps We Address
#   4. Methods               (economics complete, heat pending)
#   5. How to Use This Dashboard
#
# Collapsible method details use the .econ-accordion classes already
# defined in www/styles.css.
# ============================================================


# ============================================================
# Helper: collapsible methods panel
# ============================================================
# Matches the accordion pattern used on the Maps tab so the two pages
# behave the same way. Collapsed by default; pass open = TRUE to expand.

methods_panel <- function(title, ..., open = FALSE) {
  
  details_args <- list(
    class = "econ-accordion",
    
    tags$summary(
      class = "econ-accordion-summary",
      tags$span(class = "econ-accordion-caret", "\u25B6"),
      title
    ),
    
    tags$div(
      class = "econ-accordion-body",
      ...
    )
  )
  
  if (isTRUE(open)) details_args$open <- NA
  
  do.call(tags$details, details_args)
}


overview_tab <- tabPanel(
  
  "Overview",
  
  tags$div(
    
    class = "tab-header",
    
    tags$h1("The Impact of Data Centers on Communities"),
    
    tags$p(
      class = "subtitle",
      "How data center development affects the local economy, and the surrounding environment."
    )
  ),
  
  fluidPage(
    
    tags$div(
      
      class = "overview-body",
      
      # ----------------------------------------------------
      # 1. Background
      # ----------------------------------------------------
      tags$h2("Background"),
      tags$p(
        "The rapid boom in cloud computing and large language models
        has led to an unprecedented surge in investment in hyperscale
        data centers. In Virginia and Texas, for instance, two states
        where data center construction has clustered, tax incentives such as no sales tax on servers, software,
        and emergency generators entice development. These incentives are offered on the assumption that the development
        will bring economic prosperity to the area. However, although the jobs are well paid, data centers employ relatively
        few workers in the long run. There is currently little empirical evidence on the impact of data centers on local
        economies in the United States; therefore, it is unclear whether data centers boost local economies.
        Policymakers should be well-informed about the effects of these facilities on communities before offering incentives."
      ),
      
      # ----------------------------------------------------
      # 2. What this project studies
      # ----------------------------------------------------
      tags$h2("What This Project Studies"),
      tags$p(
        "This project examines the effects of data centers on communities along two axes: economic outcomes and heat
        impacts. The economic analysis uses a staggered difference-in-differences design and tests a wide set of outcomes,
        including employment by sector, labor force participation, unemployment, and income, while controlling for
        demographic and socioeconomic factors. The heat analysis uses a spatial difference-in-differences model comparing
        land surface temperature immediately around a facility to control areas farther out."
      ),
      
      fluidRow(
        column(
          width = 6,
          tags$div(
            class = "info-card",
            style = "height:100%;",
            tags$h3("Economy"),
            tags$p(
              "Whether a data center opening moves the wider local economy at all: jobs, payroll, businesses,
              income, and population, beyond the fiscal contribution and the small number of high-wage jobs
              the facility itself creates."
            )
          )
        ),
        column(
          width = 6,
          tags$div(
            class = "info-card",
            style = "height:100%;",
            tags$h3("Heat & Environment"),
            tags$p(
              "The concentrated environmental stresses data centers place on their surroundings, starting with
              localized surface warming during construction and once the facility is running."
            )
          )
        )
      ),
      
      tags$br(),
      
      # ----------------------------------------------------
      # 3. Research gaps
      # ----------------------------------------------------
      tags$h2("Research Gaps We Address"),
      tags$p(
        "Existing work has documented how much energy data centers consume and that they contribute fiscally while creating a small number of high-paying jobs. What that literature
        does not answer is whether anyone else in the county is better off. Three questions remain open:"
      ),
      tags$ul(
        tags$li(
          "Whether a data center opening produces any measurable change in the ", tags$em("host county's"),
          " economy: employment, payroll, establishments, income, and population, for residents who do not
          work at the facility."
        ),
        tags$li(
          "Whether any single industry absorbs the effect. Construction, utilities, and information are the
          sectors a data center would plausibly move, and they are rarely separated out."
        ),
        tags$li(
          "Whether the facilities measurably warm the land around them, and how that warming splits between
          the construction phase and normal operation."
        )
      ),
      tags$p(
        "This dashboard reports what we find on each, including the results that came back null."
      ),
      
      # ----------------------------------------------------
      # 4. Methods
      # ----------------------------------------------------
      tags$h2("Methods"),
      
      tags$h3("Economic Analysis"),
      
      # --- Hypotheses ---------------------------------------------------
      tags$div(
        class = "info-card",
        tags$h4("Hypotheses", style = "margin-top:0;"),
        tags$p("Every outcome is tested against the same pair of hypotheses:"),
        tags$ul(
          tags$li(
            tags$strong("Null hypothesis (H", tags$sub("0"), "): "),
            "a data center opening has ", tags$strong("no effect"), " on the county's economic outcome."
          ),
          tags$li(
            tags$strong("Alternative hypothesis (H", tags$sub("1"), "): "),
            "a data center opening has ", tags$strong("an effect"), " on the outcome, meaning the effect
            is different from zero."
          )
        ),
      ),
      
      # --- Design -------------------------------------------------------
      tags$h4("Design"),
      tags$p(
        "The analysis uses a staggered difference-in-differences design (Callaway and Sant'Anna) to estimate
        whether counties experience measurable economic change after their first operational data center opens.
        Because counties are treated in different years, the estimator recovers a separate effect for each
        treatment cohort and each year, then aggregates them into one overall average treatment effect on the
        treated (ATT) and into an event-study path."
      ),
      
      withMathJax(
        tags$div(
          class = "equation-box",
          "$$ ATT(g,t) = E[Y_t(g) - Y_t(0) \\mid G_i = g] $$",
          "$$ Y_{it} = \\alpha_i + \\lambda_t + \\beta D_{it} + X_{it}'\\gamma + \\varepsilon_{it} $$"
        )
      ),
      
      methods_panel(
        "What the terms mean",
        
        tags$ul(
          tags$li(tags$strong("ATT(g, t): "), "the average treatment effect at time ", tags$em("t"),
                  " for the cohort of counties first treated in period ", tags$em("g"), "."),
          tags$li(tags$strong("Y", tags$sub("t"), "(g): "), "the outcome at time ", tags$em("t"),
                  " for a county treated starting in period ", tags$em("g"), "."),
          tags$li(tags$strong("Y", tags$sub("t"), "(0): "), "the counterfactual outcome for that same county
                  had it never been treated."),
          tags$li(tags$strong("G", tags$sub("i"), ": "), "the period in which county ", tags$em("i"),
                  " received its first data center, meaning its treatment cohort."),
          tags$li(tags$strong("Y", tags$sub("it"), ": "), "the outcome for county ", tags$em("i"),
                  " in year ", tags$em("t"), "."),
          tags$li(tags$strong("\u03B1", tags$sub("i"), ": "), "county fixed effects, absorbing fixed
                  differences between counties."),
          tags$li(tags$strong("\u03BB", tags$sub("t"), ": "), "year fixed effects, absorbing shocks common
                  to all counties in a given year."),
          tags$li(tags$strong("D", tags$sub("it"), ": "), "the treatment indicator, equal to 1 once county ",
                  tags$em("i"), " has an operational data center in year ", tags$em("t"), ". ",
                  tags$strong("\u03B2"), " is its coefficient."),
          tags$li(tags$strong("X", tags$sub("it"), ": "), "county covariates, entered with a one-year lag. ",
                  tags$strong("\u03B3"), " is their coefficient vector."),
          tags$li(tags$strong("\u03B5", tags$sub("it"), ": "), "the error term, clustered by county.")
        )
      ),
      
      # --- Sample and timing --------------------------------------------
      methods_panel(
        "Sample and treatment timing",
        
        tags$p(
          "A county is ", tags$strong("treated"), " if its first known operational data center opened in 2009
          or later. A county is a ", tags$strong("control"), " if it never received one. Counties whose only
          data centers predate 2009, and counties with a missing opening year, are excluded from the analysis
          sample."
        ),
        tags$p(
          "Treatment timing is the year of that first opening, recoded so that 2009 is period 1, 2010 is
          period 2, and so on. Never-treated counties are assigned group 0. Each model is estimated against
          two comparison groups, never-treated counties and not-yet-treated counties, so the result does not
          rest on a single choice of control."
        ),
        tags$ul(
          tags$li(tags$strong("2009: "), "the analysis window opens here, the first year of available ACS data,
                  and it is also the cutoff for counting a county as treated."),
          tags$li(tags$strong("2010: "), "the effective start for models with control variables. Covariates are
                  lagged one year, and 2009 has no 2008 to draw from, so it drops out of those models."),
          tags$li(tags$strong("2023: "), "the last year of available data, so the window ends here.")
        ),
        tags$p(
          "County-level sample counts and the treatment cohort table are on the Maps/Graphs tab."
        )
      ),
      
      # --- Outcomes -----------------------------------------------------
      methods_panel(
        "Outcomes measured",
        
        tags$p("Outcomes fall into four families:"),
        tags$ul(
          tags$li(
            tags$strong("Business activity (County Business Patterns): "),
            "employment, real annual payroll in 2017 dollars, and establishments. All log-transformed."
          ),
          tags$li(
            tags$strong("Household and labor market (ACS 5-year): "),
            "unemployment rate and labor force participation rate in percentage points, plus log median
            household income, log per capita income, log median income for female and male earners, and
            log population."
          ),
          tags$li(
            tags$strong("Sector employment (ACS, 13 sectors): "),
            "employment split by industry, to test whether an effect that is invisible in the county total
            is concentrated in one sector such as construction, utilities, or information."
          ),
          tags$li(
            tags$strong("Artificial light at night (VIIRS): "),
            "county radiance, used as an independent satellite check on the survey and administrative
            measures. Radiance tracks built activity and is not derived from ACS or CBP, so if real growth
            were occurring and the economic series were missing it, radiance would be expected to pick it up."
          )
        ),
        tags$p(
          tags$strong("Reading the estimates: "),
          "for logged outcomes, an ATT of 0.05 is roughly a 5% change. For rate outcomes, an ATT of 0.05 is a
          0.05 percentage-point change, not 5%."
        )
      ),
      
      # --- Controls -----------------------------------------------------
      methods_panel(
        "Control variables",
        
        tags$p(
          "The controlled specifications add county covariates entered with a ", tags$strong("one-year lag"),
          ": log population, log median household income, percent with a bachelor's degree or higher, percent
          high-school graduate or higher, and racial composition shares (white, Black, Hispanic, Asian, and
          American Indian or Alaska Native)."
        ),
        tags$p(
          "Lagging pairs each year's outcome with the county's characteristics from the year before, so the
          controls are not themselves affected by that year's treatment. Controlling for a variable the
          treatment has already moved would absorb part of the effect being estimated."
        ),
        tags$p(
          "Every outcome is run both with and without these covariates. Where the two specifications disagree,
          both are reported rather than one being selected after the fact."
        )
      ),
      
      # --- Heat placeholder ---------------------------------------------
      tags$h3("Heat & Environment Analysis"),
      tags$div(
        class = "info-card",
        tags$p(
          tags$em("Needs to be worked on.")
        )
      ),
      
      # ----------------------------------------------------
      # 5. How to use the dashboard
      # ----------------------------------------------------
      tags$h2("How to Use This Dashboard"),
      tags$ul(
        tags$li(
          tags$strong("Maps/Graphs"), ": the county treatment map, the growth of data centers over time, and
          the descriptive statistics and distributions behind the economic panel."
        ),
        tags$li(
          tags$strong("Results"), ": economic and heat findings. Event-study plots, a forest plot of every
          overall ATT with its confidence interval, and sortable tables for the main outcomes, sector
          employment, and nighttime lights. Each results section includes a guide to reading the numbers."
        ),
        tags$li(
          tags$strong("Team"), ": the people behind the project."
        )
      )
    )
  )
)