# ============================================================
# tabs/overviewTab.R
# ============================================================
# Overview Tab
#
# Landing page for the app. Content is drawn only from the project's
# framing/abstract: the background on data center growth, why it matters for
# communities, the two domains studied (economy, heat/environment),
# and the specific research gaps this project addresses.
# ============================================================

overview_tab <- tabPanel(
  
  "Overview",
  
  tags$div(
    
    class = "tab-header",
    
    tags$h1("The Impact of Data Centers on Communities"),
    
    tags$p(
      class = "subtitle",
      paste(
        "How hyperscale data center development affects the local economy, and the surrounding environment."
      )
    )
  ),
  
  fluidPage(
    
    tags$div(
      
      class = "overview-body",
      
      # ----------------------------------------------------
      # Background
      # ----------------------------------------------------
      tags$h2("Background [NEEDS TO BE WORKED ON]"),
      tags$p(
        "The rapid boom in cloud computing and large language models has led to an ",
        "unprecedented surge in investment in hyperscale data centers. In Virginia and ",
        "Texas — two states where data center construction has clustered — tax incentives ",
        "such as exemptions from sales tax on servers, software, and emergency generators ",
        "entice development. These incentives are offered on the assumption that the ",
        "development will bring economic prosperity to the area."
      ),
      tags$p(
        "[I NEED TO WORK ON THIS!!] But data centers require significant land, water, and electricity while providing ",
        "relatively few long-term jobs for their size. Nearby communities may face rising ",
        "land values, higher utility costs, environmental degradation, and quality-of-life ",
        "impacts such as noise pollution."
      ),
      
      # ----------------------------------------------------
      # Why it matters
      # ----------------------------------------------------
      tags$h2("Why It Matters for Communities"),
      tags$p(
        "This presents a clear challenge for policymakers, who must balance the influx of ",
        "capital investment against the best interests of their constituents. To make sound ",
        "long-term decisions about zoning, tax assessment, and utility planning, local ",
        "governments need to understand how data center development actually affects the ",
        "local economy and community."
      ),
      
      # ----------------------------------------------------
      # What this project studies
      # ----------------------------------------------------
      tags$h2("What This Project Studies"),
      tags$p("The project examines the impact of data centers across three domains:"),
      
      fluidRow(
        column(
          width = 4,
          tags$div(
            class = "overview-card",
            style = "background:#f6f8fa; border-radius:6px; padding:16px; height:100%;",
            tags$h3("Economy"),
            tags$p(
              "How data center openings affect the wider local economy — including ",
              "electricity costs borne by residents — beyond the fiscal contributions and ",
              "the few high-wage jobs the facilities create."
            )
          )
        ),
        column(
          width = 4,
          tags$div(
            class = "overview-card",
            style = "background:#f6f8fa; border-radius:6px; padding:16px; height:100%;",
            tags$h3("Heat & Environment"),
            tags$p(
              "The concentrated environmental stresses data centers place on their ",
              "surroundings: water depletion, localized heat, and noise."
            )
          )
        )
      ),
      
      tags$br(),
      
      # ----------------------------------------------------
      # Research gaps
      # ----------------------------------------------------
      tags$h2("Research Gaps We Address"),
      tags$p(
        "The existing literature has examined the impact of data center openings on land ",
        "values, documented how much energy data centers consume, and shown that they ",
        "contribute fiscally while creating a small number of high-paying jobs. Three ",
        "questions remain open:"
      ),
      tags$ul(
        tags$li(
          "How land values change ", tags$em("after"), " a data center opens — the long-term ",
          "trajectory, not just the effect at announcement or construction."
        ),
        tags$li(
          "How data centers' large electricity consumption affects the electricity costs of ",
          "the population living nearby."
        ),
        tags$li(
          "How data centers economically affect the residents who do not work for them, and ",
          "the area they live in, beyond the fiscal contribution."
        )
      ),
      tags$p(
        "This work focuses on filling these gaps — exploring the impact data centers have on ",
        "nearby land values, electricity costs, and the area's overall economy."
      ),
      
      # ----------------------------------------------------
      # How to use the dashboard
      # ----------------------------------------------------
      tags$h2("How to Use This Dashboard"),
      tags$ul(
        tags$li(tags$strong("Maps"), " — explore the spatial distribution of data centers and county classifications."),
        tags$li(tags$strong("Results"), " — view analysis outputs for each domain (built out as each analysis is completed)."),
        tags$li(tags$strong("Report"), " — read the detailed Land, Economy, and Heat reports, including methods and findings."),
        tags$li(tags$strong("Team"), " — meet the people behind the project.")
      )
    )
  )
)
