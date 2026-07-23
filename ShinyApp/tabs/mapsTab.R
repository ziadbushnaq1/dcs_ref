# ============================================================
# tabs/mapsTab.R
# ============================================================
# Maps tab
#
# Structure:
# Maps
# └── tabsetPanel
#     ├── Economics   (treatment map + group/year explanation + accordions:
#     │                Summary Table image, and reactive Distributions)
#     └── Heat        (sidebar + map)
#
# Land and Heat use the sidebar helper; Economics is full-width (no sidebar).
# ============================================================


# ============================================================
# Helper: Shared Sidebar
# ============================================================

map_sidebar <- function(county_id, ...) {
  
  tags$div(
    class = "control-panel",
    
    tags$h4("Controls"),
    tags$hr(),
    
    selectInput(
      inputId = county_id,
      label = "Filter by County",
      choices = "All",   # update server-side
      selected = "All"
    ),
    
    tags$hr(),
    
    ...
  )
}


# ============================================================
# Helper: Description Box
# ============================================================

desc_box <- function(title, text) {
  
  tags$div(
    class = "info-card",
    style = "margin-top: 12px;",
    
    tags$h5(title),
    tags$p(text)
  )
}

# ============================================================
# ECONOMICS TAB
# ============================================================
# Treatment map + data summary. Map, county-group explanation, then two
# accordions (native <details>): Summary Table and Distributions.
# Accordions are styled with a visible header bar and a caret so it is
# obvious they are clickable.

sub_economics <- tabPanel(
  
  "Economics",
  
  fluidRow(
    column(
      width = 12,
      
      # --- Treatment status map ------------------------------------------
      tags$h3("County Treatment Map"),
      tags$p(
        class = "subtitle",
        "Counties by analysis group. Hover over a county to see its name, the",
        "first known data center opening year, and the number of operational",
        "data centers located there."
      ),
      leaflet::leafletOutput("economics_map", height = "520px"),
      
      # --- What the groups and key years mean ----------------------------
      tags$div(
        class = "info-card",
        style = "margin-top:14px;",
        
        tags$h5("How counties are grouped"),
        tags$ul(
          tags$li("Treated: a county that received at least one data center whose opening year we know, with that first known opening in 2009 or later. These are the counties whose economy we test for a data center effect."),
          tags$li("Control: a county that never received a data center. These are the comparison counties, showing how the economy moved without one."),
          ),
        
        tags$h5("Which year means what", style = "margin-top:16px;"),
        tags$ul(
          tags$li(tags$b("2009 (analysis window start, treated cutoff): "),
                  "every model runs over 2009 to 2023. We begin in 2009 because that is the first year our ACS data is available. A county counts as Treated only if its first data center opened inside this window (2009 or later)."),
          tags$li(tags$b("2010 (effective start for controlled models): "),
                  "the models that include control variables lag those variables by one year. Because 2009 has no 2008 to draw the lag from, 2009 drops out of those models, so they effectively run 2010 to 2023. The baseline models without controls keep the full 2009 to 2023."),
          tags$li(tags$b("2023 (analysis window end): "),
                  "the last year of available data, so the study period ends here.")
        )
      ),
      
      tags$hr(style = "margin:22px 0;"),
      
      #-------------------
      # data centers growth map 
      tags$hr(style = "margin:22px 0;"),
      tags$h3("Growth of Data Centers Over Time"),
      tags$p(
        class = "subtitle",
        "Operational data centers by opening year. The dashed line marks 2009, the start of the analysis window."
      ),
      radioButtons(
        "dc_growth_mode", NULL,
        choices  = c("Cumulative total", "Newly opened per year"),
        selected = "Cumulative total", inline = TRUE
      ),
      plotOutput("dc_growth_plot", height = "360px"),
      
      # --- Data summary heading ------------------------------------------
      tags$h3("Economy Data Summary"),
      tags$p(
        class = "subtitle",
        "Descriptive statistics for the county-year panel used in the economy models.",
        "Each row of the underlying data is one county in one year (2009 to 2023)."
      ),
      
      # --- Accordion panel 1: summary table (collapsed) ------------------
      tags$details(
        open = NA,
        class = "econ-accordion",
        tags$summary(
          class = "econ-accordion-summary",
          tags$span(class = "econ-accordion-caret", "▶"),
          "Summary Table"
        ),
        tags$div(
          class = "econ-accordion-body",
          tags$p(
            "For each variable, this table reports the number of observations,",
            "the mean, the standard deviation (a measure of spread), and the",
            "minimum, median, and maximum. It describes the full sample the",
            "economy models are estimated on."
          ),
          tags$img(
            src = "economy_summary_table.png",
            style = "max-width:100%; height:auto; display:block; margin-top:8px;",
            alt = "Numeric summary table"
          )
        )
      ),
      
      # --- Accordion panel 2: interactive histograms (open by default) ---
      tags$details(
        open = NA,
        class = "econ-accordion",
        tags$summary(
          class = "econ-accordion-summary",
          tags$span(class = "econ-accordion-caret", "▶"),
          "Distributions"
        ),
        tags$div(
          class = "econ-accordion-body",
          selectInput(
            inputId = "econ_hist_var",
            label   = "Variable to display",
            choices = NULL,
            width   = "320px"
          ),
          checkboxInput(
            inputId = "econ_hist_log",
            label   = "Use log scale (helps for very skewed variables)",
            value   = FALSE
          ),
          uiOutput("econ_hist_desc"),
          plotOutput("econ_hist_plot", height = "360px")
        )
      )
    )
  )
)


# ============================================================
# HEAT TAB
# ============================================================
sub_heat <- tabPanel(
  
  "Heat",
  
  fluidRow(
    column(
      width = 12,
      
      tags$h3("Construction Time-Lapse"),
      tags$p(
        class = "subtitle",
        "Annual 1m aerial imagery of a hyperscale facility, before and after it",
        "became operational. Land clearing and building construction are visible",
        "in the years around the opening date."
      ),
      
      tags$div(
        style = "text-align:center;",
        tags$img(src = "timelapse/timelapse_2899.gif",
                 style = "max-width:720px; width:100%; border:1px solid #D3C0C8;")
      ),
      
      tags$div(
        class = "info-card",
        style = "margin-top:14px;",
        tags$h5("About this imagery"),
        tags$p(
          "Imagery: USDA NAIP via Google Earth Engine. This campus expanded to",
          "multiple buildings after its recorded opening year, but the facility",
          "inventory records it as a single point. Land classified as control in",
          "the analysis may therefore contain unrecorded buildings, which biases",
          "estimated temperature effects toward zero."
        )
      )
    )
  )
)

sub_dcmap <- tabPanel(
  "Data Center Locations",
  fluidRow(
    column(
      width = 12,
      tags$h3("U.S. Data Center Inventory"),
      tags$p(
        class = "subtitle",
        "All facilities in the datacentermap.com inventory, colored by facility",
        "type or development stage. Toggle to show only the facilities included",
        "in the temperature analysis."
      ),
      fluidRow(
        column(4, radioButtons("dc_map_color", "Color by",
                               choices = c("Facility type" = "capacity_type",
                                           "Stage" = "stage"),
                               selected = "capacity_type", inline = TRUE)),
        column(4, checkboxInput("dc_map_analysis_only",
                                "Analysis sample only (145 facilities)", FALSE))
      ),
      leaflet::leafletOutput("dc_location_map", height = "620px"),
      tags$p(class = "subtitle", textOutput("dc_map_count"))
    )
  )
)

sub_heatfigs <- tabPanel(
  "Temperature Figures",
  fluidRow(column(12,
                  tags$h3("Land Surface Temperature Analysis"),
                  tags$p(class = "subtitle",
                         "Key figures from the temperature analysis. Click any image to enlarge."),
                  uiOutput("heat_gallery")
  ))
)

# ============================================================
# MAIN MAPS TAB
# ============================================================

maps_tab <- tabPanel(
  
  "Maps/Graphs",
  
  tags$div(
    
    class = "tab-header",
    
    tags$h1("Maps & Spatial Analysis"),
    
    tags$p(
      class = "subtitle",
      
      paste(
        "Explore data center siting using",
        "multiple spatial perspectives including land",
        "availability, economics, and thermal conditions."
      )
    )
  ),
  
  tabsetPanel(
    
    id = "maps_subtabs",
    
    type = "tabs",
    
    sub_economics,
    sub_dcmap,
    sub_heat
  )
)