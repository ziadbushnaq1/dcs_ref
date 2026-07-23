source("global.R")

# 1. SOURCE UI TABS
source("tabs/overviewTab.R")
source("tabs/mapsTab.R")
source("tabs/resultsTab.R")
source("tabs/reportTab.R")
source("tabs/teamTab.R")

# 2. DEFINE UI
ui <- navbarPage(
  title = "Data centers impact on communities",
  fluid = TRUE,
  header = tags$head(
    withMathJax(),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  overview_tab,
  maps_tab,
  results_tab,
  report_tab,
  team_tab
)

# ============================================================
# 3. DEFINE SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ==========================================================
  # SHARED TABLE HELPERS
  # ==========================================================
  # These are used by BOTH the Report tab and the Results tab so the two
  # tabs stay in sync without duplicating render code. (The duplicate-output-ID
  # problem — same DTOutput id in reportTab.R and resultsTab.R — is why the
  # economy + sector tables previously rendered only in the Report tab.)
  
  add_result_col <- function(df) {
    df %>%
      mutate(
        Result = case_when(
          `p-value` < 0.01 ~ "Strong evidence",
          `p-value` < 0.05 ~ "Statistically significant",
          `p-value` < 0.10 ~ "Marginally significant",
          TRUE             ~ "Not significant"
        )
      )
  }
  
  style_att_table <- function(dt) {
    dt %>%
      DT::formatRound(
        columns = c("ATT", "SE", "95% CI Lower", "95% CI Upper", "p-value"),
        digits  = 3
      ) %>%
      DT::formatStyle(
        "Result",
        backgroundColor = DT::styleEqual(
          c("Strong evidence", "Statistically significant", "Marginally significant", "Not significant"),
          c("#d4edda",         "#e2f0d9",                    "#fff3cd",                "#f8d7da")
        )
      ) %>%
      DT::formatStyle(
        "ATT",
        color = DT::styleInterval(0, c("#b00020", "#006400")),
        fontWeight = "bold"
      )
  }
  
  # ----- Overall DiD results table (CBP + ACS outcomes) ------------------
  economy_results_dt <- function() {
    validate(need(
      file.exists("report/results_summary.rds"),
      "results_summary.rds not found. Re-knit the analysis Rmd (Section 13) to write it."
    ))
    
    readRDS("report/results_summary.rds") %>%
      add_result_col() %>%
      DT::datatable(
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons", "Responsive"),
        options = list(
          pageLength = 15,
          autoWidth = TRUE,
          responsive = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          order = list(list(1, "asc"))
        ),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-weight: bold; font-size: 18px;",
          "Overall Difference-in-Differences Results"
        )
      ) %>%
      style_att_table()
  }
  
  #------------growth table 
  
  output$dc_growth_plot <- renderPlot({
    validate(need(
      file.exists("report/dc_growth.rds"),
      "dc_growth.rds not found. Re-knit the analysis Rmd (Section 3)."
    ))
    g <- readRDS("report/dc_growth.rds")
    
    if (identical(input$dc_growth_mode, "Newly opened per year")) {
      p <- ggplot2::ggplot(g, ggplot2::aes(year, n_opened)) +
        ggplot2::geom_col(fill = "#4C78A8")
      ylab <- "Data centers opening each year"
    } else {
      p <- ggplot2::ggplot(g, ggplot2::aes(year, cumulative_dcs)) +
        ggplot2::geom_area(fill = "#4C78A8", alpha = 0.2) +
        ggplot2::geom_line(color = "#4C78A8", linewidth = 1) +
        ggplot2::geom_point(color = "#4C78A8", size = 1.8)
      ylab <- "Cumulative operational data centers"
    }
    
    p +
      ggplot2::geom_vline(xintercept = 2008.5, linetype = "dashed", color = "#d7191c") +
      ggplot2::labs(title = "Growth of operational data centers", x = "Year", y = ylab) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        axis.title = ggplot2::element_text(face = "bold")
      )
  })
  
  ###### event study plots 
  
  
  observe({
    if (file.exists("report/event_study_index.rds")) {
      idx <- readRDS("report/event_study_index.rds")
      updateSelectInput(session, "es_outcome", choices = sort(unique(idx$Outcome)))
    }
  })
  
  output$es_controls_ui <- renderUI({
    req(input$es_outcome)
    idx <- readRDS("report/event_study_index.rds")
    sub <- idx[idx$Outcome == input$es_outcome, ]
    tagList(
      selectInput("es_control", "Control group", choices = sort(unique(sub$`Control Group`))),
      selectInput("es_spec",    "Specification", choices = sort(unique(sub$Specification)))
    )
  })
  
  output$economy_event_study_plot <- renderImage({
    validate(need(file.exists("report/event_study_index.rds"),
                  "event_study_index.rds not found. Re-knit the analysis Rmd (Section 14)."))
    req(input$es_outcome, input$es_control, input$es_spec)
    idx <- readRDS("report/event_study_index.rds")
    row <- idx[idx$Outcome == input$es_outcome &
                 idx$`Control Group` == input$es_control &
                 idx$Specification == input$es_spec, ]
    validate(need(nrow(row) > 0, "No event study available for this combination."))
    list(src = file.path("www", "event_studies", row$file[1]),
         contentType = "image/png", width = 820,
         alt = paste("Event study:", input$es_outcome))
  }, deleteFile = FALSE)
  
  #------------ results visualization 
  
  # Shared data build for the forest plot (also read by the hover handler).
  forest_data <- reactive({
    srcs <- input$forest_sources
    validate(need(length(srcs) > 0, "Select at least one outcome group."))
    
    load_one <- function(path, source_label) {
      if (!file.exists(path)) return(NULL)
      readRDS(path) %>% dplyr::filter(!is.na(ATT)) %>% dplyr::mutate(Source = source_label)
    }
    
    parts <- list()
    if ("Main outcomes" %in% srcs)
      parts <- c(parts, list(load_one("report/results_summary.rds", "Main")))
    if ("Sector employment" %in% srcs)
      parts <- c(parts, list(load_one("report/sector_summary.rds", "Sector")))
    if ("Lights" %in% srcs) {
      lp <- c("report/lights_results_summary.rds", "outputs/lights_results_summary.rds")
      lp <- lp[file.exists(lp)][1]
      if (!is.na(lp)) {
        ldf <- readRDS(lp) %>%
          dplyr::filter(!is.na(ATT)) %>%
          dplyr::mutate(
            Source = "Lights",
            `Control Group` = dplyr::recode(`Control Group`,
                                            "nevertreated"  = "Never Treated",
                                            "notyettreated" = "Not Yet Treated"),
            Specification = dplyr::if_else(Covariates == "Yes",
                                           "Lagged ACS + Race Controls", "No Covariates")
          )
        parts <- c(parts, list(ldf))
      }
    }
    
    df <- dplyr::bind_rows(parts)
    validate(need(nrow(df) > 0, "No results found for the selected groups."))
    if (!"Specification" %in% names(df)) df$Specification <- "n/a"
    df$Specification[is.na(df$Specification)] <- "n/a"
    
    if (length(input$forest_control) > 0)
      df <- dplyr::filter(df, `Control Group` %in% input$forest_control)
    if (length(input$forest_spec) > 0)
      df <- dplyr::filter(df, Specification %in% input$forest_spec)
    validate(need(nrow(df) > 0, "No results match the selected filters."))
    
    df <- df %>%
      dplyr::mutate(
        sig = dplyr::if_else(`95% CI Lower` > 0 | `95% CI Upper` < 0,
                             "Significant (95%)", "Not significant"),
        label = paste0(Outcome, "  [", `Control Group`, " / ", Specification, "]")
      )
    # Pre-order the label factor so the plot AND nearPoints agree on row positions.
    df$label <- factor(df$label, levels = df$label[order(df$ATT)])
    df
  })
  
  output$economy_forest_plot <- renderPlot({
    df <- forest_data()
    ggplot2::ggplot(df, ggplot2::aes(ATT, label, colour = sig)) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "#666666") +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = `95% CI Lower`, xmax = `95% CI Upper`),
        orientation = "y", width = 0.25
      ) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_colour_manual(values = c("Significant (95%)" = "#238b45",
                                              "Not significant"   = "#b0b0b0")) +
      ggplot2::labs(title = "Overall ATT with 95% confidence intervals",
                    x = "ATT (intervals excluding 0 are significant)", y = NULL, colour = NULL) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title  = ggplot2::element_text(face = "bold"),
        axis.text.y = ggplot2::element_text(colour = "black"),
        axis.text.x = ggplot2::element_text(colour = "black"),
        legend.position = "top"
      )
  })
  
  output$forest_hover_info <- renderUI({
    hover <- input$forest_hover
    req(hover)
    df <- forest_data()
    pt <- shiny::nearPoints(df, hover, xvar = "ATT", yvar = "label",
                            threshold = 15, maxpoints = 1)
    if (nrow(pt) == 0) return(NULL)
    
    left_px <- hover$coords_css$x + 10
    top_px  <- hover$coords_css$y + 10
    tags$div(
      style = paste0(
        "position:absolute; z-index:100; pointer-events:none; ",
        "background:rgba(255,255,255,0.96); border:1px solid #999; ",
        "border-radius:5px; padding:8px 10px; font-size:13px; ",
        "box-shadow:0 2px 6px rgba(0,0,0,0.15); ",
        "left:", left_px, "px; top:", top_px, "px;"
      ),
      tags$b(pt$Outcome), tags$br(),
      sprintf("ATT: %.3f", pt$ATT), tags$br(),
      sprintf("95%% CI: [%.3f, %.3f]", pt$`95% CI Lower`, pt$`95% CI Upper`), tags$br(),
      sprintf("p-value: %.3f", pt$`p-value`), tags$br(),
      tags$span(style = "color:#555;", paste0(pt$`Control Group`, " / ", pt$Specification))
    )
  })
  
  #--------------Analysis sample --
  
  output$economy_sample_composition <- renderUI({
    validate(need(!is.null(map_data), "map_data.rds not found."))
    md <- sf::st_drop_geometry(map_data)
    md$status <- dplyr::recode(md$status, "Drop: Pre-2007 Only" = "Drop: Pre-2009 Only")
    
    n_treated      <- sum(md$status == "Treated")
    n_control      <- sum(md$status == "Control")
    n_drop_pre     <- sum(md$status == "Drop: Pre-2009 Only")
    n_drop_missing <- sum(md$status == "Drop: Missing Years")
    n_dcs          <- sum(md$dc_count, na.rm = TRUE)
    yrs            <- range(md$first_dc_year[md$status == "Treated"], na.rm = TRUE)
    
    tags$ul(
      tags$li(tags$b("Treated counties: "), n_treated, " (first data center opened 2009 or later)"),
      tags$li(tags$b("Control counties: "), n_control, " (never received a data center)"),
      tags$li(tags$b("Analysis sample total: "), n_treated + n_control),
      tags$li(tags$b("Excluded, pre-2009 data centers only: "), n_drop_pre),
      tags$li(tags$b("Excluded, missing opening year: "), n_drop_missing),
      tags$li(tags$b("Operational data centers (all counties): "), n_dcs),
      tags$li(tags$b("Treatment years span: "), yrs[1], "–", yrs[2])
    )
  })
  
  output$economy_cohort_table <- DT::renderDT({
    md <- sf::st_drop_geometry(map_data)
    coh <- md %>%
      dplyr::filter(status == "Treated", !is.na(first_dc_year)) %>%
      dplyr::count(`First DC Year` = first_dc_year, name = "Treated Counties") %>%
      dplyr::arrange(`First DC Year`) %>%
      dplyr::mutate(Cumulative = cumsum(`Treated Counties`))
    
    DT::datatable(
      coh, rownames = FALSE,
      options = list(dom = "t", pageLength = 20),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: bold;",
        "Treated Counties by First Data Center Year"
      )
    )
  })
  # ----- Sector employment results table ---------------------------------
  economy_sector_dt <- function() {
    validate(need(
      file.exists("report/sector_summary.rds"),
      "sector_summary.rds not found. Re-knit the analysis Rmd (Section 13) to write it."
    ))
    
    readRDS("report/sector_summary.rds") %>%
      add_result_col() %>%
      DT::datatable(
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons", "Responsive"),
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          pageLength = 15,
          responsive = TRUE,
          scrollX = TRUE,
          autoWidth = TRUE,
          order = list(list(0, "asc"))
        ),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-weight: bold; font-size: 18px;",
          "Sector Employment DiD Results (ACS sector employment, both specifications)"
        )
      ) %>%
      style_att_table()
  }
  
  # ----- Artificial light at night results table -------------------------
  # Restructured to match the sector/results tables: select down to the same
  # columns and add the Result label. Errored models (NA ATT) are dropped so
  # they are not mislabelled "Not significant".
  economy_lights_dt <- function() {
    lights_file_candidates <- c(
      "report/lights_results_summary.rds",
      "outputs/lights_results_summary.rds"
    )
    lights_file <- lights_file_candidates[file.exists(lights_file_candidates)][1]
    
    validate(need(
      !is.na(lights_file),
      "lights_results_summary.rds not found. Knit the lights analysis Rmd to write report/lights_results_summary.rds."
    ))
    
    readRDS(lights_file) %>%
      filter(!is.na(ATT)) %>%
      select(
        Outcome, `Control Group`, Specification,
        ATT, SE, `95% CI Lower`, `95% CI Upper`, `p-value`
      ) %>%
      add_result_col() %>%
      DT::datatable(
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons", "Responsive"),
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          pageLength = 10,
          responsive = TRUE,
          scrollX = TRUE,
          autoWidth = TRUE,
          order = list(list(1, "asc"))
        ),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-weight: bold; font-size: 18px;",
          "Artificial Light at Night DiD Results"
        )
      ) %>%
      style_att_table()
  }
  
  # ----------------------------------------------------------
  # RESULTS TAB — Heat: surface temperature DiD tables
  # ----------------------------------------------------------
  read_or_null <- function(p) if (file.exists(p)) readRDS(p) else NULL
  heat_poster   <- read_or_null("report/heat_poster_table.rds")
  heat_models   <- read_or_null("report/heat_models.rds")
  heat_seasonal <- read_or_null("report/heat_seasonal.rds")
  heat_meta     <- read_or_null("report/heat_meta.rds")
  heat_footnote <- read_or_null("report/heat_footnote.rds")
  
  # Subtitle comes from heat_meta so the year range cannot drift from the panel.
  output$heat_subtitle     <- renderText(heat_meta$subtitle)
  output$heat_footnote_txt <- renderText(heat_footnote)
  
  output$heat_poster_tbl <- DT::renderDT(
    DT::datatable(
      heat_poster, rownames = FALSE,
      options = list(dom = "t", pageLength = 10),
      colnames = c("Specification", "Sample", "Estimate (SE)", "Interpretation"),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: bold; font-size: 18px;",
        "Headline Results"
      )))
  
  # One row per model; Operational and Construction estimates are column pairs,
  # so sorting by any column keeps a model's numbers together.
  output$heat_models_tbl <- DT::renderDT({
    num_cols <- grep("Estimate|SE|p-value", names(heat_models), value = TRUE)
    DT::datatable(
      heat_models, rownames = FALSE, filter = "top",
      extensions = c("Buttons", "Responsive"),
      options = list(
        dom = "Bfrtip", buttons = c("copy", "csv", "excel"),
        pageLength = 15, scrollX = TRUE, autoWidth = TRUE,
        order = list(list(0, "asc"))
      ),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: bold; font-size: 18px;",
        "All Specifications (\u00b0C per treating facility)"
      )
    ) %>%
      DT::formatRound(columns = num_cols, digits = 3) %>%
      DT::formatStyle(
        columns = c("Operational Estimate", "Construction Estimate"),
        color = DT::styleInterval(0, c("#b00020", "#006400")),
        fontWeight = "bold"
      )
  })
  
  output$heat_seasonal_tbl <- DT::renderDT(
    DT::datatable(
      heat_seasonal, rownames = FALSE,
      options = list(dom = "t"),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: bold; font-size: 18px;",
        "Seasonal Breakdown (0\u2013600m, 145 facilities)"
      )) %>%
      DT::formatRound(columns = c("Estimate", "SE", "p-value"), digits = 3) %>%
      DT::formatStyle("Estimate",
                      color = DT::styleInterval(0, c("#b00020", "#006400")),
                      fontWeight = "bold"))
  
  output$heat_glossary_tbl <- DT::renderDT(
    DT::datatable(heat_meta$glossary, rownames = FALSE,
                  options = list(dom = "t", pageLength = 20)))
  
  # ==========================================================
  # MAPS TAB — Economics: treatment status map
  # ==========================================================
  # Uses `map_data` loaded in global.R (county polygons + status +
  # first_dc_year + dc_count). Hover shows county name, first known data
  # center opening year, and the count of operational data centers.
  output$economics_map <- leaflet::renderLeaflet({
    validate(need(
      !is.null(map_data),
      "map_data.rds not found. Knit the analysis Rmd to generate it."
    ))
    
    # Safety net: if map_data.rds still carries the old "Drop: Pre-2007 Only"
    # label (i.e. the economy Rmd has not been re-knit yet), recode it so the
    # value stays inside the palette domain and does not trigger the
    # "values outside the color scale" warning.
    md <- map_data
    md$status <- dplyr::recode(
      md$status,
      "Drop: Pre-2007 Only" = "Drop: Pre-2009 Only"
    )
    
    pal <- leaflet::colorFactor(
      palette = c(
        "#31a354",  # Treated             — green
        "#bdbdbd",  # Control             — grey
        "#fdae61",  # Drop: Pre-2009 Only — orange
        "#d7191c"   # Drop: Missing Years — red
      ),
      levels  = c("Treated", "Control", "Drop: Pre-2009 Only", "Drop: Missing Years"),
      na.color = "#cccccc"
    )
    
    labels <- sprintf(
      "<b>%s</b><br/>Group: %s<br/>First data center year: %s<br/>Operational data centers: %d",
      md$NAMELSAD,
      md$status,
      ifelse(is.na(md$first_dc_year), "None", as.character(md$first_dc_year)),
      md$dc_count
    ) |> lapply(htmltools::HTML)
    
    leaflet::leaflet(md) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addPolygons(
        fillColor   = ~pal(status),
        fillOpacity = 0.7,
        color       = "white",
        weight      = 0.5,
        highlightOptions = leaflet::highlightOptions(
          weight = 2, color = "#333333", fillOpacity = 0.9, bringToFront = TRUE
        ),
        label = labels,
        labelOptions = leaflet::labelOptions(
          style = list("font-size" = "13px"),
          textsize = "13px", direction = "auto"
        )
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        pal = pal,
        values = ~status,
        title = "County Group",
        opacity = 0.8
      )
  })
  
  # ==========================================================
  # MAPS TAB — Economics: data summary (reactive histograms)
  # ==========================================================
  # Reads report/economy_summary_data.rds (saved by the analysis Rmd) and plots
  # one labeled histogram at a time, chosen by input$econ_hist_var. Log toggle
  # via input$econ_hist_log for skewed variables.
  #
  # NOTE: the keys below must match the column names written by sumdat1 in the
  # economy Rmd (watch the em-dash "—" in the sector labels).
  
  econ_var_meta <- list(
    "Employment"              = list(x = "Employees (count)",               desc = "Total paid employees in a county in one year (CBP)."),
    "Real Payroll (2017$)"    = list(x = "Real annual payroll (2017 $)",    desc = "County payroll for one year, adjusted to 2017 dollars (CBP)."),
    "Establishments"          = list(x = "Establishments (count)",          desc = "Physical business locations in a county in one year (CBP)."),
    "Unemployment Rate"       = list(x = "Unemployment rate (%)",           desc = "Share of the civilian labor force unemployed (ACS)."),
    "Labor Force Part. (%)"   = list(x = "Labor force participation (%)",   desc = "Share of the population 16+ in the labor force (ACS)."),
    "Median HH Income"        = list(x = "Median household income ($)",     desc = "County median household income for one year (ACS)."),
    "Population"              = list(x = "Population (persons)",            desc = "Total county population for one year (ACS)."),
    "% Bachelor's+"           = list(x = "Bachelor's degree or higher (%)", desc = "Share of adults with a bachelor's degree or more (ACS)."),
    "% HS Grad+"              = list(x = "High-school graduate or higher (%)", desc = "Share of adults with at least a high-school diploma (ACS)."),
    
    "Median Income (Female)"  = list(x = "Median income, female earners ($)", desc = "Median income among female earners in a county-year (ACS)."),
    "Median Income (Male)"    = list(x = "Median income, male earners ($)",   desc = "Median income among male earners in a county-year (ACS)."),
    "Per Capita Income"       = list(x = "Per capita income ($)",             desc = "Per capita income in a county-year (ACS)."),
    
    "% White"                 = list(x = "White (%)",            desc = "Share of county population identifying as white (ACS, used as a lagged control)."),
    "% Black"                 = list(x = "Black (%)",            desc = "Share of county population identifying as Black (ACS, used as a lagged control)."),
    "% Hispanic"              = list(x = "Hispanic (%)",         desc = "Share of county population identifying as Hispanic (ACS, used as a lagged control)."),
    "% Asian"                 = list(x = "Asian (%)",            desc = "Share of county population identifying as Asian (ACS, used as a lagged control)."),
    "% AIAN"                  = list(x = "American Indian / Alaska Native (%)", desc = "Share identifying as American Indian or Alaska Native (ACS, used as a lagged control)."),
    
    "Emp (Agriculture/Mining)"    = list(x = "Employment (count)", desc = "ACS sector employment: agriculture, forestry, fishing, hunting, mining."),
    "Emp (Construction)"          = list(x = "Employment (count)", desc = "ACS sector employment: construction."),
    "Emp (Manufacturing)"         = list(x = "Employment (count)", desc = "ACS sector employment: manufacturing."),
    "Emp (Wholesale Trade)"       = list(x = "Employment (count)", desc = "ACS sector employment: wholesale trade."),
    "Emp (Retail Trade)"          = list(x = "Employment (count)", desc = "ACS sector employment: retail trade."),
    "Emp (Transport/Utilities)"   = list(x = "Employment (count)", desc = "ACS sector employment: transportation, warehousing, utilities."),
    "Emp (Information)"           = list(x = "Employment (count)", desc = "ACS sector employment: information."),
    "Emp (Finance/Real Estate)"   = list(x = "Employment (count)", desc = "ACS sector employment: finance, insurance, real estate."),
    "Emp (Professional/Admin)"    = list(x = "Employment (count)", desc = "ACS sector employment: professional, scientific, management, admin."),
    "Emp (Education/Health)"      = list(x = "Employment (count)", desc = "ACS sector employment: educational, health care, social assistance."),
    "Emp (Arts/Food Services)"    = list(x = "Employment (count)", desc = "ACS sector employment: arts, entertainment, recreation, food services."),
    "Emp (Other Services)"        = list(x = "Employment (count)", desc = "ACS sector employment: other services."),
    "Emp (Public Administration)" = list(x = "Employment (count)", desc = "ACS sector employment: public administration.")
  )
  
  econ_skewed_vars <- c(
    "Employment", "Real Payroll (2017$)", "Establishments", "Population", "Median HH Income",
    "Median Income (Female)", "Median Income (Male)", "Per Capita Income",
    "Emp (Agriculture/Mining)", "Emp (Construction)", "Emp (Manufacturing)",
    "Emp (Wholesale Trade)", "Emp (Retail Trade)", "Emp (Transport/Utilities)",
    "Emp (Information)", "Emp (Finance/Real Estate)", "Emp (Professional/Admin)",
    "Emp (Education/Health)", "Emp (Arts/Food Services)", "Emp (Other Services)",
    "Emp (Public Administration)"
  )
  
  # Populate the dropdown once the data file is available.
  observe({
    f <- "report/economy_summary_data.rds"
    if (file.exists(f)) {
      vars <- setdiff(names(readRDS(f)), c("status", "year"))
      updateSelectInput(session, "econ_hist_var", choices = vars, selected = vars[1])
      updateSelectInput(session, "econ_cmp_var",  choices = vars, selected = vars[1])
    }
  })
  
  output$econ_hist_desc <- renderUI({
    req(input$econ_hist_var)
    meta <- econ_var_meta[[input$econ_hist_var]]
    if (is.null(meta)) return(NULL)
    hint <- if (input$econ_hist_var %in% econ_skewed_vars)
      " This variable is very right-skewed; try the log scale toggle to see its shape more clearly." else ""
    tags$p(class = "subtitle", paste0(meta$desc, hint))
  })
  
  output$econ_hist_plot <- renderPlot({
    validate(need(
      file.exists("report/economy_summary_data.rds"),
      "economy_summary_data.rds not found. Knit the analysis Rmd to generate it."
    ))
    req(input$econ_hist_var)
    
    dat  <- readRDS("report/economy_summary_data.rds")
    vsel <- input$econ_hist_var
    validate(need(vsel %in% names(dat), "Select a variable."))
    
    v    <- dat[[vsel]]
    v    <- v[!is.na(v)]
    meta <- econ_var_meta[[vsel]]
    xlab <- if (is.null(meta)) vsel else meta$x
    
    use_log <- isTRUE(input$econ_hist_log)
    if (use_log) {
      v <- v[v > 0]
      xlab <- paste0(xlab, "  (log scale)")
    }
    
    p <- ggplot2::ggplot(data.frame(value = v), ggplot2::aes(value)) +
      ggplot2::geom_histogram(bins = 30, fill = "#4C78A8",
                              colour = "white", linewidth = 0.2) +
      ggplot2::labs(
        title = paste("Distribution of", vsel),
        x = xlab,
        y = "Tally count of county-years"
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        axis.title = ggplot2::element_text(face = "bold")
      )
    
    if (use_log) {
      p <- p + ggplot2::scale_x_log10(labels = scales::comma)
    } else {
      p <- p + ggplot2::scale_x_continuous(labels = scales::comma)
    }
    p
  })
  
  # ----- Heat: data center location map ---------------------------------
  dc_inventory <- reactive({
    f <- "report/dc_inventory.rds"
    validate(need(file.exists(f), "dc_inventory.rds not found. Run make_dc_inventory.R."))
    readRDS(f)
  })
  
  output$dc_location_map <- leaflet::renderLeaflet({
    d <- dc_inventory()
    if (isTRUE(input$dc_map_analysis_only)) d <- dplyr::filter(d, in_analysis)
    validate(need(nrow(d) > 0, "No facilities match."))
    
    varname <- input$dc_map_color %||% "capacity_type"
    d$grp <- as.character(d[[varname]])
    d$grp[is.na(d$grp)] <- "Unknown"
    
    pal <- leaflet::colorFactor("Set2", domain = sort(unique(d$grp)))
    labs <- sprintf(
      "<b>ID %s</b><br/>Type: %s<br/>Stage: %s<br/>Opened: %s",
      d$export_id, d$capacity_type, d$stage,
      ifelse(is.na(d$year_operational), "Unknown", d$year_operational)
    ) |> lapply(htmltools::HTML)
    
    leaflet::leaflet(d) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        color = ~pal(grp), fillOpacity = 0.75, radius = 4, weight = 1,
        label = labs,
        clusterOptions = leaflet::markerClusterOptions(disableClusteringAtZoom = 8)
      ) |>
      leaflet::addLegend("bottomright", pal = pal, values = ~grp,
                         title = if (varname == "stage") "Stage" else "Facility type",
                         opacity = 0.85)
  })
  
  output$dc_map_count <- renderText({
    d <- dc_inventory()
    if (isTRUE(input$dc_map_analysis_only)) d <- dplyr::filter(d, in_analysis)
    sprintf("Showing %s facilities.", format(nrow(d), big.mark = ","))
  })
  
  # heat analysis visualization
  heat_figs <- tibble::tribble(
    ~file, ~title, ~desc,
    "event_study_all.png", "Event Study",
    "Effect by year relative to facility opening. Flat estimates before year -4 support the parallel-trends assumption; the rise from -3 onward is construction, and the post-0 level is the operational effect.",
    "distance_profile_all.png", "Thermal Profile by Distance",
    "Temperature relative to each facility's own 1000-1500m control ring, before construction versus after opening. The gap is largest at the fence line and closes by roughly 1000m.",
    "heatmap_fac_3012.png", "Facility Heat Map",
    "Mean land surface temperature around one facility, before construction and after operation. Dashed rings mark the 300m, 600m, 1000m, and 1500m radii.",
    "ref_year_justification.png", "Baseline Year Justification",
    "Land cover composition by year relative to opening. Barren land peaks in the three years before opening, which is why the baseline is anchored at year -4 rather than -1."
  )
  
  output$heat_gallery <- renderUI({
    rows <- lapply(seq_len(nrow(heat_figs)), function(i) {
      f <- heat_figs[i, ]
      if (!file.exists(file.path("www", "heat_figures", f$file))) return(NULL)
      tags$div(
        class = "info-card", style = "margin-bottom:22px;",
        tags$h4(f$title),
        tags$p(class = "subtitle", f$desc),
        tags$a(href = file.path("heat_figures", f$file), target = "_blank",
               tags$img(src = file.path("heat_figures", f$file),
                        style = "width:100%; max-width:900px; height:auto;
                                 border:1px solid #D3C0C8;"))
      )
    })
    do.call(tagList, Filter(Negate(is.null), rows))
  })
  
  # ----------------------------------------------------------
  # REPORT TAB — Economy: model equation
  # ----------------------------------------------------------
  output$report_equation <- renderUI({
    withMathJax(
      tags$div(
        class = "equation-box",
        "$$
    ATT(g,t) = E[Y_t(g) - Y_t(0) \\mid G_i = g]
    $$",
        "$$
    Y_{it} = \\alpha_i + \\lambda_t + \\beta D_{it} + X_{it}'\\gamma + \\varepsilon_{it}
    $$"
      )
    )
  })
  
  # ----------------------------------------------------------
  # REPORT TAB — Economy: outcome definitions table (clickable)
  # ----------------------------------------------------------
  output$economy_outcomes_definitions_table <- DT::renderDT({
    
    outcome_definitions <- tibble::tribble(
      ~`Outcome`, ~`Variable`, ~`Data Source`, ~`Units`, ~`Definition`,
      
      "Employment", "EMP",
      "Census County Business Patterns (CBP)",
      "Number of paid employees",
      "Total paid employment across business establishments in the county.",
      
      "Real Annual Payroll", "PAYANN_REAL",
      "Census County Business Patterns (CBP)",
      "2017 inflation-adjusted dollars",
      "Total annual payroll paid by county establishments, deflated to constant 2017 dollars.",
      
      "Establishments", "ESTAB",
      "Census County Business Patterns (CBP)",
      "Number of establishments",
      "Total number of physical business locations operating in the county.",
      
      "Unemployment Rate", "unemployment_rate",
      "ACS 5-year estimates (DP03_0009P)",
      "Percentage points",
      "Share of the civilian labor force that is unemployed and actively seeking work.",
      
      "Labor Force Participation Rate", "labor_force_participation",
      "ACS 5-year estimates (DP03_0004P)",
      "Percentage points",
      "Share of the population age 16+ that is in the civilian labor force.",
      
      "Log Median Household Income", "log_median_income",
      "ACS 5-year estimates (DP03_0062)",
      "Logged dollars",
      "Median household income in the county, natural-log transformed.",
      
      "Log Population", "log_pop",
      "ACS 5-year estimates (B01003_001)",
      "Logged persons",
      "Total county population, natural-log transformed.",
      
      "Log Median Income (Female)", "log_median_inc_f",
      "County Health / ACS (ACS_MEDIAN_INC_F)",
      "Logged dollars",
      "Median income for female earners in the county, natural-log transformed.",
      
      "Log Median Income (Male)", "log_median_inc_m",
      "County Health / ACS (ACS_MEDIAN_INC_M)",
      "Logged dollars",
      "Median income for male earners in the county, natural-log transformed.",
      
      "Log Per Capita Income", "log_per_capita_inc",
      "County Health / ACS (ACS_PER_CAPITA_INC)",
      "Logged dollars",
      "Per capita income in the county, natural-log transformed."
    )
    
    DT::datatable(
      outcome_definitions,
      rownames = FALSE,
      filter = "top",
      extensions = c("Buttons", "Responsive"),
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 15,
        responsive = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE,
        order = list(list(0, "asc"))   # sort by Outcome, like the sector table
      ),
      callback = DT::JS("
      table.on('click', 'td.outcome-click', function() {
        var cell = table.cell(this);
        var row  = table.row(cell.index().row).data();
        Shiny.setInputValue('economy_outcome_clicked', {
          outcome:  row[0],
          variable: row[1],
          nonce:    Math.random()
        }, {priority: 'event'});
      });
      table.on('mouseenter', 'td.outcome-click', function() {
        $(this).css({'cursor':'pointer','color':'#0056b3','text-decoration':'underline','font-weight':'bold'});
      });
      table.on('mouseleave', 'td.outcome-click', function() {
        $(this).css({'color':'','text-decoration':'','font-weight':''});
      });
    "),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: bold; font-size: 18px;",
        "Outcome Definitions (click an outcome for full details)"
      )
    ) %>%
      DT::formatStyle("Outcome", fontWeight = "bold")
  })
  
  # ----------------------------------------------------------
  # REPORT TAB — Economy: methods specification table (DISABLED)
  # ----------------------------------------------------------
  # Disabled via if (FALSE) to avoid repeating the outcome list already shown in
  # the Outcome Definitions table. To re-enable: change `if (FALSE)` to
  # `if (TRUE)` and uncomment the DTOutput("economy_methods_table") lines in
  # reportTab.R.
  if (FALSE) {
    
    output$economy_methods_table <- DT::renderDT({
      
      cov_lagged <- paste0(
        "Lagged 1yr: log pop, log median income, % bachelor's, % HS grad, ",
        "race shares (white, Black, Hispanic, Asian, AIAN)"
      )
      
      methods_table <- tibble::tribble(
        ~`Model`, ~`Outcome`, ~`Dependent Variable`, ~`Independent Variable`, ~`Control Group`, ~`Covariates`, ~`Transformation`, ~`Years`, ~`Status`,
        
        "Section 9: Baseline CBP DiD", "Employment", "EMP",
        "First data center opening year", "Never-treated counties", "None",
        "Log outcome", "2009-2023", "Event study only",
        
        "Section 9: Baseline CBP DiD", "Real Annual Payroll", "PAYANN_REAL",
        "First data center opening year", "Never-treated counties", "None",
        "Log outcome; payroll deflated to 2017 dollars", "2009-2023", "Event study only",
        
        "Section 9: Baseline CBP DiD", "Establishments", "ESTAB",
        "First data center opening year", "Never-treated counties", "None",
        "Log outcome", "2009-2023", "Event study only",
        
        "Section 11: ACS-Controlled CBP DiD", "Employment", "EMP",
        "First data center opening year", "Not-yet-treated counties", cov_lagged,
        "Log outcome", "2010-2023 (effective)", "In results table",
        
        "Section 11: ACS-Controlled CBP DiD", "Real Annual Payroll", "PAYANN_REAL",
        "First data center opening year", "Not-yet-treated counties", cov_lagged,
        "Log outcome; payroll deflated to 2017 dollars", "2010-2023 (effective)", "In results table",
        
        "Section 11: ACS-Controlled CBP DiD", "Establishments", "ESTAB",
        "First data center opening year", "Not-yet-treated counties", cov_lagged,
        "Log outcome", "2010-2023 (effective)", "In results table",
        
        "Section 12: ACS Outcome DiD", "Unemployment Rate", "unemployment_rate",
        "First data center opening year", "Never- and not-yet-treated counties", cov_lagged,
        "Level (percentage points)", "2010-2023 (effective)", "In results table",
        
        "Section 12: ACS Outcome DiD", "Labor Force Participation Rate", "labor_force_participation",
        "First data center opening year", "Never- and not-yet-treated counties", cov_lagged,
        "Level (percentage points)", "2010-2023 (effective)", "In results table",
        
        "Section 12: ACS Outcome DiD", "Log Median Household Income", "log_median_income",
        "First data center opening year", "Never- and not-yet-treated counties", cov_lagged,
        "Already logged", "2010-2023 (effective)", "In results table",
        
        "Section 12: ACS Outcome DiD", "Log Population", "log_pop",
        "First data center opening year", "Never- and not-yet-treated counties", cov_lagged,
        "Already logged", "2010-2023 (effective)", "In results table",
        
        "Section 12: ACS Outcome DiD", "Log Median Income (Female)", "log_median_inc_f",
        "First data center opening year", "Never- and not-yet-treated counties", cov_lagged,
        "Already logged", "2010-2023 (effective)", "In results table",
        
        "Section 12: ACS Outcome DiD", "Log Median Income (Male)", "log_median_inc_m",
        "First data center opening year", "Never- and not-yet-treated counties", cov_lagged,
        "Already logged", "2010-2023 (effective)", "In results table",
        
        "Section 12: ACS Outcome DiD", "Log Per Capita Income", "log_per_capita_inc",
        "First data center opening year", "Never- and not-yet-treated counties", cov_lagged,
        "Already logged", "2010-2023 (effective)", "In results table",
        
        "Section 10: Sector Employment DiD", "ACS sector employment (13 sectors)", "emp_* (DP03_0033-0045)",
        "First data center opening year", "Never- and not-yet-treated counties", "None",
        "Log outcome; ba          ``` ` `````lanced 15-year panel", "2009-2023", "In sector table",
        
        "Section 10b: Sector Employment DiD (Controlled)", "ACS sector employment (13 sectors)", "emp_* (DP03_0033-0045)",
        "First data center opening year", "Never- and not-yet-treated counties", cov_lagged,
        "Log outcome; small cohorts dropped (min 5 counties)", "2010-2023 (effective)", "In sector table"
      )
      
      outcome_col <- which(names(methods_table) == "Outcome") - 1
      
      DT::datatable(
        methods_table,
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons", "Responsive"),
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          pageLength = 15,
          responsive = TRUE,
          scrollX = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(targets = outcome_col, className = "outcome-click")
          )
        ),
        callback = DT::JS("
    table.on('click', 'td.outcome-click', function() {
      var cell = table.cell(this);
      var row = table.row(cell.index().row).data();
      Shiny.setInputValue('economy_outcome_clicked', {
        model: row[0],
        outcome: row[1],
        variable: row[2],
        nonce: Math.random()
      }, {priority: 'event'});
    });
    table.on('mouseenter', 'td.outcome-click', function() {
      $(this).css({'cursor':'pointer','color':'#0056b3','text-decoration':'underline','font-weight':'bold'});
    });
    table.on('mouseleave', 'td.outcome-click', function() {
      $(this).css({'color':'','text-decoration':'','font-weight':''});
    });
  "),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-weight: bold; font-size: 18px;",
          "Economic Analysis Methodology by Outcome (click an outcome for details)"
        )
      ) %>%
        DT::formatStyle(
          "Status",
          backgroundColor = DT::styleEqual(
            c("In results table", "In sector table", "Event study only"),
            c("#d4edda",          "#d4edda",         "#fff3cd")
          ),
          fontWeight = "bold"
        ) %>%
        DT::formatStyle("Model", fontWeight = "bold")
    })
    
  }  # end if (FALSE) — economy_methods_table disabled
  
  # ----------------------------------------------------------
  # REPORT TAB — Economy: outcome detail modal
  # ----------------------------------------------------------
  observeEvent(input$economy_outcome_clicked, {
    
    outcome <- input$economy_outcome_clicked$outcome
    varname <- input$economy_outcome_clicked$variable
    
    details <- switch(
      outcome,
      
      "Employment" = list(
        source = "U.S. Census Bureau County Business Patterns (CBP)",
        years = "2009-2023 regression window",
        units = "Number of paid employees",
        definition = "Total paid employment in county business establishments.",
        transformation = "Natural logarithm of employment.",
        transformation_why = "Counties vary greatly in size, so logging compares relative changes rather than raw job-count changes.",
        att = "Because employment is logged, an ATT of 0.05 means employment increased by roughly 5%.",
        why = "Shows whether data centers are associated with county job growth."
      ),
      
      "Real Annual Payroll" = list(
        source = "U.S. Census Bureau County Business Patterns (CBP)",
        years = "2009-2023 regression window",
        units = "2017 inflation-adjusted dollars",
        definition = "Total annual payroll paid by county establishments, adjusted for inflation.",
        transformation = "Converted to 2017 dollars, then logged.",
        transformation_why = "Inflation adjustment makes payroll comparable across years; logging gives an approximate percent-change interpretation.",
        att = "An ATT of 0.05 means real annual payroll increased by roughly 5%.",
        why = "Captures whether data centers are associated with changes in local earnings."
      ),
      
      "Establishments" = list(
        source = "U.S. Census Bureau County Business Patterns (CBP)",
        years = "2009-2023 regression window",
        units = "Number of business establishments",
        definition = "Total number of physical business locations in the county.",
        transformation = "Natural logarithm of establishments.",
        transformation_why = "Counties differ greatly in business activity, so logging focuses on proportional change.",
        att = "An ATT of 0.05 means establishments increased by roughly 5%.",
        why = "Measures whether data centers are associated with broader business growth."
      ),
      
      "Unemployment Rate" = list(
        source = "American Community Survey 5-year estimates",
        years = "2010-2023 (effective, ACS-controlled)",
        units = "Percentage points",
        definition = "Share of the civilian labor force that is unemployed and actively seeking work.",
        transformation = "Level outcome, not logged.",
        transformation_why = "Already a percentage, so levels make the ATT a percentage-point change.",
        att = "An ATT of 0.08 means unemployment increased by 0.08 percentage points (e.g. 5.00% to 5.08%), not 8%.",
        why = "Captures whether openings are associated with broader labor-market conditions."
      ),
      
      "Labor Force Participation Rate" = list(
        source = "American Community Survey 5-year estimates",
        years = "2010-2023 (effective, ACS-controlled)",
        units = "Percentage points",
        definition = "Share of the population age 16+ in the civilian labor force.",
        transformation = "Level outcome, not logged.",
        transformation_why = "Already a percentage, so levels make the ATT a percentage-point change.",
        att = "An ATT of 0.30 means participation increased by 0.30 percentage points.",
        why = "Shows whether openings are associated with people entering or leaving the labor force."
      ),
      
      "Log Median Household Income" = list(
        source = "American Community Survey 5-year estimates",
        years = "2010-2023 (effective, ACS-controlled)",
        units = "Logged dollars",
        definition = "Median household income in the county, natural-log transformed.",
        transformation = "Natural logarithm.",
        transformation_why = "Dollar values vary widely across counties and years; logging enables a percent-change interpretation.",
        att = "An ATT of 0.04 means median household income increased by roughly 4%.",
        why = "Evaluates whether openings are associated with household economic well-being."
      ),
      
      "Log Population" = list(
        source = "American Community Survey 5-year estimates",
        years = "2010-2023 (effective, ACS-controlled)",
        units = "Logged persons",
        definition = "Total county population, natural-log transformed.",
        transformation = "Natural logarithm.",
        transformation_why = "Counties vary greatly in size, so logging focuses on proportional change.",
        att = "An ATT of 0.03 means population increased by roughly 3%.",
        why = "Evaluates whether development is associated with local growth or migration."
      ),
      
      "Log Median Income (Female)" = list(
        source = "County Health data / ACS (ACS_MEDIAN_INC_F)",
        years = "2010-2023 (effective, ACS-controlled)",
        units = "Logged dollars",
        definition = "Median income among female earners in the county, natural-log transformed.",
        transformation = "Natural logarithm.",
        transformation_why = "Logging enables an approximate percent-change interpretation.",
        att = "An ATT of 0.04 means female median income increased by roughly 4%.",
        why = "Examines whether income effects differ by sex."
      ),
      
      "Log Median Income (Male)" = list(
        source = "County Health data / ACS (ACS_MEDIAN_INC_M)",
        years = "2010-2023 (effective, ACS-controlled)",
        units = "Logged dollars",
        definition = "Median income among male earners in the county, natural-log transformed.",
        transformation = "Natural logarithm.",
        transformation_why = "Logging enables an approximate percent-change interpretation.",
        att = "An ATT of 0.04 means male median income increased by roughly 4%.",
        why = "Examines whether income effects differ by sex."
      ),
      
      "Log Per Capita Income" = list(
        source = "County Health data / ACS (ACS_PER_CAPITA_INC)",
        years = "2010-2023 (effective, ACS-controlled)",
        units = "Logged dollars",
        definition = "Per capita income in the county, natural-log transformed.",
        transformation = "Natural logarithm.",
        transformation_why = "Logging enables an approximate percent-change interpretation.",
        att = "An ATT of 0.04 means per capita income increased by roughly 4%.",
        why = "Captures whether openings are associated with average individual income."
      ),
      
      # default — catches any outcome without its own entry (e.g. sector rows)
      list(
        source = "American Community Survey 5-year estimates",
        years = "2009-2023 regression window",
        units = "See outcome definition",
        definition = paste0(outcome, "."),
        transformation = "See methodology.",
        transformation_why = "See methodology.",
        att = "Logged outcomes are approximate percent changes; rate outcomes are percentage-point changes.",
        why = "See report methodology."
      )
    )
    
    showModal(
      modalDialog(
        title = paste0(outcome, " — Outcome Details"),
        size = "l",
        easyClose = TRUE,
        
        tabsetPanel(
          type = "tabs",
          
          tabPanel(
            "Definition",
            tags$br(),
            tags$p(tags$strong("Dependent variable: "), varname),
            tags$p(tags$strong("Definition: "), details$definition),
            tags$p(tags$strong("Units: "), details$units)
          ),
          
          tabPanel(
            "Why This Transformation?",
            tags$br(),
            tags$p(tags$strong("Transformation: "), details$transformation),
            tags$p(details$transformation_why)
          ),
          
          tabPanel(
            "Interpret ATT",
            tags$br(),
            tags$p(details$att),
            tags$p(
              tags$strong("Rule of thumb: "),
              "logged outcomes are approximate percent changes; rate/percentage outcomes are percentage-point changes."
            )
          ),
          
          tabPanel(
            "Data Source",
            tags$br(),
            tags$p(tags$strong("Source: "), details$source),
            tags$p(tags$strong("Years: "), details$years)
          ),
          
          tabPanel(
            "Why It Matters",
            tags$br(),
            tags$p(details$why)
          )
        ),
        
        footer = modalButton("Close")
      )
    )
  })
  
  # ----------------------------------------------------------
  # RESULTS + REPORT TABS — Economy: main results table
  # ----------------------------------------------------------
  # Bound to BOTH tabs via the shared builder. The Report tab keeps the
  # original id; the Results tab uses a results_-prefixed id (see resultsTab.R)
  # to avoid the duplicate-output-ID collision.
  output$economy_results_summary_table         <- DT::renderDT(economy_results_dt())
  output$results_economy_results_summary_table <- DT::renderDT(economy_results_dt())
  
  # ----------------------------------------------------------
  # RESULTS + REPORT TABS — Economy: sector employment results table
  # ----------------------------------------------------------
  output$economy_sector_results_table          <- DT::renderDT(economy_sector_dt())
  output$results_economy_sector_results_table  <- DT::renderDT(economy_sector_dt())
  
  # ----------------------------------------------------------
  # RESULTS TAB — Economy: artificial light at night results table
  # ----------------------------------------------------------
  output$economy_lights_results_table <- DT::renderDT(economy_lights_dt())
  
  # ----------------------------------------------------------
  # REPORT TAB — Downloadable report
  # ----------------------------------------------------------
  # output$downloadReport <- downloadHandler(
  #   filename = function() paste0("report_", Sys.Date(), ".pdf"),
  #   content  = function(file) {
  #     rmarkdown::render("report/report.Rmd",
  #                       output_format = "pdf_document",
  #                       output_file   = file,
  #                       envir         = new.env(parent = globalenv()))
  #   }
  # )
  
}

# 4. RUN APP
app <- shinyApp(ui = ui, server = server)
app$staticPaths <- list(
  "/" = httpuv::staticPath(
    path = file.path(getwd(), "www"),
    indexhtml = FALSE,
    fallthrough = TRUE
  )
)
app