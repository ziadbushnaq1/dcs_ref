# ============================================================
# global.R — Libraries, Data Loading, Shared Objects
# Runs once when the app starts. Everything here is available
# to both ui and server.
# ============================================================

# --- Libraries ----------------------------------------------
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)   # extra leaflet controls
library(tigris)
library(arcgislayers)
library(tidycensus)
library(here)
library(gt)               # for regression tables
library(modelsummary)     # for regression tables
library(DT)               # for interactive data tables
library(plotly)

cat(">>> app wd:", getwd(), "\n")
cat(">>> map_data.rds visible:", file.exists("report/map_data.rds"), "\n")
options(tigris_use_cache = TRUE)

# --- Map data for the Economics tab (built by the analysis Rmd) ---
map_data <- if (file.exists("report/map_data.rds")) readRDS("report/map_data.rds") else NULL
