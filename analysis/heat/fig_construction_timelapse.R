# fig_construction_timelapse.R — year-by-year imagery, no GEE.
# Writes annual PNG frames + an animated GIF.
# install.packages(c("rsi","terra","magick"))
library(tidyverse); library(sf); library(terra); library(rsi); library(here); library(duckdb)
options(bitmapType = "cairo")
source(here("analysis","heat","load_hyperscale_panel.R"))

TARGET   <- 412          
BUF_M    <- 900           # ~1.8km across; tighten to 600 for a closer view
YEARS    <- 2014:2025
OUT_DIR  <- here("figures","timelapse")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

d   <- load_hyperscale_panel()
fac <- d$dc_points %>% filter(export_id == TARGET)

d$roster %>% select(export_id, year_operational) %>% arrange(year_operational) %>% print(n = Inf)

yop <- fac$year_operational[1]
aoi <- st_buffer(fac, BUF_M)
cat("Facility", TARGET, "| opened", yop, "\n")

for (yr in YEARS) {
  f_out <- file.path(OUT_DIR, sprintf("tl_%d_%d.png", TARGET, yr))
  if (file.exists(f_out)) { message("have ", yr); next }
  
  # Sentinel-2 (10m) when available, else Landsat (30m)
  ras <- NULL; src <- NA_character_
  if (yr >= 2017) {
    r <- try(get_sentinel2_imagery(
      aoi, start_date = paste0(yr, "-06-01"), end_date = paste0(yr, "-09-15"),
      output_filename = tempfile(fileext = ".tif")), silent = TRUE)
    if (!inherits(r, "try-error")) { ras <- rast(r); src <- "Sentinel-2 10m" }
  }
  if (is.null(ras)) {
    r <- try(get_landsat_imagery(
      aoi, start_date = paste0(yr, "-06-01"), end_date = paste0(yr, "-09-15"),
      output_filename = tempfile(fileext = ".tif")), silent = TRUE)
    if (inherits(r, "try-error")) { message("no imagery ", yr); next }
    ras <- rast(r); src <- "Landsat 30m"
  }
  
  # Band order differs by product — find RGB by name, fall back to position
  nm  <- tolower(names(ras))
  idx <- c(grep("^(b04|red)$", nm)[1], grep("^(b03|green)$", nm)[1], grep("^(b02|blue)$", nm)[1])
  if (any(is.na(idx))) idx <- c(3, 2, 1)
  
  phase <- case_when(yr <  yop - 3 ~ "before",
                     yr <  yop     ~ "construction",
                     TRUE          ~ "operational")
  
  ok <- try({
    png(f_out, width = 1100, height = 1100, res = 150)
    par(mar = c(0, 0, 2.4, 0))
    plotRGB(ras, r = idx[1], g = idx[2], b = idx[3], stretch = "lin", axes = FALSE)
    title(main = sprintf("%d  \u2014  %s", yr, phase),
          col.main = c(before = "grey20", construction = "#c1440e",
                       operational = "#00539c")[phase], cex.main = 1.5)
    mtext(src, side = 1, line = -1.2, adj = 0.98, cex = 0.7, col = "white")
    dev.off()
  }, silent = TRUE)
  if (inherits(ok, "try-error")) { try(dev.off(), silent = TRUE); message("plot failed ", yr); next }
  message("wrote ", yr, " (", src, ")")
}

# Assemble GIF
library(magick)
frames <- list.files(OUT_DIR, pattern = sprintf("^tl_%d_\\d{4}\\.png$", TARGET),
                     full.names = TRUE) %>% sort()
if (length(frames) > 1) {
  image_read(frames) %>%
    image_animate(fps = 1, optimize = TRUE) %>%
    image_write(file.path(OUT_DIR, sprintf("timelapse_%d.gif", TARGET)))
  cat("GIF:", length(frames), "frames\n")
}