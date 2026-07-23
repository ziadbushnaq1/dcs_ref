#NAIP construction sequence for facility 2899
library(terra); library(magick); library(here); library(tidyverse)

TARGET  <- 2899
YEAR_OP <- 2012
IN_DIR  <- here("figures","timelapse","naip_raw")
OUT_DIR <- here("figures","timelapse")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

tifs <- list.files(IN_DIR, pattern = sprintf("naip_DC%d_\\d{4}\\.tif$", TARGET),
                   full.names = TRUE)
years <- as.integer(str_extract(basename(tifs), "\\d{4}(?=\\.tif$)"))
tifs  <- tifs[order(years)]; years <- sort(years)
cat("Found", length(tifs), "frames:", paste(years, collapse = ", "), "\n")

frames <- character(0)
for (i in seq_along(tifs)) {
  yr    <- years[i]
  phase <- if (yr < YEAR_OP) "BEFORE" else "AFTER"
  col   <- "black"
  
  r <- rast(tifs[i])
  png_path <- file.path(OUT_DIR, sprintf("frame_%d_%d.png", TARGET, yr))
  png(png_path, width = 900, height = 900, res = 150)
  par(mar = c(0,0,0,0))
  plotRGB(r, r = 1, g = 2, b = 3, stretch = "lin", axes = FALSE, maxcell = 5e6)
  dev.off()
  
  # single label block: year
  label <- sprintf("  %d  ", yr)
  image_read(png_path) %>%
    image_annotate(label, font = "DejaVu Sans", size = 46, weight = 700,
                   color = col, boxcolor = "white",
                   location = "+28+24") %>%
    image_annotate(sprintf("Facility opened %d", YEAR_OP),
                   font = "DejaVu Sans", size = 24, color = "black",
                   boxcolor = "white", gravity = "southwest",
                   location = "+28+22") %>%
    image_write(png_path)
  
  frames <- c(frames, png_path)
  cat("frame", yr, phase, "\n")
}

gif <- image_read(frames) %>% image_join() %>%
  image_animate(delay = 250, optimize = TRUE)
image_write(gif, file.path(OUT_DIR, sprintf("timelapse_%d.gif", TARGET)))
cat("GIF written\n")

# Copy into the app + build the index (mirrors event_study_index.rds)
app_www <- here("ShinyApp","www","timelapse")
dir.create(app_www, showWarnings = FALSE, recursive = TRUE)
file.copy(frames, app_www, overwrite = TRUE)
file.copy(file.path(OUT_DIR, sprintf("timelapse_%d.gif", TARGET)),
          app_www, overwrite = TRUE)

idx <- tibble(export_id = TARGET, year = years,
              phase = ifelse(years < YEAR_OP, "Before", "After"),
              file  = basename(frames),
              gif   = sprintf("timelapse_%d.gif", TARGET))
saveRDS(idx, here("ShinyApp","report","timelapse_index.rds"))
cat("Index written:", nrow(idx), "frames\n")