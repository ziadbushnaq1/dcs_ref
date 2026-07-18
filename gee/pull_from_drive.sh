#!/bin/bash
module load rclone

CLUSTER_DIR="/projects/dspg_viz/ziadbushnaq1/data_centers/data/raw"

mkdir -p "$CLUSTER_DIR"

# Define all 13 targeted assets (Original Isolation Sets + New Panel Sets)
ASSETS=(
    "modis_hyperscale_all"
    "modis_iso10000m"
    "modis_iso7500m"
    "modis_iso5000m"
    "modis_iso2500m"
    "landsat_hyperscale_all"
    "landsat_iso10000m"
    "landsat_iso7500m"
    "landsat_iso5000m"
    "landsat_iso2500m"
    "landsat_all146"
    "landsat_pre2014"
    "landsat_hs_extra"
)

echo "Copying from Drive to cluster..."
for asset in "${ASSETS[@]}"; do
    echo "  Copying $asset..."
    rclone copy "gdrive:EarthEngine_Exports_${asset}" "$CLUSTER_DIR/${asset}/" --progress
done

echo "Verifying files landed..."
ls -lh "$CLUSTER_DIR"

echo "Deleting copied folders from Drive..."
# Safely purges only the specific targeted folders, leaving the rest of the Drive untouched
for asset in "${ASSETS[@]}"; do
    echo "  Purging EarthEngine_Exports_${asset}..."
    rclone purge "gdrive:EarthEngine_Exports_${asset}" 2>/dev/null
done

echo "Done."