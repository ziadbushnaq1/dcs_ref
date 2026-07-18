import ee
import geopandas as gpd
import os

# ==============================================================================
# Initialization
# ==============================================================================

def initialize_ee(project_id):
    """Initializes Earth Engine."""
    ee.Initialize(project=project_id)


def upload_shapefiles_as_assets(shp_dir, asset_root, drive_folder=None):
    tasks = []
    shp_files = [f for f in os.listdir(shp_dir) if f.endswith(".shp")]

    for shp_file in shp_files:
        asset_name = os.path.splitext(shp_file)[0]
        shp_path = os.path.join(shp_dir, shp_file)

        # geemap.shp_to_ee() fails with EE Cloud API v1 ("expression must
        # be set") because it doesn't construct a proper ee.FeatureCollection
        # expression tree. Building it explicitly via geopandas works correctly.
        gdf = gpd.read_file(shp_path).to_crs('EPSG:4326')
        features = [
            ee.Feature(
                ee.Geometry(row.geometry.__geo_interface__),
                {col: row[col] for col in gdf.columns if col != 'geometry'}
            )
            for _, row in gdf.iterrows()
        ]
        fc = ee.FeatureCollection(features)

        # delete the asset if it already exists so reruns don't fail
        asset_id = f"{asset_root}/{asset_name}"
        try:
            ee.data.deleteAsset(asset_id)
            print(f"Deleted existing asset: {asset_name}")
        except ee.EEException:
            pass  # asset didn't exist yet, nothing to delete

        task = ee.batch.Export.table.toAsset(
            collection=fc,
            description=f"upload_{asset_name}",
            assetId=asset_id,
        )
        task.start()
        print(f"Queued asset upload: {asset_name}")
        tasks.append(task)

    return tasks

# ==============================================================================
# Base variables
# FIX: previously this was called fresh INSIDE every extract_and_export() call,
# which re-pulled the FeatureCollection from the asset and recomputed
# .size()/.toList() on every single batch x dataset combination (e.g. 8
# batches x 6 dataset variants = 48 redundant server round-trips). Call this
# ONCE per session and pass dc_list/total_size into extract_and_export.
# ==============================================================================

def get_base_variables(asset_id):
    """Returns the core grid areas and feature lists. Call this ONCE.

    asset_id: the asset name under "projects/curious-setup-498812-j5/assets/"
        (e.g. "isolated_dcs_yo").
    """
    all_data_centers = ee.FeatureCollection("projects/curious-setup-498812-j5/assets/" + asset_id)
    total_size = all_data_centers.size()
    dc_list = all_data_centers.toList(total_size)
    return dc_list, total_size


# ==============================================================================
# Sampling / export
#
# FIX 1 (critical): `scale` was hardcoded to 1000 inside sample_image
#   regardless of which image_collection was passed in. Every export -
#   including the 150 m Landsat product - was actually being sampled on a
#   1000 m grid, silently discarding the resolution gain you reprojected to
#   150 m for. `scale` is now a required argument so the caller sets it to
#   match the product being sampled (150 for Landsat, 1000 for MODIS / NLCD
#   generated at 1000 m).
#
# FIX 2: dc_list / total_size are now passed in (see get_base_variables note
#   above) instead of being recomputed on every call.
#
# A useful side effect of Fix 1: elevation is added inside sample_image and
# now correctly gets sampled at whatever scale you pass in, so it lines up
# with the Landsat 150 m grid when you export Landsat, and with the MODIS
# 1000 m grid when you export MODIS - no separate elevation export needed.
# ==============================================================================

def extract_and_export(batch_index, total_batches, dc_list, total_size,
                        image_collection, task_name, scale, drop_nulls=True, buffer_m=7500,
                        destination='drive', drive_folder='EarthEngine_Exports',
                        bucket=None, gcs_prefix='EarthEngine_Exports', skip_existing=True):
    """Handles the sampling and exporting for a given batch.

    scale: sampling resolution in meters. Must match the resolution of
        `image_collection` (150 for Landsat LST, 1000 for MODIS LST and for
        an NLCD collection generated with scale=1000).

    destination: 'drive' (default, matches original behavior) or 'gcs'.
        Earth Engine batch exports can only land in Google Drive or Google
        Cloud Storage - never directly on a local/HPC filesystem - so if you
        want these pulled onto your Open OnDemand working directory without
        going through the Drive web UI, use destination='gcs' with a bucket
        you have write access to, then download with gsutil/gcloud (see
        usage note at the bottom of this file).
    bucket: required if destination='gcs'. Your GCS bucket name (no gs://).
    """

    batch_num = batch_index + 1
    description = f'{task_name}_Batch_{batch_num}'

    # checks GEE task history before queuing. If a task with this exact
    # description already completed, skip it so rerunning the loop doesn't
    # create duplicate Drive files. getTaskList() covers the last ~5000 tasks.
    # extended to also catch currently queued/running tasks
    # ADD — checks only the most recent record for this description, not any historical record
    if skip_existing:
        existing = ee.data.getTaskList()
        for t in existing:
            if t['description'] == description:
                if t['state'] in ('COMPLETED', 'READY', 'RUNNING'):
                    print(f"Skipping {description} - {t['state'].lower()}")
                    return None
                else:
                    # Most recent record is FAILED or CANCELLED — re-queue
                    break
            
    if not skip_existing:
        print(f"WARNING: skip_existing=False for {description}. "
              f"Ensure old Drive files are deleted first to avoid "
              f"duplicates in read_batches().")
    
    batch_size = total_size.divide(total_batches).ceil()
    start_index = batch_size.multiply(batch_index)
    end_index = batch_size.multiply(batch_index + 1)

    batch_list = dc_list.slice(start_index, end_index)
    grid_areas = ee.FeatureCollection(batch_list).map(lambda f: f.buffer(buffer_m))

    elevation = ee.Image('USGS/SRTMGL1_003').rename('Elevation').toFloat()

    def sample_image(image):
        image_to_sample = image.addBands(elevation).addBands(ee.Image.pixelLonLat())
        return image_to_sample.sampleRegions(
            collection=grid_areas,
            scale=scale,
            geometries=False,
            tileScale=16
        )

    pixel_grid = image_collection.map(sample_image).flatten()

    if drop_nulls:
        pixel_grid = pixel_grid.filter(ee.Filter.notNull(['LST_Celsius']))
    
    if destination == 'drive':
        task = ee.batch.Export.table.toDrive(
            collection=pixel_grid,
            description=description,
            folder=drive_folder,
            fileFormat='CSV'
        )
    elif destination == 'gcs':
        if not bucket:
            raise ValueError("bucket is required when destination='gcs'")
        task = ee.batch.Export.table.toCloudStorage(
            collection=pixel_grid,
            description=description,
            bucket=bucket,
            fileNamePrefix=f'{gcs_prefix}/{description}',
            fileFormat='CSV'
        )
    else:
        raise ValueError("destination must be 'drive' or 'gcs'")

    task.start()
    print(f"Queued: {description} @ scale={scale}m -> {destination}")
    return task


def wait_for_tasks(tasks, poll_seconds=30):
    """Blocks until every task in `tasks` (a list of ee.batch.Task, e.g.
    collected from extract_and_export's return value) finishes, printing
    status as it goes. Optional - useful right before a download step so you
    don't try to pull down files that aren't done yet.
    """
    import time
    pending = list(tasks)
    while pending:
        still_running = []
        for t in pending:
            state = t.status()['state']
            if state in ('READY', 'RUNNING', 'UNSUBMITTED'):
                still_running.append(t)
            elif state == 'COMPLETED':
                print(f"done: {t.status()['description']}")
            else:
                print(f"{state}: {t.status()['description']} - {t.status().get('error_message')}")
        pending = still_running
        if pending:
            print(f"{len(pending)} task(s) still running, checking again in {poll_seconds}s...")
            time.sleep(poll_seconds)
    print("All tasks finished.")



# ==============================================================================
# Landsat — individual LST observations at 150 m
#
# FIX 3: end_date used Dec 31, but ee.ImageCollection.filterDate() treats the
#   end bound as EXCLUSIVE. Any scene captured on Dec 31 itself was being
#   silently dropped every year. Use Jan 1 of the following year instead.
#
# NOTE: apply_landsat_scale() and apply_qa_mask() are called below but were
#   not included in the script you shared - make sure your real
#   implementations are in scope. If you don't already have them, reference
#   implementations are provided further down. The most common bug here is
#   applying the *optical* band scale factor (0.0000275 / -0.2) to the
#   thermal band by mistake - ST_B10 uses a different scale/offset
#   (0.00341802 / +149.0, output in Kelvin), which is what your later
#   `.subtract(273.15)` assumes it already received.
# ==============================================================================

def generate_all_landsat_observations(start_year, end_year):
    """Generates individual LST observations from Landsat 8 and 9 at 150m scale."""
    start_date = ee.Date.fromYMD(start_year, 1, 1)
    end_date = ee.Date.fromYMD(end_year + 1, 1, 1)  # FIX: inclusive of Dec 31

    l8 = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2") \
        .filterDate(start_date, end_date) \
        .filter(ee.Filter.lt('CLOUD_COVER', 30))

    l9 = ee.ImageCollection("LANDSAT/LC09/C02/T1_L2") \
        .filterDate(start_date, end_date) \
        .filter(ee.Filter.lt('CLOUD_COVER', 30))

    combined_landsat = l8.merge(l9)

    def process_landsat(image):
        scaled = apply_landsat_scale(image)
        masked = apply_qa_mask(scaled)

        lst_celsius = masked.select('ST_B10').subtract(273.15).rename('LST_Celsius')

        lst_150 = lst_celsius \
            .setDefaultProjection(crs='EPSG:4326', scale=30) \
            .reduceResolution(reducer=ee.Reducer.mean(), maxPixels=1024) \
            .reproject(crs='EPSG:4326', scale=150)

        date_num = ee.Number.parse(image.date().format('YYYYMMdd'))
        date_band = ee.Image.constant(date_num).rename('date_yyyymmdd').toInt()

        year = ee.Number.parse(image.date().format('YYYY'))
        year_band = ee.Image.constant(year).rename('year').toInt()

        return lst_150.addBands([date_band, year_band]).toFloat() \
            .set('system:time_start', image.get('system:time_start'))

    return combined_landsat.map(process_landsat)

def generate_all_landsat_obs_30m(start_year, end_year, aoi=None):
    """All L8/L9 LST observations at NATIVE 30 m.
    Bands: LST_Celsius, Emissivity, ST_uncertainty, scene_cloud_cover,
           date_yyyymmdd, year, month."""
    start_date = ee.Date.fromYMD(start_year, 1, 1)
    end_date   = ee.Date.fromYMD(end_year + 1, 1, 1)
    filter_geom = aoi if aoi is not None else ee.Geometry.BBox(-125, 24, -66, 50)

    l8 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2') \
        .filterDate(start_date, end_date) \
        .filter(ee.Filter.lt('CLOUD_COVER', 30)) \
        .filterBounds(filter_geom)
    l9 = ee.ImageCollection('LANDSAT/LC09/C02/T1_L2') \
        .filterDate(start_date, end_date) \
        .filter(ee.Filter.lt('CLOUD_COVER', 30)) \
        .filterBounds(filter_geom)
    combined = l8.merge(l9)

    def process_obs(image):
        scaled = apply_landsat_scale(image)
        masked = apply_qa_mask(scaled)

        lst   = masked.select('ST_B10').subtract(273.15).rename('LST_Celsius')
        emis  = masked.select('ST_EMIS').multiply(0.0001).rename('Emissivity')
        st_qa = masked.select('ST_QA').multiply(0.01).rename('ST_uncertainty')

        cc_band = ee.Image.constant(image.getNumber('CLOUD_COVER')) \
            .rename('scene_cloud_cover').toFloat().updateMask(lst.mask())
        date_band = ee.Image.constant(
            ee.Number.parse(image.date().format('YYYYMMdd'))) \
            .rename('date_yyyymmdd').toInt().updateMask(lst.mask())
        year_band  = ee.Image.constant(
            ee.Number.parse(image.date().format('YYYY'))).rename('year').toInt()
        month_band = ee.Image.constant(
            ee.Number.parse(image.date().format('MM'))).rename('month').toInt()

        return lst.addBands([emis, st_qa, cc_band, date_band, year_band, month_band]) \
                  .toFloat() \
                  .set('system:time_start', image.get('system:time_start'))

    return combined.map(process_obs)

def generate_all_l5_obs_30m(start_year, end_year, aoi=None):
    """L5 TM LST observations 1984-2011 at 30 m (thermal native ~120 m,
    distributed resampled to 30 m). Bands match generate_all_landsat_obs_30m
    plus sensor=5. ST_EMIS/ST_QA exist for L5 C2 and are included."""
    start_date = ee.Date.fromYMD(start_year, 1, 1)
    end_date   = ee.Date.fromYMD(end_year + 1, 1, 1)
    filter_geom = aoi if aoi is not None else ee.Geometry.BBox(-125, 24, -66, 50)

    l5 = ee.ImageCollection('LANDSAT/LT05/C02/T1_L2') \
        .filterDate(start_date, end_date) \
        .filter(ee.Filter.lt('CLOUD_COVER', 30)) \
        .filterBounds(filter_geom)

    def process_obs(image):
        thermal = image.select('ST_B6').multiply(0.00341802).add(149.0)
        qa   = image.select('QA_PIXEL')
        mask = qa.bitwiseAnd(1 << 3).eq(0) \
                 .And(qa.bitwiseAnd(1 << 4).eq(0)) \
                 .And(qa.bitwiseAnd(1 << 5).eq(0))
        lst  = thermal.updateMask(mask).subtract(273.15).rename('LST_Celsius')

        emis  = image.select('ST_EMIS').multiply(0.0001) \
                     .updateMask(lst.mask()).rename('Emissivity')
        st_qa = image.select('ST_QA').multiply(0.01) \
                     .updateMask(lst.mask()).rename('ST_uncertainty')
        cc    = ee.Image.constant(image.getNumber('CLOUD_COVER')) \
                     .rename('scene_cloud_cover').toFloat().updateMask(lst.mask())
        dt    = ee.Image.constant(ee.Number.parse(image.date().format('YYYYMMdd'))) \
                     .rename('date_yyyymmdd').toInt().updateMask(lst.mask())
        yr    = ee.Image.constant(ee.Number.parse(image.date().format('YYYY'))) \
                     .rename('year').toInt()
        mo    = ee.Image.constant(ee.Number.parse(image.date().format('MM'))) \
                     .rename('month').toInt()
        sens  = ee.Image.constant(5).rename('sensor').toInt()

        return lst.addBands([emis, st_qa, cc, dt, yr, mo, sens]) \
                  .toFloat().set('system:time_start', image.get('system:time_start'))

    return l5.map(process_obs)

def generate_monthly_obs_landsat(start_year, end_year, window_days=7, aoi=None):
    """Exports individual Landsat 8/9 LST observations restricted to the
    first window_days days of each month, with a month band added.

    Rather than picking one image per month in GEE (which either picks
    one globally-best scene that only covers one WRS-2 tile, or uses
    qualityMosaic which mixes pixels from different scenes within a DC
    buffer in path/row overlap zones), this exports all candidate
    observations and lets R select the single best scene per DC per month.

    The R selection step (best_date_per_dc in an_04/an_05) groups by
    DC × month × date_yyyymmdd, counts valid LST pixels per date, and
    keeps only the date with the most coverage. This guarantees all
    pixels within a DC buffer come from the same scene.

    window_days=14 ensures at least one L8 or L9 pass per WRS-2 tile
    (each has a 16-day revisit; combined ~8-day at most US latitudes).

    Output columns: LST_Celsius, date_yyyymmdd, year, month.
    """
    start_date = ee.Date.fromYMD(start_year, 1, 1)
    end_date   = ee.Date.fromYMD(end_year + 1, 1, 1)

    conus = ee.Geometry.BBox(-125, 24, -66, 50)

    filter_geom = aoi if aoi is not None else ee.Geometry.BBox(-125, 24, -66, 50)
    
    l8 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2') \
        .filterDate(start_date, end_date) \
        .filter(ee.Filter.lt('CLOUD_COVER', 30)) \
        .filterBounds(filter_geom)
    l9 = ee.ImageCollection('LANDSAT/LC09/C02/T1_L2') \
        .filterDate(start_date, end_date) \
        .filter(ee.Filter.lt('CLOUD_COVER', 30)) \
        .filterBounds(filter_geom)

    combined = l8.merge(l9)

    years  = ee.List.sequence(start_year, end_year)
    months = ee.List.sequence(1, 12)

    def map_years(year):
        def map_months(month):
            start  = ee.Date.fromYMD(year, month, 1)
            end    = start.advance(window_days, 'day')
            window = combined.filterDate(start, end)

            def process_obs(image):
                scaled = apply_landsat_scale(image)
                masked = apply_qa_mask(scaled)
                lst    = masked.select('ST_B10') \
                               .subtract(273.15).rename('LST_Celsius')

                lst_150 = lst \
                    .setDefaultProjection(crs='EPSG:4326', scale=30) \
                    .reduceResolution(reducer=ee.Reducer.mean(), maxPixels=1024) \
                    .reproject(crs='EPSG:4326', scale=150)

                date_num  = ee.Number.parse(image.date().format('YYYYMMdd'))
                date_band = ee.Image.constant(date_num) \
                    .rename('date_yyyymmdd').toInt() \
                    .updateMask(lst_150.mask())

                year_band  = ee.Image.constant(year).rename('year').toInt()
                month_band = ee.Image.constant(month).rename('month').toInt()

                return lst_150 \
                    .addBands(date_band) \
                    .addBands(year_band) \
                    .addBands(month_band) \
                    .toFloat() \
                    .set('system:time_start', image.get('system:time_start'))

            return window.map(process_obs).toList(window.size().max(1))

        return months.map(map_months)

    obs_list = years.map(map_years).flatten().flatten()
    return ee.ImageCollection.fromImages(obs_list)


def generate_monthly_landsat_mean(start_year, end_year):
    """Generates monthly mean daytime LST from Landsat 8 and 9 at 150m.

    Landsat 8/9 are sun-synchronous with a ~10am local overpass — there is
    no nighttime thermal band. This function is daytime only.

    Returns one image per month with:
      LST_Celsius: mean of all QA-masked clear observations within the month.
      n_obs:       number of valid scenes contributing to the mean (0 = fully
                   cloud-obscured, useful for filtering in R).
      year, month bands.

    The fallback image guarantees LST_Celsius always appears in the export
    schema even for months with no valid scenes (e.g. early 2013 before L8
    had full coverage), preventing the column-dropping bug seen in MODIS.
    """
    start_date = ee.Date.fromYMD(start_year, 1, 1)
    end_date   = ee.Date.fromYMD(end_year + 1, 1, 1)

    l8 = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2") \
        .filterDate(start_date, end_date) \
        .filter(ee.Filter.lt('CLOUD_COVER', 30))

    l9 = ee.ImageCollection("LANDSAT/LC09/C02/T1_L2") \
        .filterDate(start_date, end_date) \
        .filter(ee.Filter.lt('CLOUD_COVER', 30))

    combined = l8.merge(l9)

    def process_scene(image):
        scaled  = apply_landsat_scale(image)
        masked  = apply_qa_mask(scaled)
        lst     = masked.select('ST_B10').subtract(273.15).rename('LST_Celsius')
        lst_150 = lst \
            .setDefaultProjection(crs='EPSG:4326', scale=30) \
            .reduceResolution(reducer=ee.Reducer.mean(), maxPixels=1024) \
            .reproject(crs='EPSG:4326', scale=150)
        return lst_150.toFloat() \
            .set('system:time_start', image.get('system:time_start'))

    processed = combined.map(process_scene)

    years  = ee.List.sequence(start_year, end_year)
    months = ee.List.sequence(1, 12)

    def map_years(year):
        def map_months(month):
            start   = ee.Date.fromYMD(year, month, 1)
            end     = start.advance(1, 'month')
            monthly = processed.filterDate(start, end)

            fallback   = ee.Image.constant(0).subtract(273.15).rename('LST_Celsius').updateMask(ee.Image(0))
            safe       = monthly.merge(ee.ImageCollection([fallback]))

            mean_lst   = safe.mean()
            n_obs      = monthly.count().rename('n_obs')
            year_band  = ee.Image.constant(year).rename('year').toInt()
            month_band = ee.Image.constant(month).rename('month').toInt()

            return mean_lst.addBands([n_obs, year_band, month_band]).toFloat().set('system:time_start', start.millis())

        return months.map(map_months)

    monthly_list = years.map(map_years).flatten()
    return ee.ImageCollection.fromImages(monthly_list)

def generate_monthly_best_landsat(start_year, end_year, window_days=14):
    """For each month, selects the single best Landsat scene (L8 or L9) from
    the first window_days days of the month, where 'best' = the scene with
    the lowest CLOUD_COVER metadata value.

    CLOUD_COVER is a scene-level percentage already stored on every Landsat
    image in the collection. It is derived from the same QA_PIXEL band used
    in apply_qa_mask, so it is the correct proxy for scene quality. Sorting
    by CLOUD_COVER avoids any reduceRegion call and the reprojection-size
    errors that come with it.

    Output columns: LST_Celsius, date_yyyymmdd, year, month.
    """
    start_date = ee.Date.fromYMD(start_year, 1, 1)
    end_date   = ee.Date.fromYMD(end_year + 1, 1, 1)

    # filterBounds restricts candidates to scenes whose footprint intersects
    # CONUS. Without this, sort('CLOUD_COVER').first() picks the globally
    # clearest scene — often Antarctica or open ocean — which has no overlap
    # with US DC locations and produces 2-byte empty output CSVs.
    conus = ee.Geometry.BBox(-125, 24, -66, 50)
    
    l8 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2') \
        .filterDate(start_date, end_date) \
        .filter(ee.Filter.lt('CLOUD_COVER', 30)) \
        .filterBounds(conus)
    l9 = ee.ImageCollection('LANDSAT/LC09/C02/T1_L2') \
        .filterDate(start_date, end_date) \
        .filter(ee.Filter.lt('CLOUD_COVER', 30)) \
        .filterBounds(conus)
    
    combined = l8.merge(l9)

    years  = ee.List.sequence(start_year, end_year)
    months = ee.List.sequence(1, 12)

    def process_scene(image):
        """Apply scaling, QA masking, and 30m→150m reproject to one scene."""
        scaled = apply_landsat_scale(image)
        masked = apply_qa_mask(scaled)
        lst    = masked.select('ST_B10') \
                       .subtract(273.15).rename('LST_Celsius')

        lst_150 = lst \
            .setDefaultProjection(crs='EPSG:4326', scale=30) \
            .reduceResolution(reducer=ee.Reducer.mean(), maxPixels=1024) \
            .reproject(crs='EPSG:4326', scale=150)

        date_num  = ee.Number.parse(image.date().format('YYYYMMdd'))
        date_band = ee.Image.constant(date_num) \
            .rename('date_yyyymmdd').toInt() \
            .updateMask(lst_150.mask())

        return lst_150.addBands(date_band).toFloat()

    def map_years(year):
        def map_months(month):
            start  = ee.Date.fromYMD(year, month, 1)
            end    = start.advance(window_days, 'day')
            window = combined.filterDate(start, end)

            # Sort ascending by CLOUD_COVER so the least-cloudy scene is
            # first, then take that single image. This replaces the old
            # score_scene / reduceRegion approach, which required materialising
            # lst_150 at 150m over the full scene footprint and hit GEE's
            # internal reprojection grid size limit (10305x7435 pixels).
            best_raw       = ee.Image(window.sort('CLOUD_COVER').first())
            best_processed = process_scene(best_raw)

            # Fallback for months where no Landsat scene falls in the window
            # (common in early 2013 before L8 had full coverage, or in
            # persistently overcast months that all exceed CLOUD_COVER 30).
            fallback = ee.Image.constant(0).subtract(273.15) \
                .rename('LST_Celsius').updateMask(ee.Image(0)) \
                .addBands(
                    ee.Image.constant(0).rename('date_yyyymmdd').toInt()
                )

            best = ee.Image(ee.Algorithms.If(
                window.size().gt(0),
                best_processed,
                fallback
            ))

            year_band  = ee.Image.constant(year).rename('year').toInt()
            month_band = ee.Image.constant(month).rename('month').toInt()

            return best.select(['LST_Celsius', 'date_yyyymmdd']) \
                .addBands([year_band, month_band]).toFloat() \
                .set('system:time_start', start.millis())

        return months.map(map_months)

    monthly_list = years.map(map_years).flatten()
    return ee.ImageCollection.fromImages(monthly_list)

def apply_landsat_scale(image):
    """Applies the official Collection 2 Level-2 scale factors.

    Reference: https://www.usgs.gov/landsat-missions/landsat-collection-2-surface-temperature
    Optical bands (SR_B*): DN * 0.0000275 - 0.2  -> reflectance
    Thermal band (ST_B10): DN * 0.00341802 + 149.0 -> Kelvin
    """
    optical_bands = image.select('SR_B.').multiply(0.0000275).add(-0.2)
    thermal_band = image.select('ST_B10').multiply(0.00341802).add(149.0)
    return image.addBands(optical_bands, overwrite=True) \
                 .addBands(thermal_band, overwrite=True)


def apply_qa_mask(image):
    """Masks cloud, cloud shadow, cirrus, and snow using the QA_PIXEL bitmask."""
    qa = image.select('QA_PIXEL')
    cirrus_bit = 1 << 2
    cloud_bit = 1 << 3
    cloud_shadow_bit = 1 << 4
    snow_bit = 1 << 5
    mask = qa.bitwiseAnd(cirrus_bit).eq(0) \
        .And(qa.bitwiseAnd(cloud_bit).eq(0)) \
        .And(qa.bitwiseAnd(cloud_shadow_bit).eq(0)) \
        .And(qa.bitwiseAnd(snow_bit).eq(0))
    return image.updateMask(mask)


# ==============================================================================
# MODIS — individual daily LST observations at 1000 m
#
# FIX 3 (same end-date issue as Landsat): Dec 31 was being excluded.
# ADDED: QC_Day quality masking. The original kept every pixel regardless of
#   retrieval quality - bits 0-1 of QC_Day == 00 means "LST produced, good
#   quality"; anything else is cloud-affected or otherwise unreliable and was
#   silently included in the output before.
# ==============================================================================

def generate_daily_observations(start_year, end_year):
    start_date = ee.Date.fromYMD(start_year, 1, 1)
    end_date = ee.Date.fromYMD(end_year + 1, 1, 1)  # FIX: inclusive of Dec 31

    terra_daily = ee.ImageCollection("MODIS/061/MOD11A1").filterDate(start_date, end_date)

    def process_daily(image):
        date_num = ee.Number.parse(image.date().format('YYYYMMdd'))
        date_band = ee.Image.constant(date_num).rename('date_yyyymmdd').toInt()

        year = ee.Number.parse(image.date().format('YYYY'))
        year_band = ee.Image.constant(year).rename('year').toInt()

        qc = image.select('QC_Day')
        good_quality = qc.bitwiseAnd(3).eq(0)

        lst_day = image.select('LST_Day_1km').updateMask(good_quality).multiply(0.02).subtract(273.15).rename('LST_Celsius')

        return lst_day.addBands([date_band, year_band]).toFloat().set('system:time_start', image.get('system:time_start'))

    return terra_daily.map(process_daily)


# ==============================================================================
# Monthly "first week" observation (day / night)
#
# REPLACED: the old version took a plain .median() across the first 7 days
# with no quality masking, and required two near-identical functions for
# day/night. MOD11A1 doesn't report a whole-image cloud cover stat the way
# Landsat does - clear-sky/cloud status is per PIXEL, baked into QC_Day /
# QC_Night - so a given data center's clearest day within the window can
# differ from another data center's. Forcing a single calendar date for
# every pixel (e.g. via image-level cloud sorting) isn't actually optimal.
#
# This version instead: masks out any pixel-day that fails the QC mandatory
# quality flag, then averages whatever good-quality days remain in the
# window for each pixel independently. It also returns `n_clear_days` so
# you know, per pixel per month, how many observations the average is built
# from (0 = fully cloud-obscured that week, useful to filter/flag in R).
#
# If you'd rather have a literal single observation per pixel (not an
# average) instead, see generate_monthly_best_day() below, which uses
# qualityMosaic to pick, per pixel, the single clearest day in the window
# and carries its actual date along with it.
# ==============================================================================

def generate_monthly_clearsky_mean(start_year, end_year, period='day', window_days=14):
    """One value per pixel per month: the mean of all QC-good LST
    observations within the first `window_days` days of the month.

    period: 'day' or 'night'.
    Adds an `n_clear_days` band recording how many good-quality days
    contributed to each pixel's mean that month (0 if none did).
    """
    if period == 'day':
        lst_band, qc_band, out_name = 'LST_Day_1km', 'QC_Day', 'LST_Celsius'
    elif period == 'night':
        lst_band, qc_band, out_name = 'LST_Night_1km', 'QC_Night', 'LST_Night_Celsius'
    else:
        raise ValueError("period must be 'day' or 'night'")

    years = ee.List.sequence(start_year, end_year)
    months = ee.List.sequence(1, 12)

    def map_years(year):
        def map_months(month):
            start_date = ee.Date.fromYMD(year, month, 1)
            end_date = start_date.advance(window_days, 'day')

            collection = ee.ImageCollection("MODIS/061/MOD11A1").filterDate(start_date, end_date)

            def mask_quality(image):
                qc = image.select(qc_band)
                good_quality = qc.bitwiseAnd(3).eq(0)  # mandatory QA bits 0-1 == 00
                return image.select(lst_band).updateMask(good_quality).multiply(0.02).subtract(273.15).rename(out_name)

            quality_masked = collection.map(mask_quality)

            fallback = ee.Image.constant(0).multiply(0.02).subtract(273.15).rename(out_name).updateMask(ee.Image(0))
            safe_collection = quality_masked.merge(ee.ImageCollection([fallback]))
            mean_lst = safe_collection.mean()
            n_clear = quality_masked.count().rename('n_clear_days')

            year_band = ee.Image.constant(year).rename('year').toInt()
            month_band = ee.Image.constant(month).rename('month').toInt()

            return mean_lst.addBands([n_clear, year_band, month_band]).toFloat().set('system:time_start', start_date.millis())

        return months.map(map_months)

    monthly_list = years.map(map_years).flatten()
    return ee.ImageCollection.fromImages(monthly_list)


def generate_monthly_best_day(start_year, end_year, period='day', window_days=7):
    """For each month, selects the single best MODIS daily image from the first
    window_days days of the month, where 'best' = the day with the most
    QC-valid pixels over CONUS. Every pixel in the output for a given month
    comes from the same calendar date — unlike qualityMosaic, which independently
    picks the best source date per pixel and can produce a patchwork of different
    dates within a single data center buffer.

    period: 'day' uses QC_Day / LST_Day_1km; 'night' uses QC_Night / LST_Night_1km.
    Output columns: LST_Celsius (or LST_Night_Celsius), date_yyyymmdd, year, month.
    """
    if period == 'day':
        lst_band, qc_band, out_name = 'LST_Day_1km', 'QC_Day', 'LST_Celsius'
    elif period == 'night':
        lst_band, qc_band, out_name = 'LST_Night_1km', 'QC_Night', 'LST_Night_Celsius'
    else:
        raise ValueError("period must be 'day' or 'night'")

    # CONUS bounding box used to count valid pixels per image for ranking.
    # Using CONUS rather than a global extent keeps the count relevant to
    # where your data centers are located and avoids counting clear pixels
    # over oceans or other continents as "good sky conditions."
    conus = ee.Geometry.BBox(-125, 24, -66, 50)

    years  = ee.List.sequence(start_year, end_year)
    months = ee.List.sequence(1, 12)

    def map_years(year):
        def map_months(month):
            start_date = ee.Date.fromYMD(year, month, 1)
            end_date   = start_date.advance(window_days, 'day')

            collection = ee.ImageCollection('MODIS/061/MOD11A1') \
                .filterDate(start_date, end_date)

            def score_image(image):
                qc           = image.select(qc_band)
                good_quality = qc.bitwiseAnd(3).eq(0)
                lst          = image.select(lst_band) \
                                   .updateMask(good_quality) \
                                   .multiply(0.02).subtract(273.15) \
                                   .rename(out_name)

                # Count valid (unmasked) pixels over CONUS at 1000m as a
                # ranking score. More valid pixels = clearer sky = better day.
                # scale=1000 matches MODIS native resolution, keeping computation fast.
                result  = lst.mask().reduceRegion(
                    reducer   = ee.Reducer.sum(),
                    geometry  = conus,
                    scale     = 1000,
                    maxPixels = 1e8
                )
                # Guard against null result (fully cloudy image over CONUS).
                n_valid = ee.Number(ee.Algorithms.If(
                    result.contains(out_name),
                    result.get(out_name),
                    ee.Number(0)
                ))

                date_num  = ee.Number.parse(image.date().format('YYYYMMdd'))
                date_band = ee.Image.constant(date_num) \
                    .rename('date_yyyymmdd').toInt()

                return lst.addBands(date_band).toFloat() \
                    .set('n_valid', n_valid) \
                    .set('system:time_start', image.get('system:time_start'))

            scored = collection.map(score_image)

            # Sort descending by n_valid so the day with the most clear pixels
            # is first. Take that single image — all pixels for this month/DC
            # come from this one date.
            best = ee.Image(scored.sort('n_valid', False).first())

            year_band  = ee.Image.constant(year).rename('year').toInt()
            month_band = ee.Image.constant(month).rename('month').toInt()

            return best.select([out_name, 'date_yyyymmdd']) \
                .addBands([year_band, month_band]).toFloat() \
                .set('system:time_start', start_date.millis())

        return months.map(map_months)

    monthly_list = years.map(map_years).flatten()
    return ee.ImageCollection.fromImages(monthly_list)


# ==============================================================================
# Annual NLCD land-cover proportions
#
# FIX 4: `scale` is now a parameter instead of hardcoded to 1000. You need
#   land cover proportions matched to EACH pixel grid you're sampling onto -
#   call this with scale=1000 to align with MODIS, and again with scale=150
#   to align with Landsat. Previously this could only ever be joined to the
#   MODIS grid.
# FIX 5: only the upper year bound was clamped (to 2023). The Annual NLCD
#   Collection 1.0 asset covers 1985-2023 (confirmed against current USGS/
#   MRLC documentation as of mid-2026) - the lower bound is now clamped too,
#   so calling this with an earlier start_year won't return a fully masked
#   image from an empty .first().
# ==============================================================================

NLCD_MIN_YEAR = 1985  # verify against the asset's current coverage if it's been extended
NLCD_MAX_YEAR = 2023  # verify against the asset's current coverage if it's been extended


def generate_annual_nlcd(start_year, end_year, scale=1000):
    """Generates annual NLCD land-cover proportions at the given `scale`.

    Call with scale=1000 to align with the MODIS pixel grid, or scale=150 to
    align with the Landsat pixel grid.
    """
    years = ee.List.sequence(start_year, end_year)

    def process_year(year):
        nlcd_year = ee.Number(year)
        nlcd_year = ee.Number(ee.Algorithms.If(nlcd_year.gt(NLCD_MAX_YEAR), NLCD_MAX_YEAR, nlcd_year))
        nlcd_year = ee.Number(ee.Algorithms.If(nlcd_year.lt(NLCD_MIN_YEAR), NLCD_MIN_YEAR, nlcd_year))

        nlcd = ee.ImageCollection("projects/sat-io/open-datasets/USGS/ANNUAL_NLCD/LANDCOVER") \
            .filter(ee.Filter.calendarRange(nlcd_year, nlcd_year, 'year')) \
            .first() \
            .select([0], ['Landcover'])

        nlcd_classes = [11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 51, 52, 71, 72, 73, 74, 81, 82, 90, 95]
        nlcd_bands = [nlcd.eq(c).rename(f'NLCD_{c}') for c in nlcd_classes]
        nlcd_stack = ee.Image(nlcd_bands)

        nlcd_proportions = nlcd_stack.reduceResolution(
            reducer=ee.Reducer.mean(),
            maxPixels=4096  # bumped up to comfortably cover both 150m and 1000m targets
        ).reproject(
            crs='EPSG:4326',
            scale=scale
        )

        year_band = ee.Image.constant(year).rename('year').toInt()

        return nlcd_proportions.addBands(year_band).toFloat() \
            .set('system:time_start', ee.Date.fromYMD(year, 1, 1).millis())

    annual_collection = ee.ImageCollection.fromImages(years.map(process_year))
    return annual_collection


# ==============================================================================
# Usage note
#
# This file is a MODULE - it only defines functions, it doesn't run anything
# on import. Run the actual extraction/export from a separate notebook, e.g.:
#
#   import gee_functions as dc
#   dc.initialize_ee('your-google-cloud-project-id')
#   dc_list, total_size = dc.get_base_variables('isolated_dcs_yo')
#   ...
#
# See dc_lst_run.ipynb for the full runnable workflow.
#
# GETTING FILES ONTO YOUR HPC WORKING DIRECTORY:
# Export.table.toDrive()/.toCloudStorage() both run as asynchronous,
# server-side batch jobs - nothing lands on disk where you're running
# Python/R, regardless of destination. To pull CSVs onto your Open OnDemand
# working directory:
#
#   destination='gcs' (recommended for HPC, fully scriptable):
#     extract_and_export(..., destination='gcs', bucket='your-bucket-name')
#     then, from a terminal on the cluster (gcloud SDK usually available as
#     a module, e.g. `module load google-cloud-sdk`):
#       gcloud storage cp -r gs://your-bucket-name/EarthEngine_Exports ./data/raw/
#
#   destination='drive' (default, matches the original script):
#     files land in My Drive > EarthEngine_Exports once each task completes.
#     Either download manually from drive.google.com, or, if your cluster
#     has rclone configured with a Drive remote:
#       rclone copy gdrive:EarthEngine_Exports ./data/raw/ -P
# ==============================================================================