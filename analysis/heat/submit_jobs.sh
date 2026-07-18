#!/bin/bash
# A unified launcher for Data Center Batch Jobs
# Usage: ./submit_jobs.sh [job_type]
# Example: ./submit_jobs.sh an_05_array
JOB_TYPE=$1
ACCOUNT="dspg_viz"
PARTITION="normal_q"
BASE_DIR="/projects/dspg_viz/ziadbushnaq1/data_centers"

# 1. Define the common environment setup commands
SETUP_CMDS="
module reset
module load R/4.4.2-gfbf-2024a
module load R-bundle-CRAN/2024.11-foss-2024a
export RSTUDIO_PANDOC=/apps/arch/software/RStudio-Server/2024.12.0+467-foss-2024a-Java-17-R-4.4.2/bin/pandoc
cd ${BASE_DIR}
"

# 2. Check if the user provided an argument
if [ -z "$JOB_TYPE" ]; then
    echo "Error: Please specify a job type."
    echo "Available options: duckdb_merge, an_04, an_05, an_05_array, an_07, an_08_hs, an_08_all, fig_viz, trend_vis, trim, conflict_check"
    exit 1
fi

# 3. Match the argument to the correct Slurm submission
case $JOB_TYPE in
    duckdb_merge)
        sbatch \
            --job-name=duckdb_merge \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=8 \
            --mem=48G \
            --time=2:00:00 \
            --output=${BASE_DIR}/duckdb_merge_%j.log \
            --wrap="$SETUP_CMDS
            Rscript gee/duckdbMerge.R"
        ;;

    an_04)
        sbatch \
            --job-name=an04_landsat \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=8 \
            --mem=196G \
            --time=6:00:00 \
            --output=${BASE_DIR}/an04_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/run_an_04.R"
        ;;
        
    an_05)
        sbatch \
            --job-name=an05_landsat \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=8 \
            --mem=96G \
            --time=12:00:00 \
            --output=${BASE_DIR}/run_an_05_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/run_an_05.R"
        ;;
        
    an_05_array)
        sbatch \
            --job-name=an05_array \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=8 \
            --mem=96G \
            --time=6:00:00 \
            --array=0-3 \
            --output=${BASE_DIR}/an05_%A_%a.log \
            --wrap="$SETUP_CMDS
            ASSETS=(landsat_iso10000m landsat_iso7500m landsat_iso5000m landsat_iso2500m)
            AID=\${ASSETS[\$SLURM_ARRAY_TASK_ID]}
            echo \"==== Rendering: \$AID (array task \$SLURM_ARRAY_TASK_ID) ====\"
            Rscript analysis/heat/run_an_05_one.R \"\$AID\""
        ;;
        
    an_07)
        sbatch \
            --job-name=an07_night \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=8 \
            --mem=48G \
            --time=4:00:00 \
            --output=${BASE_DIR}/an07_%j.log \
            --wrap="$SETUP_CMDS
            Rscript -e 'options(bitmapType = \"cairo\"); rmarkdown::render(\"analysis/heat/an_07_modis_night.Rmd\", output_format = rmarkdown::html_document(toc = TRUE, toc_float = TRUE, code_folding = \"hide\"), output_file = \"an_07_modis_night.html\")'"
        ;;
        
    an_08_hs)
        sbatch \
            --job-name=an08_hyper30 \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=8 \
            --mem=196G \
            --time=6:00:00 \
            --output=${BASE_DIR}/an08_hs_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/an_08_landsat_hyperscale.R"
        ;;
        
    an_08_all)
        sbatch \
            --job-name=an08_all30 \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=8 \
            --mem=196G \
            --time=12:00:00 \
            --output=${BASE_DIR}/an08_all_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/an_08_landsat_all.R"
        ;;
        
    fig_viz)
        sbatch \
            --job-name=fig_viz \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=8 \
            --mem=128G \
            --time=2:00:00 \
            --output=${BASE_DIR}/fig_viz_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/fig_distance_profile.R"
        ;;

    fig_event)
        sbatch \
            --job-name=fig_event \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=8 \
            --mem=128G \
            --time=2:00:00 \
            --output=${BASE_DIR}/fig_event_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/fig_event_study.R"
        ;;
        
    trend_vis)
        sbatch --job-name=hs_trend --account=$ACCOUNT --partition=$PARTITION \
            --nodes=1 --ntasks=1 --cpus-per-task=4 --mem=96G --time=2:00:00 \
            --output=${BASE_DIR}/hstrend_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/fig_hyperscale_trend.R"
        ;;

    eval_contam)
        sbatch \
            --job-name=eval_contam \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=8 \
            --mem=48G \
            --time=2:00:00 \
            --output=${BASE_DIR}/eval_contam_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/eval_contamination.R"
        ;;
        
    #claude
    contam_eval)
        sbatch \
            --job-name=contam_eval \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=8 \
            --mem=48G \
            --time=2:00:00 \
            --output=${BASE_DIR}/contam_eval_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/contamination_eval.R"
        ;;
        
    fig_contam)
        sbatch \
            --job-name=fig_contam \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=4 \
            --mem=32G \
            --time=1:00:00 \
            --output=${BASE_DIR}/fig_contam_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/fig_contamination.R"
        ;;
    
    fig_map)
        sbatch \
            --job-name=fig_map \
            --account=$ACCOUNT \
            --partition=$PARTITION \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=4 \
            --mem=64G \
            --time=1:00:00 \
            --output=${BASE_DIR}/fig_map_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/fig_facility_map.R"
        ;;
        
    trim)
        # Usage: ./submit_jobs.sh trim <asset_id> [pcts...]  e.g. trim landsat_hyperscale_all 1 5
        shift
        sbatch --job-name=sens_trim --account=$ACCOUNT --partition=$PARTITION \
            --nodes=1 --ntasks=1 --cpus-per-task=8 --mem=96G --time=6:00:00 \
            --output=${BASE_DIR}/trim_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/sens_lst_extreme.R $*"
        ;;

    conflict_check)
        sbatch --job-name=conflict_chk --account=$ACCOUNT --partition=$PARTITION \
            --nodes=1 --ntasks=1 --cpus-per-task=4 --mem=128G --time=2:00:00 \
            --output=${BASE_DIR}/conflict_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/filtering_check.R"
        ;;
        
    var_check)
        sbatch --job-name=var_chk --account=$ACCOUNT --partition=$PARTITION \
            --nodes=1 --ntasks=1 --cpus-per-task=4 --mem=128G --time=2:00:00 \
            --output=${BASE_DIR}/var_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/check_lst_variation.R"
        ;;
    
    seam)
        sbatch --job-name=seam_placebo --account=$ACCOUNT --partition=$PARTITION \
            --ntasks=1 --cpus-per-task=8 --mem=96G --time=3:00:00 \
            --output=${BASE_DIR}/seam_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/check_seam_placebo.R"
        ;;
        
    boot_loo)
        sbatch --job-name=boot_loo --account=$ACCOUNT --partition=$PARTITION \
            --ntasks=1 --cpus-per-task=2 --mem=240G --time=06:00:00 \
            --output=${BASE_DIR}/bootloo_%j.log \
            --wrap="$SETUP_CMDS
            Rscript analysis/heat/check_bootstrap_loo.R"
        ;;
        
    *)
        echo "Unknown job type: $JOB_TYPE"
        echo "Available options: duckdb_merge, an_04, an_05, an_05_array, an_07, an_08_hs, an_08_all, fig_viz, trend_vis, trim, conflict_check"
        exit 1
        ;;
        
esac

echo "Submission triggered for: $JOB_TYPE"