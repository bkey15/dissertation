#!/bin/bash
#SBATCH -A p33143
#SBATCH -p short
#SBATCH -N 1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=24
#SBATCH -t 01:00:00
#SBATCH --mem-per-cpu=4G
#SBATCH --array=1-240
#SBATCH --job-name=imp_dml_fits_bcr_slx_gen_tree_xgb_%a
#SBATCH --output=imp_dml_fits_bcr_slx_gen_tree_xgb.%a.out
#SBATCH --mail-type=ALL
#SBATCH --mail-user=briankey@u.northwestern.edu

module use ~/privatemodules
module load R/4.5.1-openblas

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

# Create a private temporary directory for this array task
export TMPDIR=/tmp/$USER/${SLURM_JOB_ID}_${SLURM_ARRAY_TASK_ID}
mkdir -p "$TMPDIR"
trap 'rm -rf "$TMPDIR"' EXIT

cd /projects/p33143

Rscript "scripts/ch1/iii_bcr_slx/1_fit_model/b_tree/6b(i)_fit_tree_xgb_model_dml_bcr_slx_gen_quest.R"

# submit this script with sbatch from the command line:
# cd /home/bdk6881/job_scripts/ch1/iii_bcr_slx/1_fit_model/b_tree/imp_dml_fits_bcr_slx_gen_tree_xgb
# sbatch i_imp_dml_fits_bcr_slx_gen_tree_xgb.sh