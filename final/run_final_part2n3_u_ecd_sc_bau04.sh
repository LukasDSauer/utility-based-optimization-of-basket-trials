#!/bin/bash

# GENERATED AUTOMATICALLY BY gen_run_final_files.R, DO NOT EDIT BY HAND!

# SLURM SETTINGS
#SBATCH --nodes=1
#SBATCH --ntasks=100
#SBATCH --time=9-12
#SBATCH --mem=400g
#SBATCH --job-name=part2n3_u_ecd_sc_bau04
#SBATCH --partition=compute
#SBATCH --output="out/final_part2n3_u_ecd_sc_bau04_slurm-%j.out"

# SESSION INFO
echo "Comparison study: Utility-based optimization of Fujikawa’s basket trial design"
echo "Log of job execution on bwForCluster BinAC 2"
echo "Job name: $SLURM_JOB_NAME"
echo "Temporary directory: $TMPDIR"
echo "Date:"
date
echo "Node details:"
uname -a
echo ""
echo ""

# LOAD MODULES
echo "--------------------------------------------------------------------------------"
echo "Loading modules:"
module load math/R/4.6.0-mkl-2025.0-gnu-14.2
module load bio/jags/4.3.2
echo ""
echo ""

# RUN R RSCRIPT
echo "--------------------------------------------------------------------------------"
echo "Rscript output:"
Rscript  final/run_final_part2n3_u_ecd_sc_bau04.R
