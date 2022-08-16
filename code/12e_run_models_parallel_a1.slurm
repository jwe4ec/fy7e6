#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=26
#SBATCH --time=90:00:00
#SBATCH --partition=standard
#SBATCH --account=pactlab
#SBATCH --output=../jobs/job_%a.out
##SBATCH --array=1-12,49-50   # runs all "a1" models for final run
#SBATCH --array=1-2           # runs only two models for testing

module purge
module load goolf/7.1.0_3.1.4 R/4.1
module load jags

Rscript 12d_run_models_parallel.R $SLURM_ARRAY_TASK_ID $SLURM_CPUS_PER_TASK