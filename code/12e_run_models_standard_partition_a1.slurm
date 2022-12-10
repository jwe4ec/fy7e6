#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=40
#SBATCH --time=4-02:00:00
#SBATCH --partition=standard
#SBATCH --account=pactlab
#SBATCH --output=../jobs/job_%a.out
#SBATCH --array=1-12,49          # Run all "a1" models

module purge
module load goolf/7.1.0_3.1.4 R/4.1
module load jags

Rscript 12d_run_models_standard_partition.R $SLURM_ARRAY_TASK_ID $SLURM_CPUS_PER_TASK