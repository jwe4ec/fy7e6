#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --time=3-00:00:00
#SBATCH --partition=standard
#SBATCH --account=pactlab
#SBATCH --output=../jobs/job_%a.out
#SBATCH --array=13-48,50-52           # Run all "a2" models

module purge
module load goolf/7.1.0_3.1.4 R/4.1.1 # Rivanna does not have 4.1.2
module load jags

Rscript 12d_run_models_std_partition.R $SLURM_ARRAY_TASK_ID $SLURM_CPUS_PER_TASK