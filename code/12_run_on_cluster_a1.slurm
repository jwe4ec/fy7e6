#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks=1
##SBATCH --cpus-per-task=17   # commented out for testing
#SBATCH --cpus-per-task=4
#SBATCH --time=03:30:00
##SBATCH --partition=standard # commented out for testing
#SBATCH --partition=dev
#SBATCH --account=pactlab
#SBATCH --output=job_%a.out
##SBATCH --array=1-12,49-50   # runs all "a1" models
#SBATCH --array=1


module purge
module load goolf/7.1.0_3.1.4 R/4.1
module load jags

Rscript 12_run_models_parallel.R

