#!/bin/bash

#SBATCH -n 1
#SBATCH --time=01:00:00
#SBATCH --partition=standard
#SBATCH --account=pactlab
#SBATCH --output=concat.out # Updated by separate Bash script
#SBATCH --mem=34000

module purge
module load goolf/7.1.0_3.1.4 R/4.1.1
module load jags

Rscript 14d_test_concatenate_results.R
