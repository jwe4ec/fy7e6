#!/bin/bash

#SBATCH --ntasks=340 # Requested number of cores (Rivanna limits to 1000 cores
                     # per job on parallel partition). Will auto-find available 
                     # cores across nodes and run R Script below on every core.
#SBATCH --time=00:10:00
#SBATCH --partition=parallel
#SBATCH --account=pactlab
#SBATCH --output=job_5.out # Updated by separate Bash script
#SBATCH --mem=136000

module purge
module load goolf/7.1.0_3.1.4 R/4.1.1
module load jags
myNum=$1                   # Updated by separate Bash script
echo $myNum
srun Rscript 13a_run_models_parallel_partition_a1.R  $myNum