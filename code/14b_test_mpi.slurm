#!/bin/bash

#SBATCH --nodes=25
#SBATCH --ntasks-per-node=2
#SBATCH --cpus-per-task=10
##SBATCH --ntasks=200 # Requested number of cores (Rivanna limits to 1000 cores
                     # per job on parallel partition). Will auto-find available 
                     # cores across nodes and run R Script below on every core.
#SBATCH --time=06:00:00
#SBATCH --partition=parallel
#SBATCH --account=pactlab
#SBATCH --output=job_14.out # Updated by separate Bash script
##SBATCH --mem=224000
##SBATCH --mem=40000

module purge
module load goolf/7.1.0_3.1.4 R/4.1.1
module load jags
myNum=$1                   # Updated by separate Bash script
bootstrap_start=$2
bootstrap_stop=$3
echo $myNum
srun Rscript 14a_test_file.R  $myNum $bootstrap_start $bootstrap_stop
