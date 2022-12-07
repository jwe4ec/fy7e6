#!/bin/bash

#SBATCH --ntasks=300  #Total number of tasks 
#SBATCH --time=00:10:00      
#SBATCH --partition=parallel   
#SBATCH --account=pactlab
#SBATCH --output=test_5.out
#SBATCH --mem=136000

module purge
module load goolf/7.1.0_3.1.4 R/4.1.1
module load jags
myNum=$1
echo $myNum
srun Rscript 13a_run_models_with_mpi.R  $myNum

