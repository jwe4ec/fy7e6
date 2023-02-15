#!/bin/bash

#SBATCH --ntasks=1
#SBATCH --time=00:20:00
#SBATCH --partition=standard
#SBATCH --account=pactlab
#SBATCH --output=concat.out
#SBATCH --mem=34000
#SBATCH --array=1-12,49

module purge
module load goolf/7.1.0_3.1.4 R/4.1.1

Rscript 13c_concatenate_results.R $SLURM_ARRAY_TASK_ID