#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=00:20:00
#SBATCH --partition=standard
#SBATCH --account=pactlab
#SBATCH --output=../jobs/install_pkgs.out

module purge
module load goolf/7.1.0_3.1.4 R/4.1
module load jags

Rscript 12b_install_pkgs_rivanna.R