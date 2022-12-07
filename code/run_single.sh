#!/bin/bash

i=$1
echo "Submitting job with indes:  $1"

## Update slurm script with a different outfile
outfile="test_$i.out"
sed -i "s/test_[[:digit:]]*.out/${outfile}/g" 13b_run_models_with_mpi.slurm 

sbatch 13b_run_models_with_mpi.slurm  $i