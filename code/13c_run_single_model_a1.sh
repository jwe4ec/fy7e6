#!/bin/bash

i=$1 ## Index to parameter_table (myNum) provided by user when running this script.
     ## Only indices for "a1" models (1-12, 49) should be provided.

echo "Submitting job with parameter_table index (myNum):  $1"

## Update Slurm script with a different outfile

### TODO: Can we store job outfiles in a ../jobs folder?





outfile="job_$i.out"
sed -i "s/job_[[:digit:]]*.out/${outfile}/g" 13b_run_models_parallel_partition_a1.slurm

sbatch 13b_run_models_parallel_partition_a1.slurm  $i