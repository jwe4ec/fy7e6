#!/bin/bash

## Note: Only indices for "a1" models (1-12, 49) should be provided

for (( i=1; i<=3; i++ )); do
   echo $i

   ## Update Slurm script with a different outfile
   
   ### TODO: Can we store job outfiles in a ../jobs folder?

   


   
   outfile="job_$i.out"
   echo ${outfile}
   sed -i "s/job_[[:digit:]]*.out/${outfile}/g" 13b_run_models_parallel_partition_a1.slurm

   ## Set wait time so jobs don't try to access the same file at the same time
   wait=$(( 15*i ))
   sbatch --begin=now+"$wait"minutes 13b_run_models_parallel_partition_a1.slurm  $i

done