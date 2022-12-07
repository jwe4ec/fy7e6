#!/bin/bash

for (( i=1; i<=3; i++ )); do
   echo $i

   ## Update slurm script with a different outfile
   outfile="test_$i.out"
   echo ${outfile}
   sed -i "s/test_[[:digit:]]*.out/${outfile}/g" 13b_run_models_with_mpi.slurm 

   ## Set the wait time so that instances don't try to access the same file
   ## at the same time
   wait=$(( 15*i )) 
   sbatch --begin=now+"$wait"minutes 13b_run_models_with_mpi.slurm  $i


done