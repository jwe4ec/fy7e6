#!/bin/bash

#SBATCH --nodes=25
#SBATCH --ntasks-per-node=2 # TODO: Specifies 50 workers, each with 10 cores, for
#SBATCH --cpus-per-task=10  # a total of 500 cores (where Rivanna's limit is 1000)?
                            # If we are running 100 bootstrap samples at a time, do
                            # we need 500 cores? Or can we run more samples at a time?
                            
#SBATCH --time=06:00:00
#SBATCH --partition=parallel
#SBATCH --account=pactlab
#SBATCH --output=job_14.out # TODO: Not updated by separate Bash script as it was before.
                            # Also, can we store job outfiles in a "jobs" folder?
                            
##SBATCH --mem=224000       # TODO: Which of these memory lines is needed?
##SBATCH --mem=40000

module purge
module load goolf/7.1.0_3.1.4 R/4.1.1
module load jags
myNum=$1                    # Defined by "param_row" of separate Bash script, which
                            # for a given row of "parameter_table" submits separate 
                            # jobs for each set of 100 bootstrap samples (for a total
                            # of 6800 bootstrap samples). Each job is delayed by a set
                            # amount of time so the prior job can finish running. User
                            # should provide only indices for "a1" models (1-12, 49) 
                            # for "param_row" when running separate Bash script.
                            
bootstrap_start=$2          # Defined by "c" of separate Bash script
bootstrap_stop=$3           # Defined by "bs_stop" of separate Bash script
echo $myNum
srun Rscript 14a_test_run_models_parallel_partition_a1.R  $myNum $bootstrap_start $bootstrap_stop