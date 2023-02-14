#!/bin/bash

#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
                            
#SBATCH --time=06:00:00
#SBATCH --partition=standard
#SBATCH --account=pactlab
#SBATCH --output=job_14.out # TODO: Not updated by separate Bash script as it was before.
                            # Also, can we store job outfiles in a "jobs" folder?
                            
##SBATCH --mem=224000       # TODO: Which of these memory lines is needed?
##SBATCH --mem=40000
#SBATCH --array=1-2000

module purge
module load goolf/7.1.0_3.1.4 R/4.1.1
module load jags
myNum=12                    # Defined by "param_row" of separate Bash script, which
                            # for a given row of "parameter_table" submits separate 
                            # jobs for each set of 100 bootstrap samples (for a total
                            # of 2000 bootstrap samples). Each job is delayed by a set
                            # amount of time so the prior job can finish running. User
                            # should provide only indices for "a1" models (1-12, 49) 
                            # for "param_row" when running separate Bash script.
                            
echo $myNum

cp  "../data/final_clean/wd_c1_corr_itt_2000.RData" "../data/final_clean/wd_c1_corr_itt_2000.RData_${SLURM_ARRAY_TASK_ID}"
cp  "../data/final_clean/wd_c1_corr_s5_train_compl_2000.RData" "../data/final_clean/wd_c1_corr_s5_train_compl_2000.RData_${SLURM_ARRAY_TASK_ID}"

cp "../results/bayesian/efficacy/model_and_initial_values/inits_efficacy.RData"  "../results/bayesian/efficacy/model_and_initial_values/inits_efficacy.RData_${SLURM_ARRAY_TASK_ID}"  

cp "../results/bayesian/dropout/model_and_initial_values/inits_dropout.RData"  "../results/bayesian/dropout/model_and_initial_values/inits_dropout.RData_${SLURM_ARRAY_TASK_ID}"

#Rscript 14a_test_run_models_parallel_partition_a1.R  $myNum $SLURM_ARRAY_TASK_ID
#Rscript 14a_test_run_models_parallel_partition_a1.R  $myNum $SLURM_ARRAY_TASK_ID
Rscript 15a_test_run_models_array_a1.R $myNum $SLURM_ARRAY_TASK_ID


rm  "../data/final_clean/wd_c1_corr_itt_2000.RData_${SLURM_ARRAY_TASK_ID}"
rm  "../data/final_clean/wd_c1_corr_s5_train_compl_2000.RData_${SLURM_ARRAY_TASK_ID}"
rm "../results/bayesian/efficacy/model_and_initial_values/inits_efficacy.RData_${SLURM_ARRAY_TASK_ID}"  
rm "../results/bayesian/dropout/model_and_initial_values/inits_dropout.RData_${SLURM_ARRAY_TASK_ID}"

