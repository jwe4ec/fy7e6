#!/bin/bash

#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
                            
#SBATCH --time=06:00:00
#SBATCH --partition=standard
#SBATCH --account=pactlab
#SBATCH --output=job_1.out  # Update this number manually based on "myNum" below
#SBATCH --array=1-2000

module purge
module load goolf/7.1.0_3.1.4 R/4.1.1 # Rivanna does not have 4.1.2
module load jags
myNum=1                     # Specify the desired "a1" model to run based on its
                            # row in "parameter_table". User should provide only 
                            # indices for "a1" models (1-12, 49). (Could write Bash 
                            # script to run this script for all desired models.)
                            
echo $myNum

# Create "../a1_outputs" folder if it doesn't already exist

dir="../a1_outputs"

if [ ! -d "$dir" ]; then
   mkdir "$dir"
fi

# Temporarily copy data and initial values for each job in array

cp "../data/final_clean/wd_c1_corr_itt_2000.RData"            "../data/final_clean/wd_c1_corr_itt_2000.RData_${SLURM_ARRAY_TASK_ID}"
cp "../data/final_clean/wd_c1_corr_s5_train_compl_2000.RData" "../data/final_clean/wd_c1_corr_s5_train_compl_2000.RData_${SLURM_ARRAY_TASK_ID}"
cp "../results/bayesian/efficacy/model_and_initial_values/inits_efficacy.RData" "../results/bayesian/efficacy/model_and_initial_values/inits_efficacy.RData_${SLURM_ARRAY_TASK_ID}"
cp "../results/bayesian/dropout/model_and_initial_values/inits_dropout.RData"   "../results/bayesian/dropout/model_and_initial_values/inits_dropout.RData_${SLURM_ARRAY_TASK_ID}"

Rscript 15a_run_models_std_partition_lg_array_a1.R $myNum $SLURM_ARRAY_TASK_ID

rm "../data/final_clean/wd_c1_corr_itt_2000.RData_${SLURM_ARRAY_TASK_ID}"
rm "../data/final_clean/wd_c1_corr_s5_train_compl_2000.RData_${SLURM_ARRAY_TASK_ID}"
rm "../results/bayesian/efficacy/model_and_initial_values/inits_efficacy.RData_${SLURM_ARRAY_TASK_ID}"  
rm "../results/bayesian/dropout/model_and_initial_values/inits_dropout.RData_${SLURM_ARRAY_TASK_ID}"