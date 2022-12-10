# fy7e6

This repository contains analysis code for this project on the Open Science Framework (OSF): [https://osf.io/fy7e6/](https://osf.io/fy7e6/).

## Data

The present scripts import intermediate clean data ([v1.0.1](https://doi.org/10.5281/zenodo.6192907)) from the [Public Component](https://osf.io/s8v3h/) of the [MindTrails Calm Thinking Study project](https://osf.io/zbd52/) on OSF. These data were outputted from the study's centralized data cleaning, which was led by Jeremy Eberle and [Sonia Baee](https://github.com/soniabaee) and is described on the [MT-Data-CalmThinkingStudy](https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy) GitHub repo.

The scripts also import two columns extracted from `R01_coach_completion_record.csv`, which is a [coaching-related table](https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy#coaching-related-data-on-uva-box) stored privately in a MindTrails UVA Box folder. These data were derived from the raw Coach Session Tracking table and were not cleaned centrally, but cleaned by Alex Werntz and Allie Silverman. Jeremy Eberle extracted the columns as `coach_completion.csv` on 1/20/2022.

## Code

The imported data are considered intermediately cleaned because further analysis-specific cleaning is required for any given analysis. The present scripts perform this further cleaning and analyses for the manuscript, the main outcomes paper for the Calm Thinking study.

Scripts 1-10 were run on a Windows 10 Pro laptop (12 GB of RAM; Intel Core i5-4300U CPU @ 1.90GHz, 2494 Mhz, 2 cores, 4 logical processors). Script 11 was used for testing the analysis models (in series).

Scripts 12a-12f were used to run the "a1" and "a2" analysis models in parallel on the [Standard](https://www.rc.virginia.edu/userinfo/rivanna/queues/) partition of the [Rivanna](https://www.rc.virginia.edu/userinfo/computing-environments/) supercomputer, which uses multiple cores on one node.
- "a1" models are run via `sbatch 12e_run_models_standard_partition_a1.slurm` on Rivanna's command line, which uses a job array to submit jobs for desired models based on their row numbers in `parameter_table`.
  - Initial models used 500 bootstrap samples with 20,000 total MCMC iterations. When we tried using 6,800 bootstrap samples with 20,000 iterations, all jobs timed out. Thus, we will run these models on the Parallel partition (below).
- "a2" models are run via `sbatch 12f_run_models_standard_partition_a2.slurm` on Rivanna's command line, which also uses a job array.
  - Initial models used 20,000 total MCMC iterations. Final models used 1,000,000 total MCMC iterations.

Scripts 13a-13d will be used to run the "a1" models with 6,800 bootstrap samples in parallel on the [Parallel](https://www.rc.virginia.edu/userinfo/rivanna/queues/) partition of [Rivanna](https://www.rc.virginia.edu/userinfo/computing-environments/), which uses multiple cores across multiple nodes. Nodes communicate with one another using Message Passing Interface (MPI).
- "a1" models are run via `13c_run_single_model_a1.sh i` on Rivanna's command line, where `i` is the row number of `parameter_table` for the desired model ("a1" models have row numbers 1-12 and 49).
  - This script uses `i` to update the name of the job outfile and to define `myNum` in `13b_run_models_parallel_partition_a1.slurm`. It then submits the Slurm script, which passes `myNum` to `13a_run_models_parallel_partition_a1.R`.
  - When running one model at a time, be sure to allow a delay (e.g., 15 min) before submitting the next model to avoid multiple jobs trying to access the same files at once and to ensure computing resources are available.
- Alternatively, in theory all "a1" jobs can be submitted at once using `13d_run_many_models_a1.sh`, which automatically implements a delay between jobs, but as of 12/20/2022, this script still has problems.

Scripts in the `psyc_5705_anlys` folder were used for initial analyses by Jeremy Eberle and [Katie Daniel](https://github.com/KatharineDaniel) for a Fall 2020 course project.

## Results

- TODO: Add results to components on OSF project using `osfr` package
  - results_bayesian_set1_efficacy_out_a
  - results_bayesian_set1_efficacy_out_b
  - results_bayesian_set1_dropout_out
  - results_bayesian_set2_efficacy_out_a
  - results_bayesian_set2_efficacy_out_b
  - results_bayesian_set2_dropout_out
- TODO: Describe components on OSF project

## Acknowledgments

We thank [Jackie Huband](https://www.rc.virginia.edu/about/people/huband/) of [Research Computing](https://www.rc.virginia.edu/) at the University of Virginia, which manages the [Rivanna](https://www.rc.virginia.edu/userinfo/computing-environments/) supercomputer, for her consultation on parallelizing the code.

## TODOs

- Resolve TODOs in `compute_flow.R`, `further_clean_demog_data.R`, `run_models_parallel.R`
- Compute table for raw means and standard deviations of outcomes by condition over time (`compute_raw_m_and_sd.R`)
- Compute rates of item- and scale-level missingness (`compute_missing_data_rates.R`)
- Run analyses and pool results
- Document steps for running scripts on Rivanna in README (inc. runtimes and storage requirements)
