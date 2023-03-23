# fy7e6

This repository contains analysis code for this project on the Open Science Framework (OSF): [https://osf.io/fy7e6/](https://osf.io/fy7e6/).

## Data

The present scripts import intermediate clean data ([v1.0.1](https://doi.org/10.5281/zenodo.6192907)) from the [Public Component](https://osf.io/s8v3h/) of the [MindTrails Calm Thinking Study project](https://osf.io/zbd52/) on OSF. These data were outputted from the study's centralized data cleaning, which was led by Jeremy Eberle and [Sonia Baee](https://github.com/soniabaee) and is described on the [MT-Data-CalmThinkingStudy](https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy) GitHub repo.

The scripts also import two columns extracted from `R01_coach_completion_record.csv`, which is a [coaching-related table](https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy#coaching-related-data-on-uva-box) stored privately in a MindTrails UVA Box folder. These data were derived from the raw Coach Session Tracking table and were not cleaned centrally, but cleaned by Alex Werntz and Allie Silverman. Jeremy Eberle extracted the columns as `coach_completion.csv` on 1/20/2022.

## Code

The imported data are considered intermediately cleaned because further analysis-specific cleaning is required for any given analysis. The present scripts perform this further cleaning and analyses for the manuscript, the main outcomes paper for the Calm Thinking study.

Scripts 1-10 were run on a Windows 10 Pro laptop (12 GB of RAM; Intel Core i5-4300U CPU @ 1.90GHz, 2494 Mhz, 2 cores, 4 logical processors). Script 11 was used for testing the analysis models (in series).

Scripts 12a-12f were used to run the "a1" and "a2" analysis models in parallel on the [Standard](https://www.rc.virginia.edu/userinfo/rivanna/queues/) partition of the [Rivanna](https://www.rc.virginia.edu/userinfo/computing-environments/) supercomputer, which uses multiple cores on one node.
- "a1" models are run via `sbatch 12e_run_models_std_partition_a1.slurm` on Rivanna's command line (CLI), which uses a job array to submit jobs for all desired models at once based on their row numbers in `parameter_table`. Each job in the array runs one model, and analyses for a given model are run across bootstrap samples in parallel using a `foreach() %dopar%` command in the `run_analysis()` function.
  - Initial models used 500 bootstrap samples with 20,000 total MCMC iterations. When we tried using 6,800 bootstrap samples with 20,000 iterations, all jobs timed out. Thus, we tried to run these models on the Parallel partition (see below), but we still faced long run times due to Rivanna's resource limitations, even after reducing to 2,000 bootstrap samples. We ultimately used Scripts 14a-14d below to run these models for 2,000 bootstrap samples on the Standard partition using a large job array.
- "a2" models are run via `sbatch 12f_run_models_std_partition_a2.slurm` on Rivanna's CLI, which also uses a job array in which each job in the array runs one model.
  - Initial models used 20,000 total MCMC iterations. Final models used 1,000,000 total MCMC iterations.
  
Script 13 trims results from Scripts 12a-12f so that they are small enough to load into R for many models at once. Note that results from Scripts 14a-14d below do not need to be trimmed, as they contain fewer saved objects.

Scripts 14a-14d were used to run the "a1" analysis models in parallel on the Standard partition using 2,000 bootstrap samples with 20,000 iterations.
- The models are run via `sbatch 14b_run_models_std_partition_lg_array_a1.slurm` on Rivanna's CLI, which uses a large job array to submit jobs for a single desired model based on its row number in `parameter_table`. Each job in the array analyzes one bootstrap sample. A separate job array must be run for each desired model. Once all job arrays have run, results across bootstrap samples for each model are then concatenated by running `sbatch 14d_concatenate_results.slurm`, which is a job array in which each job handles one model.

### `parallel_partition` Folder

We tried to run "a1" models in parallel on the [Parallel](https://www.rc.virginia.edu/userinfo/rivanna/queues/) partition of [Rivanna](https://www.rc.virginia.edu/userinfo/computing-environments/), which uses multiple cores across multiple nodes (nodes communicate with one another using Message Passing Interface). However, we abandoned this approach due to long run times; these scripts may have unresolved issues and inaccurate comments.

Scripts 13a-13d were used to try running the "a1" models using 6,800 bootstrap samples with 20,000 iterations.
- A single desired model is run via `13c_run_single_model_a1.sh i` on Rivanna's CLI, where `i` is the row number of `parameter_table` for the desired model.
  - This script uses `i` to update the name of the job outfile and to define `myNum` in `13b_run_models_parallel_partition_a1.slurm`. It then submits the Slurm script, which passes `myNum` to `13a_run_models_parallel_partition_a1.R`.
  - When running one model at a time, allow a delay (e.g., 15 min) before submitting the next model to avoid multiple jobs trying to access the same files at once and to ensure computing resources are available.
- Alternatively, in theory all "a1" jobs can be submitted at once using `13d_run_many_models_a1.sh`, which automatically implements a delay between jobs, but as of 12/20/2022, this script still has problems.

Scripts 14a-14f are updated versions of 13a-13d. We tried to analyze 2,000 bootstrap samples (instead of 6,800) given long run times when trying to analyze 6,800 (e.g., taking several days to run only one model and needing to run each model in series). In the updated scripts, each worker analyzes multiple bootstrap samples, given that we need to analyze 2,000 bootstrap samples with no more than 1,000 cores (limit of the [Parallel](https://www.rc.virginia.edu/userinfo/rivanna/queues/) partition).
- Scripts 14a-14c run the models on separate sets of bootstrap samples and output results for smaller subsets of bootstrap samples.
- Scripts 14d-14e concatenate the results across all subsets into one list for the model. Script 14f may be redundant with Script 14e.

Script 15a is an updated version of 14a. The run times were improved but still too long due to Rivanna's limitations.

### `psyc_5705_anlys` Folder

Scripts in this folder were used for initial analyses by Jeremy Eberle and [Katie Daniel](https://github.com/KatharineDaniel) for a Fall 2020 course project.

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

- Compute GMA d
- Compute table for raw means and standard deviations of outcomes by condition over time (`compute_raw_m_and_sd.R`)
- Compute rates of item- and scale-level missingness (`compute_missing_data_rates.R`)

### Hold

- Revise Scripts 14a-14d for reproducibility (e.g., fix file paths, confirm package versions)
- Remove selected results to decrease file size
  - Remove remaining `model_samples_*.RData` and `plots_*.pdf` for `a1` models with 500 bootstrap samples (see Script 13)
  - Then consider removing more (e.g., `results.RData` files before trimming)
- Consider transferring individual results from `scratch` directory before Script 14c was run
- Rerun "a1" models to test significance of marginal effects
  - Simple effects of time by treatment arm
  - Time-specific means by treatment arm
  - Marginal effects for dropout models (note: make "marg[11]" one of the "para" parameters)
  
- Resolve TOC issue for full tables
- Resolve border issue in some summary tables
- Resolve border issue in demographics tables

- Italicize package names in general notes for plots

- Resolve TODOs in `compute_flow.R`, `further_clean_demog_data.R`, `run_models_parallel.R`
- Document steps for running scripts on Rivanna in README (inc. runtimes and storage requirements)