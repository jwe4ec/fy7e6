# fy7e6

This repository contains analysis code for this project on the Open Science Framework (OSF): [https://osf.io/fy7e6/](https://osf.io/fy7e6/).

## Data

The present scripts import intermediate clean data ([v1.0.1](https://doi.org/10.5281/zenodo.6192907)) from the [Public Component](https://osf.io/s8v3h/) of the [MindTrails Calm Thinking Study project](https://osf.io/zbd52/) on OSF. These data were outputted from the study's centralized data cleaning, which was led by Jeremy Eberle and [Sonia Baee](https://github.com/soniabaee) and is described on the [MT-Data-CalmThinkingStudy](https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy) GitHub repo.

It also imports two columns extracted from `R01_coach_completion_record.csv`, which is a [coaching-related table](https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy#coaching-related-data-on-uva-box) stored privately in a MindTrails UVA Box folder. These data were derived from the raw Coach Session Tracking table and were not cleaned centrally, but cleaned by Alex Werntz and Allie Silverman. Jeremy Eberle extracted the columns as `coach_completion.csv` on 1/20/2022.

## Code

The imported data are considered intermediately cleaned because further analysis-specific cleaning is required for any given analysis. The present scripts perform this further cleaning and analyses for the manuscript, the main outcomes paper for the Calm Thinking study.

Scripts 1-10 were run on a Windows 10 Pro laptop (12 GB of RAM; Intel Core i5-4300U CPU @ 1.90GHz, 2494 Mhz, 2 cores, 4 logical processors). Script 11 was used for testing the analysis models (in series).

Scripts 12a-12f were used to run the analysis models in parallel on the [Standard](https://www.rc.virginia.edu/userinfo/rivanna/queues/) partition of the [Rivanna](https://www.rc.virginia.edu/userinfo/computing-environments/) supercomputer, managed by [Research Computing](https://www.rc.virginia.edu/) at the University of Virginia. We thank [Jackie Huband](https://www.rc.virginia.edu/about/people/huband/) for her consultation.

Scripts in the `psyc_5705_anlys` folder were used for initial analyses by Jeremy Eberle and [Katie Daniel](https://github.com/KatharineDaniel) for a Fall 2020 course project.

## TODOs

- Resolve TODOs in `compute_flow.R`, `further_clean_demog_data.R`, `run_models_parallel.R`
- Compute table for raw means and standard deviations of outcomes by condition over time (`compute_raw_m_and_sd.R`)
- Compute rates of item- and scale-level missingness (`compute_missing_data_rates.R`)
- Run analyses and pool results
- Document steps for running scripts on Rivanna in README (inc. runtimes and storage requirements)
