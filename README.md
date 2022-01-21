# fy7e6

This repository contains analysis code for this project on the Open Science Framework (OSF): [https://osf.io/fy7e6/](https://osf.io/fy7e6/).

## Data

This project imports intermediate clean data (**TODO: vX.X.X.X**) from the [Public Component](https://osf.io/s8v3h/) of the [MindTrails Calm Thinking Study project](https://osf.io/zbd52/) on OSF. These data were outputted from the study's centralized data cleaning led by Jeremy Eberle and Sonia Baee, described on the [MT-Data-CalmThinkingStudy](https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy) GitHub repo.

It also imports selected columns from `R01_coach_completion_record.csv`, which is a [coaching-related dataset](https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy#coaching-related-data-on-uva-box) stored privately in a MindTrails UVA Box folder. These data were not centrally cleaned, but rather cleaned by Alex Werntz and Allie Silverman. Jeremy Eberle extracted the columns as `coach_completion.csv` on 1/20/2022.

## Code

The imported data are considered intermediately cleaned because further analysis-specific cleaning is required for any given analysis. The present scripts perform this further cleaning and analyses for the manuscript, the main outcomes paper for the Calm Thinking study.

## TODOs

- Resolve TODOs in participant flow
- Conduct further cleaning
- Compute Table S1 (Reans and Standard Deviations of Outcomes by Condition Over Time for the Session 1 Assessment Completer Sample)
- Compute rates of item- and scale-level missingness
- Compute Table S2 (Demographic Characteristics by Condition for the Session 1 Assessment Completer Sample)
- Run analyses
