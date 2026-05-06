# Exploring Multidrug Resistance Patterns in Community-Acquired *E. coli* Urinary Tract Infections Using Machine Learning

This repository contains code and data for the analysis of multidrug resistance patterns in community-acquired *Escherichia coli* urinary tract infections using various machine learning techniques.

---

## Data Repository

This folder contains the antibiotic surveillance datasets used in the study.

---

## Scripts Folder

This folder includes all scripts required to perform the analyses.

### Core Scripts

- `main` : Imports necessary packages, defines parameters, and sequentially runs the scripts  
- `functions` : Contains all custom functions defined for this study

### Descriptive scripts
- `Figure1` : Generates Figure 1 (individual antibiotic resistance prevalence)  
- `Figure2` : Generates Figure 2

### Analysis Steps

- `1_data_prep` : Preprocessing of the datasets  
- `3_simulation_bdd` : Simulates datasets under the hypothesis of mutual independence of resistance traits  
- `4_apriori` : Applies the Apriori algorithm to both observed and simulated datasets  
- `5_filtre_itemsets` : Filters Apriori results using `eSup` and `cLift`  
- `6_describ_itemsets` : Describes the selected patterns after filtering  
- `7_plot_reseaux` : Creates and plots the network of resistance patterns  
- `7_bis_legende_reseaux` : Plots the legend for the network figure

### Extended Analyses

- `A_results_all_data` : Performs the full analysis on the datasets from 2018 to 2022  
- `B_bootstrap_2018` : Bootstraps the 2018 dataset and analyzes the resampled sets  
- `C_analyses_samples_2018_size` : Subsamples datasets from 2019 to 2022 to match the 2018 sample size  
- `D_analyses_samples_regional_prop` : Builds datasets with isolate counts proportional to regional populations  
- `E_analyses_by_age_class` : Conducts stratified analyses by age group  
- `F_analyses_by_gender` : Conducts stratified analyses by gender

---

## Results Folder

This folder contains the outputs of the various analyses.

### `Results_all_data`

- Results from the complete datasets (2018–2022)

### `Results_bs_2018`

- Results from bootstrapped versions of the 2018 dataset

### `Results_by_age_class`

Stratified results by age group:

- Individuals under 65 (2018–2022)  
- Individuals over 65 (2018–2022)  
- Bootstrapped results for individuals under 65

### `Results_by_gender`

Stratified results by gender:

- `results_men` : Men (2018–2022)  
- `results_bs_men` : Bootstrapped datasets for men  
- `results_women` : Women (2018–2022)  
- `results_sampled_women_size_men` : Women datasets sampled to match the size of the men datasets

### `Results_age_gender`

Stratified results by age and gender:

- `results_over_65_men`  
- `results_over_65_women`  
- `results_under_65_men`  
- `results_under_65_women`

### `Results_samples_2018_size`

- Results of datasets from 2019 to 2022, sampled to match the size of the 2018 dataset

### `Results_samples_regio_prop`

- Results of datasets from 2019 to 2022 with isolate counts proportional to regional population sizes

### `Results_minsup_ESBL`

- Results from analyses on the ESBL datasets with a minimum support of 0.001 (compared to 0.01 in the main analyses)
