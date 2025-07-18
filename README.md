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

### Analysis Steps

- `1_data_prep` : Dataset preprocessing  
- `2_data_description` : Generates Figure 1 (individual antibiotic resistance prevalence)  
- `3_simulation_bdd` : Simulates datasets under the hypothesis of mutual independence of resistance traits  
- `4_apriori` : Applies the Apriori algorithm to both observed and simulated datasets  
- `5_filtre_itemsets` : Prunes the Apriori results using `eSup` and `cLift`  
- `6_describ_itemsets` : Describes the selected patterns after pruning  
- `7_plot_reseaux` : Creates and plots the network of resistance patterns  
- `7_bis_legende_reseaux` : Plots the legend for the network figure

### Extended Analyses

- `A_results_all_data` : Analyzes the complete datasets from 2018 to 2022  
- `B_bootstrap_2018` : Bootstraps the 2018 dataset and analyzes the resamples  
- `C_analyses_samples_2018_size` : Subsamples 2019–2022 datasets to match the 2018 sample size  
- `D_analyses_samples_regional_prop` : Creates datasets proportionate to regional populations  
- `E_analyses_by_age_class` : Performs stratified analyses by age class  
- `F_analyses_by_gender` : Performs stratified analyses by gender

---

## Results Folder

This folder contains the outputs of the different analyses.

### `Results_all_data`

Contains analyses from the complete datasets (2018–2022)

### `Results_bs_2018`

Contains bootstrapped results from the 2018 dataset

### `Results_by_age_class`

Stratified results by age group:

- Individuals under 65 (2018–2022)  
- Individuals over 65 (2018–2022)  
- Bootstrapped results for under-65 group

### `Results_by_gender`

Stratified results by gender:

- `results_men` : Men (2018–2022)  
- `results_bs_men` : Bootstrapped men datasets  
- `results_women` : Women (2018–2022)  
- `results_sampled_women_size_men` : Women sampled at the same size as men

### `Results_samples_2018_size`

Analyses of 2019–2022 datasets sampled to match 2018 size

### `Results_samples_regio_prop`

Analyses of 2019–2022 datasets with isolate counts proportional to regional populations
