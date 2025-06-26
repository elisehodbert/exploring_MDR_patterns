Overview
This repository contains scripts and functions to analyze antibiotic resistance patterns using antibiotic susceptibility test (AST) data. The workflow includes data preparation, simulations under the null hypothesis, application of association rule mining to detect significant resistance patterns, and descirptive graphs to visualize them.

functions.R : useful functions for all scripts.

Scripts
1_data_prep.R (approx. 3 minutes)
Purpose: Import raw antibiogram data.

Preprocess data:
Remove columns for antibiotics that will not be studied.
Format columns properly.
Filter out rows containing carbapenemase-producing bacteria.
Split the data into two datasets: ESBL (extended-spectrum beta-lactamase) and non-ESBL.

Output:
Two cleaned datasets (ESBL and non-ESBL) saved into donnees.RData.

3_simulation_bdd.R (approx. 45 minutes)
Purpose:
Simulate 100 datasets per year and per bacteria type (ESBL vs. non-ESBL), under the null hypothesis that antibiotic resistances are independent.

4_apriori.R
Description:
Run the Apriori algorithm on:
Observed datasets.
Simulated datasets using parallel computing.

5_filtre_itemsets.R
Purpose:
Filter observed itemsets based on quality measures (eSup and cLift).

6_describ_itemsets.R
Purpose:
Provide summary statistics for observed itemsets:
Count of itemsets after minimum support filter.
Count of itemsets after applying eSupport and cLift thresholds.
Distribution of number of antibiotics per pattern.
Distribution of number of antibiotic classes per pattern.
Top most frequent patterns.

7_plot_reseau.R
Purpose:
Visualize the significant associations that pass all filters as a network graph.



