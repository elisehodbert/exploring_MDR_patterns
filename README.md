# Exploring multidrug resistance patterns in community-acquired  E; . coli   urinary tract infections with machine learning

Code repository.

## Data repository

Contains the antibiotic surveillance datasets

# Scripts folder

Contains all the scripts to perform the analyses:

main: importing all the necessary packages, defining parameters, launching successive scripts
functions: contains all the functions we defined for this study

1_data_prep : preprocessing of the datasets
2_data_description : creation of figure 1 (individual antibiotic resistance prevalence)
3_simulation_bdd : simulation of the datasets under the hypothesis of mutual independence of the resistance traits
4_apriori : applying the apriori algorithm to the observed and simulated datasets
5_filtre_itemsets : pruning the patterns returned by apriori using eSup and cLift
6_describ_itemsets: description of the patterns selected after pruning
7_plot_reseaux: creation and plotting of the network
7_bis_legende_reseaux: plotting of the network figure legend

A.results_all_data: performing the analyses for all the datasets from 2018 to 2022
B.bootstrap_2018: bootstrapping the 2018 dataset and performing the analyses on the bootstrapped datasets
C.analyses_samples_2018_size: subsampling the datasets from 2019 to 2022 at the size of the 2018 dataset and performing the analyses on the subsampled datasets
D.analyses_samples_regional_prop: creating datasets with a number of isolates proportional to the regional populations and performing the analyses
E.analyses_by_age_class: performing the stratified analyses by age class
F.analyses_by_gender : performing the analyses stratified by gender

## Results folder

### Results_all_data

Contains the results of the analyses conducted on the whole datasets from 2018 to 2022

### Results_bs_2018

Contains the results of the bootstrapped datasets of 2018

### Results_by_age_class

Contains the results of the analyses stratified by age class :

- results for individuals under 65 from 2018 to 2022
- results for individuals over 65 from 2018 to 2022
- results bootstrapped for under 65

### Results_by gender

Contains the results of the analyses stratified by gender:

- results_men : results for all the men from 2018 to 2022
- results_bs_men : results for bootstrapped datasets of men from 2018 to 2022
- results_women : results for all the women from 2018 to 2022
- results_sampled_women_size_men:results for the women datasets sampled at the size of the men dataset from 2018 to 2022

### Results_samples_2018_size

Contains the results of the analyses from 2019 to 2022, sampled at the size of 2018

### Results_samples_regio_prop

Contains the results of the analyses from 2019 to 2022, with a number of isolates proportional to the regional populations

