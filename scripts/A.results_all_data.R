##### 1. Analysis on all the data #####

dossier_enreg = "results_all_data/"
chemin_donnees = "data/"
folder_creation(dossier_enreg,1) # creating the folders needed to perform analyses

list_datasets = c(
  "2018_BLSE",
  "2019_BLSE",
  "2020_BLSE",
  "2021_BLSE",
  "2022_BLSE",
  "2018_non_BLSE",
  "2019_non_BLSE",
  "2020_non_BLSE",
  "2021_non_BLSE",
  "2022_non_BLSE"
)

list_data_simul <- c( #Simulated data is very voluminous. We cut the lists of datasets into smaller lists so that the computer does not crash
  "2018_BLSE",
  "2019_BLSE",
  "2020_BLSE",
  "2021_BLSE",
  "2022_BLSE",
  "2018_non_BLSE_1_20",
  "2018_non_BLSE_21_40",
  "2018_non_BLSE_41_60",
  "2018_non_BLSE_61_80",
  "2018_non_BLSE_81_100",
  "2019_non_BLSE_1_20",
  "2019_non_BLSE_21_40",
  "2019_non_BLSE_41_60",
  "2019_non_BLSE_61_80",
  "2019_non_BLSE_81_100",
  "2020_non_BLSE_1_20",
  "2020_non_BLSE_21_40",
  "2020_non_BLSE_41_60",
  "2020_non_BLSE_61_80",
  "2020_non_BLSE_81_100",
  "2021_non_BLSE_1_15",
  "2021_non_BLSE_16_30",
  "2021_non_BLSE_31_45",
  "2021_non_BLSE_46_60",
  "2021_non_BLSE_61_75",
  "2021_non_BLSE_76_90",
  "2021_non_BLSE_91_100",
  "2022_non_BLSE_1_15",
  "2022_non_BLSE_16_30",
  "2022_non_BLSE_31_45",
  "2022_non_BLSE_46_60",
  "2022_non_BLSE_61_75",
  "2022_non_BLSE_76_90",
  "2022_non_BLSE_91_100"
)

# a. Data description
source("scripts/2_data_description.R")

# b. Simulating databases under H0 : antimicrobial resistances are independent
source("scripts/3_simulation_bdd.R")
source("scripts/figure_2_plot_nb_resis_obs_simul.R")

# c. Algorithme Apriori
source("scripts/4_apriori.R")

# Re-assembling lists of datasets that were split to save computer memory
concat_itemsets_non_BLSE(dossier_enreg, "2018_non_BLSE", c("1","20","40","60","80","100"))
concat_itemsets_non_BLSE(dossier_enreg, "2019_non_BLSE", c("1","20","40","60","80","100"))
concat_itemsets_non_BLSE(dossier_enreg, "2020_non_BLSE", c("1","20","40","60","80","100"))
concat_itemsets_non_BLSE(dossier_enreg, "2021_non_BLSE", c("1","15","30","45","60","75","90","100"))
concat_itemsets_non_BLSE(dossier_enreg, "2022_non_BLSE", c("1","15","30","45","60","75","90","100"))

# d. Filtering itemsets with eSup and cLift cut-off values
source("scripts/5_filtre_itemsets.R")
source("scripts/6_describ_itemsets.R") # describing itemsets

# e. Plotting the networks
source("scripts/7_plot_reseaux.R")
source("scripts/7_bis_legende_reseaux.R") # drawing the legend

list_graphs <- paste0("graph_",list_datasets,"_0.95")
plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 5, 2) # drawing the figure with all networks


# testing number of patterns from 2018 to 2022
nb_patterns <- read_excel("results_all_data/tab_recap_itemsets_0.95.xlsx")
nb_obs_patterns_ESBL <- unlist(nb_patterns[1:5,2])
nb_obs_patterns_non_ESBL <- unlist(nb_patterns[6:10,2])
nb_pruned_patterns_ESBL <- unlist(nb_patterns[1:5,3])
nb_pruned_patterns_non_ESBL <- unlist(nb_patterns[6:10,3]) 

mk.test(nb_obs_patterns_ESBL)
mk.test(nb_obs_patterns_non_ESBL)
mk.test(nb_pruned_patterns_ESBL)
mk.test(nb_pruned_patterns_non_ESBL)
