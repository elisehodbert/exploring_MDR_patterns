dossier_enreg = "results_all_data/"
chemin_donnees = "data/"

list_datasets = c(
  "2018_BLSE"
  # "2019_BLSE",
  # "2020_BLSE",
  # "2021_BLSE",
  # "2022_BLSE"
  # "2018_non_BLSE",
  # "2019_non_BLSE",
  # "2020_non_BLSE",
  # "2021_non_BLSE",
  # "2022_non_BLSE"
)


list_data_simul <- c( #Simulated data is very voluminous. We cut the lists of datasets into smaller lists so that the computer does not crash
  "2018_BLSE"
  # "2019_BLSE",
  # "2020_BLSE",
  # "2021_BLSE",
  # "2022_BLSE"
  # "2018_non_BLSE_1_20",
  # "2018_non_BLSE_21_40",
  # "2018_non_BLSE_41_60",
  # "2018_non_BLSE_61_80",
  # "2018_non_BLSE_81_100",
  # "2019_non_BLSE_1_20",
  # "2019_non_BLSE_21_40",
  # "2019_non_BLSE_41_60",
  # "2019_non_BLSE_61_80",
  # "2019_non_BLSE_81_100",
  # "2020_non_BLSE_1_20",
  # "2020_non_BLSE_21_40",
  # "2020_non_BLSE_41_60",
  # "2020_non_BLSE_61_80",
  # "2020_non_BLSE_81_100",
  # "2021_non_BLSE_1_15",
  # "2021_non_BLSE_16_30",
  # "2021_non_BLSE_31_45",
  # "2021_non_BLSE_46_60",
  # "2021_non_BLSE_61_75",
  # "2021_non_BLSE_76_90",
  # "2021_non_BLSE_91_100",
  # "2022_non_BLSE_1_15",
  # "2022_non_BLSE_16_30",
  # "2022_non_BLSE_31_45",
  # "2022_non_BLSE_46_60",
  # "2022_non_BLSE_61_75",
  # "2022_non_BLSE_76_90",
  # "2022_non_BLSE_91_100"
)

dossier_enreg = "results_minsup_ESBL/minsup_0_1/"
folder_creation(dossier_enreg, 1)
minsup_BLSE = 0.1
source("scripts/4_apriori.R")
source("scripts/5_filtre_itemsets.R")
source("scripts/6_describ_itemsets.R") # describing itemsets
source("scripts/7_plot_reseaux.R")
source("scripts/7_bis_legende_reseaux.R") # drawing the legend
list_graphs <- paste0("graph_",list_datasets,"_0.95")
plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 1, 1) # drawing the figure with all networks

dossier_enreg = "results_minsup_ESBL/minsup_0_05/"
folder_creation(dossier_enreg, 1)
minsup_BLSE = 0.05
source("scripts/4_apriori.R")
source("scripts/5_filtre_itemsets.R")
source("scripts/6_describ_itemsets.R") # describing itemsets
source("scripts/7_plot_reseaux.R")
source("scripts/7_bis_legende_reseaux.R") # drawing the legend
list_graphs <- paste0("graph_",list_datasets,"_0.95")
plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 1, 1) # drawing the figure with all networks

dossier_enreg = "results_minsup_ESBL/minsup_0_01/"
folder_creation(dossier_enreg, 1)
minsup_BLSE = 0.01
source("scripts/4_apriori.R")
source("scripts/5_filtre_itemsets.R")
source("scripts/6_describ_itemsets.R") # describing itemsets
source("scripts/7_plot_reseaux.R")
source("scripts/7_bis_legende_reseaux.R") # drawing the legend
list_graphs <- paste0("graph_",list_datasets,"_0.95")
plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 1, 1) # drawing the figure with all networks

dossier_enreg = "results_minsup_ESBL/minsup_0_005/"
folder_creation(dossier_enreg, 1)
minsup_BLSE = 0.005
source("scripts/4_apriori.R")
source("scripts/5_filtre_itemsets.R")
source("scripts/6_describ_itemsets.R") # describing itemsets
source("scripts/7_plot_reseaux.R")
source("scripts/7_bis_legende_reseaux.R") # drawing the legend
list_graphs <- paste0("graph_",list_datasets,"_0.95")
plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 1, 1) # drawing the figure with all networks

dossier_enreg = "results_minsup_ESBL/minsup_0_001/"
folder_creation(dossier_enreg, 1)
minsup_BLSE = 0.001
source("scripts/4_apriori.R")
source("scripts/5_filtre_itemsets.R")
source("scripts/6_describ_itemsets.R") # describing itemsets
source("scripts/7_plot_reseaux.R")
source("scripts/7_bis_legende_reseaux.R") # drawing the legend
list_graphs <- paste0("graph_",list_datasets,"_0.95")
plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 1, 1) # drawing the figure with all networks

