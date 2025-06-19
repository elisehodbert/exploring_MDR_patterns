chemin_donnees = "data/"

list_datasets = c(
  "2018_BLSE",
  # "2019_BLSE",
  # "2020_BLSE",
  # "2021_BLSE",
  "2022_BLSE"
  # "2018_non_BLSE",
  # "2019_non_BLSE",
  # "2020_non_BLSE",
  # "2021_non_BLSE",
  # "2022_non_BLSE"
)


list_data_simul <- c( #Simulated data is very voluminous. We cut the lists of datasets into smaller lists so that the computer does not crash
  "2018_BLSE",
  # "2019_BLSE",
  # "2020_BLSE",
  # "2021_BLSE",
  "2022_BLSE"
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

# Test sur un minsup
dossier_enreg = "results_minsup_ESBL/minsup_0_001/"
folder_creation(dossier_enreg, 10)
minsup_BLSE = 0.001
source("scripts/4_apriori.R")
source("scripts/5_filtre_itemsets.R")
source("scripts/6_describ_itemsets.R") # describing itemsets
source("scripts/7_plot_reseaux.R")
source("scripts/7_bis_legende_reseaux.R") # drawing the legend
list_graphs <- paste0("graph_",list_datasets,"_0.95")
plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 1, 1) # drawing the figure with all networks


for (essai in 1:10){
  set.seed(21*essai)
  dossier_enreg = paste0("results_minsup_ESBL/minsup_0_001/", essai, "/")
  chemin_donnees = paste0("results_minsup_ESBL/minsup_0_001/", essai, "/sampled_datasets/")
  minsup_BLSE = 0.001
  source("scripts/3_simulation_bdd.R")
  source("scripts/4_apriori.R")
  source("scripts/5_filtre_itemsets.R")
  source("scripts/6_describ_itemsets.R") # describing itemsets
  source("scripts/7_plot_reseaux.R")
  source("scripts/7_bis_legende_reseaux.R") # drawing the legend
  list_graphs <- paste0("graph_",list_datasets,"_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 1, 1) # drawing the figure with all networks
}

# Faire tous les minsup d'un coup
thresholds <- c(0.013, 0.014, 0.011, 0.012)

scripts <- list(
  "scripts/4_apriori.R",
  "scripts/5_filtre_itemsets.R",
  "scripts/6_describ_itemsets.R",
  "scripts/7_plot_reseaux.R",
  "scripts/7_bis_legende_reseaux.R"
)

for (th in thresholds) {
  # construction du chemin directement dans dossier_enreg
  suffix <- gsub("\\.", "_", format(th, scientific = FALSE))
  dossier_enreg <- file.path("results_minsup_ESBL", paste0("minsup_", suffix), "")
  dossier_enreg <- paste0(dossier_enreg,"/")
  
  # création du dossier et mise à jour du minsup
  folder_creation(dossier_enreg, 1)
  minsup_BLSE <- th
  
  # exécution des scripts qui utilisent dossier_enreg et minsup_BLSE
  lapply(scripts, source)
  
  # assemblage des graphes dans ce même dossier
  list_graphs <- paste0("graph_", list_datasets, "_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 1, 1)
}





### Graphe densité par minsup BLSE

list_minsup <- paste0("minsup_", c("0_001","0_005","0_01","0_011","0_012","0_013","0_014","0_015","0_02","0_05","0_06","0_08","0_1"))
list_minsup <- paste0("minsup_", c("0_001","0_005","0_01","0_02","0_05","0_06","0_08","0_1"))
list_minsup <- paste0("minsup_", c("0_001","0_005","0_01","0_02","0_05","0_1"))

densities <- sapply(list_minsup, function(m) {
  read.xlsx(file.path("results_minsup_ESBL", m, "describ_graphs_0.95.xlsx"))[1, 3]
})

densities[is.na(densities)] <- 0

df <- data.frame(
  minsup = factor(list_minsup, levels = list_minsup,
                  labels = gsub("_", ".", sub("minsup_", "", list_minsup))),
  density = densities,
  stringsAsFactors = FALSE
)

ggplot(df, aes(x = minsup, y = density, group = 1)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = "minsup", y = "density") +
  theme_minimal()

### Calculer la densité (médiane, IC) pour minsup BLSE = 0.001 (2018)

# on crée la liste des sous-dossiers rep1 à rep10
list_reps <- paste0("minsup_0_001/rep", 1:10)

# on parcourt chacun pour en extraire la densité (cellule [1,3])
densities <- sapply(list_reps, function(m) {
  read.xlsx(
    file.path("results_minsup_ESBL", m, "describ_graphs_0.95.xlsx")
  )[1, 3]
})

# nommer le vecteur pour plus de clarté
names(densities) <- paste0("rep", 1:10)

densities

### Calculer la densité (médiane, IC) pour minsup BLSE = 0.001 (2022)

# on crée la liste des sous-dossiers rep1 à rep10
list_reps <- paste0("minsup_0_001/rep", 1:10)

# on parcourt chacun pour en extraire la densité (cellule [2,3])
densities <- sapply(list_reps, function(m) {
  read.xlsx(
    file.path("results_minsup_ESBL", m, "describ_graphs_0.95.xlsx")
  )[2, 3]
})

# nommer le vecteur pour plus de clarté
names(densities) <- paste0("rep", 1:10)

densities

