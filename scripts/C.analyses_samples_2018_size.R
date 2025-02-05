# Defining which datasets we want to work on in this part
list_datasets = c(
  "2019_BLSE",
  "2020_BLSE",
  "2021_BLSE",
  "2022_BLSE",
  "2019_non_BLSE",
  "2020_non_BLSE",
  "2021_non_BLSE",
  "2022_non_BLSE"
)

list_data_simul <- c( #Simulated data is very voluminous. We cut the lists of datasets into smaller lists so that the computer does not crash
  "2019_BLSE",
  "2020_BLSE",
  "2021_BLSE",
  "2022_BLSE",
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
  "2021_non_BLSE_1_20",
  "2021_non_BLSE_21_40",
  "2021_non_BLSE_41_60",
  "2021_non_BLSE_61_80",
  "2021_non_BLSE_81_100",
  "2022_non_BLSE_1_20",
  "2022_non_BLSE_21_40",
  "2022_non_BLSE_41_60",
  "2022_non_BLSE_61_80",
  "2022_non_BLSE_81_100"
)


# On crée les dossiers appropriés :
folder_creation("results_samples_2018_size", 50)



# Number of rows of samples which will be created
taille_BLSE = nrow(EC_2018_BLSE)
taille_non_BLSE = nrow(EC_2018_non_BLSE)

# samples creation
for (i in 1:length(list_datasets)){

  for (essai in 31:50){
    set.seed(21*essai)
    
    nom_data = paste0("EC_", list_datasets[i])
    
    if(str_detect(list_datasets[i], "non_BLSE")){
      data_ssech <- creation_ssech(dossier_enreg, get(nom_data), taille_non_BLSE)
    } else {
      data_ssech <- creation_ssech(dossier_enreg, get(nom_data), taille_BLSE)
    }
    
    #Save
    nom_data_ssech = paste0("sampled_", list_datasets[i])
    assign(nom_data_ssech, data_ssech)
    save(list = nom_data_ssech, 
         file = paste0("results_samples_2018_size/rep",essai,"/sampled_datasets/", nom_data_ssech, ".RData"))
    cat(paste0(nom_data," : rep ",essai," sampled"),"\n")
    
    }
}

pvalue = "0.95"

for (essai in 45:50){
  set.seed(21*essai)
  dossier_enreg = paste0("results_samples_2018_size/rep", essai, "/")
  chemin_donnees = paste0("results_samples_2018_size/rep", essai, "/sampled_datasets/")

  source("scripts/3_simulation_bdd.R")
  source("scripts/4_apriori.R")

  for (i in 1:length(list_datasets)){
    if(str_detect(list_datasets[i], "non_BLSE")){
      concat_itemsets_non_BLSE(dossier_enreg,list_datasets[i], c("1","20","40","60","80","100"))
    }
  }

  source("scripts/5_filtre_itemsets.R")

  source("scripts/6_describ_itemsets.R")

  source("scripts/7_plot_reseaux.R")
  
  list_graphs <- paste0("graph_",list_datasets,"_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 5, 2) # drawing the figure with all networks
}

### Density test ###

nb_ite = 1:10
densite_recap = data.frame(dataset = list_datasets)

# Importing the datasets
for(i in nb_ite){
  for (j in 1:length(list_datasets)){
    load(paste0("results_samples_2018_size/rep", i, "/reseaux/graph_",list_datasets[j],"_", pvalue, ".RData")) 
  }
  densite = sapply(list_datasets, function(dataset){
    graph = get(paste0("graph_", dataset, "_", pvalue))
    return(edge_density(graph))
  })
  
  nom_var = paste0("ite_",i)
  densite_recap[[nom_var]] = densite
}

# Mean density
test = data.frame(t(densite_recap))
colnames(test) = test[1,]
test <- test %>%
  filter(!row_number() %in% c(1))

test <- as.data.frame(sapply(test, as.numeric))

means <- sapply(test, mean)

# mann-kendall
# mk.test(means[1:4])
# mk.test(means[5:8])

sd_values <- sapply(test, sd) # sd avec la moyenne (pas la méthode qu'on a choisie au final)

summary_df <- data.frame(Mean = means, SD = sd_values)
write.xlsx(summary_df, file = "results_samples_2018_size/recap_densite.xlsx", rowNames = TRUE)