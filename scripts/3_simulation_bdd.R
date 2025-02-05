##### SIMULATING DATASETS UNDER H0 #####

list_datasets_EC <- paste0("EC_",list_datasets)

for (i in 1:length(list_datasets)){
  nom_list_simul <- paste0("list_simul_", list_datasets[i])
  
  # charging the right dataset (subset if we work on a subset of total data, otherwise total data)
  if (str_detect(chemin_donnees, "sampled")){
    load(paste0(chemin_donnees, "sampled_", list_datasets[i], ".RData"))
    df <- get(paste0("sampled_", as.character(list_datasets[i])))
  } else {
    load(paste0(chemin_donnees, list_datasets_EC[i], ".RData"))
    df <- get(as.character(list_datasets_EC[i]))
  }
  
  res <- list_simul_bdd(df, antibiotic_names, n) # creation of the list of simulated datasets
  assign(nom_list_simul, res) # giving the right name
  save(list = nom_list_simul, file = paste0(dossier_enreg, "H0_simulations/", nom_list_simul, ".RData")) # saving the dataset
  cat(paste(format(Sys.time()), nom_list_simul, " : list of datasets created and saved"), "\n")
  rm(list = nom_list_simul) # deleting object from environment to save space
  gc()
}


# Checking the simulated datasets
# for (i in 1:length(list_datasets)){
#   load(paste0(dossier_enreg, "H0_simulations/list_simul_", list_datasets[i], ".RData"))
# }

# Checking that the proportions are the same in the observed and simulated datasets
# verif_prop(EC_2018_BLSE, list_simul_2018_BLSE)
# verif_prop(EC_2019_BLSE, list_simul_2019_BLSE)
# verif_prop(EC_2020_BLSE, list_simul_2020_BLSE)
# verif_prop(EC_2021_BLSE, list_simul_2021_BLSE)
# 
# verif_prop(EC_2018_non_BLSE, list_simul_2018_non_BLSE)
# verif_prop(EC_2019_non_BLSE, list_simul_2019_non_BLSE)
# verif_prop(EC_2020_non_BLSE, list_simul_2020_non_BLSE)
# verif_prop(EC_2021_non_BLSE, list_simul_2021_non_BLSE)

# Checking that NAs are in the same places in the observed and simulated datasets
# verif_NA(EC_2018_BLSE, list_simul_2018_BLSE)
# verif_NA(EC_2019_BLSE, list_simul_2019_BLSE)
# verif_NA(EC_2020_BLSE, list_simul_2020_BLSE)
# verif_NA(EC_2021_BLSE, list_simul_2021_BLSE)
# 
# verif_NA(EC_2018_non_BLSE, list_simul_2018_non_BLSE)
# verif_NA(EC_2019_non_BLSE, list_simul_2019_non_BLSE)
# verif_NA(EC_2020_non_BLSE, list_simul_2020_non_BLSE)
# verif_NA(EC_2021_non_BLSE, list_simul_2021_non_BLSE)