list_datasets_EC <- paste0("EC_",list_datasets)

##### OBSERVED DATA #####

for (i in 1:length(list_datasets)){
  nom_data = list_datasets[i]
  
  # Loading the dataset (distinguishing total data and subsets like men/women, +65yo/-65yo, ...)
  if (str_detect(chemin_donnees, "sampled")){
    load(paste0(chemin_donnees, "sampled_", list_datasets[i], ".RData"))
    database <- get(paste0("sampled_", as.character(list_datasets[i])))
  }
  else{
    load(paste0(chemin_donnees, list_datasets_EC[i], ".RData"))
    database <- get(as.character(list_datasets_EC[i]))
  }
  
  # defining minsup (en fonction de BLSE ou non BLSE)either ESBL or non-ESBL)
  if (str_detect(nom_data, "non_BLSE")){
    minsup_a_util = minsup_non_BLSE
  } else if (str_detect(nom_data, "BLSE")){
    minsup_a_util = minsup_BLSE
  }
  
  # applying apriori algorithm
  itemset <- calc_itemset_obs(database, minsup_a_util)
  
  # saving the pattern
  nom_itemset <- paste0("itemset_obs_", nom_data)
  assign(nom_itemset, itemset)
  save(list = nom_itemset, 
       file = paste0(dossier_enreg, "itemset_obs/", nom_itemset, ".RData"))
  
  cat(paste(format(Sys.time()), "Observed patterns saved for :", nom_itemset),"\n")
}



##### SIMULATED DATA #####

for (i in 1:length(list_data_simul)){
  
  if (str_detect(list_data_simul[i], "non_BLSE")){
    minsup_a_util = minsup_non_BLSE # Setting the right minsup
    
    # defining the dataset to import (without the list indexes)
    lim = unlist(strsplit(list_data_simul[i], "_"))
    
    # setting the limits of the sublist to use
    if(length(lim) > 5){    # si analyses régionales :
      nom_dataset = paste(lim[1:(length(lim)-2)], collapse = "_")
      nom_data <- paste0("list_simul_", nom_dataset)
    } else{ # tous les autre cas
      nom_dataset = paste(lim[1:3], collapse = "_")
      nom_data <- paste0("list_simul_", nom_dataset)
    }
    
    debut = as.numeric(lim[length(lim) - 1])
    fin = as.numeric(lim[length(lim)])
    
    nom_itemsets <- paste0("itemsets_simul_", 
                           paste(lim[1:(length(lim))], collapse = "_"))
    
  } else if (str_detect(list_data_simul[i], "BLSE")){
    minsup_a_util = minsup_BLSE
    nom_itemsets <- paste0("itemsets_simul_", list_data_simul[i])
    nom_data <- paste0("list_simul_", list_data_simul[i])
    
    # in that case, no need to cut the list into multiple sublists
    debut = 1
    fin = 100
  }
  
  # importing the dataset
  load(paste0(dossier_enreg, "H0_simulations/", nom_data, ".RData"))
  list_simul = get(nom_data)
  list_simul = list_simul[debut:fin]
  
  # applying the apriori algorithm
  itemsets <- parallel_calc_list_simul(list_simul, minsup_a_util)
  
  # saving the patterns
  assign(nom_itemsets, itemsets)
  save(list = nom_itemsets, 
       file = paste0(dossier_enreg, "itemsets_simul/", nom_itemsets, ".RData"))
  
  #cat of the script progress
  cat(paste(format(Sys.time()), "Simulated patterns saved for :", nom_itemsets), "\n")
  
  # removing heavy objects from the environment
  rm(itemsets)
  rm(list = nom_itemsets)
  rm(list_simul)
  for (nom_list in list_datasets_EC){
    if (exists(nom_data)) {
      rm(list = nom_data, envir = .GlobalEnv)
    }}
  gc()
}

