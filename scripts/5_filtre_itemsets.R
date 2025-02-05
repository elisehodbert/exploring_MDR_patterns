##### Pruning itemsets using eSup and cLift #####

summary_cut_off_values = data.frame("dataset" = list_datasets, 
                           "cut_off_eSup" = c(NA), 
                           "cut_off_cLift" = c(NA))

for (i in 1:length(list_datasets)){
  # Loading observed and simulated datasets
  load(paste0(dossier_enreg,"itemset_obs/itemset_obs_", list_datasets[i], ".RData"))
  load(paste0(dossier_enreg,"itemsets_simul/itemsets_simul_", list_datasets[i], ".RData"))
  
  itemset_obs <- get(paste0("itemset_obs_", list_datasets[i]))
  itemsets_simul <- get(paste0("itemsets_simul_", list_datasets[i]))
  
  # Calculating cut-off values for eSup and cLift
  cut_off <- cut_off_calc(itemsets_simul, pvalue)
  summary_cut_off_values$cut_off_eSup[i] <- cut_off$eSup
  summary_cut_off_values$cut_off_cLift[i] <- cut_off$cLift
  
  # Pruning the itemsets
  itemset_filtre <- itemset_obs[itemset_obs@quality$eSup > cut_off$eSup & itemset_obs@quality$cLift > cut_off$cLift]

  # Saving the pruned itemset
  nom_itemsets = paste0("itemset_filtre_", pvalue, "_", list_datasets[i])
  assign(nom_itemsets, itemset_filtre)
  save(list = nom_itemsets, 
       file = paste0(dossier_enreg,"itemset_filtre_", pvalue, "/", nom_itemsets, ".RData"))
  
  rm(itemset_obs)
  rm(itemsets_simul)
  gc()
}

write.xlsx(summary_cut_off_values,file = paste0(dossier_enreg, "summary_cut_off_values", pvalue, ".xlsx"))