# Creating folders
folder_creation("results_samples_regio_prop", 10)

# Defining which datasets we want to work on in this part
list_years = as.character(2019:2022)

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

list_data_simul = c(
  "2019_BLSE",
  "2020_BLSE",
  "2021_BLSE",
  "2022_BLSE",
  "2019_non_BLSE_1_100",  
  "2020_non_BLSE_1_100",  
  "2021_non_BLSE_1_100",  
  "2022_non_BLSE_1_100"
)

regions_with_Corse = append(as.character(regions), "Corse", after = 4) # we include Corse for this analysis

# Defining sample size
tab_nb_isolats = read_excel("results_samples_regio_prop/nb_isolats_region.xlsx")

# ESBL or non-ESBL?

categ_pheno <- read_excel("data/categories_phenotypes_e_coli.xlsx")

blse_phenotypes <- categ_pheno %>% 
  filter(BLSE == "OUI" & EPC == "NON") %>%
  pull(phenotype)

non_blse_phenotypes <- categ_pheno %>% 
  filter(AUTRE == "OUI" & EPC == "NON" & BLSE == "NON") %>%
  pull(phenotype)

# Creating samples
for (essai in 1:10){ #for each rep
  set.seed(31*essai)
  dossier_enreg = paste0("results_samples_regio_prop/rep", essai, "/")
  
  for (i in 1:length(list_years)){ # for each year/phenotype subset
    data = get(paste0("EC_",list_years[i]))
    
    for (ind_region in 1:length(regions_with_Corse)){ # for each region
      
      data_region = data %>% 
        filter(region == as.character(regions_with_Corse[ind_region]))
      
      taille_ssech <- unlist(tab_nb_isolats[tab_nb_isolats$region == regions_with_Corse[ind_region], "nb_isolats"])
      
      data_ssech <- creation_ssech_replace(dossier_enreg, 
                                           data_region, 
                                           taille_ssech)
      data_ssech$phenotype <- nettoyer_vecteur(data_ssech$phenotype)
      
      # Saving ESBL
      dataset_BLSE = data_ssech %>% filter(phenotype %in% nettoyer_vecteur(blse_phenotypes))
      nom_data_region_BLSE = paste0("sampled_", list_years[i], "_BLSE_", regions_with_Corse[ind_region])
      assign(nom_data_region_BLSE, dataset_BLSE)
      save(list = nom_data_region_BLSE, 
           file = paste0(dossier_enreg, "sampled_datasets/", nom_data_region_BLSE, ".RData"))
      
      # Saving non-ESBL
      dataset_non_BLSE = data_ssech %>% 
        filter(phenotype %in% nettoyer_vecteur(non_blse_phenotypes) | is.na(phenotype))
      nom_data_region_non_BLSE = paste0("sampled_", list_years[i], "_non_BLSE_", regions_with_Corse[ind_region])
      assign(nom_data_region_non_BLSE, dataset_non_BLSE)
      save(list = nom_data_region_non_BLSE, 
           file = paste0(dossier_enreg, "sampled_datasets/", nom_data_region_non_BLSE, ".RData"))
      
      rm(list = c(nom_data_region_BLSE, nom_data_region_non_BLSE))
    }
  }
  cat(paste0("Rep ", essai, " sampled for all regions"), "\n")
}

# Gathering of data sets by year and phenotype

for (essai in 1:10){
  for(i in 1:length(list_datasets)){
    for (ind_region in 1:length(regions_with_Corse)){
      load(paste0("results_samples_regio_prop/rep", essai, "/sampled_datasets/sampled_", list_datasets[i], "_", regions_with_Corse[ind_region],".RData"))
    }
    dataset = data.frame(get(paste0("sampled_", list_datasets[i], "_", regions_with_Corse[1])))
    for (j in 2:length(regions_with_Corse)){
      dataset = rbind(dataset,get(paste0("sampled_", list_datasets[i], "_", regions_with_Corse[j])))
    }
    nom_dataset = paste0("sampled_", list_datasets[i])
    assign(nom_dataset, dataset)
    save(list = nom_dataset, 
         file = paste0("results_samples_regio_prop/rep", essai, "/sampled_datasets/sampled_", list_datasets[i], ".RData"))
  } 
}



### Analyses ###

# Performing analyses

pvalue = "0.95"

for (essai in 1:10){
  set.seed(21*essai)
  dossier_enreg = paste0("results_samples_regio_prop/rep", essai, "/")
  chemin_donnees = paste0("results_samples_regio_prop/rep", essai, "/sampled_datasets/")

  source("scripts/3_simulation_bdd.R")
  source("scripts/4_apriori.R")

  # Concaténation des itemsets simul
  for (i in 1:length(list_datasets)){
    if(str_detect(list_datasets[i], "non_BLSE")){
      concat_itemsets_non_BLSE(dossier_enreg,list_datasets[i], c("1","100"))
    }
  }

  # filtre
  source("scripts/5_filtre_itemsets.R")

  # describ itemsets
  source("scripts/6_describ_itemsets.R")

  # plot reseaux
  source("scripts/7_plot_reseaux.R")
  
  list_graphs <- paste0("graph_",list_datasets,"_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 4, 2) # drawing the figure with all networks
}


### Mean density

nb_ite = 1:10
densite_recap = data.frame(dataset = list_datasets)

# Importing datasets
for(i in nb_ite){
  for (j in 1:length(list_datasets)){
    load(paste0("results_samples_regio_prop/rep", i, "/reseaux/graph_",list_datasets[j],"_", pvalue, ".RData"))
  }
  densite = sapply(list_datasets, function(dataset){
    graph = get(paste0("graph_", dataset, "_", pvalue))
    return(edge_density(graph))
  })
  
  nom_var = paste0("rep_",i)
  densite_recap[[nom_var]] = densite
}

# Mean
test = data.frame(t(densite_recap))
colnames(test) = test[1,]
test <- test %>%
  filter(!row_number() %in% c(1))

test <- as.data.frame(sapply(test, as.numeric))

means <- sapply(test, mean)

# mann-kendall
mk.test(means[1:4])
mk.test(means[5:8])

sd_values <- sapply(test, sd)

summary_df <- data.frame(Mean = means, SD = sd_values)
write.xlsx(summary_df, file = "results_samples_regio_prop/recap_densite.xlsx", rowNames = TRUE)