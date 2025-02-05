### Defining which datasets we will be working on ####

list_datasets = c(
  "2018_BLSE",
  "2018_non_BLSE"
)

list_data_simul = c(
  "2018_BLSE",
  "2018_non_BLSE_1_25",
  "2018_non_BLSE_26_50",
  "2018_non_BLSE_51_75",
  "2018_non_BLSE_76_100"
)

### Creating the appropriate folders ###

folder_creation("results_bs_2018", 50)

### Creating bootstrap datasets

for (i in 1:length(list_datasets)){
  nom_data = paste0("EC_", list_datasets[i])
  data = get(nom_data)
  
  # Creating datasets
  for (essai in 31:50){
    set.seed(essai*43)
    # Sampling
    data_bs = data %>% 
      sample_n(size = nrow(data), replace = TRUE)
    
    #Save
    nom_data_bs = paste0("sampled_", list_datasets[i])
    assign(nom_data_bs, data_bs)
    save(list = nom_data_bs, 
         file = paste0("results_bs_2018/rep", essai, "/sampled_datasets/", nom_data_bs, ".RData"))
    
    cat(paste0(nom_data," : rep ",essai," sampled"),"\n")
  }
}

# Performing analyses

for (essai in 31:50){
  set.seed(essai*43)
  dossier_enreg = paste0("results_bs_2018/rep", essai,"/")
  chemin_donnees = paste0("results_bs_2018/rep", essai, "/sampled_datasets/")
  
  source("scripts/3_simulation_bdd.R")
  source("scripts/4_apriori.R")
  
  itemsets_simul_2018_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2018_non_BLSE", c("1","25","50","75","100"))

  pvalue = "0.95"
  source("scripts/5_filtre_itemsets.R")
  
  source("scripts/6_describ_itemsets.R")
  
  # Plots
  source("scripts/7_plot_reseaux.R")
  
  list_graphs <- paste0("graph_",list_datasets,"_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 5, 2) # drawing the figure with all networks
}



### Calculating median density and testing the slope ###

nb_ite = c(1:40)

# Importing density 2018

list_datasets = c(
  "2018_BLSE",
  "2018_non_BLSE"
)

list_data_simul = c(
  "2018_BLSE",
  "2018_non_BLSE_1_25",
  "2018_non_BLSE_26_50",
  "2018_non_BLSE_51_75",
  "2018_non_BLSE_76_100"
)


densite_recap_2018 = data.frame(dataset = list_datasets)
for(i in nb_ite){
  stats_graph = read.xlsx(paste0("results_bs_2018/rep",i,"/describ_graphs_0.95.xlsx"))
  nom_var = paste0("ite_",i)
  densite_recap_2018[[nom_var]] = stats_graph$density
}

# Importing density 2019-2022
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

list_data_simul <- c(
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

densite_recap_2019_2022 = data.frame(dataset = list_datasets)

for(i in nb_ite){ 
  for (j in 1:length(list_datasets)){
    load(paste0("results_samples_2018_size/rep", i, "/reseaux/graph_",list_datasets[j],"_", pvalue, ".RData")) 
  }
  densite = sapply(list_datasets, function(dataset){
    graph = get(paste0("graph_", dataset, "_", pvalue))
    return(edge_density(graph))
  })
  
  nom_var = paste0("ite_",i)
  densite_recap_2019_2022[[nom_var]] = densite
}


### Concatenation

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

list_data_simul <- c(
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

densite_recap = rbind(densite_recap_2018[1,], densite_recap_2019_2022[1:4,], densite_recap_2018[2,], densite_recap_2019_2022[5:8,])

# Creating a summary table
densite_recap_stats = sapply(1:10, function(row) {
  density <- unlist(densite_recap[row, 2:ncol(densite_recap)])
  median_density <- median(density)
  ci_lower <- quantile(density, 0.025)
  ci_upper <- quantile(density, 0.975)
  c(median = median_density, lower = ci_lower, upper = ci_upper)
})
colnames(densite_recap_stats) = list_datasets

densite_recap_stats <- t(densite_recap_stats)
colnames(densite_recap_stats) <- c("median", "lower", "upper")
densite_recap_median <- densite_recap_stats[, "median"]
densite_recap_ci_lower <- densite_recap_stats[, "lower"]
densite_recap_ci_upper <- densite_recap_stats[, "upper"]

densite_recap_median_BLSE = densite_recap_median[1:5]
densite_recap_median_non_BLSE = densite_recap_median[6:10]
indices <- seq_along(densite_recap_median_BLSE)

# Pearson
cor.test(indices, densite_recap_median_BLSE, method = "pearson")
cor.test(indices, densite_recap_median_non_BLSE, method = "pearson")

# spearman
cor.test(indices, densite_recap_median_BLSE, method = "spearman")
cor.test(indices, densite_recap_median_non_BLSE, method = "spearman")

#  mann-kendall
# mk.test(densite_recap_median_BLSE)
# mk.test(densite_recap_median_non_BLSE)

# sd_values <- sapply(test, sd)
# summary_df <- data.frame(Mean = means, SD = sd_values)
write.xlsx(densite_recap_stats, file = "results_samples_2018_size/recap_densite.xlsx", rowNames = TRUE)
