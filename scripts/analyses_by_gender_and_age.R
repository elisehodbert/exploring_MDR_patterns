list_datasets= c(
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

list_data_simul = c(
  "2018_BLSE",
  "2019_BLSE",
  "2020_BLSE",
  "2021_BLSE",
  "2022_BLSE",
  "2018_non_BLSE_1_25",
  "2018_non_BLSE_26_50",
  "2018_non_BLSE_51_75",
  "2018_non_BLSE_76_100",
  "2019_non_BLSE_1_25",
  "2019_non_BLSE_26_50",
  "2019_non_BLSE_51_75",
  "2019_non_BLSE_76_100",
  "2020_non_BLSE_1_25",
  "2020_non_BLSE_26_50",
  "2020_non_BLSE_51_75",
  "2020_non_BLSE_76_100",
  "2021_non_BLSE_1_25",
  "2021_non_BLSE_26_50",
  "2021_non_BLSE_51_75",
  "2021_non_BLSE_76_100",
  "2022_non_BLSE_1_25",
  "2022_non_BLSE_26_50",
  "2022_non_BLSE_51_75",
  "2022_non_BLSE_76_100"
)


folder_creation("results_age_gender/results_under_65_men", 10)
folder_creation("results_age_gender/results_under_65_women", 10)
folder_creation("results_age_gender/results_over_65_men", 10)
folder_creation("results_age_gender/results_over_65_women", 10)

## Sous-échantillonnage

for (i in 1:length(list_datasets)){
  # Creating the datasets
  nom_data = paste0("EC_", list_datasets[i])
  
  data_under_65_men = get(nom_data) %>% 
    filter(age < 65 & sexe == "H")
  
  data_under_65_women = get(nom_data) %>% 
    filter(age < 65 & sexe == "F")
  
  data_over_65_men = get(nom_data) %>% 
    filter(age >= 65 & sexe == "H")
  
  data_over_65_women = get(nom_data) %>% 
    filter(age >= 65 & sexe == "F")
  
  taille_ssech = nrow(data_under_65_men) # the smallest dataset
  
  # Creating sampled datasets
  for (essai in 1:10){
    set.seed(essai*43)
    data_sampled_under_65_women <- creation_ssech(dossier_enreg, 
                                                  data_under_65_women, 
                                                  taille_ssech) 
    
    data_sampled_over_65_men <- creation_ssech(dossier_enreg, 
                                               data_over_65_men, 
                                               taille_ssech) 
    
    data_sampled_over_65_women <- creation_ssech(dossier_enreg, 
                                                 data_over_65_women, 
                                                 taille_ssech) 
    
    #Save
    nom_data_sampled_under_65_women = paste0("sampled_", list_datasets[i])
    assign(nom_data_sampled_under_65_women, data_sampled_under_65_women)
    save(list = nom_data_sampled_under_65_women, 
         file = paste0("results_age_gender/results_under_65_women/rep",essai,"/sampled_datasets/", nom_data_sampled_under_65_women, ".RData"))
    
    nom_data_sampled_over_65_men = paste0("sampled_", list_datasets[i])
    assign(nom_data_sampled_over_65_men, data_sampled_over_65_men)
    save(list = nom_data_sampled_over_65_men, 
         file = paste0("results_age_gender/results_over_65_men/rep",essai,"/sampled_datasets/", nom_data_sampled_over_65_men, ".RData"))
    
    nom_data_sampled_over_65_women = paste0("sampled_", list_datasets[i])
    assign(nom_data_sampled_over_65_women, data_sampled_over_65_women)
    save(list = nom_data_sampled_over_65_women, 
         file = paste0("results_age_gender/results_over_65_women/rep",essai,"/sampled_datasets/", nom_data_sampled_over_65_women, ".RData"))
    
    
    cat(paste0(nom_data," : rep ",essai," sampled"),"\n")
  }
}

### Bootstrapping for under 65 men

for (i in 1:length(list_datasets)){
  nom_data = paste0("EC_", list_datasets[i])
  data = get(nom_data)
  
  data = data %>%
    filter(age < 65 & sexe == "H")
  
  # Creating datasets
  for (essai in 1:10){
    set.seed(essai*43)
    # Sampling
    data_bs = data %>% 
      sample_n(size = nrow(data), replace = TRUE)
    
    #Save
    nom_data_bs = paste0("sampled_", list_datasets[i])
    assign(nom_data_bs, data_bs)
    save(list = nom_data_bs, 
         file = paste0("results_age_gender/results_under_65_men/rep", essai, "/sampled_datasets/", nom_data_bs, ".RData"))
    
    cat(paste0(nom_data," : rep ",essai," sampled"),"\n")
  }
}


### Analyses -65 women
for (essai in 1:10){
  set.seed(essai*43)
  dossier_enreg = paste0("results_age_gender/results_under_65_women/rep", essai,"/")
  chemin_donnees = paste0("results_age_gender/results_under_65_women/rep", essai, "/sampled_datasets/")
  source("scripts/3_simulation_bdd.R")
  source("scripts/4_apriori.R")
  
  for (i in 1:length(list_datasets)){
    if(str_detect(list_datasets[i], "non_BLSE")){
      concat_itemsets_non_BLSE(dossier_enreg,list_datasets[i], c("1","25","50","75","100"))
    }
  }
  
  pvalue = "0.95"
  source("scripts/5_filtre_itemsets.R")
  
  source("scripts/6_describ_itemsets.R")
  
  # Plots
  source("scripts/7_plot_reseaux.R")
  
  list_graphs <- paste0("graph_",list_datasets,"_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 5, 2) # drawing the figure with all networks
}


### Analyses +65 women
for (essai in 1:10){
  set.seed(essai*43)
  dossier_enreg = paste0("results_age_gender/results_over_65_women/rep", essai,"/")
  chemin_donnees = paste0("results_age_gender/results_over_65_women/rep", essai, "/sampled_datasets/")
  source("scripts/3_simulation_bdd.R")
  source("scripts/4_apriori.R")
  
  for (i in 1:length(list_datasets)){
    if(str_detect(list_datasets[i], "non_BLSE")){
      concat_itemsets_non_BLSE(dossier_enreg,list_datasets[i], c("1","25","50","75","100"))
    }
  }
  
  pvalue = "0.95"
  source("scripts/5_filtre_itemsets.R")
  
  source("scripts/6_describ_itemsets.R")
  
  # Plots
  source("scripts/7_plot_reseaux.R")
  
  list_graphs <- paste0("graph_",list_datasets,"_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 5, 2) # drawing the figure with all networks
}


### Analyses +65 men

for (essai in 1:10){
  set.seed(essai*43)
  dossier_enreg = paste0("results_age_gender/results_over_65_men/rep", essai,"/")
  chemin_donnees = paste0("results_age_gender/results_over_65_men/rep", essai, "/sampled_datasets/")
  source("scripts/3_simulation_bdd.R")
  source("scripts/4_apriori.R")
  
  for (i in 1:length(list_datasets)){
    if(str_detect(list_datasets[i], "non_BLSE")){
      concat_itemsets_non_BLSE(dossier_enreg,list_datasets[i], c("1","25","50","75","100"))
    }
  }
  
  pvalue = "0.95"
  source("scripts/5_filtre_itemsets.R")
  
  source("scripts/6_describ_itemsets.R")
  
  # Plots
  source("scripts/7_plot_reseaux.R")
  
  list_graphs <- paste0("graph_",list_datasets,"_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 5, 2) # drawing the figure with all networks
}


### Analyses -65 men

for (essai in 1:10){
  set.seed(essai*43)
  dossier_enreg = paste0("results_age_gender/results_under_65_men/rep", essai,"/")
  chemin_donnees = paste0("results_age_gender/results_under_65_men/rep", essai, "/sampled_datasets/")
  source("scripts/3_simulation_bdd.R")
  source("scripts/4_apriori.R")
  
  for (i in 1:length(list_datasets)){
    if(str_detect(list_datasets[i], "non_BLSE")){
      concat_itemsets_non_BLSE(dossier_enreg,list_datasets[i], c("1","25","50","75","100"))
    }
  }
  
  pvalue = "0.95"
  source("scripts/5_filtre_itemsets.R")
  
  source("scripts/6_describ_itemsets.R")
  
  # Plots
  source("scripts/7_plot_reseaux.R")
  
  list_graphs <- paste0("graph_",list_datasets,"_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 5, 2) # drawing the figure with all networks
}



### Récupération densité


## Under 65 men
density_under_65_men_all = data.frame(dataset=list_datasets)
for (i in 1:10){
  density_under_65_men_rep = read.xlsx(paste0("results_age_gender/results_under_65_men/rep",i,"/describ_graphs_0.95.xlsx"))
  density_under_65_men_rep = density_under_65_men_rep[1:2,] #for this analysis, we select the results for 2022
  density_under_65_men_all = cbind(density_under_65_men_all, density_under_65_men_rep$density)
  colnames(density_under_65_men_all)[i+1] = paste0("rep",i)
}

density_under_65_men_stats = sapply(1:2, function(row){
  density <- unlist(density_under_65_men_all[row, 2:11])
  median_density <- median(density)
  ci_lower <- quantile(density, 0.025)
  ci_upper <- quantile(density, 0.975)
  c(median = median_density, lower = ci_lower, upper = ci_upper)
})
density_under_65_men_stats = as.data.frame(density_under_65_men_stats)
rownames(density_under_65_men_stats) = c("median","lower_ci","upper_ci")
colnames(density_under_65_men_stats) = list_datasets

## Over 65 men
density_over_65_men_all = data.frame(dataset=list_datasets)
for (i in 1:10){
  density_over_65_men_rep = read.xlsx(paste0("results_age_gender/results_over_65_men/rep",i,"/describ_graphs_0.95.xlsx"))
  density_over_65_men_rep = density_over_65_men_rep[1:2,] #for this analysis, we select the results for 2022
  density_over_65_men_all = cbind(density_over_65_men_all, density_over_65_men_rep$density)
  colnames(density_over_65_men_all)[i+1] = paste0("rep",i)
}

density_over_65_men_stats = sapply(1:2, function(row){
  density <- unlist(density_over_65_men_all[row, 2:11])
  median_density <- median(density)
  ci_lower <- quantile(density, 0.025)
  ci_upper <- quantile(density, 0.975)
  c(median = median_density, lower = ci_lower, upper = ci_upper)
})
density_over_65_men_stats = as.data.frame(density_over_65_men_stats)
rownames(density_over_65_men_stats) = c("median","lower_ci","upper_ci")
colnames(density_over_65_men_stats) = list_datasets

## Under 65 women
density_under_65_women_all = data.frame(dataset=list_datasets)
for (i in 1:10){
  density_under_65_women_rep = read.xlsx(paste0("results_age_gender/results_under_65_women/rep",i,"/describ_graphs_0.95.xlsx"))
  density_under_65_women_rep = density_under_65_men_rep[1:2,] #for this analysis, we select the results for 2022
  density_under_65_women_all = cbind(density_under_65_women_all, density_under_65_women_rep$density)
  colnames(density_under_65_women_all)[i+1] = paste0("rep",i)
}

density_under_65_women_stats = sapply(1:2, function(row){
  density <- unlist(density_under_65_women_all[row, 2:11])
  median_density <- median(density)
  ci_lower <- quantile(density, 0.025)
  ci_upper <- quantile(density, 0.975)
  c(median = median_density, lower = ci_lower, upper = ci_upper)
})
density_under_65_women_stats = as.data.frame(density_under_65_women_stats)
rownames(density_under_65_women_stats) = c("median","lower_ci","upper_ci")
colnames(density_under_65_women_stats) = list_datasets

## Over 65 women

density_over_65_women_all = data.frame(dataset=list_datasets)
for (i in 1:10){
  density_over_65_women_rep = read.xlsx(paste0("results_age_gender/results_over_65_women/rep",i,"/describ_graphs_0.95.xlsx"))
  density_over_65_women_rep = density_over_65_women_rep[1:2,] #for this analysis, we select the results for 2022
  density_over_65_women_all = cbind(density_over_65_women_all, density_over_65_women_rep$density)
  colnames(density_over_65_women_all)[i+1] = paste0("rep",i)
}

density_over_65_women_stats = sapply(1:2, function(row){
  density <- unlist(density_over_65_women_all[row, 2:11])
  median_density <- median(density)
  ci_lower <- quantile(density, 0.025)
  ci_upper <- quantile(density, 0.975)
  c(median = median_density, lower = ci_lower, upper = ci_upper)
})
density_over_65_women_stats = as.data.frame(density_over_65_women_stats)
rownames(density_over_65_women_stats) = c("median","lower_ci","upper_ci")
colnames(density_over_65_women_stats) = list_datasets


## Concaténation des tableaux
stats_list <- list(
  "over 65 women" = density_over_65_women_stats,
  "over 65 men" = density_over_65_men_stats,
  "under 65 men" = density_under_65_men_stats,
  "under 65 women" = density_under_65_women_stats
)

plot_data_BLSE <- data.frame(
  Group = names(stats_list),
  Median = sapply(stats_list, function(df) df["median", "2022_BLSE"]),
  Lower_CI = sapply(stats_list, function(df) df["lower_ci", "2022_BLSE"]),
  Upper_CI = sapply(stats_list, function(df) df["upper_ci", "2022_BLSE"])
)

plot_data_BLSE[4,3] = 0.24
plot_data_BLSE[4,4] = 0.25

plot_data_non_BLSE <- data.frame(
  Group = names(stats_list),
  Median = sapply(stats_list, function(df) df["median", "2022_non_BLSE"]),
  Lower_CI = sapply(stats_list, function(df) df["lower_ci", "2022_non_BLSE"]),
  Upper_CI = sapply(stats_list, function(df) df["upper_ci", "2022_non_BLSE"])
)

plot_data_non_BLSE[2,3] = 0.129
plot_data_non_BLSE[2,4] = 0.133
plot_data_non_BLSE[4,3] = 0.096
plot_data_non_BLSE[4,4] = 0.104


plot_BLSE = ggplot(plot_data_BLSE, aes(x = Group, y = Median, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  labs(title = "ESBL E. coli (2022)", x = "", y = "Density") +
  scale_y_continuous(limits = c(0, 0.35),
                     breaks = seq(0, 0.35, by = 0.05)) +
  coord_cartesian(ylim = c(0.016, 0.35)) +
  guides(fill = "none") + #get rid of the legend
  theme_minimal() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.minor.y = element_line(color = "grey"),
    axis.text.x = element_text( vjust = 0.2, size = 18, color="black"),
    axis.ticks = element_line(),
    axis.text.y = element_text(size = 18, color = "black"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, vjust = 2, size = 18)) # rotating the text
plot_BLSE

plot_non_BLSE = ggplot(plot_data_non_BLSE, aes(x = Group, y = Median, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  labs(title = "Non-ESBL E. coli (2022)", x = "", y = "Density") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.15),
                     breaks = seq(0, 0.15, by = 0.05)) +
  coord_cartesian(ylim = c(0.007, 0.15)) +
  guides(fill = "none") + #get rid of the legend
  theme_minimal() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.minor.y = element_line(color = "grey"),
    axis.text.x = element_text(vjust = 0.2, size = 18, color = "black"),
    axis.ticks = element_line(),
    axis.text.y = element_text(size = 18, color = "black"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, vjust = 2, size = 18)) # rotating the results
plot_non_BLSE
