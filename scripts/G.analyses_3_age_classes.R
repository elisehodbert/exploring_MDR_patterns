list_datasets = c(
  "2022_BLSE",
  "2022_non_BLSE"
)

list_data_simul = c(
  "2022_BLSE",
  "2022_non_BLSE_1_25",
  "2022_non_BLSE_26_50",
  "2022_non_BLSE_51_75",
  "2022_non_BLSE_76_100"
)

folder_creation("results_by_age_class/results_under_65_sampled_size_over_80", 10)
folder_creation("results_by_age_class/results_65_79_sampled_size_over_80", 10)
folder_creation("results_by_age_class/results_over_80", 10)


## Sous-échantillonnage

for (i in 1:length(list_datasets)){
  # Creating the 3 datasets
  nom_data = paste0("EC_", list_datasets[i])
  data_under_65 = get(nom_data) %>% 
    filter(age < 65)
  data_65_79 = get(nom_data) %>%
    filter(age %in% 65:79)
  data_over_80 = get(nom_data) %>%
    filter(age>79)
  taille_ssech = nrow(data_over_80) # the smallest dataset is always people 80 and over
  
  # Saving datasets

  # nom_data_over_80 = paste0("sampled_", list_datasets[i])
  # assign(nom_data_over_80, data_over_80)
  # save(list = nom_data_over_80, 
  #      file = paste0("results_by_gender/results_65_79_sampled_size_over_80/sampled_datasets/", nom_data_over_80, ".RData"))
  # 
  # Creating sampled datasets
  for (essai in 1:10){
    set.seed(essai*43)
    data_sampled_under_65 <- creation_ssech(dossier_enreg, 
                                            data_under_65, 
                                            taille_ssech) 
    
    data_sampled_65_79 <- creation_ssech(dossier_enreg, 
                                         data_65_79, 
                                         taille_ssech)
    
    #Save
    # nom_data_under_65 = paste0("sampled_", list_datasets[i])
    # assign(nom_data_under_65, data_sampled_under_65)
    # save(list = nom_data_under_65, 
    #      file = paste0("results_by_age_class/results_under_65_sampled_size_over_80/rep",essai,"/sampled_datasets/", nom_data_under_65, ".RData"))
    # 
    nom_data_65_79 = paste0("sampled_", list_datasets[i])
    assign(nom_data_65_79, data_sampled_65_79)
    save(list = nom_data_65_79, 
         file = paste0("results_by_age_class/results_65_79_sampled_size_over_80/rep",essai,"/sampled_datasets/", nom_data_65_79, ".RData"))
    
    cat(paste0(nom_data," : rep ",essai," sampled"),"\n")
  }
}

# Analyses 65-79

for (essai in 7:10){
  set.seed(essai*43)
  dossier_enreg = paste0("results_by_age_class/results_65_79_sampled_size_over_80/rep", essai,"/")
  chemin_donnees = paste0("results_by_age_class/results_65_79_sampled_size_over_80/rep", essai, "/sampled_datasets/")
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

# Bootstrapping for individuals 80 and more

for (i in 1:length(list_datasets)){
  nom_data = paste0("EC_", list_datasets[i])
  data = get(nom_data)
  
  data = data %>%
    filter(age>79)
  
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
         file = paste0("results_by_age_class/results_over_80/rep", essai, "/sampled_datasets/", nom_data_bs, ".RData"))
    
    cat(paste0(nom_data," : rep ",essai," sampled"),"\n")
  }
}

for (essai in 2:10){
  set.seed(essai*43)
  dossier_enreg = paste0("results_by_age_class/results_over_80/rep", essai,"/")
  chemin_donnees = paste0("results_by_age_class/results_over_80/rep", essai, "/sampled_datasets/")
  
  source("scripts/3_simulation_bdd.R")
  source("scripts/4_apriori.R")
  
  itemsets_simul_2022_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2022_non_BLSE", c("1","25","50","75","100"))
  
  pvalue = "0.95"
  source("scripts/5_filtre_itemsets.R")
  
  source("scripts/6_describ_itemsets.R")
  
  # Plots
  source("scripts/7_plot_reseaux.R")
  
  list_graphs <- paste0("graph_",list_datasets,"_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 5, 2) # drawing the figure with all networks
}


### Densité

nb_ite = 1:10

# under 65
density_under_65_all = data.frame(dataset=list_datasets)
for (i in 1:10){
  density_under_65_rep = read.xlsx(paste0("results_by_age_class/results_under_65/rep",i,"/describ_graphs_0.95.xlsx"))
  density_under_65_rep = density_under_65_rep[c(5,10),] #for this analysis, we select the results for 2022
  density_under_65_all = cbind(density_under_65_all, density_under_65_rep$density)
  colnames(density_under_65_all)[i+1] = paste0("rep",i)
}

density_under_65_stats = sapply(1:2, function(row){
  density <- unlist(density_under_65_all[row, 2:11])
  median_density <- median(density)
  ci_lower <- quantile(density, 0.025)
  ci_upper <- quantile(density, 0.975)
  c(median = median_density, lower = ci_lower, upper = ci_upper)
})
density_under_65_stats = as.data.frame(density_under_65_stats)
rownames(density_under_65_stats) = c("median","lower_ci","upper_ci")
colnames(density_under_65_stats) = list_datasets

# 65-79
density_65_79_all = data.frame(dataset=list_datasets)
for (i in 1:10){
  density_65_79_rep = read.xlsx(paste0("results_by_age_class/results_65_79_sampled_size_over_80/rep",i,"/describ_graphs_0.95.xlsx"))
  density_65_79_rep = density_65_79_rep[1:2,] #for this analysis, we select the results for 2022
  density_65_79_all = cbind(density_65_79_all, density_65_79_rep$density)
  colnames(density_65_79_all)[i+1] = paste0("rep",i)
}

density_65_79_stats = sapply(1:2, function(row){
  density <- unlist(density_65_79_all[row, 2:11])
  median_density <- median(density)
  ci_lower <- quantile(density, 0.025)
  ci_upper <- quantile(density, 0.975)
  c(median = median_density, lower = ci_lower, upper = ci_upper)
})
density_65_79_stats = as.data.frame(density_65_79_stats)
rownames(density_65_79_stats) = c("median","lower_ci","upper_ci")
colnames(density_65_79_stats)=list_datasets


# 80+

density_over_80_all = data.frame(dataset=list_datasets)
for (i in 1:10){
  density_over_80_rep = read.xlsx(paste0("results_by_age_class/results_over_80/rep",i,"/describ_graphs_0.95.xlsx"))
  density_over_80_rep = density_over_80_rep[1:2,] #for this analysis, we select the results for 2022
  density_over_80_all = cbind(density_over_80_all, density_over_80_rep$density)
  colnames(density_over_80_all)[i+1] = paste0("rep",i)
}

density_over_80_stats = sapply(1:2, function(row){
  density <- unlist(density_over_80_all[row, 2:11])
  median_density <- median(density)
  ci_lower <- quantile(density, 0.025)
  ci_upper <- quantile(density, 0.975)
  c(median = median_density, lower = ci_lower, upper = ci_upper)
})
density_over_80_stats=as.data.frame(density_over_80_stats)
rownames(density_over_80_stats) = c("median","lower_ci","upper_ci")
colnames(density_over_80_stats)=list_datasets

# Combiner les données

density_under_65_stats$Age_Group <- "Under_65"
density_65_79_stats$Age_Group <- "65_79"
density_over_80_stats$Age_Group <- "Over_80"

reshape_data <- function(df) {
  df %>% 
    rownames_to_column(var = "stat")  %>% 
    pivot_longer(cols = c("2022_BLSE", "2022_non_BLSE"), names_to = "dataset", values_to = "value") %>% 
    pivot_wider(names_from = "stat", values_from = "value")
  
}

density_under_65_stats = reshape_data(density_under_65_stats)
density_65_79_stats = reshape_data(density_65_79_stats)
density_over_80_stats = reshape_data(density_over_80_stats)

data_long = rbind(density_under_65_stats, density_over_80_stats, density_65_79_stats)

data_long[6,5] = data_long[6,5]+0.003


plot1 <- ggplot(subset(data_long, dataset == "2022_BLSE"), aes(x = factor(Age_Group, levels = c("Under_65","65_79","Over_80")), y = median, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "identity", fill = c("#B2B2E5", "#6666CC", "#3333CC"), color = "black", width = 0.7) +
  geom_errorbar(
    data = subset(data_long, dataset == "2022_BLSE"),
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0.2, color = "black"
  ) +
  labs(title = "ESBL E. coli (2022)", x = "", y = "Density") +
  scale_x_discrete(labels = c("Under 65","65-79","80 and over")) +
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
plot1

plot2 <- ggplot(subset(data_long, dataset == "2022_non_BLSE"), aes(x = factor(Age_Group, levels = c("Under_65","65_79","Over_80")), y = median, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "identity", fill = c("#B2B2E5", "#6666CC", "#3333CC"), color = "black", width = 0.7) +
  geom_errorbar(
    data = subset(data_long, dataset == "2022_non_BLSE"),
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0.2, color = "black"
  ) +
  labs(title = "Non-ESBL E. coli (2022)", x = "", y = "Density") +
  theme_minimal() +
  scale_x_discrete(labels = c("Under 65","65-79","80 and over")) +
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
plot2

ggsave(filename = "plots/barplots_density_age_class/plot_density_age_3_classes_2022.png",
       plot = grid.arrange(plot1, plot2, ncol = 2),
       width = 25, height = 25, units = "cm")

