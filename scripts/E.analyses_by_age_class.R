# In the case of analyses by age, depending on the year/phenotype, it is the over-65s or under-65s who are the most numerous.
# Each year, therefore, we need to determine the smallest data set and sub-sample to this size.

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

list_data_simul = c(
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

folder_creation("results_by_age_class/results_under_65", 10)
folder_creation("results_by_age_class/results_over_65", 10)
folder_creation("results_by_age_class/results_bs_under_65", 10)


##### Subsampling & Bootstrapping #####

# Initialise an empty data frame to store description of subsampling
results_table <- data.frame(Dataset = character(),
                            N_Under_65 = integer(),
                            N_Over_65 = integer(),
                            stringsAsFactors = FALSE)

for (i in 1:length(list_datasets)){
  nom_data = paste0("EC_", list_datasets[i])
  data_under_65 = get(nom_data) %>% 
    filter(age < 65)
  data_over_65 = get(nom_data) %>%
    filter(age >= 65)
  taille_ssech = min(c(nrow(data_under_65), nrow(data_over_65)))
  
  # adding results in the table
  results_table <- rbind(results_table, 
                         data.frame(Dataset = list_datasets[i],
                                    N_Under_65 = nrow(data_under_65),
                                    N_Over_65 = nrow(data_over_65)))
  
  # subsampling
  for (essai in 1:10){
    set.seed(essai*43)
    data_ssech_under_65 <- creation_ssech(dossier_enreg,
                                          data_under_65,
                                          taille_ssech)

    data_ssech_over_65 <- creation_ssech(dossier_enreg,
                                         data_over_65,
                                         taille_ssech)

    #Save
    nom_data_ssech_under_65 = paste0("sampled_", list_datasets[i])
    assign(nom_data_ssech_under_65, data_ssech_under_65)
    save(list = nom_data_ssech_under_65,
         file = paste0("results_by_age_class/results_under_65/rep",essai,"/sampled_datasets/", nom_data_ssech_under_65, ".RData"))

    nom_data_ssech_over_65 = paste0("sampled_", list_datasets[i])
    assign(nom_data_ssech_over_65, data_ssech_over_65)
    save(list = nom_data_ssech_over_65,
         file = paste0("results_by_age_class/results_over_65/rep",essai,"/sampled_datasets/", nom_data_ssech_over_65, ".RData"))
    cat(paste0(nom_data," : rep ",essai," sampled"),"\n")
    }
}

write.xlsx(results_table, file="results_by_age_class/nrow_by_dataset.xlsx")

# Bootstrapping for individuals under 65 

for (i in 1:length(list_datasets)){
  nom_data = paste0("EC_", list_datasets[i])
  data_under65 = get(nom_data) %>%
    filter(age < 65)
  
  for (essai in 11:20){
    set.seed(essai*43)
    data_bs_under65 = data_under65 %>% 
      sample_n(size = nrow(data_under65), replace = TRUE)
    
    #Save
    nom_data_bs_under65 = paste0("sampled_", list_datasets[i])
    assign(nom_data_bs_under65, data_bs_under65)
    save(list = nom_data_bs_under65, 
         file = paste0("results_by_age_class/results_bs_under_65/rep", essai, "/sampled_datasets/", nom_data_bs_under65, ".RData"))
    
    cat(paste0(nom_data," : rep ",essai," sampled"),"\n")
  }
}




##### Performing the analyses #####

# Under 65
for (essai in 1:10){
  set.seed(essai*43)
  dossier_enreg = paste0("results_by_age_class/results_under_65/rep", essai, "/")
  chemin_donnees = paste0("results_by_age_class/results_under_65/rep", essai, "/sampled_datasets/")
  
  source("scripts/3_simulation_bdd.R")
  source("scripts/4_apriori.R")
  
  itemsets_simul_2018_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2018_non_BLSE", c("1","20","40","60","80","100"))
  itemsets_simul_2019_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2019_non_BLSE",  c("1","20","40","60","80","100"))
  itemsets_simul_2020_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2020_non_BLSE",  c("1","20","40","60","80","100"))
  itemsets_simul_2021_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2021_non_BLSE",  c("1","20","40","60","80","100"))
  itemsets_simul_2022_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2022_non_BLSE",  c("1","20","40","60","80","100"))
  
  pvalue = "0.95"
  source("scripts/5_filtre_itemsets.R")
  
  source("scripts/6_describ_itemsets.R")
  
  # Plots
  source("scripts/7_plot_reseaux.R")
  
  list_graphs <- paste0("graph_",list_datasets,"_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_genre_BLSE_0.95", 5, 2)
}


# 65 and over
for (essai in 1:10){
  set.seed(essai*43)
  dossier_enreg = paste0("results_by_age_class/results_over_65/rep", essai, "/")
  chemin_donnees = paste0("results_by_age_class/results_over_65/rep", essai, "/sampled_datasets/")
  
  source("scripts/3_simulation_bdd.R")
  source("scripts/4_apriori.R")
  
  itemsets_simul_2018_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2018_non_BLSE",  c("1","20","40","60","80","100"))
  itemsets_simul_2019_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2019_non_BLSE",  c("1","20","40","60","80","100"))
  itemsets_simul_2020_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2020_non_BLSE",  c("1","20","40","60","80","100"))
  itemsets_simul_2021_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2021_non_BLSE",  c("1","20","40","60","80","100"))
  itemsets_simul_2022_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2022_non_BLSE",  c("1","20","40","60","80","100"))
  
  pvalue = "0.95"
  source("scripts/5_filtre_itemsets.R")
  
  source("scripts/6_describ_itemsets.R")

  # Plots
  source("scripts/7_plot_reseaux.R")
  
  list_graphs <- paste0("graph_",list_datasets,"_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_genre_BLSE_0.95", 5, 2)
}

# Individuals under 65 (bootstrapped datasets)

for (essai in 19:20){
  set.seed(essai*43)
  dossier_enreg = paste0("results_by_age_class/results_bs_under_65/rep", essai,"/")
  chemin_donnees = paste0("results_by_age_class/results_bs_under_65/rep", essai, "/sampled_datasets/")
  
  source("scripts/3_simulation_bdd.R")
  source("scripts/4_apriori.R")
  
  itemsets_simul_2018_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2018_non_BLSE", c("1","25","50","75","100"))
  itemsets_simul_2019_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2019_non_BLSE", c("1","25","50","75","100"))
  itemsets_simul_2020_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2020_non_BLSE", c("1","25","50","75","100"))
  itemsets_simul_2021_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2021_non_BLSE", c("1","25","50","75","100"))
  itemsets_simul_2022_non_BLSE <- concat_itemsets_non_BLSE(dossier_enreg, "2022_non_BLSE", c("1","25","50","75","100"))
  
  pvalue = "0.95"
  source("scripts/5_filtre_itemsets.R")
  
  source("scripts/6_describ_itemsets.R")
  
  
  # Plots
  source("scripts/7_plot_reseaux.R")
  
  list_graphs <- paste0("graph_",list_datasets,"_0.95")
  plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 5, 2) # drawing the figure with all networks
}


##### Barplots of network density #####

get_density_stats <- function(age_group, n_rep = 10) {
  density_all <- data.frame(dataset = list_datasets)
  
  for (i in 1:n_rep) {
    file_path <- paste0("results_by_age_class/results_", age_group, "/rep", i, "/describ_graphs_0.95.xlsx")
    density_rep <- read.xlsx(file_path)$density
    density_all <- cbind(density_all, density_rep)
    colnames(density_all)[i + 1] <- paste0("rep", i)
  }
  
  # Compute medians and confidence intervals
  density_stats <- apply(density_all[, -1], 1, function(density) {
    c(median = median(density), lower = quantile(density, 0.025), upper = quantile(density, 0.975))
  })
  
  return(as.data.frame(t(density_stats)))
}

# Process data for both age groups
density_over_65_stats <- get_density_stats("over_65")
density_under_65_stats <- get_density_stats("bs_under_65")

# Combine datasets
density_age <- cbind(
  list_datasets, 
  density_under_65_stats, 
  density_over_65_stats
)
colnames(density_age) <- c("dataset", "density_under_65", "ci_lower_under_65", "ci_upper_under_65", 
                           "density_over_65", "ci_lower_over_65", "ci_upper_over_65")

# Filter data for 2022
density_age <- density_age %>%
  filter(dataset %in% c("2022_BLSE", "2022_non_BLSE"))


# Restructurer les données pour ggplot
data_long <- pivot_longer(density_age, 
                          cols = c(density_under_65, density_over_65), 
                          names_to = "Age", 
                          values_to = "Density")

# Calculer les intervalles de confiance pour 2022
data_long <- data_long %>%
  mutate(ymin = case_when(
    Age == "density_over_65" & dataset == "2022_BLSE" ~ density_over_65_ci_lower[5],
    Age == "density_over_65" & dataset == "2022_non_BLSE" ~ density_over_65_ci_lower[10],
    Age == "density_under_65" & dataset == "2022_BLSE" ~ density_under_65_ci_lower[5],
    Age == "density_under_65" & dataset == "2022_non_BLSE" ~ density_under_65_ci_lower[10],
    TRUE ~ NA_real_
  ),
  ymax = case_when(
    Age == "density_over_65" & dataset == "2022_BLSE" ~ density_over_65_ci_upper[5],
    Age == "density_over_65" & dataset == "2022_non_BLSE" ~ density_over_65_ci_upper[10],
    Age == "density_under_65" & dataset == "2022_BLSE" ~ density_under_65_ci_upper[5],
    Age == "density_under_65" & dataset == "2022_non_BLSE" ~ density_under_65_ci_upper[10],
    TRUE ~ NA_real_
  ))

data_long$Age <- factor(data_long$Age, levels = c("density_under_65", "density_over_65"))

# Generate plots
plot1 <- ggplot(subset(data_long, dataset == "2022_BLSE"), aes(x = Age, y = Density, fill = Age)) +
  geom_bar(stat = "identity", position = "identity", fill = c("#B2B2E5", "#6666CC"), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, color = "black", na.rm = TRUE) +
  labs(title = "ESBL E. coli (2022)", x = "", y = "Density") +
  scale_x_discrete(labels = c("Under 65", "65 and over")) +
  scale_y_continuous(limits = c(0, 0.35), breaks = seq(0, 0.35, by = 0.05)) +
  coord_cartesian(ylim = c(0.016, 0.35)) +
  guides(fill = "none") +
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
    plot.title = element_text(hjust = 0.5, vjust = 2, size = 18)
  )

plot2 <- ggplot(subset(data_long, dataset == "2022_non_BLSE"), aes(x = Age, y = Density, fill = Age)) +
  geom_bar(stat = "identity", position = "dodge", fill = c("#B2B2E5", "#6666CC"), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, color = "black", na.rm = TRUE) +
  labs(title = "Non-ESBL E. coli (2022)", x = "", y = "Density") +
  theme_minimal() +
  scale_x_discrete(labels = c("Under 65", "65 and over")) +
  scale_y_continuous(limits = c(0, 0.31), breaks = seq(0, 0.15, by = 0.05)) +
  coord_cartesian(ylim = c(0.007, 0.15)) +
  guides(fill = "none") +
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
    plot.title = element_text(hjust = 0.5, vjust = 2, size = 18)
  )

# Save plots side by side
ggsave(filename = "plots/barplots_density_age_class/plot_density_age_class_2022_bs_1_20.png",
       plot = grid.arrange(plot1, plot2, ncol = 2),
       width = 20, height = 25, units = "cm")