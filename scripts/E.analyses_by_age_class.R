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

# Initialise an empty data frame to store the results
results_table <- data.frame(Dataset = character(),
                            N_Under_65 = integer(),
                            N_Over_65 = integer(),
                            stringsAsFactors = FALSE)

# Retrieve the smallest number of lines and ssech at this size
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











### Barplots density +/-65 ans

# list_density_age <- lapply(1:10, function(i){
#   density_under_65 = read.xlsx(paste0("results_by_age_class/results_under_65/rep",i,"/describ_graphs_0.95.xlsx"))
#   density_under_65 = density_under_65 %>%
#     select(-mean_degree)
#   density_over_65 = read.xlsx(paste0("results_by_age_class/results_over_65/rep",i,"/describ_graphs_0.95.xlsx"))
#   density_over_65 = density_over_65 %>%
#     select(-mean_degree)
#   density_age = merge(density_under_65, density_over_65, by = "dataset")
#   colnames(density_age) = c("dataset", "density_under_65", "density_over_65")
#   return(density_age)
# })

# # Vecteur de under_65 2022 BLSE
# vector_density_under_65 <- sapply(list_density_age, function(dataset) dataset$density_under_65[9])
# density_under_65 = mean(vector_density_under_65) # on prend temporairement la moyenne en attendant de régler le problème
# 
# # Vecteur de over_65 2022 BLSE
# vector_density_over_65 <- sapply(list_density_age, function(dataset) dataset$density_over_65[9])
# 
# density_under_65_all = data.frame(dataset=list_datasets)
# for (i in 1:10){
#   density_under_65_rep = read.xlsx(paste0("results_by_age_class/results_under_65/rep",i,"/describ_graphs_0.95.xlsx"))
#   density_under_65_all = cbind(density_under_65_all, density_under_65_rep$density)
#   colnames(density_under_65_all)[i+1] = paste0("rep",i)
# }
# density_under_65_all
# 
# density_under_65 = sapply(1:10,function(row){ # vecteur des moyennes
#   mean(unlist(density_under_65_all[row,2:11]))
# })
# 
# std_error_under_65 = sapply(1:10, function(row){ # vecteur des erreur standards 
#   density <- unlist(density_under_65_all[row,2:11])
#   sd(density)/sqrt(length(density))
# })
# 
# 
# 
# density_over_65_all = data.frame(dataset=list_datasets)
# for (i in 1:10){
#   density_over_65_rep = read.xlsx(paste0("results_by_age_class/results_over_65/rep",i,"/describ_graphs_0.95.xlsx"))
#   density_over_65_all = cbind(density_over_65_all, density_over_65_rep$density)
#   colnames(density_over_65_all)[i+1] = paste0("rep",i)
# }
# density_over_65_all
# 
# density_over_65 = sapply(1:10,function(row){ # vecteur des moyennes
#   mean(unlist(density_over_65_all[row,2:11]))
# })
# 
# std_error_over_65 = sapply(1:10, function(row){ # vecteur des erreur standards 
#   density <- unlist(density_over_65_all[row,2:11])
#   sd(density)/sqrt(length(density))
# })
# 
# density_age = data.frame(dataset = list_datasets, 
#                          density_under_65 = density_under_65,
#                          density_over_65 = density_over_65, 
#                          std_error_under_65 = c(rep(0,5),std_error_under_65[6:9], 0),
#                          std_error_over_65 = c(std_error_over_65[1:5], rep(0,4), std_error_over_65[10]))
# 
# 
# # Pour faire un barplot sur la densité de 2022
# 
# data = density_age %>%
#   filter(dataset %in% c("2022_BLSE", "2022_non_BLSE"))
# 
# data_long <- pivot_longer(data, 
#                           cols = c(density_under_65, density_over_65), 
#                           names_to = "Age_Group", 
#                           values_to = "Density")
# 
# # Create two separate plots
# plot1 <- ggplot(subset(data_long, dataset == "2022_BLSE"), aes(x = Age_Group, y = Density, fill = Age_Group)) +
#   geom_bar(stat = "identity", position = "identity", fill =c("#B2B2E5", "#6666CC"), color = "black", width = 0.7) +
#   geom_errorbar(
#     data = subset(data_long, dataset == "2022_BLSE" & Age_Group == "density_over_65"),
#     aes(ymin = Density - std_error_over_65, ymax = Density + std_error_over_65),
#     width = 0.2, color = "black"
#   ) +
#   labs(title = "ESBL E. coli (2022)", x = "", y = "Density") +
#   scale_x_discrete(labels = c("65 and over", "Under 65")) +
#   scale_y_continuous(limits = c(0, 0.35),
#                      breaks = seq(0, 0.35, by = 0.05)) +
#   coord_cartesian(ylim = c(0.016, 0.35)) +
#   guides(fill = "none") + #get rid of the legend
#   theme_minimal() +
#   theme(
#     axis.line = element_line(colour = "black"),
#     panel.grid = element_blank(),
#     panel.grid.major.y = element_line(color = "grey"),
#     panel.grid.minor.y = element_line(color = "grey"),
#     axis.text.x = element_text( vjust = 0.2, size = 18, color="black"),
#     axis.ticks = element_line(),
#     axis.text.y = element_text(size = 18, color = "black"),
#     legend.text = element_text(size = 18),
#     axis.title = element_text(size = 18),
#     plot.title = element_text(hjust = 0.5, vjust = 2, size = 18)) # rotation du texte
# plot1
# 
# plot2 <- ggplot(subset(data_long, dataset == "2022_non_BLSE"), aes(x = Age_Group, y = Density, fill = Age_Group)) +
#   geom_bar(stat = "identity", position = "dodge", fill = c("#B2B2E5", "#6666CC"), color = "black", width = 0.7) +
#   geom_errorbar(
#     data = subset(data_long, dataset == "2022_non_BLSE" & Age_Group == "density_over_65"),
#     aes(ymin = Density - std_error_over_65, ymax = Density + std_error_over_65),
#     width = 0.2, color = "black"
#   ) +
#   labs(title = "Non-ESBL E. coli (2022)", x = "", y = "Density") +
#   theme_minimal() +
#   scale_x_discrete(labels = c("65 and over", "Under 65")) +
#   scale_y_continuous(limits = c(0, 0.15),
#                      breaks = seq(0, 0.15, by = 0.05)) +
#   coord_cartesian(ylim = c(0.007, 0.15)) +
#   guides(fill = "none") + #get rid of the legend
#   theme_minimal() +
#   theme(
#     axis.line = element_line(colour = "black"),
#     panel.grid = element_blank(),
#     panel.grid.major.y = element_line(color = "grey"),
#     panel.grid.minor.y = element_line(color = "grey"),
#     axis.text.x = element_text(vjust = 0.2, size = 18, color = "black"),
#     axis.ticks = element_line(),
#     axis.text.y = element_text(size = 18, color = "black"),
#     legend.text = element_text(size = 18),
#     axis.title = element_text(size = 18),
#     plot.title = element_text(hjust = 0.5, vjust = 2, size = 18)) # rotation du texte
# plot2
# 
# # Arrange plots side by side
# ggsave(filename = "plots/barplots_density_age_class/plot_density_age_2022.png", # changer l'emplacement de sauvegarde
#        plot = grid.arrange(plot1, plot2, ncol = 2),
#        width = 20, height = 25, units = "cm")




# #  IC 2.5% et 97.5%
# 
# # density
# 
# list_density_age <- lapply(1:10, function(i){
#   density_under_65 = read.xlsx(paste0("results_by_age_class/results_under_65/rep",i,"/describ_graphs_0.95.xlsx"))
#   density_under_65 = density_under_65 %>%
#     select(-mean_degree)
#   density_over_65 = read.xlsx(paste0("results_by_age_class/results_over_65/rep",i,"/describ_graphs_0.95.xlsx"))
#   density_over_65 = density_over_65 %>%
#     select(-mean_degree)
#   density_age = merge(density_under_65, density_over_65, by = "dataset")
#   colnames(density_age) = c("dataset", "density_under_65", "density_over_65")
#   return(density_age)
# })
# 
# #  under_65 2022 BLSE
# # vector_density_under_65 <- sapply(list_density_age, function(dataset) dataset$density_under_65[9])
# # density_under_65 = mean(vector_density_under_65) # on prend temporairement la moyenne en attendant de régler le problème
# 
# # over_65 2022 BLSE
# vector_density_over_65 <- sapply(list_density_age, function(dataset) dataset$density_over_65[9])
# 
# density_under_65_all = data.frame(dataset=list_datasets)
# for (i in 1:10){
#   density_under_65_rep = read.xlsx(paste0("results_by_age_class/results_under_65/rep",i,"/describ_graphs_0.95.xlsx"))
#   density_under_65_all = cbind(density_under_65_all, density_under_65_rep$density)
#   colnames(density_under_65_all)[i+1] = paste0("rep",i)
# }
# density_under_65_all
# 
# 
# density_under_65_stats = sapply(1:10, function(row){
#   density <- unlist(density_under_65_all[row, 2:11])
#   median_density <- median(density)
#   ci_lower <- quantile(density, 0.025)
#   ci_upper <- quantile(density, 0.975)
#   c(median = median_density, lower = ci_lower, upper = ci_upper)
# })
# 
# # transpose the results
# density_under_65_stats <- t(density_under_65_stats)
# colnames(density_under_65_stats) <- c("median", "lower", "upper")
# density_under_65_median <- density_under_65_stats[, "median"]
# density_under_65_ci_lower <- density_under_65_stats[, "lower"]
# density_under_65_ci_upper <- density_under_65_stats[, "upper"]
# 
# # over65
# density_over_65_all = data.frame(dataset=list_datasets)
# for (i in 1:10){
#   density_over_65_rep = read.xlsx(paste0("results_by_age_class/results_over_65/rep",i,"/describ_graphs_0.95.xlsx"))
#   density_over_65_all = cbind(density_over_65_all, density_over_65_rep$density)
#   colnames(density_over_65_all)[i+1] = paste0("rep",i)
# }
# density_over_65_all
# 
# density_over_65_stats = sapply(1:10, function(row){
#   density <- unlist(density_over_65_all[row, 2:11])
#   median_density <- median(density)
#   ci_lower <- quantile(density, 0.025)
#   ci_upper <- quantile(density, 0.975)
#   c(median = median_density, lower = ci_lower, upper = ci_upper)
# })
# 
# # transpose the results
# density_over_65_stats <- t(density_over_65_stats)
# colnames(density_over_65_stats) <- c("median", "lower", "upper")
# density_over_65_median <- density_over_65_stats[, "median"]
# density_over_65_ci_lower <- density_over_65_stats[, "lower"]
# density_over_65_ci_upper <- density_over_65_stats[, "upper"]
# 
# density_age = data.frame(list_datasets, density_under_65_median, density_over_65_median, density_over_65_ci_lower, density_over_65_ci_upper)
# colnames(density_age) = c("dataset", "density_under_65", "density_over_65", "ci_lower_over_65", "ci_upper_over_65")
# 
# # Barplot for 2022
# 
# data = density_age %>%
#   filter(dataset %in% c("2022_BLSE", "2022_non_BLSE"))
# 
# 
# data_long <- pivot_longer(data,
#                           cols = c(density_under_65, density_over_65),
#                           names_to = "Age_Group",
#                           values_to = "Density")
# #data_long$ci_lower_over_65[c(1,3)] = 0
# #data_long$ci_upper_over_65[c(1,3)] = 0
# 
# # Create two separate plots
# plot1 <- ggplot(subset(data_long, dataset == "2022_BLSE"), aes(x = Age_Group, y = Density, fill = Age_Group)) +
#   geom_bar(stat = "identity", position = "identity", fill =c("#B2B2E5", "#6666CC"), color = "black", width = 0.7) +
#   geom_errorbar(
#     data = subset(data_long, dataset == "2022_BLSE" & Age_Group == "density_over_65"),
#     aes(ymin = ci_lower_over_65, ymax = ci_upper_over_65),
#     width = 0.2, color = "black"
#   ) +
#   labs(title = "ESBL E. coli (2022)", x = "", y = "Density") +
#   scale_x_discrete(labels = c("65 and over", "Under 65")) +
#   scale_y_continuous(limits = c(0, 0.35),
#                      breaks = seq(0, 0.35, by = 0.05)) +
#   coord_cartesian(ylim = c(0.016, 0.35)) +
#   guides(fill = "none") + #get rid of the legend
#   theme_minimal() +
#   theme(
#     axis.line = element_line(colour = "black"),
#     panel.grid = element_blank(),
#     panel.grid.major.y = element_line(color = "grey"),
#     panel.grid.minor.y = element_line(color = "grey"),
#     axis.text.x = element_text( vjust = 0.2, size = 18, color="black"),
#     axis.ticks = element_line(),
#     axis.text.y = element_text(size = 18, color = "black"),
#     legend.text = element_text(size = 18),
#     axis.title = element_text(size = 18),
#     plot.title = element_text(hjust = 0.5, vjust = 2, size = 18)) # rotating the text
# plot1
# 
# plot2 <- ggplot(subset(data_long, dataset == "2022_non_BLSE"), aes(x = Age_Group, y = Density, fill = Age_Group)) +
#   geom_bar(stat = "identity", position = "dodge", fill = c("#B2B2E5", "#6666CC"), color = "black", width = 0.7) +
#   geom_errorbar(
#     data = subset(data_long, dataset == "2022_non_BLSE" & Age_Group == "density_over_65"),
#     aes(ymin = ci_lower_over_65-0.001, ymax = ci_upper_over_65),
#     width = 0.2, color = "black"
#   ) +
#   labs(title = "Non-ESBL E. coli (2022)", x = "", y = "Density") +
#   theme_minimal() +
#   scale_x_discrete(labels = c("65 and over", "Under 65")) +
#   scale_y_continuous(limits = c(0, 0.15),
#                      breaks = seq(0, 0.15, by = 0.05)) +
#   coord_cartesian(ylim = c(0.007, 0.15)) +
#   guides(fill = "none") + #get rid of the legend
#   theme_minimal() +
#   theme(
#     axis.line = element_line(colour = "black"),
#     panel.grid = element_blank(),
#     panel.grid.major.y = element_line(color = "grey"),
#     panel.grid.minor.y = element_line(color = "grey"),
#     axis.text.x = element_text(vjust = 0.2, size = 18, color = "black"),
#     axis.ticks = element_line(),
#     axis.text.y = element_text(size = 18, color = "black"),
#     legend.text = element_text(size = 18),
#     axis.title = element_text(size = 18),
#     plot.title = element_text(hjust = 0.5, vjust = 2, size = 18)) # rotating the results
# plot2
# 
# # Arrange plots side by side
# ggsave(filename = "plots/barplots_density_age_class/plot_density_age_2022.png",
#        plot = grid.arrange(plot1, plot2, ncol = 2),
#        width = 20, height = 25, units = "cm")
