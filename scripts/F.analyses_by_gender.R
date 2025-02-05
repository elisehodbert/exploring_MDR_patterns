### Defining which datasets we will be working on ####

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

### Creating the appropriate folders ###

folder_creation("results_by_gender/results_men", 1)
folder_creation("results_by_gender/results_women", 1)
folder_creation("results_by_gender/results_sampled_women_size_men", 10)

### Creating men dataset, women dataset, and sampled women dataset ###

for (i in 1:length(list_datasets)){
  # Creating men and women datasets
  nom_data = paste0("EC_", list_datasets[i])
  data_women = get(nom_data) %>% 
    filter(sexe == "F")
  data_men = get(nom_data) %>%
    filter(sexe == "H")
  taille_ssech = nrow (data_men) # the smallest dataset is always the men's
  
  # Saving men and women datasets
  nom_data_women = paste0("sampled_", list_datasets[i])
  assign(nom_data_women, data_women)
  save(list = nom_data_women, 
       file = paste0("results_by_gender/results_women/sampled_datasets/", nom_data_women, ".RData"))
  
  nom_data_men = paste0("sampled_", list_datasets[i])
  assign(nom_data_men, data_men)
  save(list = nom_data_men, 
       file = paste0("results_by_gender/results_men/sampled_datasets/", nom_data_men, ".RData"))
  
  # Creating sampled women dataset
  for (essai in 1:10){
    set.seed(essai*43)
    data_sampled_women <- creation_ssech(dossier_enreg, 
                                          data_women, 
                                          taille_ssech) 
    
    #Save
    nom_data_sampled_women = paste0("sampled_", list_datasets[i])
    assign(nom_data_sampled_women, data_sampled_women)
    save(list = nom_data_sampled_women, 
         file = paste0("results_by_gender/results_sampled_women_size_men/rep", essai, "/sampled_datasets/", nom_data_sampled_women, ".RData"))

    cat(paste0(nom_data," : rep ",essai," sampled"),"\n")
    }
}

# Creating a table with the number of isolates

for (i in 1:length(list_datasets)){
  load(paste0("results_by_gender/results_women/sampled_datasets/sampled_",list_datasets[i],".RData"))
  print(paste(list_datasets[i],nrow(get(paste0("sampled_",list_datasets[i])))))
}

# Performing analyses

# Men

dossier_enreg = "results_by_gender/results_men/"
chemin_donnees = paste0("results_by_gender/results_men/sampled_datasets/")

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



# Women

dossier_enreg = "results_by_gender/results_women/"
chemin_donnees = paste0("results_by_gender/results_women/sampled_datasets/")

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



# Sampled women
for (essai in 1:10){
  set.seed(essai*43)
  dossier_enreg = paste0("results_by_gender/results_sampled_women_size_men/rep", essai,"/")
  chemin_donnees = paste0("results_by_gender/results_sampled_women_size_men/rep", essai, "/sampled_datasets/")
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


### Test stat différence de densité hommes / femmes

# patterns_hommes = read_excel("analyses_genre/data_hommes/tab_recap_itemsets_0.95.xlsx")
# patterns_femmes = read_excel("analyses_genre/data_femmes/tab_recap_itemsets_0.95.xlsx")
# 
# patterns_BLSE = data.frame("sexe" = c(rep("H",4), rep("F",4)),
#                            "annee" = rep(c("2018","2019","2020","2021"),2),
#                            "nb_patterns" = c(patterns_hommes[1:4,]$nb_itemsets_filtre,
#                                              patterns_femmes[1:4,]$nb_itemsets_filtre))
# patterns_non_BLSE = data.frame("sexe" = c(rep("H",4), rep("F",4)),
#                                "annee" = rep(c("2018","2019","2020","2021"),2),
#                                "nb_patterns" = c(patterns_hommes[5:8,]$nb_itemsets_filtre,
#                                                  patterns_femmes[5:8,]$nb_itemsets_filtre))
# ggplot(patterns_BLSE, aes(fill = sexe, y = nb_patterns, x = annee)) +
#   geom_bar(position="dodge", stat = "identity") + 
#   scale_fill_discrete() +
#   ggtitle("Number of patterns identified in ESBL E. coli") +
#   theme_minimal() +
#   theme(axis.line = element_line(colour = "black"), 
#         axis.text.x = element_text(hjust = 1, size = 16, color="black"),
#         axis.text.y = element_text(size=16,color="black"),
#         axis.title = element_blank(),
#         plot.title = element_text(hjust=0.5,vjust=2, size=20)) # rotation du texte  
# 
# ggplot(patterns_non_BLSE, aes(fill = sexe, y = nb_patterns, x = annee)) +
#   geom_bar(position="dodge", stat = "identity") +
#   ggtitle("Number of patterns identifed in non-ESBL E. coli") +
#   theme_minimal() +
#   theme(axis.line = element_line(colour = "black"), 
#         axis.text.x = element_text(hjust = 1, size = 16, color="black"),
#         axis.text.y = element_text(size=16,color="black"),
#         axis.title = element_blank(),
#         plot.title = element_text(hjust=0.5,vjust=2, size=20)) # rotation du texte  



### Barplots densité

# Récupération densité

# density_women_all = data.frame(dataset=list_datasets)
# for (i in 1:10){
#   density_women_rep = read.xlsx(paste0("results_by_gender/results_sampled_women_size_men/rep",i,"/describ_graphs_0.95.xlsx"))
#   density_women_all = cbind(density_women_all, density_women_rep$density)
#   colnames(density_women_all)[i+1] = paste0("rep",i)
# }
# density_women_all
# 
# density_women = sapply(1:10,function(row){ # vecteur des moyennes
#   mean(unlist(density_women_all[row,2:11]))
# })
# 
# std_error_women = sapply(1:10, function(row){ # vecteur des erreur standards
#   density <- unlist(density_women_all[row,2:11])
#   sd(density)/sqrt(length(density))
# })
# 
# density_men = read.xlsx("results_by_gender/results_men/describ_graphs_0.95.xlsx")
# density_men = density_men %>%
#   select(-mean_degree)
# density_genre = cbind(density_men,density_women)
# colnames(density_genre) = c("dataset", "density_men", "density_women")

# # test de pente pour la densité (pas intéressant finalement)
# density_women_ESBL = density_genre$density_women[c(1,3,5,7,9)]
# density_women_non_ESBL = density_genre$density_women[c(2,4,6,8,10)]
# density_men_ESBL = density_genre$density_men[c(1,3,5,7,9)]
# density_men_non_ESBL = density_genre$density_men[c(2,4,6,8,10)]
# 
# ks.test(density_women_ESBL,density_men_ESBL)
# ks.test(density_women_non_ESBL,density_men_non_ESBL)


# pour faire un barplot sur la densité de 2022
# 
# data = density_genre %>%
#   filter(dataset %in% c("2022_BLSE", "2022_non_BLSE"))
# 
# data_long <- pivot_longer(data, cols = c(density_women, density_men), names_to = "Gender", values_to = "Density")
# 
# std_error_women_2022_BLSE = std_error_women[5]
# std_error_women_2022_non_BLSE = std_error_women[10]
# 
# data_long$ymin = c(data_long$Density[1] - std_error_women_2022_BLSE, NA, data_long$Density[3] - std_error_women_2022_non_BLSE, NA)
# data_long$ymax = c(data_long$Density[1] + std_error_women_2022_BLSE, NA, data_long$Density[3] + std_error_women_2022_non_BLSE, NA)
# 
# 
# # Create two separate plots
# plot1 <- ggplot(subset(data_long, dataset == "2022_BLSE"), aes(x = Gender, y = Density, fill = Gender)) +
#   geom_bar(stat = "identity", position = "identity", fill = c(addFade("#DF5B90",0.5) ,"#DF5B90"), color = "black", width = 0.7) +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, color = "black", na.rm = TRUE) +  # Ajout de l'erreur uniquement pour les "Women"
#   labs(title = "ESBL E. coli (2022)", x = "", y = "Density") +
#   scale_x_discrete(labels = c("Men", "Women")) +
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
# plot2 <- ggplot(subset(data_long, dataset == "2022_non_BLSE"), aes(x = Gender, y = Density, fill = Gender)) +
#   geom_bar(stat = "identity", position = "dodge", fill = c(addFade("#DF5B90",0.5) ,"#DF5B90"), color = "black", width = 0.7) +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, color = "black", na.rm = TRUE) +  # Ajout de l'erreur uniquement pour les "Women"
#   labs(title = "Non-ESBL E. coli (2022)", x = "", y = "Density") +
#   theme_minimal() +
#   scale_x_discrete(labels = c("Men", "Women")) +
#   scale_y_continuous(limits = c(0, 0.31),
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
# ggsave(filename = "plots/barplots_density_gender/plot_density_gender_2022.png",
#        plot = grid.arrange(plot1, plot2, ncol = 2),
#        width = 20, height = 25, units = "cm")






### median and confidence intervals (2.5-97.5) ###

# importing women
density_women_all = data.frame(dataset=list_datasets)
for (i in 1:10) {
  density_women_rep = read.xlsx(paste0("results_by_gender/results_sampled_women_size_men/rep", i, "/describ_graphs_0.95.xlsx"))
  density_women_all = cbind(density_women_all, density_women_rep$density)
  colnames(density_women_all)[i+1] = paste0("rep", i)
}

# calculus of median and confidence intervals
density_women_stats = sapply(1:10, function(row) {
  density <- unlist(density_women_all[row, 2:11])
  median_density <- median(density)
  ci_lower <- quantile(density, 0.025)
  ci_upper <- quantile(density, 0.975)
  c(median = median_density, lower = ci_lower, upper = ci_upper)
})

# transposing
density_women_stats <- t(density_women_stats)
colnames(density_women_stats) <- c("median", "lower", "upper")
density_women_median <- density_women_stats[, "median"]
density_women_ci_lower <- density_women_stats[, "lower"]
density_women_ci_upper <- density_women_stats[, "upper"]

# importing men
density_men = read.xlsx("results_by_gender/results_men/describ_graphs_0.95.xlsx")
density_men = density_men %>% select(-mean_degree)

# combining men and women
density_genre = cbind(density_men, density_women_median, density_women_ci_lower, density_women_ci_upper)
colnames(density_genre) = c("dataset", "density_men", "density_women", "ci_lower_women", "ci_upper_women")

# filtering
data = density_genre %>%
  filter(dataset %in% c("2022_BLSE", "2022_non_BLSE"))

data_long <- pivot_longer(data, cols = c(density_women, density_men), names_to = "Gender", values_to = "Density")

# Confidence intervals
data_long <- data_long %>%
  mutate(ymin = case_when(
    Gender == "density_women" & dataset == "2022_BLSE" ~ density_women_ci_lower[5],
    Gender == "density_women" & dataset == "2022_non_BLSE" ~ density_women_ci_lower[10],
    TRUE ~ NA_real_
  ),
  ymax = case_when(
    Gender == "density_women" & dataset == "2022_BLSE" ~ density_women_ci_upper[5],
    Gender == "density_women" & dataset == "2022_non_BLSE" ~ density_women_ci_upper[10],
    TRUE ~ NA_real_
  ))

# plots
plot1 <- ggplot(subset(data_long, dataset == "2022_BLSE"), aes(x = Gender, y = Density, fill = Gender)) +
  geom_bar(stat = "identity", position = "identity", fill = c(addFade("#DF5B90", 0.5), "#DF5B90"), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, color = "black", na.rm = TRUE) +
  labs(title = "ESBL E. coli (2022)", x = "", y = "Density") +
  scale_x_discrete(labels = c("Men", "Women")) +
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

plot2 <- ggplot(subset(data_long, dataset == "2022_non_BLSE"), aes(x = Gender, y = Density, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", fill = c(addFade("#DF5B90", 0.5), "#DF5B90"), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, color = "black", na.rm = TRUE) +
  labs(title = "Non-ESBL E. coli (2022)", x = "", y = "Density") +
  theme_minimal() +
  scale_x_discrete(labels = c("Men", "Women")) +
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

# Saving the plots
ggsave(filename = "plots/barplots_density_gender/plot_density_gender_2022_2.png",
       plot = grid.arrange(plot1, plot2, ncol = 2),
       width = 20, height = 25, units = "cm")
