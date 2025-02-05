###### Supplementary Figure XX : Map of the number of isolates per region for all years #####


# Adding a "year" column
EC_2018$year <- "EC_2018"
EC_2019$year <- "EC_2019"
EC_2020$year <- "EC_2020"
EC_2021$year <- "EC_2021"
EC_2022$year <- "EC_2022"

all_data <- rbind(EC_2018, EC_2019, EC_2020, EC_2021,EC_2022) # combine in a single dataframe
all_data$region = as.factor(all_data$region)

summary_table <- table(all_data$region, all_data$year) # number of isolates for each region per year
summary_df <- as.data.frame.matrix(summary_table)
summary_df$region <- rownames(summary_df)

summary_df_long <- gather(summary_df, key = "year", value = "occurrences", -region) # reorganize data


# Proportions
summary_df_prop <- summary_df %>%
  mutate(EC_2018 = 100 * EC_2018/sum(EC_2018),
         EC_2019 = 100 * EC_2019/sum(EC_2019),
         EC_2020 = 100 * EC_2020/sum(EC_2020),
         EC_2021 = 100 * EC_2021/sum(EC_2021),
         EC_2022 = 100 * EC_2022/sum(EC_2022)
  )


# Utiliser tidyr pour réorganiser les données
summary_df_long_prop <- gather(summary_df_prop, key = "year", value = "occurrences", -region)

# Barplot for number of isolates for each region per year
graph <- ggplot(summary_df_long, aes(x = region, y = occurrences, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of isolates for each region per year",
       x = "Region",
       y = "Number of isolates") +
  scale_fill_discrete(name = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

png("repartition_region_years_nb.png", width = 40, height = 21, units = "cm", res = 1000)
plot(graph)
dev.off()


# Map for number of isolates for each region per year

# source : https://www.data.gouv.fr/fr/datasets/contours-des-regions-francaises-sur-openstreetmap/#_
fra <- read_sf("cartes_geo/regions-20180101-shp/")

fra <- fra %>% # selecting only regions of France métropolitaine
  filter(!(nom %in% c("La Réunion", "Martinique","Guadeloupe","Guyane","Mayotte")))

# Matching names of regions
fra$nom <- forcats::fct_recode(fra$nom,
                                      "Auvergne_Rhone_Alpes" = "Auvergne-Rhône-Alpes",
                                      "Bourgogne_Franche_Comte" = "Bourgogne-Franche-Comté", 
                                      "Bretagne" = "Bretagne", 
                                      "Centre_Val_de_Loire" = "Centre-Val de Loire", 
                                      "Corse" = "Corse", 
                                      "Grand_Est" = "Grand Est",                 
                                      "Hauts_de_France" = "Hauts-de-France",           
                                      "Ile_de_France" = "Île-de-France", 
                                      "Normandie" = "Normandie", 
                                      "Nouvelle_Aquitaine" = "Nouvelle-Aquitaine", 
                                      "Occitanie" = "Occitanie",                 
                                      "Pays_de_la_Loire" = "Pays de la Loire", 
                                      "Provence_Alpes_Cote_d_Azur" = "Provence-Alpes-Côte d'Azur")

fra <- merge(fra, summary_df_prop, by.x = "nom", by.y = "region")

color_scale <- scale_fill_gradient(low = "white", high = "midnightblue", limits = c(0,20)) #color scale


# Creating a map for each year
fra_map_2018 <- ggplot(fra) + 
  geom_sf(aes(fill = EC_2018), show.legend = T, color = "grey2", size = 0.2) + 
  coord_sf(datum = NA, expand = FALSE) +
  color_scale +
  ggtitle("2018") +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none") +
  guides(fill = guide_colorbar(title = "Number of isolates", 
                               ncol = 2))

fra_map_2019 <- ggplot(fra) + 
  geom_sf(aes(fill = EC_2019), show.legend = T, color = "grey2", size = 0.2) + 
  coord_sf(datum = NA, expand = FALSE) +
  color_scale +
  ggtitle("2019") +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none") +
  guides(fill = guide_colorbar(title = "Number of isolates", 
                               ncol = 2))

fra_map_2020 <- ggplot(fra) + 
  geom_sf(aes(fill = EC_2020), show.legend = T, color = "grey2", size = 0.2) + 
  coord_sf(datum = NA, expand = FALSE) +
  color_scale +
  ggtitle("2020") +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none") +
  guides(fill = guide_colorbar(title = "Number of isolates", 
                               ncol = 2))

fra_map_2021 <- ggplot(fra) + 
  geom_sf(aes(fill = EC_2021), show.legend = T, color = "grey2", size = 0.2) + 
  coord_sf(datum = NA, expand = FALSE) +
  color_scale +
  ggtitle("2021") +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none") +
  guides(fill = guide_colorbar(title = "Number of isolates", 
                               ncol = 2))
fra_map_2022 <- ggplot(fra) + 
  geom_sf(aes(fill = EC_2022), show.legend = T, color = "grey2", size = 0.2) + 
  coord_sf(datum = NA, expand = FALSE) +
  color_scale +
  ggtitle("2022") +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16,),
        legend.position = "none") +
  guides(fill = guide_colorbar(title = "Number of isolates", 
                               ncol = 2))

# Combine all graphs into an only graph
combined_plot <- plot_grid(
  fra_map_2018, 
  fra_map_2019, 
  fra_map_2020, 
  fra_map_2021, 
  fra_map_2022, 
  nrow = 1  # arrange them in one row
)

ggsave(
  filename = "all_fra_maps_isolates_region.jpg", 
  plot = combined_plot, 
  device = "jpg", 
  width = 29.7, 
  height = 11, 
  units = "cm", 
  dpi = 1000
)

# Legend only
fra_map_test <- ggplot(fra) + 
  geom_sf(aes(fill = EC_2018), show.legend = T, color = "grey2", size = 0.2) + 
  # enlever l'affichage des coordonnés et de la grille
  coord_sf(datum = NA, expand = FALSE) +
  color_scale +
  ggtitle("2018") +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16)) +
  guides(fill = guide_colorbar(title = "Proportion of isolates", 
                               ncol = 2))

legend <- get_legend(fra_map_test)
plot(legend)

ggsave(
  filename = "legend_all_fra_maps_isolates_region.jpg", 
  plot = legend, 
  device = "jpg", 
  width = 29.7, 
  height = 21, 
  units = "cm", 
  dpi = 1000
)
 


# Même tableau mais proprotions à la place de count

summary_df_prop <- summary_df %>%
  mutate(EC_2018 = 100 * EC_2018/sum(EC_2018),
         EC_2019 = 100 * EC_2019/sum(EC_2019),
         EC_2020 = 100 * EC_2020/sum(EC_2020),
         EC_2021 = 100 * EC_2021/sum(EC_2021),
         EC_2022 = 100 * EC_2022/sum(EC_2022)
         )


# Utiliser tidyr pour réorganiser les données
summary_df_long_prop <- gather(summary_df_prop, key = "year", value = "occurrences", -region)

# Utiliser ggplot2 pour créer le graphique à barres
graph_prop <- ggplot(summary_df_long_prop, aes(x = region, y = occurrences, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion d'isolats par région pour chaque année",
       x = "Région",
       y = "Proportion (%)") +
  scale_fill_discrete(name = "Année") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


png("repartition_region_years_prop.png", width = 40, height = 21, units = "cm", res = 1000)
plot(graph_prop)
dev.off()


# Fraction de BLSE par région et par année

prop_year <- function(data_BLSE,data_tot,reg){
  nb_BLSE = nrow(data_BLSE %>% filter(region == reg))
  nb_tot = nrow(data_tot %>% filter(region == reg))
  return(nb_BLSE/nb_tot*100)
}

summary_df <- summary_df %>%
  filter(!(region %in% c("Polynésie Française",
                         "Nouvelle Calédonie",
                         "Guyane",
                         "Mayotte",
                         "La Reunion",
                         "Martinique",
                         "Guadeloupe")))

summary_BLSE = summary_df %>%
  select(region) %>%
  mutate(prop_BLSE_2018 = sapply(summary_df$region, prop_year, data_BLSE = EC_2018_BLSE, data_tot = EC_2018),
         prop_BLSE_2019 = sapply(summary_df$region, prop_year, data_BLSE = EC_2019_BLSE, data_tot = EC_2019),
         prop_BLSE_2020 = sapply(summary_df$region, prop_year, data_BLSE = EC_2020_BLSE, data_tot = EC_2020),
         prop_BLSE_2021 = sapply(summary_df$region, prop_year, data_BLSE = EC_2021_BLSE, data_tot = EC_2021),
         prop_BLSE_2022 = sapply(summary_df$region, prop_year, data_BLSE = EC_2022_BLSE, data_tot = EC_2022),

         )



# Test stat pour voir si les distributions sont différentes selon les régions

test <- t(summary_table)
#test <- test[,-c(7, 8, 11, 12, 17, 19, 20)] # supprimer les domtom

chisq.test(test)


# Idée : sous-échantillonner à la même proportion par région pour chaque année
valeurs <- c(36757, 21996, 36435, 15465, 37455, 24662, 18481, 31754, 56672, 43443, 44868, 45559) # le plus petit Number of isolates par région

# Additionner les valeurs
total <- sum(valeurs)

# Calculer les proportions relatives
proportions <- valeurs / total

# Afficher les résultats
resultats <- data.frame(Valeurs = valeurs, Proportion = proportions)
print(resultats)