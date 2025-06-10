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


pop_regio = read.xlsx("pop_regio.xlsx")

summary_df <- summary_df %>%
  left_join(
    pop_regio %>% 
      select(Region, pop_2019 = Population.municipale.2019),
    by = c("region" = "Region")
  )

# Proportions
# summary_df_prop <- summary_df %>%
#   mutate(EC_2018 = 100 * EC_2018/sum(EC_2018),
#          EC_2019 = 100 * EC_2019/sum(EC_2019),
#          EC_2020 = 100 * EC_2020/sum(EC_2020),
#          EC_2021 = 100 * EC_2021/sum(EC_2021),
#          EC_2022 = 100 * EC_2022/sum(EC_2022)
#   )

summary_df_prop <- summary_df %>%
  mutate(EC_2018 = EC_2018/pop_2019,
         EC_2019 = EC_2019/pop_2019,
         EC_2020 = EC_2020/pop_2019,
         EC_2021 = EC_2021/pop_2019,
         EC_2022 = EC_2022/pop_2019
  )


# Utiliser tidyr pour réorganiser les données
# summary_df_long_prop <- gather(summary_df_prop, key = "year", value = "occurrences", -region)

# Barplot for number of isolates for each region per year
# graph <- ggplot(summary_df_long, aes(x = region, y = occurrences, fill = factor(year))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Number of isolates for each region per year",
#        x = "Region",
#        y = "Number of isolates") +
#   scale_fill_discrete(name = "Year") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# png("repartition_region_years_nb.png", width = 40, height = 21, units = "cm", res = 1000)
# plot(graph)
# dev.off()


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

color_scale <- scale_fill_gradient(low = "white", high = "midnightblue", limits = c(0,0.03)) #color scale


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
  filename = "all_fra_maps_isolates_region_regio_pop.jpg", 
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
  guides(fill = guide_colorbar(title = "Proportion of isolates divided by the regional population", 
                               ncol = 2))

legend <- get_legend(fra_map_test)
plot(legend)

ggsave(
  filename = "legend_all_fra_maps_isolates_region_regio_pop.jpg", 
  plot = legend, 
  device = "jpg", 
  width = 29.7, 
  height = 21, 
  units = "cm", 
  dpi = 1000
)
