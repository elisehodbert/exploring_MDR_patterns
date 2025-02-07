dossier_enreg = "results_all_data/"
chemin_donnees = "data/"

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


# Assign colors to each antibiotic class for the bar plots
color_vector <- c(rep(colors[1], 3),
                  rep(colors[2], 1),
                  rep(colors[3], 5),
                  rep(colors[4], 2),
                  rep(colors[5], 2),
                  rep(colors[6], 4),
                  rep(colors[7], 1),
                  rep(colors[8], 2),
                  rep(colors[9], 1),
                  rep(colors[10], 4),
                  rep(colors[11], 1),
                  rep(colors[12], 1))

##### Function to Create Error Bars and Plot Resistance Rates #####

plot_resistance_with_error_bars <- function(dataset_name) {
  dataset <- get(dataset_name)
  dataset <- dataset %>% select(all_of(antibiotic_names))
  resistance_data <- data.frame(antibiotic = antibiotic_names)
  
  valid_count <- colSums(!is.na(dataset))
  
  resistance_count <- dataset %>%
    summarise_all(~ sum(. == "R", na.rm = TRUE))
  
  resistance_rate <- resistance_count / valid_count * 100
  resistance_rate <- t(resistance_rate)
  colnames(resistance_rate)[1] <- "percentage"
  
  resistance_sd <- sqrt(resistance_rate / 100 * (1 - resistance_rate / 100) / valid_count) * 100
  colnames(resistance_sd)[1] <- "std_error"
  
  resistance_data <- cbind(resistance_data, resistance_rate, resistance_sd)
  resistance_data$antibiotic <- factor(antibiotic_names, levels = antibiotic_names)
  
  # Plot
  ggplot(resistance_data, aes(x = antibiotic, y = percentage)) +
    geom_bar(stat = "identity", fill = color_vector, color = "black") +
    geom_errorbar(aes(ymin = resistance_rate - resistance_sd, ymax = resistance_rate + resistance_sd), width = 0.2, color = "black") +
    labs(title = "", x = "", y = "Resistance Rate (%)") +
    theme_minimal() +
    theme(axis.line = element_line(color = "black"), 
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "grey"),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2, size = 14, color = "black"),
          axis.ticks = element_line(),
          axis.text.y = element_text(size = 14, color = "black"),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(hjust = 0.5, vjust = 2, size = 20)) + 
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, by = 10))
}

##### Generate Plots for Each Dataset #####

# Create and save a plot for each year/phenotype subset
sapply(list_datasets, function(dataset) {
  figure <- plot_resistance_with_error_bars(paste0("EC_", dataset))
  ggsave(filename = paste0("plots/antibioresist_prev/plot_resistance_antibio_", dataset, ".png"), 
         plot = figure, width = 29.7, height = 10, units = "cm")
})

fig1 = ggarrange(plot_resistance_with_error_bars("EC_2022_BLSE"), 
          plot_resistance_with_error_bars("EC_2022_non_BLSE"),
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

pdf("fig1.pdf", width = 29.7/2.54, height = 21/2.54)
fig1
dev.off()

library(officer)
library(rvg)

# Créer un PPT au format A4 paysage
ppt <- read_pptx() %>%
  add_slide(layout = "Blank", master = "Office Theme") %>%
  ph_with(dml(code = print(fig1)), location = ph_location_fullsize())

# Sauvegarde du fichier
print(ppt, target = "fig1.pptx")


# Create and save combined plots for ESBL and non-ESBL results for each year
sapply(as.character(2018:2022), function(year) {
  figure <- ggarrange(plot_resistance_with_error_bars(paste0("EC_", year, "_BLSE")), 
                      plot_resistance_with_error_bars(paste0("EC_", year, "_non_BLSE")),
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2)
  ggsave(filename = paste0("plots/antibioresist_prev/plot_resistance_antibio_", year, ".png"), 
         plot = figure, width = 29.7, height = 21, units = "cm")
})

##### Descriptive Table for Figure 1 #####

create_resistance_table <- function(dataset) {
  results <- data.frame(Antibiotic = character(), Resistant_Count = integer(), 
                        Total_Count = integer(), Resistance_Percentage = numeric(), 
                        stringsAsFactors = FALSE)
  
  for (antibiotic in antibiotic_names) {
    if (antibiotic %in% names(dataset)) {
      resistant_count <- sum(dataset[[antibiotic]] == "R", na.rm = TRUE)
      susceptible_count <- sum(dataset[[antibiotic]] == "S", na.rm = TRUE)
      total_count <- resistant_count + susceptible_count
      resistance_percentage <- ifelse(total_count > 0, (resistant_count / total_count) * 100, NA)
      
      results <- rbind(results, data.frame(Antibiotic = antibiotic, 
                                           Resistant_Count = resistant_count, 
                                           Total_Count = total_count, 
                                           Resistance_Percentage = resistance_percentage))
    } else {
      warning(paste("The column", antibiotic, "does not exist in the dataset."))
    }
  }
  return(results)
}

# Save the descriptive tables to Excel files
write.xlsx(create_resistance_table(EC_2022_BLSE), 
           file = "plots/antibioresist_prev/prev_resist_2022_BLSE.xlsx")

write.xlsx(create_resistance_table(EC_2022_non_BLSE), 
           file = "plots/antibioresist_prev/prev_resist_2022_non_BLSE.xlsx")

##### Creating a Summary Table with Gender and Age #####
# 
# combined_data <- bind_rows(
#   mutate(EC_2018, dataset = "EC_2018"),
#   mutate(EC_2019, dataset = "EC_2019"),
#   mutate(EC_2020, dataset = "EC_2020"),
#   mutate(EC_2021, dataset = "EC_2021"),
#   mutate(EC_2022, dataset = "EC_2022")
# )
# 
# summary_table <- combined_data %>%
#   tbl_summary(include = c("age", "sexe"), by = dataset)
# 
# write.xlsx(summary_table$table_body, 
#            file = "results_all_data/summary_table_age_gender.xlsx")
# 
# ##### ESBL Proportion Over Time #####
# 
# # Count the number of ESBL and non-ESBL cases each year
# esbl_counts <- c(nrow(EC_2018_BLSE), nrow(EC_2019_BLSE), 
#                  nrow(EC_2020_BLSE), nrow(EC_2021_BLSE), nrow(EC_2022_BLSE))
# 
# non_esbl_counts <- c(nrow(EC_2018_non_BLSE), nrow(EC_2019_non_BLSE), 
#                      nrow(EC_2020_non_BLSE), nrow(EC_2021_non_BLSE), nrow(EC_2022_non_BLSE))
# 
# esbl_vs_non_esbl <- matrix(c(esbl_counts, non_esbl_counts), nrow = 2, byrow = TRUE)
# colnames(esbl_vs_non_esbl) <- c("EC_2018", "EC_2019", "EC_2020", "EC_2021", "EC_2022")
# rownames(esbl_vs_non_esbl) <- c("ESBL", "Non_ESBL")
# 
# # Mann-Kendall test for ESBL proportion over time
# esbl_proportion <- esbl_counts / (esbl_counts + non_esbl_counts)
# mk.test(esbl_proportion)
# 
# ##### Gender Proportion Over Time #####
# 
# # Count the number of females and males each year
# female_counts <- sapply(list(EC_2018, EC_2019, EC_2020, EC_2021, EC_2022), function(data) {
#   nrow(filter(data, sexe == "F"))
# })
# 
# male_counts <- sapply(list(EC_2018, EC_2019, EC_2020, EC_2021, EC_2022), function(data) {
#   nrow(filter(data, sexe == "H"))
# })
# 
# gender_proportion <- matrix(c(female_counts, male_counts), nrow = 2, byrow = TRUE)
# colnames(gender_proportion) <- c("EC_2018", "EC_2019", "EC_2020", "EC_2021", "EC_2022")
# rownames(gender_proportion) <- c("Female", "Male")
# 
# # Mann-Kendall test for gender proportion over time
# female_proportion <- female_counts / (female_counts + male_counts)
# mk.test(female_proportion)