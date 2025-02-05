dataset_names = c("EC-2018","EC-2019","EC-2020","EC-2021","EC-2022")

##### SECTION 1: Data Import and Initial Setup #####

# Import datasets and clean column names
imported_datasets <- lapply(dataset_names, function(file_name) {
  data <- read_excel(paste0("data/", file_name, ".xlsx"), na = "", guess_max = Inf)
  colnames(data) <- gsub(" ", "_", colnames(data))
  return(data)
})

# Update dataset names to replace dashes with underscores
dataset_names <- gsub("-", "_", dataset_names)

# Reorder columns in each dataset to match the first dataset (e.g., 2018)
reordered_datasets <- lapply(imported_datasets, function(data) {
  return(data[colnames(imported_datasets[[1]])])
})


##### SECTION 2: Data Type Conversion #####

# Define the columns that need to be factors or integers
factor_columns <- c("type_prelevement", "sexe", "phenotype", "hebergement", "departement", "region", "espece")
antibiotic_columns <- colnames(reordered_datasets[[1]])[11:ncol(reordered_datasets[[1]])]
factor_columns <- c(factor_columns, antibiotic_columns)
integer_columns <- c("age")

# Convert column types in each dataset
convert_col_types <- function(dataframe, var_factor, var_integer) {
  # Verify these columns exist in the dataframe
  existing_var_factor <- var_factor[var_factor %in% colnames(dataframe)]
  existing_var_integer <- var_integer[var_integer %in% colnames(dataframe)]
  
  # Convert the types of the existing columns
  dataframe <- dataframe %>%
    mutate_at(vars(all_of(existing_var_factor)), as.factor) %>%
    mutate_at(vars(all_of(existing_var_integer)), as.integer)
  
  return(dataframe)
}

converted_datasets <- lapply(reordered_datasets, function(data) {
  convert_col_types(data, factor_columns, integer_columns)
})


##### SECTION 3: Data Recoding #####

# Recoding "I" to "R" in the antibiotic columns
recoded_datasets <- lapply(converted_datasets, function(data) {
  data %>% 
    mutate_at(vars(all_of(antibiotic_columns)), ~ as.character(recode(., "I" = "R"))) %>%
    mutate_at(vars(all_of(antibiotic_columns)), ~ factor(recode(., "I" = "R")))
})


##### SECTION 4: Cleaning Region Names #####

# Clean and standardize region names
cleaned_region_datasets <- lapply(recoded_datasets, function(data) {
  data$region <- gsub("-", "_", data$region)
  data$region <- gsub(" ", "_", data$region)
  data$region <- gsub("'", "_", data$region)
  data$region <- as.factor(data$region)
  return(data)
})


##### SECTION 5: Row Filtering #####

# Filter datasets for urine samples and metropolitan France regions only
filtres_lignes <- function(dataframe) {
  # Uniquement prélèvements urinaires
  dataframe <- dataframe %>%
    filter(type_prelevement == "URINES") %>%
    mutate(type_prelevement = droplevels(type_prelevement))
  # Uniquement France métropolitaine
  dataframe <- filter(dataframe, !(region %in% c("Polynésie_Française",
                                                 "Nouvelle_Calédonie",
                                                 "Guyane",
                                                 "Mayotte",
                                                 "La_Reunion",
                                                 "Martinique",
                                                 "Guadeloupe")))
  dataframe <- dataframe %>% mutate(region = droplevels(region))
  return(dataframe)
}

filtered_row_datasets <- lapply(cleaned_region_datasets, function(data) {
  filtres_lignes(data)
})


##### SECTION 6: Amoxicillin Data Adjustment #####

# Modify AMC variable by selecting the first available value
modif_AMC <- function(database) {
  result <- database %>%
    mutate(AMC_compil = ifelse(!is.na(AMC), AMC, AMC_urine)) %>%
    mutate(AMC_compil = as.factor(AMC_compil)) %>%
    mutate(AMC_compil = fct_recode(AMC_compil, "S" = "2", "R" = "1")) %>%
    select(-c(AMC, AMC_urine, AMC03)) %>%
    mutate(AMC = AMC_compil) %>%
    select(-AMC_compil) %>%
    relocate(AMC, .before = AMP)
  return(result)
}

amc_modified_datasets <- lapply(filtered_row_datasets, function(data) {
  modif_AMC(data)
})

##### Calcul des mean testing rates pour chaque antibiotique #####

# Create a function to summarize each dataset
summarize_antibiotics <- function(dataset) {
  antibiotic_columns = setdiff(antibiotic_columns, c("AMC_urine","AMC03", "FQ", "C3G")) # on enlève les colonnes qui ne sont pas des antibios

  # Initialize a dataframe to store the summary
  summary_df <- data.frame(
    Antibiotic = antibiotic_columns,
    Total_Tested = numeric(length(antibiotic_columns)),
    Mean_Testing_Rate = numeric(length(antibiotic_columns))
  )
  
  # Loop through each column to calculate summary statistics
  for (i in seq_along(antibiotic_columns)) {
    col_data <- dataset[,antibiotic_columns[[i]]]
    
    # Calculate total tested (non-NA values)
    total_tested <- sum(!is.na(col_data))
    
    # Calculate mean testing rate
    mean_testing_rate <- total_tested / nrow(dataset)
    
    # Store results
    summary_df[i, "Total_Tested"] <- total_tested
    summary_df[i, "Mean_Testing_Rate"] <- mean_testing_rate
  }
  
  return(summary_df)
}

# Apply the function to each dataset in the list and store results in a list
antibiotic_summaries <- lapply(amc_modified_datasets, summarize_antibiotics)

write.csv(antibiotic_summaries[[1]], file="antibiotics_selection/antibiotic_summaries_2018.csv")
write.csv(antibiotic_summaries[[2]], file="antibiotics_selection/antibiotic_summaries_2019.csv")
write.csv(antibiotic_summaries[[3]], file="antibiotics_selection/antibiotic_summaries_2020.csv")
write.csv(antibiotic_summaries[[4]], file="antibiotics_selection/antibiotic_summaries_2021.csv")
write.csv(antibiotic_summaries[[5]], file="antibiotics_selection/antibiotic_summaries_2022.csv")

# Combine summaries to calculate average testing rate across datasets
average_testing_rate <- Reduce(function(x, y) {
  merge(x, y, by = "Antibiotic", all = TRUE, suffixes = c("_x", "_y"))
}, antibiotic_summaries)

average_testing_rate$Mean_Testing_Rate <- rowMeans(
  average_testing_rate[, grep("Mean_Testing_Rate", colnames(average_testing_rate))],
  na.rm = TRUE
)

# Create a table with Antibiotic names and their average testing rate
final_summary <- average_testing_rate[, c("Antibiotic", "Mean_Testing_Rate")]
write.csv(final_summary, file = "antibiotics_selection/final_summary.csv")

##### SECTION 7: Column Filtering #####

# Columns to remove
columns_to_remove <- c("C3G", "FQ", "MEM", "PIP", "TMP", "TCC", "CN", "AZT", "CS", "TGC")

# Filter out unnecessary columns
filtres_colonnes <- function(dataframe, var_a_enlever) {
  if (any(var_a_enlever %in% colnames(dataframe))) {
    dataframe <- dataframe %>% select(-all_of(var_a_enlever))
  }
  return(dataframe)
}

filtered_column_datasets <- lapply(amc_modified_datasets, function(data) {
  filtres_colonnes(data, var_a_enlever = columns_to_remove)
})


##### SECTION 8: Phenotype-Based Filtering #####

# Loading the excel with the phenotypes
categ_pheno <- read_excel("data/categories_phenotypes_e_coli.xlsx")

# Defining ESBL and CPE phenotypes
blse_phenotypes <- categ_pheno %>% # à changer quand on en saura plus
  filter(BLSE == "OUI" & EPC == "NON") %>%
  pull(phenotype)

non_blse_phenotypes <- categ_pheno %>% # à changer quand on en saura plus
  filter(AUTRE == "OUI" & EPC == "NON" & BLSE == "NON") %>%
  pull(phenotype)
  
# Create separate datasets for ESBL and non-ESBL phenotypes
blse_datasets <- lapply(filtered_column_datasets, function(data) {
  data$phenotype = nettoyer_vecteur(data$phenotype)
  filter(data, phenotype %in% nettoyer_vecteur(blse_phenotypes))
}) 

non_blse_datasets <- lapply(filtered_column_datasets, function(data) {
  data$phenotype = nettoyer_vecteur(data$phenotype)
  filter(data, is.na(phenotype) | phenotype %in% nettoyer_vecteur(non_blse_phenotypes))
})



##### SECTION 9: Save Datasets #####

for (i in 1:length(dataset_names)){
  # Save BLSE datasets
  blse_name = paste0(dataset_names[i], "_BLSE")
  assign(blse_name, blse_datasets[[i]])
  save(list = blse_name, file = paste0("data/",blse_name, ".RData"))
  
  # Save non-BLSE datasets
  non_blse_name = paste0(dataset_names[i], "_non_BLSE")
  assign(non_blse_name, non_blse_datasets[[i]])
  save(list = non_blse_name, file = paste0("data/", non_blse_name, ".RData"))
  
  # Save combined datasets
  combined_name = dataset_names[i]
  assign(combined_name, rbind(blse_datasets[[i]], non_blse_datasets[[i]]))
  save(list = combined_name, file = paste0("data/",combined_name, ".RData"))
}

# Update the names of the antibiotic columns after filtering
antibiotic_columns <- colnames(EC_2018)[11:ncol(EC_2018)]

# Save the final cleaned data
save(EC_2018, EC_2019, EC_2020, EC_2021, EC_2022,
     EC_2018_BLSE, EC_2019_BLSE, EC_2020_BLSE, EC_2021_BLSE, EC_2022_BLSE,
     EC_2018_non_BLSE, EC_2019_non_BLSE, EC_2020_non_BLSE, EC_2021_non_BLSE, EC_2022_non_BLSE,
     file = "data/clean_data.RData")
