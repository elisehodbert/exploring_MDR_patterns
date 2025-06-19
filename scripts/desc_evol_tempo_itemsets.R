list_data = c("2018_BLSE",
              "2019_BLSE",
              "2020_BLSE",
              "2021_BLSE",
              "2022_BLSE",
              "2018_non_BLSE",
              "2019_non_BLSE",
              "2020_non_BLSE",
              "2021_non_BLSE",
              "2022_non_BLSE")

# Calcul du Rule Overlap Ratio
calc_set_overlap_ratio <- function(A, B) {
  length(intersect(A, B)) / length(union(A, B))
}

# ROR entre hommes et femmes pour chaque année
res <- numeric(length(list_data))
for (i in seq_along(list_data)) {
  d <- list_data[i]
  load(paste0("results_by_gender/results_bs_men/rep1/itemset_filtre_0.95/itemset_filtre_",
              pvalue, "_", d, ".RData"), envir = m <- new.env())
  load(paste0("results_by_gender/results_sampled_women_size_men/rep1/itemset_filtre_0.95/itemset_filtre_",
              pvalue, "_", d, ".RData"), envir = w <- new.env())
  obj <- paste0("itemset_filtre_0.95_", d)
  res[i] <- calc_set_overlap_ratio(m[[obj]], w[[obj]])
}
data.frame(dataset = list_data, overlap = res)

# ROR entre -65 et +65 pour chaque année
res <- numeric(length(list_data))
for (i in seq_along(list_data)) {
  d <- list_data[i]
  load(paste0("results_by_age_class/results_over_65/rep1/itemset_filtre_0.95/itemset_filtre_",
              pvalue, "_", d, ".RData"), envir = m <- new.env())
  load(paste0("results_by_age_class/results_bs_under_65/rep1/itemset_filtre_0.95/itemset_filtre_",
              pvalue, "_", d, ".RData"), envir = w <- new.env())
  obj <- paste0("itemset_filtre_0.95_", d)
  res[i] <- calc_set_overlap_ratio(m[[obj]], w[[obj]])
}
data.frame(dataset = list_data, overlap = res)





# Fonction pour calculer la Cumulative Rule Stability (CRS)
calculate_crs <- function(rulesets) {
  num_rulesets <- length(rulesets)
  cumulative_stability <- numeric(num_rulesets - 1)
  
  for (i in 1:(num_rulesets - 1)) {
    ruleset_A <- rulesets[i]
    ruleset_B <- rulesets[i + 1]
    
    shared_rules <- intersect(ruleset_A, ruleset_B)
    
    if (length(shared_rules) > 0) {
      # Calcul de la stabilité cumulative pour les règles partagées
      stability_values <- sapply(shared_rules, function(rule) {
        set_overlap_ratio <- calc_set_overlap_ratio(rule$lhs, subset(ruleset_B, subset = lhs %in% rule$lhs)$lhs)
        return(set_overlap_ratio)
      })
      
      # Moyenne des valeurs de ROR pour les règles partagées
      cumulative_stability[i] <- mean(stability_values)
    }
  }
  
  return(cumulative_stability)
}



calculate_CRS <- function(itemsets_list) {
  # Vérifier que la liste d'itemsets est non vide
  if (length(itemsets_list) < 2) {
    stop("La liste d'itemsets doit contenir au moins deux ensembles.")
  }
  
  # Initialiser le CRS pour la première année
  CRS <- rule_overlap(itemsets_list[[1]], itemsets_list[[2]])
  
  # Calculer le CRS pour les années suivantes
  for (i in 2:(length(itemsets_list) - 1)) {
    overlap <- rule_overlap(itemsets_list[[i]], itemsets_list[[i + 1]])
    CRS <- (1/i) * ((i-1) * CRS + overlap)
  }
  
  return(CRS)
}



# Exemple d'utilisation avec des itemsets pour les années 2018 à 2021
itemsets_2018_BLSE <- inspect(slot(itemset_filtre_0.95_2018_BLSE,"items"))$items
itemsets_2019_BLSE <- inspect(slot(itemset_filtre_0.95_2019_BLSE,"items"))$items
itemsets_2020_BLSE <- inspect(slot(itemset_filtre_0.95_2020_BLSE,"items"))$items
itemsets_2021_BLSE <- inspect(slot(itemset_filtre_0.95_2021_BLSE,"items"))$items

itemsets_2018_non_BLSE <- inspect(slot(itemset_filtre_0.95_2018_non_BLSE,"items"))$items
itemsets_2019_non_BLSE <- inspect(slot(itemset_filtre_0.95_2019_non_BLSE,"items"))$items
itemsets_2020_non_BLSE <- inspect(slot(itemset_filtre_0.95_2020_non_BLSE,"items"))$items
itemsets_2021_non_BLSE <- inspect(slot(itemset_filtre_0.95_2021_non_BLSE,"items"))$items

itemsets_list_BLSE <- list(itemsets_2018_BLSE, itemsets_2019_BLSE, itemsets_2020_BLSE, itemsets_2021_BLSE)
itemsets_list_non_BLSE <- list(itemsets_2018_non_BLSE, itemsets_2019_non_BLSE, itemsets_2020_non_BLSE, itemsets_2021_non_BLSE)


# Calculer le CRS pour l'année 2021
CRS_BLSE <- calculate_CRS(itemsets_list_BLSE)
CRS_BLSE
CRS_non_BLSE <- calculate_CRS(itemsets_list_non_BLSE)
CRS_non_BLSE



# Afficher le résultat
print(paste("CRS BLSE:", CRS_BLSE))
print(paste("CRS non BLSE:", CRS_non_BLSE))
