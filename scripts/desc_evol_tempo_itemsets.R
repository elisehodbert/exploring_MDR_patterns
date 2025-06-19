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




# Fonction CRS utilisant la fonction calc_set_overlap_ratio fournie
compute_CRS <- function(R_list) {
  n   <- length(R_list)
  CRS <- rep(NA_real_, n)  # on initialise un vecteur de length n avec NA
  
  if (n < 2) {
    warning("Il faut au moins deux ensembles de règles pour calculer CRS")
    return(CRS)
  }
  
  # i = 1
  CRS[1] <- calc_set_overlap_ratio(R_list[[1]], R_list[[2]])
  
  # i > 1 (jusqu'à n-1)
  for (i in 2:(n-1)) {
    overlap_i     <- calc_set_overlap_ratio(R_list[[i]], R_list[[i+1]])
    CRS[i]        <- ((i - 1) * CRS[i-1] + overlap_i) / i
  }
  
  # CRS[n] reste NA car pas de R_{n+1}
  return(CRS)
}

# Calcul du CRS sur toutes les années, séparément pour BLSE et non-BLSE

# on charge et on extrait tout en une passe
itemsets_all <- lapply(setNames(list_data, list_data), function(ld) {
  # chemin du fichier .RData
  f <- sprintf(
    "save_results_all_data_with_AMP/itemset_filtre_%s/itemset_filtre_%s_%s.RData",
    pvalue, pvalue, ld
  )
  load(f, envir = environment())  # charge dans l’environnement courant
  # reconstitue le nom de l’objet chargé
  obj <- get(sprintf("itemset_filtre_%s_%s", pvalue, ld))
  inspect(slot(obj, "items"))$items
})

# on scinde en deux listes
itemsets_list_BLSE     <- itemsets_all[1:5]
itemsets_list_non_BLSE <- itemsets_all[6:10]


CRS_BLSE = compute_CRS(itemsets_list_BLSE)
CRS_non_BLSE = compute_CRS(itemsets_list_non_BLSE)




for (i in 1:length(list_data)){
  load(paste0("save_results_all_data_with_AMP/itemset_filtre_", pvalue, "/itemset_filtre_", pvalue, "_", list_data[i], ".RData"))
}
itemsets_2018_BLSE <- inspect(slot(itemset_filtre_0.95_2018_BLSE,"items"))$items
itemsets_2019_BLSE <- inspect(slot(itemset_filtre_0.95_2019_BLSE,"items"))$items
itemsets_2020_BLSE <- inspect(slot(itemset_filtre_0.95_2020_BLSE,"items"))$items
itemsets_2021_BLSE <- inspect(slot(itemset_filtre_0.95_2021_BLSE,"items"))$items
itemsets_2022_BLSE <- inspect(slot(itemset_filtre_0.95_2022_BLSE,"items"))$items


itemsets_2018_non_BLSE <- inspect(slot(itemset_filtre_0.95_2018_non_BLSE,"items"))$items
itemsets_2019_non_BLSE <- inspect(slot(itemset_filtre_0.95_2019_non_BLSE,"items"))$items
itemsets_2020_non_BLSE <- inspect(slot(itemset_filtre_0.95_2020_non_BLSE,"items"))$items
itemsets_2021_non_BLSE <- inspect(slot(itemset_filtre_0.95_2021_non_BLSE,"items"))$items
itemsets_2022_non_BLSE <- inspect(slot(itemset_filtre_0.95_2022_non_BLSE,"items"))$items


itemsets_list_BLSE <- list(itemsets_2018_BLSE, itemsets_2019_BLSE, itemsets_2020_BLSE, itemsets_2021_BLSE, itemsets_2022_BLSE)
itemsets_list_non_BLSE <- list(itemsets_2018_non_BLSE, itemsets_2019_non_BLSE, itemsets_2020_non_BLSE, itemsets_2021_non_BLSE, itemsets_2022_non_BLSE)


# Calculer le CRS pour l'année 2021
CRS_BLSE <- calculate_CRS(itemsets_list_BLSE)
CRS_BLSE
CRS_non_BLSE <- calculate_CRS(itemsets_list_non_BLSE)
CRS_non_BLSE



# Afficher le résultat
print(paste("CRS BLSE:", CRS_BLSE))
print(paste("CRS non BLSE:", CRS_non_BLSE))
