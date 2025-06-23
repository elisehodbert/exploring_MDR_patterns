## Calcul du Rule Overlap Ratio (ROR) pour rep1–rep10

# 1. Paramètres
pvalue <- "0.95"
datasets <- c(
  "2018_BLSE", "2019_BLSE", "2020_BLSE", "2021_BLSE", "2022_BLSE",
  "2018_non_BLSE", "2019_non_BLSE", "2020_non_BLSE", "2021_non_BLSE", "2022_non_BLSE"
)
reps <- paste0("rep", 1:10)

# 2. Ratio d'intersection / union
calc_set_overlap_ratio <- function(A, B) {
  if (length(union(A, B)) == 0) return(NA_real_)
  length(intersect(A, B)) / length(union(A, B))
}

# 3. Fonction générique pour calculer ROR entre deux dossiers
compute_ROR <- function(path_m_prefix, path_w_prefix) {
  df <- expand.grid(
    dataset = datasets,
    rep = reps,
    stringsAsFactors = FALSE
  )
  df$overlap <- mapply(
    function(d, r) {
      # Chemin vers le dossier itemset_filtre_<pvalue>
      file_m <- file.path(
        path_m_prefix, r,
        paste0("itemset_filtre_", pvalue),
        sprintf("itemset_filtre_%s_%s.RData", pvalue, d)
      )
      file_w <- file.path(
        path_w_prefix, r,
        paste0("itemset_filtre_", pvalue),
        sprintf("itemset_filtre_%s_%s.RData", pvalue, d)
      )
      if (!file.exists(file_m) || !file.exists(file_w)) {
        # Avertissement si fichier manquant
        warning(sprintf("Fichier manquant pour %s/%s: %s ou %s", d, r, file_m, file_w))
        return(NA_real_)
      }
      # Chargement dans environnements séparés
      env_m <- new.env(); load(file_m, envir = env_m)
      env_w <- new.env(); load(file_w, envir = env_w)
      obj_name <- sprintf("itemset_filtre_%s_%s", pvalue, d)
      A <- env_m[[obj_name]]
      B <- env_w[[obj_name]]
      calc_set_overlap_ratio(A, B)
    },
    df$dataset, df$rep
  )
  df
}

# 4. ROR Hommes vs Femmes
res_gender <- compute_ROR(
  path_m_prefix = "results_by_gender/results_bs_men",
  path_w_prefix = "results_by_gender/results_sampled_women_size_men"
)

# 5. ROR +65 vs -65
res_age <- compute_ROR(
  path_m_prefix = "results_by_age_class/results_over_65",
  path_w_prefix = "results_by_age_class/results_bs_under_65"
)

# 6. Affichage des résultats
print("ROR Hommes vs Femmes:")
print(res_gender)
print("ROR +65 vs -65:")
print(res_age)

# 5. Résumé statistique (médiane et IC 95%)
summarize_ROR <- function(df) {
  vals <- df$overlap
  quantile(vals, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
}
summary_gender <- summarize_ROR(res_gender)
summary_age <- summarize_ROR(res_age)

# 6. Affichage des résultats
cat("Résumé ROR Hommes vs Femmes (2.5%, médiane, 97.5%):\n")
print(summary_gender)
cat("\nRésumé ROR +65 vs -65 (2.5%, médiane, 97.5%):\n")
print(summary_age)







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

# Calcul du CRS sur toutes les années, séparément pour BLSE et non-BLSE (sur 10 répétitions)

## Chargement et calcul CRS pour les années 2018–2022, p = 0.95, datasets (BLSE & non-BLSE)

# 1. Paramètres globaux
p <- "0.95"
datasets <- c("BLSE", "non_BLSE")  # Ajouter d'autres datasets si nécessaire
years <- 2018:2022
reps <- paste0("rep", 1:10)

# 2. Fonction utilitaire pour charger un itemset
load_itemset <- function(year, rep, p, dataset) {
  path <- if (year == 2018) {
    sprintf(
      "results_bs_2018/%s/itemset_filtre_%s/itemset_filtre_%s_%s_%s.RData",
      rep, p, p, year, dataset
    )
  } else {
    file.path(
      "results_samples_2018_size", rep,
      sprintf("itemset_filtre_%s", p),
      sprintf("itemset_filtre_%s_%s_%s.RData", p, year, dataset)
    )
  }
  if (!file.exists(path)) return(NULL)
  name_loaded <- load(path)
  obj <- get(name_loaded)
  inspect(slot(obj, "items"))$items
}

# 3. Chargement de tous les itemsets pour chaque dataset
itemsets_all <- setNames(
  lapply(datasets, function(dataset) {
    setNames(
      lapply(years, function(year) {
        setNames(
          lapply(reps, load_itemset, year = year, p = p, dataset = dataset),
          reps
        )
      }),
      as.character(years)
    )
  }),
  datasets
)

# 4. Calcul du CRS pour chaque dataset et chaque réplication
CRS_results <- setNames(
  lapply(datasets, function(dataset) {
    itemsets_dataset <- itemsets_all[[dataset]]
    setNames(
      lapply(reps, function(rep) {
        mini_list <- lapply(itemsets_dataset, `[[`, rep)
        compute_CRS(mini_list)
      }),
      reps
    )
  }),
  datasets
)

# 5. Statistiques (médiane et IC 95%) par dataset
CRS_summary <- lapply(CRS_results, function(CRS_by_rep) {
  vals <- sapply(CRS_by_rep, `[`, 4)
  quantile(vals, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
})

print(CRS_summary)


