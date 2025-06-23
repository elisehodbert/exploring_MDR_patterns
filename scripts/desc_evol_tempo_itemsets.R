## Calcul du Rule Overlap Ratio (ROR) pour rep1–rep10 et mise en forme tableau final

# 1. Paramètres
digits <- 2
pvalue <- "0.95"
years <- 2018:2022
datasets <- paste0(rep(years, 2), c("_BLSE", "_non_BLSE"))
reps <- paste0("rep", 1:10)

# 2. Ratio d'intersection / union
calc_set_overlap_ratio <- function(A, B) {
  if (length(union(A, B)) == 0) return(NA_real_)
  length(intersect(A, B)) / length(union(A, B))
}

# 3. Fonction générique pour calculer ROR
compute_ROR <- function(path_m, path_w) {
  df <- expand.grid(
    dataset = datasets,
    rep = reps,
    stringsAsFactors = FALSE
  )
  df$overlap <- mapply(
    function(d, r) {
      file_m <- file.path(path_m, r,
                          paste0("itemset_filtre_", pvalue),
                          sprintf("itemset_filtre_%s_%s.RData", pvalue, d))
      file_w <- file.path(path_w, r,
                          paste0("itemset_filtre_", pvalue),
                          sprintf("itemset_filtre_%s_%s.RData", pvalue, d))
      if (!file.exists(file_m) || !file.exists(file_w)) return(NA_real_)
      env_m <- new.env(); load(file_m, envir = env_m)
      env_w <- new.env(); load(file_w, envir = env_w)
      obj <- sprintf("itemset_filtre_%s_%s", pvalue, d)
      calc_set_overlap_ratio(env_m[[obj]], env_w[[obj]])
    },
    df$dataset, df$rep
  )
  df
}

# 4. Calculs pour chaque comparaison
res_gender <- compute_ROR(
  path_m = "results_by_gender/results_bs_men",
  path_w = "results_by_gender/results_sampled_women_size_men"
)
res_age    <- compute_ROR(
  path_m = "results_by_age_class/results_over_65",
  path_w = "results_by_age_class/results_bs_under_65"
)

# 5. Formatage des cellules
format_cell <- function(q) sprintf(paste0("%.", digits, "f [%.", digits, "f–%.", digits, "f]"),
                                   q[2], q[1], q[3])

# 6. Résumé par clé (BLSE / non_BLSE)
summarize_key <- function(df, suffix) {
  sapply(years, function(y) {
    key <- paste0(y, "_", suffix)
    vals <- df$overlap[df$dataset == key]
    q <- quantile(vals, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    format_cell(q)
  })
}

# 7. Construction du tableau final
tbl <- rbind(
  `ROR hommes/femmes BLSE`     = summarize_key(res_gender,    "BLSE"),
  `ROR hommes/femmes non BLSE` = summarize_key(res_gender,    "non_BLSE"),
  `ROR -65/65+ BLSE`           = summarize_key(res_age,       "BLSE"),
  `ROR -65/65+ non BLSE`       = summarize_key(res_age,       "non_BLSE")
)
colnames(tbl) <- as.character(years)

# 8. Affichage
print(tbl)


# Barplot pour mieux visualiser les résultats
# 0. Conversion de la matrice en tibble avec une colonne 'Metric'
tbl_df <- as.data.frame(tbl, stringsAsFactors = FALSE) %>% 
  rownames_to_column(var = "Metric")

# 1. Passage en format “long”
tbl_long <- tbl_df %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),   # toutes les colonnes années (2018–2022)
    names_to  = "Year",
    values_to = "CI_str"
  ) %>%
  separate(CI_str, into = c("Estimate", "CI_range"), sep = " ", extra = "merge") %>%
  mutate(
    Estimate = as.numeric(Estimate),
    CI_range = str_remove_all(CI_range, "\\[|\\]"),
    Lower    = as.numeric(str_extract(CI_range, "^[0-9\\.]+")),
    Upper    = as.numeric(str_extract(CI_range, "(?<=–)[0-9\\.]+"))
  )

custom_labels <- c(
  "ROR hommes/femmes BLSE"       = "ROR by gender (ESBL-EC)",
  "ROR hommes/femmes non BLSE"   = "ROR by gender (non-ESBL-EC)",
  "ROR -65/65+ BLSE"             = "ROR by age class (ESBL-EC)",
  "ROR -65/65+ non BLSE"         = "ROR by age class (non-ESBL-EC) "
)

# graphe
ggplot(tbl_long, aes(x = Year, y = Estimate)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  facet_wrap(~ Metric, ncol = 2, scales = "fixed",     labeller = labeller(Metric = custom_labels)
) +
  labs(
    x     = "Year",
    y     = "Rule Overlap Ratio (ROR)",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text  = element_text(size = 11, face = "bold")
  )


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


