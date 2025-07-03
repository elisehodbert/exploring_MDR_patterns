# ===========================
# RULE OVERLAP RATIO (ROR) & CUMULATIVE RULE STABILITY (CRS)
# ===========================

# --- 1. Paramètres globaux ---
digits   <- 2
pvalue   <- "0.95"
years    <- 2018:2022
datasets <- paste0(rep(years, 2), c("_BLSE", "_non_BLSE"))
reps     <- paste0("rep", 1:10)

# --- 2. Fonction d'overlap entre ensembles ---
calc_set_overlap_ratio <- function(A, B) {
  if (length(union(A, B)) == 0) return(NA_real_)
  length(intersect(A, B)) / length(union(A, B))
}

# --- 3. Fonction principale de calcul du ROR ---
compute_ROR <- function(path_m, path_w) {
  expand_grid(dataset = datasets, rep = reps) %>%
    mutate(overlap = map2_dbl(dataset, rep, function(d, r) {
      get_file <- function(path) file.path(path, r, paste0("itemset_filtre_", pvalue),
                                           sprintf("itemset_filtre_%s_%s.RData", pvalue, d))
      file_m <- get_file(path_m)
      file_w <- get_file(path_w)
      
      if (!file.exists(file_m) || !file.exists(file_w)) return(NA_real_)
      
      env_m <- new.env(); load(file_m, envir = env_m)
      env_w <- new.env(); load(file_w, envir = env_w)
      obj   <- sprintf("itemset_filtre_%s_%s", pvalue, d)
      
      calc_set_overlap_ratio(env_m[[obj]], env_w[[obj]])
    }))
}

# --- 4. Calculs ROR par comparaison ---
res_gender <- compute_ROR(
  path_m = "results_by_gender/results_bs_men",
  path_w = "results_by_gender/results_sampled_women_size_men"
)

res_age <- compute_ROR(
  path_m = "results_by_age_class/results_over_65",
  path_w = "results_by_age_class/results_bs_under_65"
)

# --- 5. Fonction d'affichage formaté ---
format_cell <- function(q) {
  sprintf(paste0("%.", digits, "f [%.", digits, "f–%.", digits, "f]"), q[2], q[1], q[3])
}

# --- 6. Résumé par suffixe ---
summarize_key <- function(df, suffix) {
  sapply(years, function(y) {
    key  <- paste0(y, "_", suffix)
    vals <- df$overlap[df$dataset == key]
    q    <- quantile(vals, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    format_cell(q)
  })
}

# --- 7. Création tableau résumé ---
tbl <- rbind(
  `ROR hommes/femmes BLSE`     = summarize_key(res_gender, "BLSE"),
  `ROR hommes/femmes non BLSE` = summarize_key(res_gender, "non_BLSE"),
  `ROR -65/65+ BLSE`           = summarize_key(res_age,    "BLSE"),
  `ROR -65/65+ non BLSE`       = summarize_key(res_age,    "non_BLSE")
)
colnames(tbl) <- as.character(years)
print(tbl)

# --- 8. Visualisation barplot ---
tbl_long <- as.data.frame(tbl) %>%
  rownames_to_column("Metric") %>%
  pivot_longer(cols = all_of(as.character(years)), names_to = "Year", values_to = "CI_str") %>%
  separate(CI_str, into = c("Estimate", "CI_range"), sep = " ", extra = "merge") %>%
  mutate(
    Estimate = as.numeric(Estimate),
    CI_range = str_remove_all(CI_range, "\\[|\\]"),
    Lower    = as.numeric(str_extract(CI_range, "^[0-9.]+")),
    Upper    = as.numeric(str_extract(CI_range, "(?<=–)[0-9.]+"))
  )

custom_labels <- c(
  "ROR hommes/femmes BLSE"     = "ROR by gender (ESBL-EC)",
  "ROR hommes/femmes non BLSE" = "ROR by gender (non-ESBL-EC)",
  "ROR -65/65+ BLSE"           = "ROR by age class (ESBL-EC)",
  "ROR -65/65+ non BLSE"       = "ROR by age class (non-ESBL-EC)"
)

ggplot(tbl_long, aes(x = Year, y = Estimate)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  facet_wrap(~ Metric, ncol = 2, labeller = labeller(Metric = custom_labels)) +
  labs(x = "Year", y = "Rule Overlap Ratio (ROR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text  = element_text(size = 11, face = "bold"))

# ========================
# CALCUL CRS
# ========================

# --- Fonctions utilitaires ---
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

compute_CRS <- function(R_list) {
  n <- length(R_list)
  CRS <- rep(NA_real_, n)
  if (n < 2) return(CRS)
  
  CRS[1] <- calc_set_overlap_ratio(R_list[[1]], R_list[[2]])
  for (i in 2:(n - 1)) {
    o <- calc_set_overlap_ratio(R_list[[i]], R_list[[i + 1]])
    CRS[i] <- ((i - 1) * CRS[i - 1] + o) / i
  }
  CRS
}

# --- Chargement des itemsets ---
itemsets_all <- setNames(
  lapply(datasets, function(dataset) {
    setNames(
      lapply(years, function(year) {
        setNames(
          lapply(reps, load_itemset, year = year, p = pvalue, dataset = dataset),
          reps
        )
      }),
      as.character(years)
    )
  }),
  datasets
)

# --- Calcul CRS pour chaque dataset/répétition ---
CRS_results <- map(itemsets_all, function(itemsets_dataset) {
  map(setNames(reps, reps), function(rep) {
    rules_by_year <- map(itemsets_dataset, `[[`, rep)
    compute_CRS(rules_by_year)
  })
})

# --- Résumé du CRS (médiane + IC 95%) pour l’année 2021 (4e index, soit i = 4) ---
CRS_summary <- map(CRS_results, ~ {
  vals <- map_dbl(.x, ~ .x[4])
  quantile(vals, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
})

print(CRS_summary)
