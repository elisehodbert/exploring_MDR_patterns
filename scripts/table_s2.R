df_phenotypes = read.xlsx("data/categories_phenotypes_e_coli.xlsx")

phenotypes_blse = df_phenotypes$phenotype[
    df_phenotypes$BLSE == "OUI" & df_phenotypes$EPC == "NON"
  ]

# Recréer l’ensemble BLSE en minuscules et sans espaces superflus
blse_set <- tolower(trimws(unique(as.character(phenotypes_blse))))

# Définir les années et construire la liste de tables de fréquences
years <- 2018:2022
freq_list <- lapply(years, function(y) {
  df <- get(paste0("EC_", y))
  orig <- as.character(df$phenotype)
  lc   <- tolower(trimws(orig))
  table(orig[lc %in% blse_set])
})
names(freq_list) <- paste0("EC_", years)

# Afficher toutes les tables
for(year in names(freq_list)) {
  cat(year, ":\n")
  print(freq_list[[year]])
  cat("\n")
}




library(openxlsx)

blse <- tolower(trimws(phenotypes_blse))
years <- 2018:2022

tab <- Reduce(function(acc, y) {
  d <- get(paste0("EC_", y)); n <- nrow(d)
  p <- tolower(trimws(d$phenotype))
  t <- table(p[p %in% blse])
  df <- data.frame(
    phenotype = names(t),
    counts    = as.integer(t),
    pct       = as.numeric(t)/n,
    stringsAsFactors = FALSE
  )
  names(df)[2:3] <- paste0(c("n_", "pct_"), y)
  merge(acc, df, by = "phenotype", all = TRUE)
}, years, init = data.frame(phenotype = character(), stringsAsFactors = FALSE))

tab[is.na(tab)] <- 0
write.xlsx(tab, "freq_BLSE_EC2018_2022.xlsx", rowNames = FALSE)

