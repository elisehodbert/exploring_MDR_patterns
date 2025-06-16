##### 1. Analysis on all the data #####

dossier_enreg = "results_all_data/"
chemin_donnees = "data/"
folder_creation(dossier_enreg,1) # creating the folders needed to perform analyses

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

list_data_simul <- c( #Simulated data is very voluminous. We cut the lists of datasets into smaller lists so that the computer does not crash
  "2018_BLSE",
  "2019_BLSE",
  "2020_BLSE",
  "2021_BLSE",
  "2022_BLSE",
  "2018_non_BLSE_1_20",
  "2018_non_BLSE_21_40",
  "2018_non_BLSE_41_60",
  "2018_non_BLSE_61_80",
  "2018_non_BLSE_81_100",
  "2019_non_BLSE_1_20",
  "2019_non_BLSE_21_40",
  "2019_non_BLSE_41_60",
  "2019_non_BLSE_61_80",
  "2019_non_BLSE_81_100",
  "2020_non_BLSE_1_20",
  "2020_non_BLSE_21_40",
  "2020_non_BLSE_41_60",
  "2020_non_BLSE_61_80",
  "2020_non_BLSE_81_100",
  "2021_non_BLSE_1_15",
  "2021_non_BLSE_16_30",
  "2021_non_BLSE_31_45",
  "2021_non_BLSE_46_60",
  "2021_non_BLSE_61_75",
  "2021_non_BLSE_76_90",
  "2021_non_BLSE_91_100",
  "2022_non_BLSE_1_15",
  "2022_non_BLSE_16_30",
  "2022_non_BLSE_31_45",
  "2022_non_BLSE_46_60",
  "2022_non_BLSE_61_75",
  "2022_non_BLSE_76_90",
  "2022_non_BLSE_91_100"
)


# b. Simulating databases under H0 : antimicrobial resistances are independent
source("scripts/3_simulation_bdd.R")

# Pour la figure 2 : il faut aussi faire les simulations sur les jeux de données complets (pas fait pour l'instant)
#source("scripts/Figure2.R")

# c. Algorithme Apriori
source("scripts/4_apriori.R")

# Re-assembling lists of datasets that were split to save computer memory
concat_itemsets_non_BLSE(dossier_enreg, "2018_non_BLSE", c("1","20","40","60","80","100"))
concat_itemsets_non_BLSE(dossier_enreg, "2019_non_BLSE", c("1","20","40","60","80","100"))
concat_itemsets_non_BLSE(dossier_enreg, "2020_non_BLSE", c("1","20","40","60","80","100"))
concat_itemsets_non_BLSE(dossier_enreg, "2021_non_BLSE", c("1","15","30","45","60","75","90","100"))
concat_itemsets_non_BLSE(dossier_enreg, "2022_non_BLSE", c("1","15","30","45","60","75","90","100"))

# d. Filtering itemsets with eSup and cLift cut-off values
source("scripts/5_filtre_itemsets.R")
source("scripts/6_describ_itemsets.R") # describing itemsets

# e. Plotting the networks
source("scripts/7_plot_reseaux.R")
source("scripts/7_bis_legende_reseaux.R") # drawing the legend

list_graphs <- paste0("graph_",list_datasets,"_0.95")
plot_graphs_assemble(dossier_enreg, list_graphs, "graph_assemble_0.95", 5, 2) # drawing the figure with all networks


# testing number of patterns from 2018 to 2022
nb_patterns <- read_excel("results_all_data/tab_recap_itemsets_0.95.xlsx")
nb_obs_patterns_ESBL <- unlist(nb_patterns[1:5,2])
nb_obs_patterns_non_ESBL <- unlist(nb_patterns[6:10,2])
nb_pruned_patterns_ESBL <- unlist(nb_patterns[1:5,3])
nb_pruned_patterns_non_ESBL <- unlist(nb_patterns[6:10,3]) 

mk.test(nb_obs_patterns_ESBL)
mk.test(nb_obs_patterns_non_ESBL)
mk.test(nb_pruned_patterns_ESBL)
mk.test(nb_pruned_patterns_non_ESBL)

# nombre d'isolats de +65 years old

resultats_tidy <- map_dfr(2018:2022, ~ {
  df <- get(paste0("EC_", .x))
  
  # Comptages
  nb_fem_65p   <- df %>% filter(sexe == "H", age >= 65) %>% nrow()
  nb_fem_total <- df %>% filter(sexe == "H", !is.na(age)) %>% nrow()
  
  tibble(
    annee             = .x,
    nb_femmes_65_plus = nb_fem_65p,
    nb_femmes_total   = nb_fem_total,
    pct_femmes_65p    = if (nb_fem_total > 0) nb_fem_65p / nb_fem_total * 100 else NA_real_
  )
})

print(resultats_tidy)

pearson_test <- cor.test(
  x      = resultats_tidy$annee,
  y      = resultats_tidy$pct_femmes_65p,
  method = "pearson"
)
print(pearson_test)

# testing the  trends of eSup and cLift cut off values

df_cutoff = read.xlsx("save_results_all_data_with_AMP/summary_cut_off_values0.95.xlsx")
df_cutoff <- df_cutoff %>%
  mutate(
    year  = as.integer(substr(dataset, 1, 4)),
    group = gsub("^[0-9]{4}_", "", dataset)
  )

# 2) Boucle simple sur groupes et métriques
results <- list()
for (grp in unique(df_cutoff$group)) {
  df_g <- filter(df_cutoff, group == grp)
  for (metric in c("cut_off_eSup", "cut_off_cLift")) {
    # fit linéaire
    fit <- lm(as.formula(paste(metric, "~ year")), data = df_g)
    res <- residuals(fit)
    # tests d’hypothèses
    ok_norm <- shapiro.test(res)$p.value > 0.05
    ok_homo <- bptest(fit)$p.value      > 0.05
    # choix du test
    method <- if (ok_norm && ok_homo) "pearson" else "kendall"
    cor_t  <- cor.test(df_g$year, df_g[[metric]],
                       method = method,
                       alternative = "greater")
    mk_t   <- mk.test(df_g[[metric]], alternative = "greater")
    # stocker
    results[[paste(grp, metric, sep = "_")]] <-
      list(
        group         = grp,
        metric        = metric,
        method_used   = method,
        pearson_p     = cor_t$p.value,
        pearson_stat  = cor_t$estimate,
        mk_p          = mk_t$p.value,
        mk_stat       = mk_t$statistic
      )
  }
}

# 3) Convertir en data.frame pour résumé
library(tibble)
res_df <- bind_rows(results)
print(res_df)
