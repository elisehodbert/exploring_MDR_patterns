##### Importing the packages #####

# Importation de fichiers excel
library(openxlsx)
library(readxl)

library(gtsummary) # Tableaux descriptifs des données

# Association rule mining
library(arules)
library(arulesViz)

# Calcul en parallèle
library(parallel)
library(foreach)
library(doParallel)

# Graphes réseaux
library(igraph)

# Graphiques
library(grid)
library(gridExtra)
library(cowplot)
library(png)
library(Cairo)
library(ggpubr)

library(trend) # mann-kendall test

# Couleurs
library(randomcoloR)
library(RColorBrewer)
library(berryFunctions) # pour faire les couleurs faded du graphe nb resist obs simul

# Cartes du nombre d'isolats par région
library(sp)
library(sf)

# Divers
library(tidyverse)
library(stringr)
library(reshape2)
library(plyr)
library(splitstackshape)

#Powerpoint
library(officer)
library(rvg)

##### Chargement des fonctions utiles #####
source("scripts/functions.R")



##### Loading data #####

# Charging the dataframe with informations about antimicrobial classes

AM_class_dataframe <- read_excel("data/AM_class_for_network_plot.xlsx") %>%
  mutate(
    AM = as.factor(AM),
    Code = as.factor(Code),
    Code_eng = as.factor(Code_eng)
  )
# AM_class_dataframe <- AM_class_dataframe %>%
#   filter(AM != "AMP")

antibiotic_names = as.character(AM_class_dataframe$AM) # vector containing all names of antimicrobials
antibiotic_names <- antibiotic_names[antibiotic_names != "AMP"] # when we don't wnat to include AMP

load("data/colors.RData") # Charging colors for plotting networks of resistance associations



##### Defining parameters #####

regions = c("Auvergne_Rhone_Alpes", # defining regions we include in the study. Here, all metropolitan France
            "Bourgogne_Franche_Comte",
            "Bretagne",
            "Centre_Val_de_Loire",
            "Grand_Est",
            "Hauts_de_France",
            "Ile_de_France",
            "Normandie",
            "Nouvelle_Aquitaine",
            "Occitanie",
            "Pays_de_la_Loire",
            "Provence_Alpes_Cote_d_Azur")

# Parameters for 3_simul_datasets_H0
n = 100 # number of datasets to be simulated under H0

# Parameters for apriori
#minsup_BLSE = 0.01 # minimum support for ESBL isolates
minsup_BLSE = 0.001 # minimum support for ESBL isolates
minsup_non_BLSE = 0.001 # minimum support for non-ESBL isolates

# Parameters for pruning patterns
pvalue = "0.95"



##### Importing data #####

### Option 1 : on met en forme les données à partir des données brutes
# source("scripts/1_data_prep.R")

### Option 2 : on load directement les données
load("data/clean_data.RData") # sans AMP
load("data/clean_data_with_AMP.RData") # avec AMP




# Comptage des ages:
counts <- map_dfr(2018:2022, ~{
  df <- get(paste0("EC_", .x))
  n_under <- sum(df$age < 65, na.rm = TRUE)
  n_over  <- sum(df$age >= 65, na.rm = TRUE)
  total   <- n_under + n_over
  tibble(
    year          = .x,
    under_65      = n_under,
    under_65_pct  = round(100 * n_under / total, 1),
    `65_and_over`   = n_over,
    `65_and_over_pct` = round(100 * n_over / total, 1)
  )
})
counts


# Taux de test chez les hommes et chez les femmes
taux_test <- map_dfr(2018:2022, function(y) {
  df <- get(paste0("EC_", y))
  df %>%
    dplyr::filter(sexe %in% c("H", "F")) %>%
    pivot_longer(
      cols      = all_of(antibiotic_names),
      names_to  = "antibio",
      values_to = "valeur"
    ) %>%
    dplyr::mutate(
      tested = !is.na(valeur),
      year   = y
    ) %>%
    dplyr::group_by(year, sexe, antibio) %>%
    dplyr::summarise(pct = 100 * mean(tested), .groups = "drop")
}) %>%
  pivot_wider(names_from = sexe, values_from = pct)
taux_test

# Test stat pour savoir si la proportion est la même
test_prop <- function(y, ab) {
  df <- get(paste0("EC_", y)) %>% filter(sexe %in% c("H", "F"))
  # Nombre de H testés / non testés sur ab
  nH_test    <- sum(!is.na(df[[ab]]   & df$sexe == "H"), na.rm = TRUE)
  nH_total   <- sum(df$sexe == "H", na.rm = TRUE)
  nF_test    <- sum(!is.na(df[[ab]]   & df$sexe == "F"), na.rm = TRUE)
  nF_total   <- sum(df$sexe == "F", na.rm = TRUE)
  # prop.test
  res <- prop.test(
    x = c(nH_test, nF_test),
    n = c(nH_total, nF_total),
    correct = FALSE
  )
  tibble(
    year    = y,
    antibio = ab,
    test    = "prop.test",
    p_value = res$p.value
  )
}

test_stats <- map_dfr(taux_test$year %>% unique(), function(y) {
  map_dfr(unique(taux_test$antibio), ~ test_prop(y, .x))
})

