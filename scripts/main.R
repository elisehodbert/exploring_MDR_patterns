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

antibiotic_names = as.character(AM_class_dataframe$AM) # vector containing all names of antimicrobials

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
minsup_BLSE = 0.01 # minimum support for ESBL isolates
minsup_non_BLSE = 0.001 # minimum support for non-ESBL isolates

# Parameters for pruning patterns
pvalue = "0.95"



##### Importing data #####

### Option 1 : on met en forme les données à partir des données brutes
# source("scripts/1_data_prep.R")

### Option 2 : on load directement les données
load("data/clean_data.RData")
