##### Importing the packages #####

# Importation Excel files
library(openxlsx)
library(readxl)

# Dezscriptive statistics
library(gtsummary)

# Association set mining
library(arules)
library(arulesViz)

# Parallel computing
library(parallel)
library(foreach)
library(doParallel)

# Network graphs
library(igraph)

# Graphs
library(grid)
library(gridExtra)
library(cowplot)
library(png)
library(Cairo)
library(ggpubr)



# Additional colors
library(randomcoloR)
library(RColorBrewer)
library(berryFunctions)

# Maps
library(sp)
library(sf)

# Statistical tests
library(trend)

# Miscellaneous
library(tidyverse)
library(stringr)
library(reshape2)
library(plyr)
library(splitstackshape)

##### Loading functions #####
source("scripts/functions.R")



##### Loading data #####

AM_class_dataframe <- read_excel("data/AM_class_for_network_plot.xlsx") %>% # dataframe with informations about antimicrobial classes
  mutate(
    AM = as.factor(AM),
    Code = as.factor(Code),
    Code_eng = as.factor(Code_eng)
  )

antibiotic_names = as.character(AM_class_dataframe$AM) # vector containing all names of antimicrobials

load("data/colors.RData") # colors for plotting networks of resistance associations

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

# Parameters for dataset simulation (under the hypothesis of mutual independence H0)
n = 100 # number of datasets to be simulated 

# Parameters for apriori
minsup_BLSE = 0.01 # minimum support for ESBL isolates
minsup_non_BLSE = 0.001 # minimum support for non-ESBL isolates

# Parameters for pruning patterns
pvalue = "0.95"


##### Importing data #####

### Option 1: data formatting based on raw data
# source("scripts/1_data_prep.R")

### Option 2 : direct loading of data
load("data/clean_data.RData")
