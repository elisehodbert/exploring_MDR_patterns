dossier_enreg = "results_all_data/"
chemin_donnees = "data/"
folder_creation(dossier_enreg,1) # creating the folders needed to perform analyses

list_datasets = c(
  "2018",
  "2019",
  "2020",
  "2021",
  "2022"
)

source("scripts/3_simulation_bdd.R")

# Functions
suite_entiers <- function(vecteur){
  diff_vecteur <- diff(vecteur)
  return(all(diff_vecteur == 1))
}



# Define colors for plots
colors <- c("#FF5733", "#FF33C8", "#BA33F1", "#8338ec", "#0066ff")
faded_colors = addFade(colors, fade = 0.2)
colors[5] = "red"
faded_colors[5] = "grey"


# Boucle pour créer les résumés (df_resum) pour chaque jeu de données dans list_datasets
for (i in 1:length(list_datasets)) {
  # Infos dataset
  nom_df <- list_datasets[i]
  if (str_detect(nom_df, "non_BLSE")) {
    type_bact = "_non_BLSE"
  } else if (str_detect(nom_df, "BLSE")) {
    type_bact = "_BLSE"
  } else {
    type_bact = ""
  }
  
  annee = gsub("[^0-9]", "", nom_df)
  
  # Charger la liste des tableaux simulés correspondants
  nom_list_df <- paste0("list_simul_", annee, type_bact)
  chemin_fichier <- paste0(dossier_enreg, "H0_simulations/", nom_list_df, ".RData")
  load(chemin_fichier)
  list_simul <- get(nom_list_df)
  
  # Créer un dataframe vide avec 21 colonnes (de 0 à 20 résistances)
  resum <- data.frame(matrix(ncol = 21))
  colnames(resum) = 0:20
  
  # Parcourir chaque simulation de la liste
  for (k in 1:length(list_simul)) {
    df_simul = list_simul[[k]]
    
    # Ajouter une colonne avec le nombre de résistances (count_R) par ligne
    df_simul <- df_simul %>%
      mutate(count_R = rowSums(df_simul == "R", na.rm = TRUE))
    
    # Créer un tableau récapitulatif de la distribution des résistances
    table_R <- data.frame(t(as.data.frame(table(df_simul$count_R))))
    colnames(table_R) <- unlist(table_R[1,])
    table_R <- table_R[-1,]  # Supprimer la première ligne car elle contient les noms des colonnes
    row.names(table_R)[1] <- paste0("data_simul_", k)
    
    # Ajouter des colonnes manquantes pour les valeurs de 0 à 25 résistances
    existing_cols <- colnames(table_R)
    missing_cols <- setdiff(0:25, as.integer(existing_cols))
    new_columns <- data.frame(matrix(0, nrow = nrow(table_R), ncol = length(missing_cols)))
    colnames(new_columns) <- missing_cols
    
    # Combiner les nouvelles colonnes avec les colonnes existantes
    table_R <- cbind(table_R, new_columns)
    
    # Trier les colonnes par ordre croissant
    table_R <- table_R[, order(as.integer(colnames(table_R)))]
    
    # Convertir les colonnes en valeurs numériques
    table_R[] <- lapply(table_R, as.integer)
    
    # Convertir les counts en proportions
    table_R[1,] <- table_R / sum(table_R[1,])
    
    # Ajouter ce tableau résumé au dataframe 'resum'
    resum <- dplyr::bind_rows(table_R, resum)
  }
  
  # Supprimer la ligne supplémentaire ajoutée par défaut
  resum <- resum[-nrow(resum),]
  
  # Transposer le tableau 'resum' et ajouter les numéros de ligne
  resum_transpose <- as.data.frame(t(resum))
  resum_transpose$number = row.names(resum_transpose)
  
  # Convertir toutes les colonnes en valeurs numériques
  resum_transpose <- data.frame(apply(resum_transpose, 2, as.numeric))
  
  # Sauvegarder l'objet résumé et le supprimer de l'environnement pour libérer de la mémoire
  nom_resum <- paste0("resum_nb_resist_", annee, type_bact)
  assign(nom_resum, resum_transpose)
  
  # Supprimer les objets inutiles pour libérer de la mémoire
  rm(list_simul)
  rm(list = nom_list_df)
  
  # Sauvegarder le fichier résumé dans le répertoire approprié
  save(list = nom_resum, file = paste0(dossier_enreg, "indep_nb_resis/", nom_resum, ".RData"))
  gc()  # Collecte des ordures pour libérer de la mémoire
  
  # Afficher un message pour indiquer que le tableau résumé a été sauvegardé
  cat(paste(format(Sys.time()), "Tableau resum sauvegardé pour", list_datasets[i]), "\n")
}


# Affichage des plots

list_df_resum = paste0("resum_nb_resist_", list_datasets)

for (i in 1:length(list_df_resum)){
  nom_df <- list_df_resum[i]
  
  # Création du nom du plot
  if (str_detect(nom_df, "non_BLSE")) {
    type_bact = "_non_BLSE"
  } else if (str_detect(nom_df, "BLSE")) {
    type_bact = "_BLSE"
  } else {
type_bact = ""  
}
  
  annee = gsub("[^0-9]", "", nom_df)
  
  nom_plot <- paste0("plot_nb_resist_simul_obs_", annee, type_bact)
  
  # Importation de la liste de tableaux résum des tableaux simulés
  nom_list_df <- paste0("resum_nb_resist_", annee, type_bact)
  chemin_fichier <- paste0(dossier_enreg, "indep_nb_resis/", nom_list_df,".RData")
  load(chemin_fichier)
  df_resum <- get(nom_list_df)
  
  # melt des données simulées
  df_melted <- melt(df_resum, id.vars = "number")
  
  # plot
  plot <- ggplot(df_melted, aes(x = number, y = value, group = variable)) +
    geom_line(color="gray60", linewidth=0.2) +
    labs(title = paste0(annee, type_bact),
         x = "Number of resistances",
         y = "Number of isolates") +
    scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0,0.52)) +
    scale_x_continuous(expand = c(0,0), breaks = seq(0, 20,by=1)) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.line = element_line(colour = "black"),
          axis.ticks = element_line(), 
          axis.text.x = element_text(hjust = 1, size = 16, color = "black"),
          axis.text.y = element_text(size = 16, color = "black"),
          axis.title = element_text(size = 18),
          plot.title = element_text(hjust = 0.5, vjust = 2, size = 20)) # rotation du texte
  
  # Importation du tableau de données observées
  nom_df_obs = paste0("EC_", annee, type_bact)
  df_obs = get(nom_df_obs)
  
  # Ajout des données observées:
  df_obs_adap <- df_obs %>% select(all_of(antibiotic_names))
  resum <- data.frame(matrix(ncol=21))
  colnames(resum) = 0:20
  
  # Count R
  df_obs_adap <- df_obs_adap %>%
    mutate(count_R = rowSums(df_obs_adap == "R", na.rm = TRUE))
  
  # Création tableau résumé de la distribution
  table_R <- data.frame(t(as.data.frame(table(df_obs_adap$count_R))))
  colnames(table_R) <- unlist(table_R[1,])
  table_R <- table_R[-1,]
  row.names(table_R)[1] <- "data_obs_adap"
  
  # On ajoute les colonnes entre 0 et 25 qui n'existent pas encore
  existing_cols <- colnames(table_R)
  
  # Créez un vecteur de noms de colonnes manquantes
  missing_cols <- setdiff(0:25, as.integer(existing_cols))
  
  # Créez un dataframe avec les nouvelles colonnes remplies de zéros
  new_columns <- data.frame(matrix(0, nrow = nrow(table_R), ncol = length(missing_cols)))
  
  # Renommez les colonnes du nouveau dataframe avec les noms manquants
  colnames(new_columns) <- missing_cols
  
  # Utilisez la fonction cbind pour lier les nouveaux et anciens dataframes
  table_R <- cbind(table_R, new_columns)
  
  # colonnes triées dans l'ordre croissant
  table_R <- table_R[, order(as.integer(colnames(table_R)))]
  
  # Conversion de table R en integer
  table_R[] <- lapply(table_R, as.integer)
  
  # On transforme les count de table_R en proportion
  table_R[1,] <- table_R /sum(table_R[1,])
  
  # On assemble les table_R au sein de resum
  resum <- dplyr::bind_rows(table_R,resum)
  
  # Enlever la ligne en trop
  resum <- resum[-nrow(resum),]
  
  # Changement format
  resum_transpose <- as.data.frame(t(resum))
  resum_transpose$number = row.names(resum_transpose)
  resum_transpose <- data.frame(apply(resum_transpose,2,as.numeric))
  
  resum_transpose_obs <- resum_transpose
  colnames(resum_transpose_obs)[1] <- "data_obs"
  
  melted_obs = melt(resum_transpose_obs,id.vars = "number")
  
  # ajout des données observées sur le plot
  plot <- plot +
    geom_line(data = melted_obs, aes(x = number, y = value), color = "red")
  plot_nb_resist <- plot
  
  # Sauvegarde du plot
  ggsave(filename = paste0(dossier_enreg,"indep_nb_resis/plots_nb_resist_simul_obs/", nom_plot, ".png"),
         plot = plot_nb_resist, width = 30, height = 10, units = "cm")
  
  assign(nom_plot,plot_nb_resist)
  
  cat(paste(format(Sys.time()), "Graphe sauvegardé pour", list_df_resum[i]), "\n")
}


### Calcul des moyennes

list_df_resum = paste0("resum_nb_resist_", list_datasets)

### MOYENNES SIMULEES

# Préparation du tableau pour les moyennes
tab_mean_nb_resist_simul = setNames(as.data.frame(matrix(ncol = 100, nrow = 0)), paste0("data_simul_", 100:1))

for (i in 1:length(list_df_resum)){
  ### LOAD DES DONNEES
  
  # Load the summary data frame (données simulées)
  load(paste0(dossier_enreg, "indep_nb_resis/resum_nb_resist_", list_datasets[i], ".RData"))
  df_resum <- get(paste0("resum_nb_resist_", list_datasets[i]))
  df_resum <- df_resum[1:20,]  # Select the first 20 rows (others are always zero)
  
  ### MOYENNES SIMULEES
  
  # Melt the summary data frame and add to list
  melted_summary_df <- melt(df_resum, id.vars = "number")
  
  # Remplissage des moyennes simulées
  list_df = levels(melted_summary_df$variable)
  list_mean = map_dbl(list_df, function(variable) {
    list_df_truc = melted_summary_df %>%
      filter(variable == !!variable)
    sum(list_df_truc$number * list_df_truc$value)
  })
  tab_mean_nb_resist_simul <- rbind(tab_mean_nb_resist_simul, list_mean)
}

rownames(tab_mean_nb_resist_simul) = list_datasets
colnames(tab_mean_nb_resist_simul) = list_df

tab_mean_nb_resist_simul

### MOYENNES OBSERVEES

tab_mean_nb_resist_obs = setNames(as.data.frame(matrix(ncol = 1, nrow = 0)), c("data_obs"))
tab_R_sums_per_i = list()  # Liste pour stocker R_sums pour chaque i

for (i in 1:length(list_df_resum)) {
  # Charger le dataframe observé
  observed_df_name = paste0("EC_", list_datasets[i])
  df_obs = get(observed_df_name)
  
  # Calcul des sommes de "R" pour chaque ligne
  R_sums <- rowSums(df_obs[, 11:ncol(df_obs)] == "R", na.rm = TRUE)
  
  # Calcul de la moyenne des R_sums
  mean_R_sums <- mean(R_sums, na.rm=TRUE)
  
  # Ajouter la moyenne calculée à la table tab_mean_nb_resist_obs
  tab_mean_nb_resist_obs[i, "data_obs"] <- mean_R_sums
  
  # Stocker les R_sums pour chaque i dans une liste
  tab_R_sums_per_i[[i]] <- R_sums
}

rownames(tab_mean_nb_resist_obs) = list_datasets
colnames(tab_mean_nb_resist_obs) = "data_obs"
tab_mean_nb_resist_obs


### Ecart-type des moyennes simulées ####

IC_inf <- sapply(1:nrow(tab_mean_nb_resist_all), function(row){
  valeurs = as.numeric(tab_mean_nb_resist_all[row,3:102])
  moyenne <- mean(valeurs)
  ecart_type <- sd(valeurs)
  IC_inf <- moyenne - ecart-type
  return(IC_inf)
})

IC_sup <- sapply(1:nrow(tab_mean_nb_resist_all), function(row){
  valeurs = as.numeric(tab_mean_nb_resist_all[row,3:102])
  moyenne <- mean(valeurs)
  ecart_type <- sd(valeurs)
  IC_sup <- moyenne + ecart_type
  return(IC_sup)
})

tab_mean_nb_resist_all = tab_mean_nb_resist_all %>%
  mutate(IC_inf = IC_inf,
         IC_sup = IC_sup)


#### Autre manière de calculer la moyenne ###

melted_obs$value_integer <- round(melted_obs$value*nrow(EC_2022_non_BLSE))
values_for_median_obs = rep(melted_obs$number, melted_obs$value_integer)
median(values_for_median_obs)
mean(values_for_median_obs)
melted_summary_df_100 <- melted_summary_df %>% filter(variable == "data_simul_100")
melted_summary_df_100 <- melted_summary_df_100[1:11,]
melted_summary_df_100$value_integer <- round(melted_summary_df_100$value*nrow(EC_2022_non_BLSE))
values_for_median_simul_100 = rep(melted_summary_df_100$number, melted_summary_df_100$value_integer)
median(values_for_median_simul_100)
mean(values_for_median_simul_100)


### Test de kolmogorov-smirnov

for (i in 1:length(list_datasets)) {
  # Données simulées
  nom_resum <- paste0("resum_nb_resist_", list_datasets[i])
  load(paste0(dossier_enreg, "indep_nb_resis/", nom_resum, ".RData"))
  tab_resum <- get(nom_resum)
  mean_tab_resum = cbind(tab_resum$number, rowMeans(tab_resum[,!names(tab_resum) %in% "number"]))
  
  # Données observées (copier coller de au-dessus)
  nom_df_obs = paste0("EC_", list_datasets[i])
  df_obs = get(nom_df_obs)
  df_obs_adap <- df_obs %>% select(all_of(antibiotic_names))
  resum <- data.frame(matrix(ncol=21))
  colnames(resum) = 0:20
  df_obs_adap <- df_obs_adap %>%
    mutate(count_R = rowSums(df_obs_adap == "R", na.rm = TRUE))
  table_R <- data.frame(t(as.data.frame(table(df_obs_adap$count_R))))
  colnames(table_R) <- unlist(table_R[1,])
  table_R <- table_R[-1,]
  row.names(table_R)[1] <- "data_obs_adap"
  existing_cols <- colnames(table_R)
  missing_cols <- setdiff(0:25, as.integer(existing_cols))
  new_columns <- data.frame(matrix(0, nrow = nrow(table_R), ncol = length(missing_cols)))
  colnames(new_columns) <- missing_cols
  table_R <- cbind(table_R, new_columns)
  table_R <- table_R[, order(as.integer(colnames(table_R)))]
  table_R[] <- lapply(table_R, as.integer)
  table_R[1,] <- table_R /sum(table_R[1,])
  resum <- dplyr::bind_rows(table_R,resum)
  resum <- resum[-nrow(resum),]
  resum_transpose <- as.data.frame(t(resum))
  resum_transpose$number = row.names(resum_transpose)
  resum_transpose <- data.frame(apply(resum_transpose,2,as.numeric))
  resum_transpose_obs <- resum_transpose
  colnames(resum_transpose_obs)[1] <- "data_obs"
  
  # Test de kolmogorov-smirnov
  print(list_datasets[i])
  print(ks.test(mean_tab_resum[,2], resum_transpose_obs$data_obs))
}
