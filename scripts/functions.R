### Function to prepare the data (1_data_prep) ###

nettoyer_vecteur <- function(vecteur) {
  # Appliquer les transformations à chaque élément du vecteur
  vecteur_transforme <- sapply(vecteur, function(x) {
    x <- tolower(x)              # Convertir en minuscules
    x <- gsub("[- ]", "", x)     # Enlever les espaces et les tirets
    return(x)
  })
  vecteur_factor <- as.factor(vecteur_transforme)
  
  # Retourner le vecteur transformé
  return(vecteur_factor)
}


### Functions to create the necessary folders

create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

folder_creation <- function(folder, nb_times) {
  subfolders <- c(
    "/H0_simulations",
    "/itemset_filtre_0.95",
    "/itemset_obs",
    "/itemsets_simul",
    "/most_fq_patterns",
    "/plots_reseaux",
    "/reseaux"
  )
  
  if (folder %in% c("results_all_data/")) { #case when we work on all data
    create_dir(folder)
    for (subfolder in subfolders) {
      create_dir(paste0(folder, subfolder))
    }
  } else {
    subfolders <- c(subfolders, "/sampled_datasets") # when we do not work on all the data, we add a folder with sampled datasets
    if(nb_times >1){
      for (essai in 1:nb_times) { # if we sample multiple times, we create multiple folders named "repX"
        subfolder_path <- paste0(folder, "/rep", essai)
        create_dir(subfolder_path)
        for (subfolder in subfolders) {
          create_dir(paste0(subfolder_path, subfolder))
        }
      }
    } else { # else we create only one folder
      create_dir(folder)
      for (subfolder in subfolders) {
        create_dir(paste0(folder, subfolder))
      }    }
  }
  cat("Folders created", "\n")
}

##### Functions to simulate datasets under H0 : "resistances are independent" #####

simul_bdd <- function(database, antibiotic_names){   # Simulating a database
  database <- database %>% select(all_of(antibiotic_names))
  proportions <- colMeans(database == "R", na.rm = TRUE)   # proprotion of resistant isolates
  
  simulated_data <- data.frame(matrix(ncol = ncol(database), nrow = nrow(database))) # creation of an empty dataset
  simulated_data[] <- NA # Placing NAs in the same places than the observed dataset
  simulated_data[!is.na(database)] <- 0 # Placing temporary zeros in the other places
  colnames(simulated_data) = colnames(database)
  
  simulated_data <- sapply(1:ncol(simulated_data), function(col) { # simulating non NA data with a binomial law
    ifelse(is.na(simulated_data[, col]), NA, rbinom(n = nrow(simulated_data), size = 1, prob = proportions[col]))
  })
  simulated_data <- data.frame(simulated_data)
  colnames(simulated_data) <- colnames(database)
  
  simulated_data <- replace(simulated_data, simulated_data == 1, "R") # R corresponds to 1
  simulated_data <- replace(simulated_data, simulated_data == 0, "S") # S corresponds to 0
  
  return(simulated_data)
} 

list_simul_bdd <- function(database, antibiotic_names, n){ # Generating n simulated datasets
  lapply(1:n,function(x) {
    return(simul_bdd(database, antibiotic_names))})
}

verif_prop <- function(database,list_simul){ # checking that proportions in the simulated datasets match the observed dataset
  database <- database %>% select(all_of(antibiotic_names))
  
  prop_db = colMeans(database == "R", na.rm = TRUE)
  
  prop_list_simul = sapply(list_simul, function(db_simul){
    prop_simul = colMeans(db_simul == "R", na.rm = TRUE)
    return(round(prop_simul,digits=2) == round(prop_db,digits=1))
  })
  
  prop_list_simul
}

verif_NA <- function(database,list_simul){ # checking that NAs are in the same places in the observed and simulated datasets
  database <- database %>% select(all_of(antibiotic_names))
  
  comp_db = complete.cases(database)
  
  comp_list_simul = sapply(list_simul,function(db_simul){
    comp_simul = complete.cases(db_simul)
    return(sum(comp_simul == comp_db)/nrow(db_simul))
  })
  comp_list_simul
}



##### Functions to perform association rule mining #####

ajout_QM <- function(database_bin, mat_transac, mat_transac_opt, patterns){ # calculating eSup and cLift (adapted from Cazer 2021)
  itemset_list <- LIST(items(patterns), decode = FALSE)
  
  if(any(dim(mat_transac) != dim(mat_transac_opt))){ #transaction and optimistic transaction databases must have same columns
    warning("Different dimensions of mat_transac and optimistic mat_transac. STOP")
  }
  
  if(length(patterns) != length(itemset_list)){ #itemset_list must correspond to the sets
    warning("Different number of sets in items and itemset_list. STOP")
  }
  
  # 1. Calculating eSup
  
  #calculate pSup, oSup, eSup for each itemset
  pSup <- interestMeasure(patterns, "support", mat_transac, reuse = TRUE)
  oSup <- interestMeasure(patterns, "support", mat_transac_opt, reuse = FALSE)
  eSup <- pSup/(1- (oSup - pSup))
  
  #check that expected conditions are met
  if (all((pSup - eSup) < 1e-10) == FALSE){ #pSup should be <= eSup although a rounding error due to division in calculating eSup may result in small positive value
    warning("Expected Support is less than Pessimistic Support.")
  }
  
  if (all((eSup - oSup) < 1e-10) == FALSE){ #eSup should be <= oSup although a rounding error due to division in calculating eSup may result in small positive value
    warning("Optimistic Support is less than Expected Support.")
  }
  
  #calculate pSup, oSup, eSup for each individual AM
  pItemSupport <- itemFrequency(mat_transac)
  oItemSupport <- itemFrequency(mat_transac_opt)
  eItemSupport <- pItemSupport/(1 - (oItemSupport - pItemSupport))
  
  # 2. Calculating cLift
  
  # Calculating numerator
  AMsinPattern <- lapply(itemset_list, function(i) itemLabels(mat_transac)[i]) #for each itemset, a character vector of the AMs in the set
  NbCompRowsforAMsinPattern <- sapply(AMsinPattern, function(AMs){
    comp_rows <- complete.cases(database_bin[, AMs])
    return(sum(comp_rows))
  })
  
  numerateur = patterns@quality$support * length(mat_transac) / NbCompRowsforAMsinPattern
  
  # Calculating denominator
  denominateur <- sapply(AMsinPattern, function(AMs){
    comp_rows <- complete.cases(database_bin[, AMs])
    sup_AMs_in_comp_rows = prod(sapply(database_bin[comp_rows, AMs], function(x) mean(x, na.rm = TRUE))) # Counting the product of supports of AMs in complete rows
    return(sup_AMs_in_comp_rows)
  })
  
  cLift = data.frame(numerateur/denominateur)

  #save together as dataframe
  QM <- as.data.frame(cbind(pSup, oSup, eSup, cLift))
  colnames(QM) <- c("pSup", "oSup", "eSup", "cLift")
  
  # Adding eSup and cLift columns to patterns
  patterns@quality$eSup <- QM$eSup
  patterns@quality$cLift <- QM$cLift
  
  return(patterns)
}

calc_itemset_obs <- function(database, minsup){ # identifying itemsets in observed data
  database <- database %>% select(all_of(antibiotic_names))
  
  database_bin <- database %>% # transforming database into a binary database (R --> TRUE, S --> FALSE)
    mutate_all(~ case_when(
      . == "R" ~ TRUE,
      . == "S" ~ FALSE,
      is.na(.) ~ NA,
      TRUE ~ as.logical(.)))
  
  mat_transac <- as(database_bin, "transactions") # creating transactions matrix
  
  var_interest <- mat_transac@itemInfo[["variables"]]  
  database_bin <- database_bin %>% select(all_of(var_interest)) # keeping only columns of interest
  
  database_bin_opt <- database %>% # Creating optimistic transactions matrix to calculate eSup and cLift
    mutate_all(~ case_when(
      . == "R" ~ TRUE,
      . == "S" ~ FALSE,
      is.na(.) ~ TRUE,
      TRUE ~ as.logical(.))) %>%
    select(all_of(mat_transac@itemInfo[["variables"]])) # selecting only variables present in mat_transac
  
  mat_transac_opt <- as(database_bin_opt, "transactions")
  
  patterns <- apriori(mat_transac, # performing Apriori algorithm
                      parameter = list(support = minsup, 
                                       maxlen = 27,
                                       minlen = 2,
                                       maxtime = 0, # deleting time limit
                                       target = "frequent itemsets"),
                      control = list(verbose = FALSE)
  )
  
  patterns_QM <- ajout_QM(database_bin, mat_transac, mat_transac_opt, patterns) # Adding eSup and cLift
  return(patterns_QM)
}

parallel_calc_list_simul <- function(list_simul, minsup){ # Identifying patterns in simulated datasets using parallel calculing
  load("data/clean_data.RData") # have to load here or not found
  
  # loading antibiotic_names, the vector containing all names of antimicrobials
  AM_class_dataframe <- read_excel("data/AM_class_for_network_plot.xlsx") %>%
    mutate(
      AM = as.factor(AM),
      Code = as.factor(Code),
      Code_eng = as.factor(Code_eng)
    )
  antibiotic_names = as.character(AM_class_dataframe$AM) # vector containing all names of antimicrobials
  
  nom_list_simul <- deparse(substitute(list_simul))
   
  cl <- detectCores() %>% -8 %>% makeCluster # initalizing clusters
  registerDoParallel(cl)
  
  itemsets <- foreach(i = 1:length(list_simul), .packages = c("tidyverse", "arules")) %dopar% { # Identifying patterns with parallel calculing
    source("scripts/functions.R") # have to source each time or functions are not found
    database = list_simul[[i]]
    database <- database %>% select(all_of(antibiotic_names)) # selecting only antibiotics
    
    database_bin <- database %>% # transforming database into a binary database
      mutate_all(~ case_when(
        . == "R" ~ TRUE,
        . == "S" ~ FALSE,
        is.na(.) ~ NA,
        TRUE ~ as.logical(.)))
    
    mat_transac <- as(database_bin, "transactions") # creating the transaction matrix
    var_interest <- mat_transac@itemInfo[["variables"]] # identifying columns of interest
    database_bin <- database_bin %>% select(all_of(var_interest)) # keeping only variables of interest
    
    database_bin_opt <- database %>% # creating optimistic transaction matrix
      mutate_all(~ case_when(
        . == "R" ~ TRUE,
        . == "S" ~ FALSE,
        is.na(.) ~ TRUE,
        TRUE ~ as.logical(.))) %>%
      select(all_of(mat_transac@itemInfo[["variables"]]))
    
    mat_transac_opt <- as(database_bin_opt, "transactions")
    
    patterns <- apriori(mat_transac, # performing Apriori algorithm
                        parameter = list(support = minsup, 
                                         maxlen = 27,
                                         minlen = 2,
                                         maxtime = 0, # deleting time limit
                                         target = "frequent itemsets"
                        ))
    
    patterns_QM <- ajout_QM(database_bin, mat_transac, mat_transac_opt, patterns) # Adding eSup and cLift
    return(patterns_QM)
  }
  
  stopCluster(cl)
  return(itemsets)
}

decoup_list <- function(liste_a_decoup, taille_sous_liste){ # Splitting list into smaller lists to avoid exceeding the computer's memory capacity
  sous_listes <- split(liste_a_decoup, 
                       rep(1:(length(liste_a_decoup) %/% taille_sous_liste), 
                           each = taille_sous_liste, 
                           length.out = length(liste_a_decoup)))
  
  indices_debut <- seq(1, length(liste_a_decoup), by = taille_sous_liste) # initializing a vector with the indexes of the start of the sublists
  sous_listes_noms <- character(length(indices_debut)) # initializing a vector to store names
  
  for (i in seq_along(indices_debut)) { # generating the names
    debut <- indices_debut[i]
    fin <- min(debut + taille_sous_liste - 1, length(liste_a_decoup))
    suffixe <- paste0("_", debut, "_", fin)
    sous_listes_noms[i] <- paste0(deparse(substitute(liste_a_decoup)), suffixe)
  }
  
  names(sous_listes) = sous_listes_noms # attributing the names
  return(sous_listes)
}

concat_itemsets_non_BLSE <- function(dossier_enreg, dataset, bornes){
  load(paste0(dossier_enreg,"itemsets_simul/itemsets_simul_",dataset,"_1_", bornes[2], ".RData")) # loading the first element
  list_itemsets_concat = get(paste0("itemsets_simul_",dataset,"_1_", bornes[2])) # initializing the first element
  
  if (length(bornes) > 2){
    for (i in 2:(length(bornes)-1)){
      debut = as.numeric(bornes[i])+1 # adding 1 to go on to the next element
      fin = as.numeric(bornes[i+1])
      load(paste0(dossier_enreg,"itemsets_simul/itemsets_simul_", dataset,"_", debut, "_", fin, ".RData"))
      itemsets = get(paste0("itemsets_simul_",dataset,"_", debut, "_", fin))
      list_itemsets_concat = c(list_itemsets_concat, itemsets)
    }
  }
  
  nom_itemsets = paste0("itemsets_simul_",dataset)
  assign(nom_itemsets, list_itemsets_concat)
  save(list = nom_itemsets, 
       file = paste0(dossier_enreg, "itemsets_simul/", nom_itemsets, ".RData")) # Saving in a RData
  return(paste(nom_itemsets,": saved"))
}

##### Filter functions #####

eSup_set <- function(itemsets){
  # Initializing eSup_set that will have the values of cut-off eSup for different pvalues
  eSup_set <- data.frame(matrix(NA, nrow = 146, ncol = length(itemsets), dimnames = list(c(seq(0, 0.95, 0.01), seq(0.951, 1, 0.001)))))
  
  for (i in 1:length(itemsets)){ # Calculating values of cut-off eSup and saving them in eSup_set
    itemset = itemsets[[i]]
    eSup_set[,i] <- quantile(itemset@quality$eSup, 
                             probs = c(seq(0, 0.95, 0.01), seq(0.951, 1, 0.001)), na.rm = TRUE)
  }
  eSup_set
}

cLift_set <- function(itemsets){
  # Initializing cLift_set that will have the values of cut-off cLift for different pvalues
  cLift_set <- data.frame(matrix(NA, nrow = 146, ncol = length(itemsets), dimnames = list(c(seq(0, 0.95, 0.01), seq(0.951, 1, 0.001)))))
  
  for (i in 1:length(itemsets)){ # Calculating values of cut-off cLift and saving them in cLift_set
    itemset = itemsets[[i]]
    cLift_set[,i] <- quantile(itemset@quality$cLift, 
                              probs = c(seq(0, 0.95, 0.01), seq(0.951, 1, 0.001)), na.rm = TRUE)
  }
  cLift_set
}

cut_off_calc <- function(itemsets, pvalue){ # Returning eSup and cLift cut-off values for pvalue
  distrib_eSup <- eSup_set(itemsets)
  distrib_cLift <- cLift_set(itemsets)
  
  cut_off_eSup <- mean(unlist(distrib_eSup[pvalue,]))
  cut_off_cLift <- mean(unlist(distrib_cLift[pvalue,]))
  
  return(list(eSup = cut_off_eSup, 
              cLift = cut_off_cLift))
}



##### Functions to make networks #####

edges_from_sets <- function(sets, AM_class_dataframe, itemset_obs){ # creating edges (adapted from Cazer 2021)
  sets$items <- gsub("[{}]","", sets$items) # deleting {} otherwise it does not work
  sets$items <- gsub(" ","", sets$items) # deleting spaces otherwise it does not work
  
  SetColumn = "items" # columns with patterns
  index = 6 # number of column used to choose edge color (here, cLift)
  QMNames = "cLift" # name of column used to choose edge color (here, cLift)
  AM_class = AM_class_dataframe
  
  #give each itemset an ID (set#) and Name (category: items)
  sets$setID <- paste("set", seq(1, nrow(sets),1), sep = "")
  sets$setName <- paste(as.character(sets$cat), sets$items, sep = ":")
  
  edges <- as.data.frame(NULL) #dataframe to store edges
  
  for (i in 1:nrow(sets)){ #for each itemset
    items <- str_split(sets$items[i], ",")[[1]] #take character string of items divided by "," and turn into character vector
    
    #generate all possible 2-item combinations from the itemset
    if (length(items) > 2){ #if there are more than two items
      combos <- as.data.frame(t(combn(items, 2))) #puts 2-item combos in a dataframe, with each item in a column, in alphabetical order within the row
      dat <- combos #prepare to add more info to dataframe
      colnames(dat) <- c("Node1", "Node2") #name each item as a Node
      otherdat <- expandRows(sets[i,], nrow(combos), count.is.col = FALSE) #copy the QM info associated with the itemset to each 2-AM combination
      dat <- cbind(dat, otherdat) #add QM data to the Nodes
      
    }else{ #2-item itemsets (no combinations to make)
      dat <- cbind(Node1 = items[1], Node2 = items[2], sets[i,]) #if no combos to make, just split the items into Node1 and Node 2, copy the QM info
    }
    
    edges <- rbind(edges, dat) #bind all itemset edges together
  }
  
  edges$edgeName <- paste(edges$Node1, edges$Node2) #create edge name from the node names.
  
  edges_aggregated <- with(edges, 
                           aggregate(formula(paste('cbind(', paste(QMNames, collapse = ","), ')~edgeName', sep = "")), 
                                     FUN=function(x) mean(x, na.rm = TRUE), na.action = na.omit)) #average QM for each edge, grouped by category
  
  edges_aggregated <- separate(edges_aggregated, 
                               edgeName, 
                               into = c("Node1", "Node2"), 
                               sep = " ", 
                               remove = FALSE) #separate the edgeName into Node1 and Node2 (these were dropped from 'edges' in the previous step)
  
  #identify AM class of each node
  edges_aggregated$Node1Class <- AM_class[match(edges_aggregated$Node1,AM_class$AM),]$Code
  edges_aggregated$Node2Class <- AM_class[match(edges_aggregated$Node2,AM_class$AM),]$Code
  
  # Saving eSup of patterns with the 2 nodes
  insp = inspect(itemset_obs)
  edges_aggregated$weight <- sapply(1:nrow(edges_aggregated), function(i){
    nom_edge = paste0("{", edges$Node1[i], ", ", edges$Node2[i], "}")
    eSup = insp$eSup[which(insp$items == nom_edge)]
    eSup
  })
  
  #edge id is numerical sequence: e1, e2...
  edges_aggregated$ID = paste("e", seq(1, nrow(edges_aggregated), 1), sep = "")
  list(edges_aggregated = edges_aggregated, 
       AM_class = AM_class)
}

graph_from_edges <- function(database, edges, AM_class, edge_colors, breaks, fact_mult, titre){
  graph <- graph_from_data_frame(d = as.matrix(edges[,2:3]), # initializing graph
                                 vertices = AM_class$AM, 
                                 directed = FALSE)
  
  # edge colors according to cLift breaks
  edges$cLiftBin <- cut(edges$cLift, breaks = breaks) 
  names(edge_colors) <- levels(edges$cLiftBin)
  
  # assign edge properties
  E(graph)$QM <- as.character(edges$"cLiftBin")
  E(graph)$color <- edge_colors[match(E(graph)$QM, names(edge_colors))] # edge color based in binned clift
  
  E(graph)$width <- edges$weight * fact_mult
  E(graph)$width = replace(E(graph)$width, E(graph)$width >4, 4)
  
  # To colors nodes with the appropriate pie chart :
  dat <- database %>%
    select(all_of(AM_class$AM)) 
  
  AM_class$pieNA <- apply(dat, 2, function(AM){ # calculating NA proportion
    sum(is.na(AM))/length(AM)
  })
  
  AM_class$pieR <- apply(dat, 2, function(AM){ # calculating R proportion
    sum(AM == "R", na.rm = T)/length(AM)
  })
  
  
  AM_class$pieS <- apply(dat, 2, function(AM){ # calculating S proportion
    sum(AM == "S",na.rm=T)/length(AM)
  })
  
  #for piechart, pie values must be in one column as a list with length = length of vertex colors.
  #make vector = length of number of vertex colors (longcolors)--equal to length of number of vertices plus gray and white
  #contains pie values for NA and S
  
  AM_class$pieVal <- mapply(append, lapply(1:nrow(AM_class), function(x){ matrix(0, nrow = 1, ncol = nrow(AM_class))}), 
                            mapply(append, as.list(AM_class$pieS), as.list(AM_class$pieNA), SIMPLIFY = FALSE), SIMPLIFY = FALSE)
  
  #then fill in appropriate R value for corresponding color values
  for (j in 1:nrow(AM_class)){ #for each node
    AM_class$pieVal[[j]][j] <- AM_class$pieR[j] #fill in pieR value into the pieVal list
  }
  names(AM_class$pieVal) <- AM_class$AM
  
  
  # long colors = vector = length of number of vertices plus gray and white
  names(colors) <- unique(AM_class$Code_eng)
  AM_class$color = colors[match(AM_class$Code_eng, names(colors))] # color by AM class
  longcolors = list(c(AM_class$color,"#FFFFFF", "#DCDCDC")) # pour rajouter un pie chart avec les sensibles et les NA
  
  # assign vertex attributes to graph
  V(graph)$label <- as.character(AM_class$AM)
  V(graph)$label.color <- "black"
  V(graph)$label.cex <- 1.25
  V(graph)$frame.color <- AM_class$color
  V(graph)$shape <- "pie"
  V(graph)$pie <- AM_class$pieVal
  V(graph)$pie.color <- longcolors
  
  graph_attr(graph,"main") <- titre
  
  return(graph)
}

plot_graphs_assemble <- function(dossier_enreg, list_graphs, nom_graph_final, ncol, nrow){ #putting graphs together
  png(paste0(dossier_enreg,"plots_reseaux/", nom_graph_final, ".png"), width = 60, height = 26, units = "cm", res = 1000)
  par(mfrow = c(nrow, ncol), mar = c(1, 1, 1, 1), oma = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
  
  for (i in 1:length(list_graphs)){ # generating each graph
    # If the graph does not exist: 
    if (!file.exists(paste0(dossier_enreg, "reseaux/", list_graphs[i], ".RData"))){
      plot.new()
    } else{ # If the graph exists :
      load(paste0(dossier_enreg,"reseaux/", list_graphs[i], ".RData"))
      plot(get(list_graphs[i]), layout = layout_in_circle, main = NA)
    }}
  dev.off()
}



##### Functions to make subsamples #####

# creation_ssech_v0 <- function(dossier_enreg,list_datasets_no_EC, taille_BLSE, taille_non_BLSE){
#   for (i in 1:length(list_datasets_no_EC)){
#     dataset = get(paste0("EC_",list_datasets_no_EC[i]))
#     
#     if (str_detect(list_datasets_no_EC[i], "non_BLSE")){
#       data_ech = dataset[sample(nrow(dataset), taille_non_BLSE),]
#     } else if (str_detect(list_datasets_no_EC[i], "BLSE")){
#       data_ech = dataset[sample(nrow(dataset), taille_BLSE),]
#     }
#     
#     # Saving the subsample
#     nom_data_ech <- paste0("ssech_EC_",list_datasets_no_EC[i])
#     assign(nom_data_ech, data_ech)
#     save(list = nom_data_ech, 
#          file = paste0(dossier_enreg,"datasets_ssech/", nom_data_ech, ".RData"))
#   }
#   return("Sub-samples saved")
# }

creation_ssech <- function(dossier_enreg, dataset, taille_ssech){
  data_ech = dataset[sample(nrow(dataset), 
                            taille_ssech,
                            replace = FALSE),]
  return(data_ech)
}

creation_ssech_replace <- function(dossier_enreg, dataset, taille_ssech){
  data_ech = dataset[sample(nrow(dataset), 
                            taille_ssech,
                            replace = TRUE),]
  return(data_ech)
}
