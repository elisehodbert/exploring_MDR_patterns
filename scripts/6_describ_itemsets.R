# Creating a vector of the dataset names

for (i in 1:length(list_datasets)){
  load(paste0(dossier_enreg,"itemset_filtre_", pvalue, "/itemset_filtre_", pvalue, "_", list_datasets[i], ".RData")) 
  load(paste0(dossier_enreg,"itemset_obs/itemset_obs_", list_datasets[i], ".RData")) 
}

# Summary table

tab_recap = data.frame("dataset" = list_datasets)

# Number of patterns before pruning

tab_recap$nb_itemsets_obs = sapply(list_datasets, function(data){
  itemset <- get(paste0("itemset_obs_",data))
  return(length(itemset))
})

# Number of patterns after pruning

tab_recap$nb_itemsets_filtre = sapply(list_datasets, function(data){
      itemset <- get(paste0("itemset_filtre_", pvalue, "_",data))
      return(length(itemset))
    })




# Function to get the length of itemsets

nb_AM_itemset_function <- function(data,fct){
  # Importation données
  sets <- get(paste0("itemset_filtre_", pvalue, "_", data))
  sets <- inspect(sets)
  
  if(is.null(sets)){
    return(0)
  } else{
    # removing the braces
    sets$items <- gsub("[{}]", "", sets$items)
    sets$items <- gsub(" ", "", sets$items)
    
    # putting the antimicrobials in a list
    AM_sets = lapply(1:nrow(sets), function(i){
      items <- str_split(sets$items[i], ",")[[1]] #take character string of items divided by "," and turn into character vector
      items
    })
    if (fct == "mean"){
      length = round(mean(lengths(AM_sets)),digits = 2)
    }
    else if (fct == "max"){
      length = max(lengths(AM_sets))
    } else {
      stop("Fonction non reconnue. Utilisez 'mean' ou 'max'")
    }
    return(length)
  }
}

tab_recap$nb_AM_itemset_mean = sapply(list_datasets, nb_AM_itemset_function, fct = "mean")
tab_recap$nb_AM_itemset_max = sapply(list_datasets, nb_AM_itemset_function, fct = "max")



# Mean number of antimicrobials per pattern

nb_fam_AM_itemset_function <- function(data,fct){
  sets <- get(paste0("itemset_filtre_", pvalue, "_", data))
  sets <- inspect(sets)
  if (is.null(sets)){
    return(0)
  } else{
    sets$items <- gsub("[{}]","",sets$items)
    sets$items <- gsub(" ","",sets$items)
    
    AM_sets = lapply(1:nrow(sets), function(i){
      items <- str_split(sets$items[i], ",")[[1]]
      items
    })
    
    get_class = lapply(AM_sets,function(set){
      classes = AM_class_dataframe$Code[match(set, AM_class_dataframe$AM)]
      unique(classes)
    })
    
    if (fct == "mean"){
      length = round(mean(lengths(get_class)), digits = 2)
    }
    else if (fct == "max"){
      length = max(lengths(get_class))
    } else {
      stop("Fonction non reconnue. Utilisez 'mean' ou 'max'")
    }
    return(length)
  }
}

tab_recap$nb_fam_AM_itemset_mean <- sapply(list_datasets, nb_fam_AM_itemset_function, fct = "mean")
tab_recap$nb_fam_AM_itemset_max <- sapply(list_datasets, nb_fam_AM_itemset_function, fct = "max")

write.xlsx(tab_recap, file = paste0(dossier_enreg,"tab_recap_itemsets_",pvalue,".xlsx"))
