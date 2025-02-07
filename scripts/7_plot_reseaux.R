list_datasets_EC <- paste0("EC_",list_datasets)


colors = c("#23C997", # aminoside
           "#DCB702" , # autre
           "#7EC8E3",
           "#3E7CA5",
           "#F2B6EB" ,
           "#CB85C3" ,
           "#B155A7",
           "#62C966" ,
           "#EE637F" ,
           "#FDAC76",
           "#FF7F40" ,
           "#BCED54"
           )

edges_colors_BLSE <- c("#FFA789", "#D94851", "#A72829", "#6A0000")
edges_colors_non_BLSE <- c("#9BCCE2", "#6BAED6", "#2171B5", "#08306B")


# Parameters for plotting networks

# Defining edges color according to cLift cuts
breaks_non_BLSE <- c(1, 2, 5, 10, Inf) #bins of cLift: 1-2; 2-5; 5-10; 10-Inf
breaks_BLSE <- c(1, 1.2, 1.4, 1.6, Inf) #bins of cLift: 1-1.2; 1.2-1.6; 1.6-1.8; 1.8-Inf

# Factors to adjust edge width in networks
fact_mult_BLSE = 5
fact_mult_non_BLSE = 34



### Graph plotting

describ_graphs = data.frame(dataset = list_datasets,
                            mean_degree = NA,
                            density = NA)

for (i in 1:length(list_datasets)){
  load(paste0(dossier_enreg,"itemset_filtre_", pvalue, "/itemset_filtre_", pvalue, "_", list_datasets[i], ".RData")) 
  load(paste0(dossier_enreg,"itemset_obs/itemset_obs_", list_datasets[i], ".RData")) 
  
  itemset <- get(paste0("itemset_filtre_", pvalue, "_", list_datasets[i]))
  
  # ESBL or non-ESBL?
  if (str_detect(list_datasets[i], "non_BLSE")){
    type_bact = "non_BLSE"
    titre = "Non-ESBL"
    edges_colors = edges_colors_non_BLSE
    breaks = breaks_non_BLSE
    fact_mult = fact_mult_non_BLSE
  } else if (str_detect(list_datasets[i], "BLSE")){
    type_bact = "BLSE"
    titre = "ESBL"
    edges_colors = edges_colors_BLSE
    breaks = breaks_BLSE
    fact_mult = fact_mult_BLSE
  }
  
  annee = gsub("[^0-9]", "", list_datasets[i])
  
  if(!(is.null(inspect(itemset)))){ # plotting only if there are selected patterns
    # creating the network graph
    edges_and_vertices <- edges_from_sets(inspect(itemset),
                                          AM_class_dataframe,
                                          itemset_obs = get(paste0("itemset_obs_", list_datasets[i])))
    
    # Loading the dataset (distinguishing total data and subsets like men/women, +65yo/-65yo, ...)
    if (str_detect(chemin_donnees, "sampled")){
      load(paste0(chemin_donnees, "sampled_", list_datasets[i], ".RData"))
      database <- get(paste0("sampled_", as.character(list_datasets[i])))
    }
    else{
      load(paste0(chemin_donnees, list_datasets_EC[i], ".RData"))
      database <- get(as.character(list_datasets_EC[i]))
    }
    
    graph <- graph_from_edges(database,
                              edges_and_vertices$edges_aggregated,
                              edges_and_vertices$AM_class,
                              edges_colors,
                              breaks,
                              fact_mult,
                              list_datasets[i]
                                     )
    
    # Network analysis
    describ_graphs$mean_degree[i] = mean(degree(graph, mode = "all"))
    describ_graphs$density[i] = edge_density(graph)
    
    # Saving the graph
    nom_graph <- paste0("graph_", list_datasets[i], "_", pvalue)
    assign(nom_graph, graph)
    save(list = nom_graph, 
         file = paste0(dossier_enreg,"reseaux/", nom_graph, ".RData"))
    
    # Saving a plot of the graph
    ppt <- read_pptx() %>%
      add_slide(layout = "Blank", master = "Office Theme") %>%
      ph_with(dml(code = plot(graph,
                              layout = layout_in_circle
      )), location = ph_location_fullsize())
    print(ppt, target = paste0(dossier_enreg, "plots_reseaux/", nom_graph , ".pptx"))
  }
}

write.xlsx(describ_graphs, file = paste0(dossier_enreg, "describ_graphs_",pvalue,".xlsx"))

