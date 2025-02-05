####### Plotting of the legend ###########

# Edge color
filename <- paste0(dossier_enreg,"plots_reseaux/legende/legend_color_AMSets_non_BLSE.png")
Cairo(file = filename, type = "png", units = "cm", width = 29.7, height = 21, pointsize = 8, res = 300)
plot.new()
legend('center', 
       legend = c(
         paste(breaks_non_BLSE[1], "-", breaks_non_BLSE[2]),
         paste(breaks_non_BLSE[2], "-", breaks_non_BLSE[3]),
         paste(breaks_non_BLSE[3], "-", breaks_non_BLSE[4]),
         paste(">", breaks_non_BLSE[4])),
       text.width = 0.1,
       col = edges_colors_non_BLSE[c(1:4)], 
       lty = 'solid', 
       lwd = 4,
       ncol = 4, 
       cex = 2,
       title = "Line Color: Association strength (cLift)")
dev.off()

filename <- paste0(dossier_enreg,"plots_reseaux/legende/legend_color_AMSets_BLSE.png")
Cairo(file = filename, type = "png", units = "cm", width = 29.7, height = 21, pointsize = 8, res = 300)
plot.new()
legend('center', 
       legend = c(
         paste(breaks_BLSE[1], "-", breaks_BLSE[2]),
         paste(breaks_BLSE[2], "-", breaks_BLSE[3]),
         paste(breaks_BLSE[3], "-", breaks_BLSE[4]),
         paste(">", breaks_BLSE[4])),       
       text.width = 0.1,
       col = edges_colors_BLSE[c(1:4)], 
       lty = 'solid', 
       lwd = 4, 
       ncol = 4, 
       cex = 2,
       title = "Line Color: Association strength (cLift)")
dev.off()

# légende en utilisant eSup pour l'épaisseur des lignes

# Legend of eSUp used for line width
# width values are to be put in lwd

# eSup values are between 0 and 1
# then a multiplication factor is applied to have a better scale and not have problems when plotting
# for non-ESBL: fact_mult = 5
# for ESBL: fact_mult = 33 (and we put a limit for very high values of support)

# the values to put in the legend are :
# summary(edges[,"n"])


filename <- paste0(dossier_enreg,"plots_reseaux/legende/legend_width_AMSets_non_BLSE.png")
Cairo(file = filename, type = "png", units = "cm", width = 29.7, height = 21, pointsize = 8, res = 300)
plot.new()
legend('center', 
       legend = c("0.03", "0.06", "0.09", "\u2265 0.12 "), 
       text.width = 0.1,
       col = edges_colors_non_BLSE[3], 
       lty = 'solid', 
       lwd = c(1,2,3,4), 
       ncol = 4, 
       cex = 2,
       title = "Line Width: Association frequency (eSup)")
dev.off()

filename <- paste0(dossier_enreg,"plots_reseaux/legende/legend_width_AMSets_BLSE.png")
Cairo(file = filename, 
      type = "png", 
      units = "cm", 
      width = 29.7,
      height = 21, 
      pointsize = 8, 
      res = 300)
plot.new()
legend('center', 
       legend = c("0.2", "0.4", "0.6", "\u2265 0.8 "), 
       text.width = 0.1,
       col = edges_colors_BLSE[2], 
       lty = 'solid', 
       lwd = c(1,2,3,4), 
       ncol = 4, 
       cex = 2,
       title = "Line Width: Association frequency (eSup)")
dev.off()

# Legend with the colors represneting the antimicrobials classes
noms_fr = unique(AM_class_dataframe$Code)
noms_eng = unique(AM_class_dataframe$Code_eng)



filename <- paste0(dossier_enreg,"plots_reseaux/legende/legend_color_nodes.png")
Cairo(file = filename, type = "png", units = "cm", width = 29.7*2, height = 21, pointsize = 8, res = 300)
plot.new()
legend('center',
       legend = noms_eng, 
       text.width = 0.17,
       col = colors, 
       lty = "blank", 
       lwd = 10, 
       ncol = 4, 
       cex = 2, 
       pch = 19,
       title = "Node Color: Antibiotic class")
dev.off()

