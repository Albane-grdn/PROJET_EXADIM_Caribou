# SNP visualization  : visualization of the genetic diversity of each ecotype 

# DATA -------------------------------------------------------------------------
## Allelic composition by ecotypes ---------------------------------------------
data.gen.eco <- data.gen
data.gen.eco[,ncol(data.gen.eco)+1] <- data.env$ecotype
colnames(data.gen.eco)[ncol(data.gen.eco)] <- "ecotype"

l.ecot <- unique(data.gen.eco$ecotype)
for (e in l.ecot) {
  ecot <- filter(data.gen.eco, data.gen.eco$ecotype == e) # on filtre par ecotypes
  # gestion de la colonne individu : passée en rownames 
  rownames(ecot) <- ecot$individu 
  ecot <- ecot[,-c(1)]
  
  sum.rare.all <- data.frame(colnames(ecot[,1:6000]), colSums(ecot[,1:6000])) # on somme le nb d'allèles rares pour chaque SNP
  colnames(sum.rare.all)[1] <- "SNP" # renommage
  colnames(sum.rare.all)[2] <- "count" # renommage
  sum.rare.all[,3] <- sum.rare.all[,2]/(nrow(ecot)*2) # on quantifie le % d'invidus homozygote rares dans l'écotype
  colnames(sum.rare.all)[3] <- "frequency" # renommage
  
  
  sum.rare.all <- sum.rare.all[order(sum.rare.all$count),] # on ordonne pour avoir SNP présentant le moins d'allèle rare à ceux avec le plus d'allèles rares 
  
  SNP.interest <- sum.rare.all %>%
    filter(sum.rare.all$frequency <=0.1 | sum.rare.all$frequency >= 0.9)
    
  assign(paste0(e,"_snp"),sum.rare.all) # on crée un nouveau df par écotype 
  assign(paste0("SNP.interest.",e),SNP.interest) # on crée un nouveau df par écotype 
  
}
rm(ecot,
   sum.rare.all,
   SNP.interest)

# VISUALIZATION  ---------------------------------------------------------------

for (e in l.ecot) {
  g <- ggplot(get(paste0(e, "_snp")), aes(x = reorder(SNP, frequency), y = frequency)) +
    geom_col(color = "steelblue", alpha = 0.7)+
    scale_x_discrete(
      labels = function(x) ifelse(x %in% get(paste0("SNP.interest.",e)), x, "")
    )+
    theme_minimal() +
    labs(x = paste0("SNP ", e), y = "Fréquence allèles rares")
  assign(paste0("plt.SNP.",e),g)
  
}

png(file.path(paste0(p.figures,"/SNP_ecotypes_distribution.png")),
    width = 600,
    height = 800,
    res = 110,
    units = "px")
grid.arrange(plt.SNP.Arctique,
             plt.SNP.Haut_Arctique, 
             plt.SNP.Foret_Ouest, 
             plt.SNP.Foret_boreale, 
             plt.SNP.Foret_atlantique,
             ncol = 2, nrow= 3)
dev.off()

  

