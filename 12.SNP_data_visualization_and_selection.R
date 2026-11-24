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
  # rownames(sum.rare.all) <- NULL # suppr des rownames redondant avec col SNP 
  
  sum.rare.all <- sum.rare.all[order(sum.rare.all$count),] # on ordonne pour avoir SNP présentant le moins d'allèle rare à ceux avec le plus d'allèles rares 
  
  SNP.interest <- sum.rare.all %>%
    filter(sum.rare.all$count <=1 | sum.rare.all$count >= max(sum.rare.all$count)-1)
    
  assign(paste0(e,"_snp"),sum.rare.all) # on crée un nouveau df par écotype 
  assign(paste0("SNP.interest.",e),SNP.interest) # on crée un nouveau df par écotype 
  
}
rm(ecot,
   sum.rare.all,
   SNP.interest)

# VISUALIZATION  ---------------------------------------------------------------
g <- ggplot(Haut_Arctique_snp, aes(x = reorder(SNP, count), y = count)) +
  geom_point(size = 2, color = "steelblue", alpha = 0.7)+
  scale_x_discrete(
    labels = function(x) ifelse(x %in% SNP.interest.Haut_Arctique, x, "")
  )
  # theme_minimal() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)) +
  # labs(x = "SNP", y = "Nombre d'allèles rares")

