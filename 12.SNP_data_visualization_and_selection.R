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
  
  assign(paste0(e,"_snp"),sum.rare.all) # on crée un nouveau df par écotype 
  
}
rm(ecot,
   sum.rare.all,
   SNP.interest)

## Visualization ---------------------------------------------------------------

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

## SNP selection ---------------------------------------------------------------

for (e in l.ecot) {
  sum.rare.all <- get(paste0(e, "_snp"))
  SNP.interest.10.90 <-  sum.rare.all %>%
    filter(sum.rare.all$frequency <=0.1 | sum.rare.all$frequency >= 0.9)
  
  SNP.interest.5.95 <- get(paste0(e, "_snp")) %>%
    filter(sum.rare.all$frequency <=0.05 | sum.rare.all$frequency >= 0.95)
  
  SNP.interest.2.98 <- get(paste0(e, "_snp")) %>%
    filter(sum.rare.all$frequency <=0.02 | sum.rare.all$frequency >= 0.98)
  
  
  assign(paste0("SNP.interest.",e, ".10.90"),SNP.interest.10.90) # on crée un nouveau df par écotype 
  assign(paste0("SNP.interest.",e, ".5.95"),SNP.interest.5.95) # on crée un nouveau df par écotype 
  assign(paste0("SNP.interest.",e, ".2.98"),SNP.interest.2.98) # on crée un nouveau df par écotype 
  
}

rm(sum.rare.all)

SNP.interest.10.90.all <- data.frame(SNP = unique(c(SNP.interest.Arctique.10.90$SNP, 
                                       SNP.interest.Foret_atlantique.10.90$SNP, 
                                       SNP.interest.Foret_boreale.10.90$SNP,
                                       SNP.interest.Haut_Arctique.10.90$SNP, 
                                       SNP.interest.Foret_Ouest.10.90$SNP)))

SNP.interest.5.95.all <- data.frame(SNP = unique(c(SNP.interest.Arctique.5.95$SNP, 
                                                    SNP.interest.Foret_atlantique.5.95$SNP, 
                                                    SNP.interest.Foret_boreale.5.95$SNP,
                                                    SNP.interest.Haut_Arctique.5.95$SNP, 
                                                    SNP.interest.Foret_Ouest.5.95$SNP)))

SNP.interest.2.98.all <- data.frame(SNP = unique(c(SNP.interest.Arctique.2.98$SNP, 
                                                    SNP.interest.Foret_atlantique.2.98$SNP, 
                                                    SNP.interest.Foret_boreale.2.98$SNP,
                                                    SNP.interest.Haut_Arctique.2.98$SNP, 
                                                    SNP.interest.Foret_Ouest.2.98$SNP)))
# NOT WORKING YET 
# for (e in l.ecot) {
#  rm(as.character(paste0("SNP.interest.", e, ".2.98")))
# }
