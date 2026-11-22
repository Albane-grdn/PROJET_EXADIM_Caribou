# DATA ANALYSIS : CCA (constrained ordination) to get the projection of individuals
# in a new multidimensional space created by linear environmental variables in order
# to detect potential gradients.

summary(data.gen[1:6,1:8]) #tbl faune: site = indiv, sp = SNP
summary(data.env) #tbl milieu site = indiv, var = var environnementales

# DATA -------------------------------------------------------------------------
##tbl fauna: site = indiv, sp = SNP --------------------------------------------
data.gen.cca <- readRDS(file = file.path(latest.gen)) #load latest version of genetic data
rownames(data.gen.cca)<- data.gen.cca$individu #turn individuals's ids into rownames 
data.gen.cca <- data.gen.cca[,-1] #delete the individual column

##tbl milieu site = indiv, var = var environnementales--------------------------
data.env.cca <- data.env[,-c(1,2)] # delete non quantitative variables

#' Remarque: le tbl faune est très déséquilibré (bcp de zéros) comparé au tbl millieu:
#' nécessité de transformer le tableau milieu (log)?? MAIS valeurs <0 donc pas log possible
#' QUE HACEMOS???????

 
# CCA --------------------------------------------------------------------------
## Package vegan ---------------------------------------------------------------
cca1 <- cca(data.gen.cca~.,
            data = data.env.cca)
summary(cca1)
plot(cca1)

plot(cca1)
text(cca1, dis="cn")
points(cca1, pch=21, col="red", bg="yellow", cex=0.8)
text(cca1, "species", col="blue", cex=0.8)

anova.cca(cca1)
# Permutation test for cca under reduced model
# Permutation: free
# Number of permutations: 999
# 
# Model: cca(formula = data.gen.cca ~ longitude + latitude + temp_moy_annuelle + amplitude_thermique_journaliere + saisonnalite_temperature + precip_annuelles + saisonnalite_precip + ndvi + altitude + couverture_arboree, data = data.env.cca)
# Df ChiSquare      F Pr(>F)    
# Model    10   0.29932 1.6887  0.001 ***
#   Residual 77   1.36480                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova.cca(cca1, by="term") 
# Permutation test for cca under reduced model
# Terms added sequentially (first to last)
# Permutation: free
# Number of permutations: 999
# 
# Model: cca(formula = data.gen.cca ~ longitude + latitude + temp_moy_annuelle + amplitude_thermique_journaliere + saisonnalite_temperature + precip_annuelles + saisonnalite_precip + ndvi + altitude + couverture_arboree, data = data.env.cca)
# Df ChiSquare      F Pr(>F)    
# longitude                        1   0.06916 3.9018  0.001 ***
#   latitude                         1   0.05907 3.3328  0.001 ***
#   temp_moy_annuelle                1   0.02284 1.2886  0.014 *  
#   amplitude_thermique_journaliere  1   0.02135 1.2047  0.029 *  
#   saisonnalite_temperature         1   0.02609 1.4718  0.002 ** 
#   precip_annuelles                 1   0.02339 1.3199  0.007 ** 
#   saisonnalite_precip              1   0.02003 1.1298  0.082 .  
# ndvi                             1   0.01681 0.9483  0.713    
# altitude                         1   0.02097 1.1831  0.040 *  
#   couverture_arboree               1   0.01961 1.1066  0.128    
# Residual                        77   1.36480                  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
