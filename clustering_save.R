# Clustering 

## CLUSTERING DISTANCE GENETIQUE    --------------------------------------------
distgo = daisy(data.gen.perm, metric = "euclidean") # A marché une fois avec les id des ind mais lpus maintenant AAAAAAAAAAAAH 

ecot <- as.factor(data.env$ecotype)
# col.ecot <- c("#4DAF4A", "#FF7F00", "#377EB8","#E41A1C", "#984EA3")
col.ecot <- c("#FF7F00", "#984EA3","#4DAF4A", "#377EB8","#E41A1C")
col.ecot.ind <- col.ecot[as.integer(ecot)]  
### à revoir   -----------------------------------------------------------------
plot.new()
hgo <- hclust(distgo) ; plot(hgo, hang = -1)
cut.5.ecot = cutree(hgo, h = 73.45) ; abline(h = 73.45, col = "blue") # Découpage de l'arbre 
cut.8.ecot = cutree(hgo, h = 72) ; abline(h = 72, col = "red") # Découpage de l'arbre 

adonis2(distgo ~ cut.5.ecot,
        permutations = 9999) # Utilisation directe des résultats du clustering à 3 groupes pour une PERMANOVA
adonis2(distgo ~ cut.8.ecot,
        permutations = 9999) # Utilisation directe des résultats du clustering à 3 groupes pour une PERMANOVA

plot(hgo, hang = -1, labels = FALSE)   # on masque les étiquettes par défaut
# Placer des points colorés au niveau des feuilles
n <- length(hgo$order)
points(x = 1:n,
       y = rep(0, n),
       pch = 19, col = col.ecot.ind[hgo$order])
# Étiquettes colorées
text(x = 1:n, y = -0.02 * max(hgo$height),
     labels = hgo$labels[hgo$order],
     col = col.ecot.ind[hgo$order],
     srt = 90, adj = 1, xpd = TRUE, cex = 0.6)
legend("topright", legend = levels(ecot), fill = col.ecot, bty = "n")



# ## CLUSTERING DISTANCE GEOGRAPHIQUE   ---------------------------------------------------------------
# data.geo.perm <- readRDS(file = file.path(latest.env))
# data.geo.perm <- data.geo.perm[,c(1,3,4)]
# rownames(data.geo.perm)<- data.geo.perm$individu 
# data.geo.perm <- data.geo.perm[, -c(1)] # Suppress the individual column 
# 
# distgo.geo = daisy(data.geo.perm, metric = "euclidean") # A marché une fois avec les id des ind mais lpus maintenant AAAAAAAAAAAAH 
# 
# ecot <- as.factor(data.env$ecotype)
# # col.ecot <- c("#4DAF4A", "#FF7F00", "#377EB8","#E41A1C", "#984EA3")
# col.ecot <- c("#FF7F00", "#984EA3","#4DAF4A", "#377EB8","#E41A1C")
# col.ecot.ind <- col.ecot[as.integer(ecot)]  




### à revoir   ---------------------------------------------------------------
plot.new()
hgo.geo <- hclust(distgo.geo)
plot(hgo.geo, hang = -1, labels = FALSE)   # on masque les étiquettes par défaut
# Placer des points colorés au niveau des feuilles
n <- length(hgo.geo$order)
points(x = 1:n,
       y = rep(0, n),
       pch = 19, col = col.ecot.ind[hgo.geo$order])
# Étiquettes colorées
text(x = 1:n, y = -0.02 * max(hgo.geo$height),
     labels = hgo.geo$labels[hgo.geo$order],
     col = col.ecot.ind[hgo.geo$order],
     srt = 90, adj = 1, xpd = TRUE, cex = 0.6)
legend("topright", legend = levels(ecot), fill = col.ecot, bty = "n")






# ## PCA -------------------------------------------------------------------------
# pca1 <- dudi.pca(gen_ind,cent=T,scale=T, scannf = FALSE, nf = 2)
# barplot(pca1$eig[1:50],main="PCA eigenvalues", col=heat.colors(50))
# 
# # Labels overlap, prefer adegraphics
# # ade4::s.class(pca1$li, pop(gen_ind), 
# #         clabel = 0.5)
# 
# df  <- data.frame(pca1$li[, 1:2], pop = pop(gen_ind))
# cen <- aggregate(cbind(Axis1, Axis2) ~ pop, data = df, FUN = mean)
# 
# ### Plotting PCA results -------------------------------------------------------
# s.class(as.data.frame(pca1$li), pop(gen_ind),
#         plabels = list(cex = 0.5,
#                        col = "black",
#                        optim = TRUE,
#                        boxes = list(draw = TRUE)
#                        
#                        )
#         )












### cuts to tester apport de l'ajout de groupes --------------------------------

cut3 = cutree(hgo, h = 72.2) ; abline(h = 72.2, col = "blue") # Découpage de l'arbre 
# à 0.8 = on obtient 3 groupes, permet de créer un vecteur d'intersection décrivant à quel groupe appartient l'sp
adonis2(distgo ~ cut3) # Utilisation directe des résultats du clustering à 3 groupes pour une PERMANOVA

cut5 = cutree(hgo, h = 0.6) ; abline(h = 0.6, col = "red") # Découpage de l'arbre à 0.8 = on obtient 6 groupes
adonis2(distgo ~ cut5) # Utilisation directe des résultats du clustering à 5 groupes pour une PERMANOVA


