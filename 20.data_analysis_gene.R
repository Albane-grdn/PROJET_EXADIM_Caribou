# DATA ANALYSIS : RDA of rare gene abundance at SNP sites  
# -> Maps of the environnemental data 

data.gen <- readRDS(file = file.path(paste0(p.proc.data,"/data.gen_2026-10-01.RData")))
data.env <- readRDS(file = file.path(paste0(p.proc.data,"/data.env_2026-10-01.RData")))

# DATA PREPARATION ----------------------------------------------------

# Chaque ligne est un individu (car c'est lorsque l'individu est échantillonné 
# que l'on récupère les données environnementales, chaque colonne est un SNP 
# et par un point de la RDA on traduit l'abondance en allèle rare expliqué par 
# les variables environnementales)

ind.gen <- as.data.frame((data.gen[,2:6001]))

ind.gen.hel <- decostand(ind.gen, method = "hellinger") # Transform to mimize the double 0 impact 
env.scaled   <- as.data.frame(scale(data.env_num[])) # data.env_num object created in 10.data_vizualisation 
# Est ce qu'on normalise aussi long et lat ??

# REDUNDANCE ANALYSIS  ---------------------------------------------------------
rda.full <- rda(ind.gen.hel ~ ., data = env.scaled)

## Results  --------------------------------------------------------------------
summary(rda.full)
RsquareAdj(rda.full) # Part de variance expliquée (R² ajusté = la valeur à rapporter)
# 14% (18% en ajoutant la position de prélèvement) de la diversité en allèle rare 
# est expliquée par les variables environnementales
# R² ajusté = 0.059 (0.074 en ajoutant la position ) pas fou... mais on a uniquement
# les variables numérique,peut être que la diversité allélique est davantage expliquée
# par l'écotype 

## Triplot rda.full ------------------------------------------------------------
ev <- round(100 * rda.full$CCA$eig / rda.full$tot.chi, 1)

plot(rda.full, scaling = 2, type = "n",
     xlab = paste0("RDA1 (", ev[1], " %)"),
     ylab = paste0("RDA2 (", ev[2], " %)"),
     main = "")
points(rda.full, display = "sites", scaling = 2, pch = 19, col = "grey50") # individus 
text(rda.full,   display = "species", scaling = 2, col = "darkred", cex = 0.7) # SNP
text(rda.full,   display = "bp",      scaling = 2, col = "navy",    cex = 0.9) # variables environnementales

# Version plus lisible : n'afficher que les espèces les mieux ajustées
gof <- goodness(rda.full, choices = 1:2, display = "species")[, 2]
top <- names(sort(gof, decreasing = TRUE))[1:12]
sp  <- scores(rda.full, display = "species", scaling = 2)[top, ]
plot(rda.full, scaling = 2, display = c("sites", "bp"),
     main = "RDA - 12 espèces les mieux expliquées")
text(sp[, 1], sp[, 2], labels = rownames(sp), col = "darkred", cex = 0.75)


## Diagnosis and adjustement  -------------------------------------------------- 
# Multi-colinéarité 
# Colinéarité entre variables explicatives (VIF > 10 = problème)
vif.cca(rda.full) # temp moy explose quand on ajoute la position, valeur de longitude
# très haute 

# Sélection de variables 
#    Sélection pas à pas, arrêtée au R² ajusté du modèle complet
#    (Blanchet et al. 2008)
rda.0 <- rda(ind.gen.hel ~ 1, data = env.scaled)
# /!\ RAME DE FOU : faire la sélection basée sur le seuil du VIF 
# sel <- ordiR2step(rda.0, scope = formula(rda.full),
# R2scope = TRUE, permutations = 999, trace = FALSE)
# sel$anova
# formula(sel)
# 
# rda.sel <- sel
# RsquareAdj(rda.sel)
# anova(rda.sel, permutations = 999)
# anova(rda.sel, by = "margin", permutations = 999)
# vif.cca(rda.sel)

# Sélection des variables à la main pour seuil VIF = 10
env.scaled.restr <- env.scaled[,-c(2,3)] # On supprime latitude et temp. moy.
rda.restr <- rda(ind.gen.hel ~ ., data = env.scaled.restr)
summary(rda.restr)
RsquareAdj(rda.restr)
# 15% de la diversité en allèle rare 
# est expliquée par les variables environnementales
# R² ajusté = 0.066

## Triplot rda.restr -----------------------------------------------------------

ev <- round(100 * rda.restr$CCA$eig / rda.restr$tot.chi, 1)

plot(rda.restr, scaling = 2, type = "n",
     xlab = paste0("RDA1 (", ev[1], " %)"),
     ylab = paste0("RDA2 (", ev[2], " %)"),
     main = "")
points(rda.restr, display = "sites", scaling = 2, pch = 19, col = "grey50") # individus 
text(rda.restr,   display = "species", scaling = 2, col = "darkred", cex = 0.7) # SNP
text(rda.restr,   display = "bp",      scaling = 2, col = "navy",    cex = 0.9) # variables environnementales

# CANONICAL CORREPONDENCE ANALYSIS ---------------------------------------------

# xxx

mil.log <- log(rpjdl$mil + 1)

cca <- cca(rpjdl$fau~., data = mil.log)
# constrained : part de l'inertie de l'AFC expliquée par l'ACP
# 25.2% de la variation de la distribution des sp est expliquée par l'environnement
# l'axe 1 représente 62.6% de l'inertie totale contrainte 
# l'axe 2 représente 17.1% de l'inertie totale contrainte 

ordirgl(cca, type = "t", scaling = 1) # position des sites qui sont issues de la moyenne pondérées des sp qu'ils contiennent 

