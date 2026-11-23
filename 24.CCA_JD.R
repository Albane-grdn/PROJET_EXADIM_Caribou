#################################################################
#### L'analyse canonique des correspondances basé sur le tp #####
################################################################

# library
library(vegan)

# data
data.env
data.gen

# vérification match des individus + même ordre
identical(data.gen$individu, data.env$individu)

# prep donnée
matrix.gen <- data.gen %>%
  select(-individu)
matrix.env <- data.env %>%
  select(-individu, -ecotype)

# cca
cca2<-cca(matrix.gen ~ ., data = matrix.env)
cca2 

# on essaye d'expliquer les barycentres en fonctions des variables environnementales
# Call:

#                Inertia    Proportion    Rank
# Total          1.6641     1.0000     
#' Constrained    0.2993     *0.1799*     10
# Unconstrained  1.3648     0.8201        77

# variances total = 1,66 = inertie tot de l'afc
# constrained = la part de l'afc expliqué par l'acp (soit snp expliqué par les variables environnementales)
#' *18%* est expliqué par des variables 

summary(cca2)

levels(as.factor(data.env$ecotype))
col_ecotype <- c(
  "Arctique" = "white",
  "Foret_atlantique" = "#40BDB9",
  "Foret_boreale" = "darkgreen",
  "Foret_Ouest" = "forestgreen", 
  "Haut_Arctique" = "darkblue"
)
col_ind <- col_ecotype[as.character(data.env$ecotype)]


plot(cca2, type = "n")
text(cca2, display = "species", col = "lightblue", cex = 0.8)
ordiellipse(
  cca2,
  groups = data.env$ecotype,
  display = "sites",
  kind = "sd",
  conf = 0.95,
  col = col_ecotype,
  lwd = 1,
  draw = "polygon",
  alpha = 80
)
points(
  cca2,
  display = "sites",
  pch = 21,
  col = "black",
  bg = col_ind,
  cex = 0.8
)
text(cca2, display = "cn")
legend(
  "topright",
  legend = names(col_ecotype),
  pt.bg = col_ecotype,
  pch = 21,
  col = "black",
  title = "Ecotype"
)

#### boucle for pour tester chaque écotype ###

ecotypes <- unique(data.env$ecotype)

col_ecotype <- c(
  "Arctique" = "white",
  "Foret_atlantique" = "#40BDB9",
  "Foret_boreale" = "darkgreen",
  "Foret_Ouest" = "forestgreen",
  "Haut_Arctique" = "darkblue"
)

# Stocker les résultats
cca_results <- list()

for (eco in ecotypes) {
  
  # 1. Filtrer les données environnementales
  data.env.select <- data.env %>%
    filter(ecotype == eco)
  
  # 2. Filtrer les données génétiques
  data.gen.select <- data.gen %>%
    filter(individu %in% data.env.select$individu)
  
  # 3. Réordonner les individus
  data.gen.select <- data.gen.select %>%
    arrange(match(individu, data.env.select$individu))
  
  # 4. Préparer les matrices
  matrix.gen <- data.gen.select %>%
    select(-individu)
  
  matrix.env <- data.env.select %>%
    select(-individu, -ecotype)
  
  # 5. Réaliser la CCA
  cca2 <- cca(matrix.gen ~ ., data = matrix.env)
  
  # 6. Stocker le résultat
  cca_results[[eco]] <- cca2
  
  # 7. Graphique
  plot(cca2, type = "n",
       main = paste("CCA -", eco))
  
  text(cca2, display = "species",
       col = adjustcolor("lightblue", alpha.f = 0.2),
       cex = 0.8)
  
  points(cca2,
         display = "sites",
         pch = 21,
         col = "black",
         bg = col_ecotype[eco],
         cex = 0.8)
  
  text(cca2, display = "cn")
  
  legend("topright",
         legend = eco,
         pt.bg = col_ecotype[eco],
         pch = 21,
         col = "black",
         title = "Ecotype")
}

