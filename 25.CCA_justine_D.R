#################################################################
#### L'analyse canonique des correspondances basÃ© sur le tp #####
################################################################

# data
data.env
data.gen

# vÃ©rification match des individus + mÃªme ordre
identical(data.gen$individu, data.env$individu)

# Ã  check aprÃ¨s : rownames(data.gen)<- data.gen$individu
# prep donnÃ©e
matrix.gen <- data.gen %>%
  select(-individu)
matrix.env <- data.env %>%
  select(-individu, -ecotype)



matrix.env.log <- data.env %>%
  mutate(across(5:12, ~ log(.x+22.4))) %>%
  select(-individu, -ecotype)
matrix.env.log$longitude <- matrix.env.log$longitude*(-1)

# cca
cca2<-cca(matrix.gen ~ ., data = matrix.env.log)
cca2 

vif.cca(cca2) #check de la multicolinÃ©aritÃ© entre les variables explicatives
#latuitude 15, donc Ã§a DEGAGE

matrix.env.log <- data.env %>%
  mutate(across(5:12, ~ log(.x+22.4))) %>%
  select(-individu, -ecotype, -latitude)
matrix.env.log$longitude <- matrix.env.log$longitude*(-1)

cca2<-cca(matrix.gen ~ ., data = matrix.env.log)
cca2 

vif.cca(cca2)


# on essaye d'expliquer les barycentres en fonctions des variables environnementales
# Call:

#                Inertia    Proportion    Rank
# Total          1.6641     1.0000     
#' Constrained    0.2993     *0.1646*     10
# Unconstrained  1.3648     0.8201        77

# variances total = 1,66 = inertie tot de l'afc
# constrained = la part de l'afc expliquÃ© par l'acp (soit snp expliquÃ© par les variables environnementales)
#' *16,5%* est expliquÃ© par des variables 

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

#### boucle for pour tester chaque Ã©cotype ###

ecotypes <- unique(data.env$ecotype)

col_ecotype <- c(
  "Arctique" = "white",
  "Foret_atlantique" = "#40BDB9",
  "Foret_boreale" = "darkgreen",
  "Foret_Ouest" = "forestgreen",
  "Haut_Arctique" = "darkblue"
)

# Stocker les rÃ©sultats
cca_results <- list()

for (eco in ecotypes) {
  
  # 1. Filtrer les donnÃ©es environnementales
  data.env.select <- data.env %>%
    filter(ecotype == eco)
  
  # 2. Filtrer les donnÃ©es gÃ©nÃ©tiques
  data.gen.select <- data.gen %>%
    filter(individu %in% data.env.select$individu)
  
  # 3. RÃ©ordonner les individus
  data.gen.select <- data.gen.select %>%
    arrange(match(individu, data.env.select$individu))
  
  # 4. PrÃ©parer les matrices
  matrix.gen <- data.gen.select %>%
    select(-individu)
  
  matrix.env <- data.env.select %>%
    select(-individu, -ecotype)
  
  # 5. RÃ©aliser la CCA
  cca2 <- cca(matrix.gen ~ ., data = matrix.env)
  
  # 6. Stocker le rÃ©sultat
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

