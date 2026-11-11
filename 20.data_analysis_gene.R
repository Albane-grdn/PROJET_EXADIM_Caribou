# DATA ANALYSIS : RDA of rare gene abundance at SNP sites  
# -> Maps of the environnemental data 

data.gen <- readRDS(file = file.path(paste0(p.proc.data,"/data.gen_2026-10-01.RData")))
data.env <- readRDS(file = file.path(paste0(p.proc.data,"/data.env_2026-10-01.RData")))

# DATA PREPARATION ----------------------------------------------------
ind.gen <- as.data.frame((data.gen[,2:6001]))
# colnames(ind.gen) <- data.gen$individu

ind.gen.hel <- decostand(ind.gen, method = "hellinger") # Transform to mimize the double 0 impact 
env.scaled   <- as.data.frame(scale(data.env_num[,3:10])) # data.env_num object created in 10.data_vizualisation 
# Est ce qu'on normalise aussi long et lat ??


rda.full <- rda(ind.gen.hel ~ ., data = env.scaled)
summary(rda.full)
# 14% expliqué par variables environnementales

# Part de variance expliquée (R² ajusté = la valeur à rapporter)
RsquareAdj(rda.full)

# Multi-colinéarité 
# Colinéarité entre variables explicatives (VIF > 10 = problème)
vif.cca(rda.full)


# Triplot
ev <- round(100 * rda.full$CCA$eig / rda.full$tot.chi, 1)

plot(rda.full, scaling = 2, type = "n",
     xlab = paste0("RDA1 (", ev[1], " %)"),
     ylab = paste0("RDA2 (", ev[2], " %)"),
     main = "")
points(rda.full, display = "sites", scaling = 2, pch = 19, col = "grey50")
text(rda.full,   display = "species", scaling = 2, col = "darkred", cex = 0.7)
text(rda.full,   display = "bp",      scaling = 2, col = "navy",    cex = 0.9)

# Version plus lisible : n'afficher que les espèces les mieux ajustées
gof <- goodness(rda.full, choices = 1:2, display = "species")[, 2]
top <- names(sort(gof, decreasing = TRUE))[1:12]
sp  <- scores(rda.full, display = "species", scaling = 2)[top, ]
plot(rda.full, scaling = 2, display = c("sites", "bp"),
     main = "RDA - 12 espèces les mieux expliquées")
text(sp[, 1], sp[, 2], labels = rownames(sp), col = "darkred", cex = 0.75)

