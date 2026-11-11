# DATA MINING : Visualization 
# -> Charts and graphs of the data 

# SETUP ------------------------------------------------------------------------
# Call to latest processed data function 
latest.gen <- get_latest_gen(p.proc.data) 
latest.env <- get_latest_env(p.proc.data) 

data.gen <- readRDS(file = file.path(latest.gen))
data.env <- readRDS(file = file.path(latest.env))

# DATASET GLOBAL ---------------------------------------------------------------
summary(data.env)
length(unique(data.env$individu))    #88 individus
unique(data.env$ecotype)             #5 écotypes

# # DONNEES GENOTYPIQUES -------------------------------------------------------
# # Histogramme des fq alléliques de la population 
# # ⚠️ to heavy bc too many columns
# for (x in 2:ncol(data.gen)) {
#   hist((data.gen[,x]), main = colnames(data.gen[x]))
# }


# DONNEES ENVIRONNEMENTALES ----------------------------------------------------

## VAR. ENV (TOUTES) -----------------------------------------------------------
# Histogramme des fq d'individus pour les variables environnementales numériques
data.env_num <- select_if(data.env, is.numeric)
par(mfrow= c(3,4))#split the window
for (x in 1:ncol(data.env_num)) {
  hist((data.env_num[,x]), main = colnames(data.env_num[x]))
  
}
par(mfrow= c(1,1)) #reinitiate the window

## ECOTYPES --------------------------------------------------------------------
# Histogramme des fq d'individus selon les écotypes 
hist(str_count(data.env$ecotype), main = (colnames(data.env$ecotype)))

## TEMPERATURE -----------------------------------------------------------------
### Pluviométrie ---------------------------------------------------------------
ggplot(data = data.env) +
  geom_point(
    aes(
      x = individu,
      y = precip_annuelles,
      fill = ecotype
    ),
    shape = 21,
    size = 3,
    colour = "black"
  ) +
  scale_fill_manual(
    values = pal.ecot(unique(data.env$ecotype))
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 90)
  )

### Indice de saisonalité pluviométrique ---------------------------------------
ggplot(data = data.env) +
  geom_point(
    aes(
      x = individu,
      y = saisonnalite_precip,
      fill = ecotype
    ),
    shape = 21,
    size = 3,
    colour = "black"
  ) +
  scale_fill_manual(
    values = pal.ecot(unique(data.env$ecotype))
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 90)
  )

## TEMPERATURE -----------------------------------------------------------------
### Temperature moyenne annuelle -----------------------------------------------
ggplot(data = data.env) +
  geom_point(
    aes(
      x = individu,
      y = temp_moy_annuelle,
      fill = ecotype
    ),
    shape = 21,
    size = 3,
    colour = "black"
  ) +
  scale_fill_manual(
    values = pal.ecot(unique(data.env$ecotype))
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 90)
  )

### Amplitude journalière de température ---------------------------------------
ggplot(data = data.env) +
  geom_point(
    aes(
      x = individu,
      y = amplitude_thermique_journaliere,
      fill = ecotype
    ),
    shape = 21,
    size = 3,
    colour = "black"
  ) +
  scale_fill_manual(
    values = pal.ecot(unique(data.env$ecotype))
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 90)
  )

## NDVI ------------------------------------------------------------------------
ggplot(data = data.env) +
  geom_point(
    aes(
      x = individu,
      y = ndvi,
      fill = ecotype
    ),
    shape = 21,
    size = 3,
    colour = "black"
  ) +
  scale_fill_manual(
    values = pal.ecot(unique(data.env$ecotype))
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 90)
  )

## ALTITUDE --------------------------------------------------------------------
ggplot(data = data.env) +
  geom_point(
    aes(
      x = individu,
      y = altitude,
      fill = ecotype
    ),
    shape = 21,
    size = 3,
    colour = "black"
  ) +
  scale_fill_manual(
    values = pal.ecot(unique(data.env$ecotype))
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 90)
  )


## COUVERTURE ARBOREE-----------------------------------------------------------
ggplot(data = data.env) +
  geom_point(
    aes(
      x = individu,
      y = couverture_arboree,
      fill = ecotype
    ),
    shape = 21,
    size = 3,
    colour = "black"
  ) +
  scale_fill_manual(
    values = pal.ecot(unique(data.env$ecotype))
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 90)
  )
