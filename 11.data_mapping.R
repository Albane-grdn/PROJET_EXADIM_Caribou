# DATA MINING : Mapping 
# -> Maps of the environnemental data 


data.gen <- readRDS(file = file.path(paste0(p.proc.data,"/data.gen_2026-10-01.RData")))
data.env <- readRDS(file = file.path(paste0(p.proc.data,"/data.env_2026-10-01.RData")))

# CARTOGRAPHIE DES COV. ENV.----------------------------------------------------

## PLUVIOMETRIE-----------------------------------------------------------------
bins <- c(10, 20, 30, 40, 50, 60, 70, 80) # min = 11, max = 76
pal.rain <- colorBin("YlGnBu", 
                     domain = data.env$saisonnalite_precip,
                     bins = bins)

map.env.rain <- leaflet(data.env) %>%
  addProviderTiles("Stadia.StamenTerrain") %>%
  addCircleMarkers(lng=data.env$longitude,
                   lat= data.env$latitude,
                   radius = ~(data.env$precip_annuelles)/50,
                   color = ~pal.rain(data.env$saisonnalite_precip),
                   stroke = TRUE,
                   weight = 0.5,
                   opacity = 1,
                   fill = TRUE,
                   fillOpacity = 0.9)%>%
  addLegend(pal = pal.rain, values = ~data.env$saisonnalite_precip,
            opacity = 1,
            title = "Indice de saisonalité des précipitations",
            position = "bottomright")

map.env.rain  # Print the map


## TEMPERATURE------------------------------------------------------------------
bins <- c(0, 2, 4, 6, 8, 10, 12, 14) # min = 4.9, max = 12.9
pal.amp.temp <- colorBin("YlGnBu", 
                         domain = data.env$amplitude_thermique_journaliere,
                         bins = bins)
bins <- c(-25, -20, -15, -10, -5, 0, 5) # min = -21.4, max = 2.3
pal.temp.moy <- colorBin("RdYlBu", 
                         domain = data.env$temp_moy_annuelle,
                         bins = bins, 
                         reverse = T)

map.env.temp <- leaflet(data.env) %>%
  addProviderTiles("Stadia.StamenTerrain") %>%
  addCircleMarkers(lng=data.env$longitude,
                   lat= data.env$latitude,
                   # radius = ~(data.env$temp_moy_annuelle)/50,
                   
                   # TEMPERATURE MOYENNE
                   color = ~pal.temp.moy(data.env$temp_moy_annuelle),
                   # AMPLITUDE DE TEMPERATURE
                   # color = ~pal.amp.temp(data.env$amplitude_thermique_journaliere),
                   
                   stroke = TRUE,
                   weight = 0.5,
                   opacity = 1,
                   fill = TRUE,
                   fillOpacity = 0.9)%>%
  
  # TEMPERATURE MOYENNE
  addLegend(pal = pal.temp.moy, values = ~data.env$temp_moy_annuelle,
            opacity = 1,
            title = "Température moyenne annuelles",
            position = "bottomright"
  # AMPLITUDE DE TEMPERATURE
  # addLegend(pal = pal.amp.temp, values = ~data.env$amplitude_thermique_journaliere,
  #           opacity = 1,
  #           title = "Amplitude journalière de température",
  #           position = "bottomright"
  )

map.env.temp

# ------------------------------------------------------------------------------
