# DATA MINING : Mapping 
# -> Maps of the environnemental data 


data.gen <- readRDS(file = file.path(paste0(p.proc.data,"/data.gen_2026-10-01.RData")))
data.env <- readRDS(file = file.path(paste0(p.proc.data,"/data.env_2026-10-01.RData")))

# CARTOGRAPHIE DES COV. ENV.----------------------------------------------------

## PLUVIOMETRIE & SAISONALITE---------------------------------------------------
bins <- c(10, 20, 30, 40, 50, 60, 70, 80) # min = 11, max = 76
pal.rain <- colorBin("YlGnBu", 
                     domain = data.env$saisonnalite_precip,
                     bins = bins)

map.env.rain <- leaflet(data.env) %>%
  addProviderTiles("Stadia.StamenTerrain") %>%
  addCircleMarkers(lng=data.env$longitude,
                   lat= data.env$latitude,
                   radius = ~(data.env$precip_annuelles)/50,
                   color = "black",
                   fillColor = ~pal.rain(data.env$saisonnalite_precip),
                   stroke = TRUE,
                   weight = 0.5,
                   opacity = 1,
                   fill = TRUE,
                   fillOpacity = 0.9)%>%
  addLegend(pal = pal.rain, values = ~data.env$saisonnalite_precip,
            opacity = 1,
            title = "Indice de saisonalité des précipitations",
            position = "bottomright")%>%
  addLegendSize(values = data.env$precip_annuelles,
                color = "black",
                fillOpacity = 0,
                shape = "circle",
                title = "Précipitations annuelles (mm)",
                position = "bottomright"   # or "bottomleft"
)

map.env.rain  # Print the map


## TEMPERATURE & AMPLITUDE------------------------------------------------------
###Palette amplitude journalière de température
bins <- c(0, 2, 4, 6, 8, 10, 12, 14) # min = 4.9, max = 12.9
pal.amp.temp <- colorBin("YlGnBu", 
                         domain = data.env$amplitude_thermique_journaliere,
                         bins = bins)

###Palette température moyenne annuelle
bins <- c(-25, -20, -15, -10, -5, 0, 5) # min = -21.4, max = 2.3
pal.temp.moy <- colorBin("RdYlBu", 
                         domain = data.env$temp_moy_annuelle,
                         bins = bins, 
                         reverse = T)

map.env.amp.temp <- leaflet(data.env) %>%
  addProviderTiles("Stadia.StamenTerrain") %>%
  addCircleMarkers(lng=data.env$longitude,
                   lat= data.env$latitude,
                   
                   #Amplitude de température
                   radius = ~data.env$amplitude_thermique_journaliere,
                   
                   #Température moyenne
                   color = "black",
                   fillColor = ~pal.temp.moy(data.env$temp_moy_annuelle),
                   
                   stroke = TRUE,
                   weight = 0.5,
                   opacity = 1,
                   fill = TRUE,
                   fillOpacity = 0.9)%>%
  
  #Légende température moyenne
  addLegend(pal = pal.temp.moy, values = ~data.env$temp_moy_annuelle,
            opacity = 1,
            title = "Température moyenne annuelles (°C)",
            position = "bottomright")%>%
  
  #Légende amplitude de température
  addLegendSize(pal =pal.amp.temp, values = data.env$amplitude_thermique_journaliere,
                color = "black",
                fillOpacity = 0,
                shape = "circle",
                title = "Amplitude journalière de température (°C)",
                position = "bottomright"   # or "bottomleft"
  )


map.env.amp.temp  # Print the map


## ECOTYPES --------------------------------------------------------------------
pal.ecot <- colorFactor(
                palette = "Set1", 
                domain = data.env$ecotype,
                reverse = TRUE)

map.env.ecot <- leaflet(data.env) %>%
  addProviderTiles("Stadia.StamenTerrain") %>%
  addCircleMarkers(lng=data.env$longitude,
                   lat= data.env$latitude,
                   radius = 5,
                   
                   # Set a color per ecotype
                   color = "black",
                   fillColor = ~pal.ecot(data.env$ecotype),
                   
                   stroke = TRUE,
                   weight = 0.5,
                   opacity = 1,
                   fill = TRUE,
                   fillOpacity = 0.9)%>%
  
  # Legend
  addLegend(pal = pal.ecot, values = ~data.env$ecotype,
            opacity = 1,
            title = "Ecotype",
            position = "bottomright"
  )

map.env.ecot

