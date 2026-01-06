library(ggplot2); library(usmap);library(sf); library(dplyr); library(tigris) 
arkansas_sf_data = tigris::counties(state = "AR", class = "sf") %>% filter(STATEFP == "05") %>% select(NAME, geometry)
casino_locations_df = data.frame(
  city = c("Hot Springs", "Pine Bluff", "West Memphis"),
  latitude = c(34.5034, 34.2281, 35.1481),
  longitude = c(-93.0552, -92.0035, -90.1862),
  county = c("Garland", "Jefferson", "Crittenden"))
top_cities_df = data.frame(
  city = c("Little Rock", "Fort Smith", "Jonesboro", "Fayetteville", "Bentonville"),
  latitude = c(34.7465, 35.3859, 35.8423, 36.0626, 36.3728),
  longitude = c(-92.2896, -94.3986, -90.7042, -94.1575, -94.2089))
arkansas_sf_data = arkansas_sf_data %>% mutate(has_casino = NAME %in% casino_locations_df$county)
casino_locations_sf = st_as_sf(casino_locations_df, coords = c("longitude", "latitude"), crs = 4326)
top_cities_sf = st_as_sf(top_cities_df, coords = c("longitude", "latitude"), crs = 4326)
ggplot() +
  geom_sf(data = arkansas_sf_data, aes(fill = has_casino), color = "grey50", linewidth = 0.5) +
  geom_sf(data = casino_locations_sf, size = 3, shape = 21, color = "black", fill = "black") +
  geom_sf_text(data = casino_locations_sf, aes(label = city), nudge_y = 0.15, color = "black", fontface = "bold", size = 4) +
  geom_sf(data = top_cities_sf, size = 2, shape = 16, color = "black", fill = "black") +
  geom_sf_text(data = top_cities_sf, aes(label = city), nudge_y = 0.1, color = "black", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("TRUE" = "grey", "FALSE" = "white"), labels = c("Yes", "No"), name = "County with Casino") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = 'none',
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) + theme(legend.position = 'none'); ggsave('casino_map.png', height = 5, width = 5)
################################# dynamic html map
library(leaflet); library(sf); library(tigris); library(dplyr)
arkansas_counties = counties(state = "AR", class = "sf")
casino_locations = data.frame(
  city = c("Hot Springs", "Pine Bluff", "West Memphis"),
  latitude = c(34.5034, 34.2281, 35.1481),
  longitude = c(-93.0552, -92.0035, -90.1862),
  county = c("Garland", "Jefferson", "Crittenden")
)
arkansas_counties = arkansas_counties %>% mutate(has_casino = NAME %in% casino_locations$county)
county_colors = colorFactor(palette = c("white", "darkgrey"), domain = arkansas_counties$has_casino)
m = leaflet(options = leafletOptions(minZoom = 7)) %>%
  setView(lng = -92.2896, lat = 34.7465, zoom = 7) %>%
  addProviderTiles(providers$CartoDB.Positron)
m = m %>%
  addPolygons(
    data = arkansas_counties,
    fillColor = ~county_colors(has_casino),
    fillOpacity = 0.5,
    color = "black",
    weight = 1,
    popup = paste0("<b>", arkansas_counties$NAME, " County</b><br>",
                   "Casino: ", ifelse(arkansas_counties$has_casino, "Yes", "No"))
  )
m = m %>%
  addCircles(
    data = casino_locations,
    lng = ~longitude,
    lat = ~latitude,
    radius = 2000, # A fixed radius for the dot size (in meters)
    weight = 2,    # Line weight of the circle
    color = "black", # Border color of the circle
    fillColor = "red", # Fill color of the circle
    fillOpacity = 0.8, # Opacity of the fill
    popup = ~paste0("<b>", city, "</b><br>Casino in ", county, " County"),
    # Add a label for the city name
    label = ~city,
    # Customize the label to be visible and in black
    labelOptions = labelOptions(
      noHide = TRUE, # Always show the label
      direction = 'top',
      textsize = "12px",
      style = list(
        'color' = 'black',
        'font-weight' = 'bold'
      )
    )
  )
m; class(m)
