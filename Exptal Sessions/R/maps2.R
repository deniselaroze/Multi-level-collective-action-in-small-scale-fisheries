library(sf)
library(ggplot2)

# Paths to the shapefiles
region_shp_path <- paste0(path_datos, "/Regional.shp")
cities_shp_path <- paste0(path_datos, "/Toponimos.shp")

# Load shapefiles
regions <- st_read(region_shp_path)
cities <- st_read(cities_shp_path)

# Ensure CRS is EPSG:4326 (latitude/longitude)
regions <- st_transform(regions, crs = 4326)
cities <- st_transform(cities, crs = 4326)

# Filter for "Region de Valparaíso"
valparaiso_region <- regions[regions$Region == "Región de Valparaíso", ]

# Validate and fix region geometries if needed
if (!all(st_is_valid(valparaiso_region))) {
  valparaiso_region <- st_make_valid(valparaiso_region)
}

# Filter the region based on bounding box (spatial extent)
valparaiso_region_filtered <- valparaiso_region[
  st_bbox(valparaiso_region)[["xmin"]] < -80 &
    st_bbox(valparaiso_region)[["ymin"]] < -30, 
]

# Filter cities within the desired region
valparaiso_cities <- cities[cities$Region == "DE VALPARAISO", ]

# Convert city names to uppercase for matching
valparaiso_cities$Nombre <- toupper(valparaiso_cities$Nombre)

# Filter cities by names
city_patterns <- toupper(c(
  "Las Cruces", "Quisco", "Algarrobo", "Horcon",
  "Molles", "La Ballena", "Cachagua", "Maitencillo",
  "Papudo", "Valparaíso", "Quintero"
))

city_names <- c(
  "LAS CRUCES", "EL QUISCO", "ALGARROBO", "ISLOTE HORCONES",
  "LOS MOLLES", "ESTERO DE LA BALLENA", "CACHAGUA", "MAITENCILLO",
  "PAPUDO", "VALPARAÍSO", "QUINTERO"
)
valparaiso_cities_filtered <- valparaiso_cities[valparaiso_cities$Nombre %in% city_names, ]

# Add coordinates to the city dataset
city_coords <- st_coordinates(valparaiso_cities_filtered)
valparaiso_cities_filtered <- cbind(valparaiso_cities_filtered, city_coords)

# Filter cities based on longitude and latitude
valparaiso_cities_filtered <- valparaiso_cities_filtered[
  valparaiso_cities_filtered$X < -80 & valparaiso_cities_filtered$Y < -30, 
]

# Plot the filtered map
ggplot() +
  # Plot filtered region polygons only
  geom_sf(data = valparaiso_region_filtered, fill = "lightblue", color = "black") +
  
  # Plot filtered city points
  geom_sf(data = valparaiso_cities_filtered, aes(geometry = geometry), color = "red", size = 2) +
  
  # Annotate city names
  geom_text(
    data = valparaiso_cities_filtered, 
    aes(x = X, y = Y, label = Nombre), 
    size = 3, vjust = -1
  ) +
  
  # Add labels and minimal theme
  labs(
    title = "Filtered Region and Cities in Valparaíso 2",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()
