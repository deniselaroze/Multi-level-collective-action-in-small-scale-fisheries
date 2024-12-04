library(sf)
library(ggplot2)
library(dplyr) # For data manipulation
library(ggrepel)


rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Geo"




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

# Filter cities within the desired region
valparaiso_cities <- cities[cities$Region == "DE VALPARAISO", ]

# Convert city names to uppercase for matching
valparaiso_cities$Nombre <- toupper(valparaiso_cities$Nombre)

# Filter cities by names and refine with additional conditions
city_patterns <- c(  
  "LAS CRUCES", "EL QUISCO", "ALGARROBO", "HORCÓN",
  "LOS MOLLES", "ESTERO DE LA BALLENA", "CACHAGUA", "MAITENCILLO",
  "PAPUDO", "VALPARAÍSO", "QUINTERO"
)
valparaiso_cities_filtered <- valparaiso_cities[
  valparaiso_cities$Nombre %in% city_patterns & 
    !(valparaiso_cities$Clase_Topo %in% c("Elementos del Relieve", "Comuna")) & 
    (valparaiso_cities$Comuna != "LOS ANDES") & 
    !(valparaiso_cities$Nombre == "LOS MOLLES" & valparaiso_cities$Comuna != "LA LIGUA"),
]

# Add coordinates to the city dataset
city_coords <- st_coordinates(valparaiso_cities_filtered)
valparaiso_cities_filtered <- cbind(valparaiso_cities_filtered, city_coords)

#Clean up names and add duplicated for extra sessions
valparaiso_cities_filtered <- valparaiso_cities_filtered %>%
  mutate(Nombre = if_else(Nombre == "ESTERO DE LA BALLENA", "LA BALLENA", Nombre))
horcon_row <- valparaiso_cities_filtered[valparaiso_cities_filtered$Nombre == "HORCÓN", ]
horcon_row <- horcon_row %>%
  mutate(Nombre = "HORCÓN 2")
quintero_row <- valparaiso_cities_filtered[valparaiso_cities_filtered$Nombre == "QUINTERO", ]
quintero_row <- quintero_row %>%
  mutate(Nombre = "QUINTERO EL MANZANO")

valparaiso_cities_filtered <- bind_rows(valparaiso_cities_filtered, horcon_row, quintero_row)


# Jitter coordinates for overlapping points
set.seed(123) # Ensure reproducibility


valparaiso_cities_filtered <- valparaiso_cities_filtered %>%
  mutate(
    X_jitter = X + rnorm(n(), mean = 0, sd = 0.02), # Increased jitter range
    Y_jitter = Y + rnorm(n(), mean = 0, sd = 0.02)
  )


# Assign colors to each city name
valparaiso_cities_filtered <- valparaiso_cities_filtered %>%
  mutate(Color = case_when(
    Nombre %in% c("LAS CRUCES", "EL QUISCO") ~ "Session 1 and 2",
    Nombre %in% c("ALGARROBO", "HORCÓN") & !duplicated(Nombre) ~ "Session 3",
    Nombre %in% c("VALPARAÍSO", "QUINTERO", "QUINTERO EL MANZANO") ~ "Session 4",
    Nombre %in% c("LOS MOLLES", "LA BALLENA", "PAPUDO") ~ "Session 5",
    Nombre %in% c("HORCÓN 2", "CACHAGUA", "MAITENCILLO") ~ "Session 6"
  ))


#table(valparaiso_cities_filtered$Nombre)


# Calculate the last third of the x-axis range
x_min <- st_bbox(valparaiso_region)$xmin
x_max <- st_bbox(valparaiso_region)$xmax
x_last_bit <- c(x_min + 9.5 * (x_max - x_min) / 10, x_max)

# Calculate the first quarter of the y-axis range
y_min <- st_bbox(valparaiso_region)$ymin
y_max <- st_bbox(valparaiso_region)$ymax
y_first_quarter <- c(y_min, y_min + (y_max - y_min) / 4)

# Convert Color to a factor for better control over color mapping
valparaiso_cities_filtered$Color <- factor(valparaiso_cities_filtered$Color, 
                                           levels = c(
                                             "Session 1 and 2", 
                                             "Session 3", 
                                             "Session 4", 
                                             "Session 5", 
                                             "Session 6"
                                           ))

# Updated Plot with geom_text_repel (no box around city names)
ggplot() +
  # Plot filtered region polygons only
  geom_sf(data = valparaiso_region, fill = "wheat1", color = "black") +
  
  # Plot filtered city points with colors
  geom_point(
    data = valparaiso_cities_filtered, 
    aes(x = X_jitter, y = Y_jitter, color = Color), 
    size = 2
  ) +
  
  # Annotate city names using geom_text_repel (no box, colored text)
  geom_text_repel(
    data = valparaiso_cities_filtered, 
    aes(x = X_jitter, y = Y_jitter, label = Nombre, color = Color),
    size = 3, # Text size
    fontface = "bold", # Bold text
    show.legend = FALSE,
    box.padding = 0.5, # Padding around text for clearer spacing
    point.padding = 0.5, # Padding around points
    segment.color = "grey50" # Line color for segments
  ) +
  
  # Add zoomed limits for the last 1/3 of the x-axis and last 1/4 of the y-axis
  coord_sf(xlim = x_last_bit, ylim = y_first_quarter) +
  
  # Apply ColorBrewer RdBu palette
  scale_color_brewer(palette = "Set1") +
  
  # Add labels and minimal theme
  labs(
    title = "Locations included in the sample",
    x = "Longitude",
    y = "Latitude",
    color = "Sessions"
  ) +
  
  # Customize text elements for larger, bold font
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # Title
    axis.title = element_text(size = 14, face = "bold"), # Axis titles
    axis.text = element_text(size = 12), # Axis text
    legend.title = element_text(size = 12, face = "bold"), # Legend title
    legend.text = element_text(size = 12) # Legend text
  )





  
  
  
  
  