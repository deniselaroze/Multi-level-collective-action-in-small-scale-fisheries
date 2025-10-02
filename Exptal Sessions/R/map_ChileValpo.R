library(sf)
library(ggplot2)
library(dplyr) # For data manipulation
library(ggrepel)
library(viridis)
library(patchwork) # For combining plots
library(ggspatial) # For adding scale bars and north arrows

rm(list=ls())
# --- User Configuration ---
# NOTE: Please update these paths to match the locations on your computer.
# Path to the folder where you want to save the output map.
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
# Path to the folder containing the geographic shapefiles.
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Geo"
# --- End of User Configuration ---


# 1. --- Data Loading and Preparation ---

# Construct full paths to the shapefiles
region_shp_path <- paste0(path_datos, "/Regional.shp")
cities_shp_path <- paste0(path_datos, "/Toponimos.shp")

# Load shapefiles into R
regions <- st_read(region_shp_path)
cities <- st_read(cities_shp_path)

# Ensure both datasets use the standard WGS84 coordinate reference system (CRS)
regions <- st_transform(regions, crs = 4326)
cities <- st_transform(cities, crs = 4326)

# Clean up geometries in the main regions shapefile to prevent potential errors
if (!all(st_is_valid(regions))) {
  regions <- st_make_valid(regions)
}


# 2. --- Filtering and Cleaning Spatial Data ---

# Filter for "Region de Valparaíso"
valparaiso_region <- regions[regions$Region == "Región de Valparaíso", ]

# This check is now slightly redundant but is good practice to keep
if (!all(st_is_valid(valparaiso_region))) {
  valparaiso_region <- st_make_valid(valparaiso_region)
}

# Filter cities that are within the Valparaíso region
valparaiso_cities <- cities[cities$Region == "DE VALPARAISO", ]

# Convert city names to uppercase for consistent matching
valparaiso_cities$Nombre <- toupper(valparaiso_cities$Nombre)

# Define the specific cities/towns of interest for the study
city_patterns <- c(  
  "LAS CRUCES", "EL QUISCO", "ALGARROBO", "HORCÓN",
  "LOS MOLLES", "ESTERO DE LA BALLENA", "CACHAGUA", "MAITENCILLO",
  "PAPUDO", "VALPARAÍSO", "QUINTERO"
)

# Apply filters to get the final list of cities
valparaiso_cities_filtered <- valparaiso_cities[
  valparaiso_cities$Nombre %in% city_patterns & 
    !(valparaiso_cities$Clase_Topo %in% c("Elementos del Relieve", "Comuna")) & 
    (valparaiso_cities$Comuna != "LOS ANDES") & 
    !(valparaiso_cities$Nombre == "LOS MOLLES" & valparaiso_cities$Comuna != "LA LIGUA"),
]

# Extract and add X/Y coordinates to the city data frame
city_coords <- st_coordinates(valparaiso_cities_filtered)
valparaiso_cities_filtered <- cbind(valparaiso_cities_filtered, city_coords)

# Clean up names and create duplicates for locations with multiple sessions
valparaiso_cities_filtered <- valparaiso_cities_filtered %>%
  mutate(Nombre = if_else(Nombre == "ESTERO DE LA BALLENA", "LA BALLENA", Nombre))

# Duplicate Horcón for a second session
horcon_row <- valparaiso_cities_filtered[valparaiso_cities_filtered$Nombre == "HORCÓN", ]
horcon_row <- horcon_row %>%
  mutate(Nombre = "HORCÓN 2")

# Duplicate Quintero for another session group
quintero_row <- valparaiso_cities_filtered[valparaiso_cities_filtered$Nombre == "QUINTERO", ]
quintero_row <- quintero_row %>%
  mutate(Nombre = "QUINTERO EL MANZANO")

# Combine original and duplicated rows
valparaiso_cities_filtered <- bind_rows(valparaiso_cities_filtered, horcon_row, quintero_row)

# --- ROBUST FIX: Filter for Santiago using a more flexible method ---
santiago <- cities[grepl("SANTIAGO", toupper(cities$Nombre)) & grepl("METROPOLITANA", toupper(cities$Region)), ]
# Take the first match if there are multiple
if (nrow(santiago) > 1) {
  santiago <- santiago[1, ]
}
# ---


# 3. --- Data for Visualization ---

# Jitter coordinates for overlapping points to prevent labels from colliding
set.seed(123) # for reproducibility
valparaiso_cities_filtered <- valparaiso_cities_filtered %>%
  mutate(
    X_jitter = X + rnorm(n(), mean = 0, sd = 0.02),
    Y_jitter = Y + rnorm(n(), mean = 0, sd = 0.02)
  )

# Assign a session group to each city for color-coding
valparaiso_cities_filtered <- valparaiso_cities_filtered %>%
  mutate(Color = case_when(
    Nombre %in% c("LAS CRUCES", "EL QUISCO") ~ "Session 1 and 2",
    Nombre %in% c("ALGARROBO", "HORCÓN") & !duplicated(Nombre) ~ "Session 3",
    Nombre %in% c("VALPARAÍSO", "QUINTERO", "QUINTERO EL MANZANO") ~ "Session 4",
    Nombre %in% c("LOS MOLLES", "LA BALLENA", "PAPUDO") ~ "Session 5",
    Nombre %in% c("HORCÓN 2", "CACHAGUA", "MAITENCILLO") ~ "Session 6"
  ))

# Convert the 'Color' column to a factor for ordered legend
valparaiso_cities_filtered$Color <- factor(valparaiso_cities_filtered$Color, 
                                           levels = c(
                                             "Session 1 and 2", 
                                             "Session 3", 
                                             "Session 4", 
                                             "Session 5", 
                                             "Session 6"
                                           ))

# Define the geographic bounding box to zoom in on the relevant coastal area
x_min <- st_bbox(valparaiso_region)$xmin
x_max <- st_bbox(valparaiso_region)$xmax
x_last_bit <- c(x_min + 9.5 * (x_max - x_min) / 10, x_max)

y_min <- st_bbox(valparaiso_region)$ymin
y_max <- st_bbox(valparaiso_region)$ymax
y_top_limit <- y_min + (y_max - y_min) / 4

# Add a 5% buffer to the top of the y-axis to make space for the title
y_buffer <- (y_top_limit - y_min) * 0.05 
y_limits_with_buffer <- c(y_min, y_top_limit + y_buffer)


# 4. --- Create the Main Map (Zoomed In) ---

main_map <- ggplot() +
  # Plot the Valparaíso region with a light gray fill
  geom_sf(data = valparaiso_region, fill = "gray90", color = "black") +
  
  # Add the city points, colored by session
  geom_point(
    data = valparaiso_cities_filtered, 
    aes(x = X_jitter, y = Y_jitter, color = Color), 
    size = 2
  ) +
  
  # Add city name labels that repel each other to avoid overlap
  geom_text_repel(
    data = valparaiso_cities_filtered, 
    aes(x = X_jitter, y = Y_jitter, label = Nombre, color = Color),
    size = 3,
    fontface = "bold",
    show.legend = FALSE,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = "grey50"
  ) +
  
  # Add a scale bar to the bottom-right
  annotation_scale(location = "br", width_hint = 0.4, style = "ticks") +
  
  # Set the map's coordinate limits to the zoomed-in area with the new buffer
  coord_sf(xlim = x_last_bit, ylim = y_limits_with_buffer, expand = FALSE) +
  
  # Apply a color-blind friendly Viridis color palette
  scale_color_viridis_d(begin = 0, end = 0.9) +
  
  # Add titles and labels
  labs(
    x = "Longitude",
    y = "Latitude",
    color = "Sessions"
  ) +
  
  # Apply a minimal theme and customize text elements for readability
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )

# 5. --- Create the Context Map (Full Chile) ---

# Create a matrix of the corner coordinates of the zoom box
box_coords <- matrix(c(
  x_last_bit[1], y_limits_with_buffer[1], # bottom-left
  x_last_bit[2], y_limits_with_buffer[1], # bottom-right
  x_last_bit[2], y_limits_with_buffer[2], # top-right
  x_last_bit[1], y_limits_with_buffer[2], # top-left
  x_last_bit[1], y_limits_with_buffer[1]  # close the polygon
), ncol = 2, byrow = TRUE)

# Create a simple feature (sfc) polygon object from the coordinates
zoom_box <- st_sfc(st_polygon(list(box_coords)), crs = 4326)

# --- ROBUST METHOD to find panel edges ---
# Create a bounding box for cropping that excludes oceanic islands
crop_bbox <- st_bbox(c(xmin = -82, xmax = -66, ymin = -56, ymax = -17), crs = st_crs(regions))
# Use st_intersection which is more stable than st_crop
continental_chile <- st_intersection(regions, st_as_sfc(crop_bbox))
# Get the precise y-coordinates of the visible continental landmass
panel_bbox <- st_bbox(continental_chile)
panel_ymax <- panel_bbox$ymax
panel_ymin <- panel_bbox$ymin


context_map <- ggplot() +
  # Draw all regions of Chile in a light gray
  geom_sf(data = regions, fill = "gray80", color = "white", size = 0.2) +
  # Highlight the Valparaíso region in a distinct color to make it stand out
  geom_sf(data = valparaiso_region, fill = "#440154FF", color = "black") + 
  # Add the black rectangle showing the zoomed-in area
  geom_sf(data = zoom_box, fill = NA, color = "black", size = 1) +
  
  # Add a red dot for Santiago
  geom_point(data = santiago, 
             aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]), 
             color = "red", 
             size = 3, 
             shape = 18) + # Using shape 18, a diamond
  
  # --- Add a text label for Santiago and move it to the left ---
  geom_text(data = santiago,
            aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]),
            label = "SANTIAGO",
            color = "black",
            fontface = "bold",
            size = 3,
            nudge_x = -1.5, # Nudge the label to the LEFT
            hjust = "right") + # Align text to the right
  # ---
  
  # --- Add angled connecting lines to calculated panel edges ---
  # Top line
  annotate(
    "segment",
    x = x_last_bit[1], y = y_limits_with_buffer[2], # Top-left of box
    xend = -Inf, yend = panel_ymax,               # Extend to top-left of panel
    color = "black", size = 0.8
  ) +
  # Bottom line
  annotate(
    "segment",
    x = x_last_bit[1], y = y_limits_with_buffer[1], # Bottom-left of box
    xend = -Inf, yend = panel_ymin,               # Extend to bottom-left of panel
    color = "black", size = 0.8
  ) +
  
  # Set coordinate limits to crop out distant oceanic islands
  # and add 'clip = "off"' to allow lines to be drawn outside the plot panel
  coord_sf(xlim = c(-82, -66), expand = FALSE, clip = "off") +
  
  # Remove all theme elements for a clean look
  theme_void() +
  # Add a margin to the left of the context map to make space for the lines
  theme(plot.margin = margin(l = 20))


# 6. --- Combine Maps Side-by-Side and Save ---

# Use the '+' operator from patchwork to place plots side-by-side
combined_map <- main_map + context_map +
  plot_layout(widths = c(1.5, 1)) 

# Add a single, overarching title for the combined plot
combined_map_with_title <- combined_map + 
  plot_annotation(title = 'Locations included in the sample',
                  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))


# Display the final combined map
print(combined_map_with_title)

# Save the combined map to a file
ggsave(file = paste0(path_github, "Outputs/Map_with_zoom_lines.png"), plot = combined_map_with_title, device = "png", width = 10, height = 8)

