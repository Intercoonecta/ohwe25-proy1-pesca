# 1. Script para descargar datos de GFW y convertimos a raster por año
library(gfwr)
library(sf)
library(dplyr)
library(terra)
library(satin)  # para mantener coherencia con tu flujo anterior

## https://globalfishingwatch.org/our-apis/tokens
# usethis::edit_r_environ()  # solo la primera vez para poner GFW_TOKEN en .Renviron

# GFW_TOKEN = "PASTE_YOUR_TOKEN_HERE"
key <- gfw_auth()
# key <- Sys.getenv("GFW_TOKEN")

# Ruta base (ajusta si es necesario)
ruta_base <- "C:/Users/jeuee/Documents/1.Cursos/7.Hackaton2025/ohwe25-proy1-pesca"
ruta_data <- file.path(ruta_base, "data")

# Cargamos el polígono (para el AOI de GFW)
coords <- readRDS(file.path(ruta_data, "poligono.rds"))

# Aseguramos que sea matriz numérica
coords_mat <- as.matrix(coords)

# Tomamos las dos primeras columnas como (lon, lat)
poly_coords <- coords_mat[, 1:2, drop = FALSE]

# Cerramos polígono
if (!all(poly_coords[1, ] == poly_coords[nrow(poly_coords), ])) {
  poly_coords <- rbind(poly_coords, poly_coords[1, ])
}

# Construimos el polígono sf en WGS84
poly_sfc <- st_polygon(list(poly_coords)) |>
  st_sfc(crs = 4326)

aoi <- st_sf(geometry = poly_sfc)

#Aquí descargamos los datos año por año (solamente hay quue modificar el nombre de la variable y las fechas)
fh2024 <- gfw_ais_fishing_hours(
   spatial_resolution = "HIGH",
   temporal_resolution = "YEARLY",
   start_date = "2024-01-01",
   end_date   = "2024-12-31",
   region = aoi,
   region_source = "USER_SHAPEFILE")

fh_todos <- bind_rows(fh2021 |> mutate(year = 2021),
                      fh2022 |> mutate(year = 2022),
                      fh2023 |> mutate(year = 2023),
                      fh2024 |> mutate(year = 2024))

#head(fh_todos)

write.csv(fh_todos, file = "fh_2021_2024_todos.csv",
          row.names = FALSE)
