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

# Mismo código de arriba pero por arte de pesca
#fh2024 <- gfw_ais_fishing_hours(
#  spatial_resolution = "HIGH",
#  temporal_resolution = "YEARLY",
#  start_date = "2024-01-01",
#  end_date   = "2024-12-31",
#  group_by = "GEARTYPE",
#  region = aoi,
#  region_source = "USER_SHAPEFILE") |> 
#  filter(geartype == "tuna_purse_seines")

# 2. Tablas raster (usando la misma grilla para todos)
# Renombramos la columna
col_esfuerzo <- "Apparent Fishing Hours"

# Hacemos una pequeña función
tabla_a_raster <- function(df, plantilla = NULL) {
  puntos <- df |>
    dplyr::transmute(
      x = Lon,
      y = Lat,
      value = .data[[col_esfuerzo]]
    )
  r0 <- terra::rast(puntos, type = "xyz", crs = "EPSG:4326")
  if (is.null(plantilla)) {
    return(r0)
  } else {
    return(terra::resample(r0, plantilla, method = "bilinear"))
  }
}

# Convertimos cada año a raster (hay que asegurarnos de tener fh20xx cargados)
r2021 <- tabla_a_raster(fh2021)          # crea plantilla
r2022 <- tabla_a_raster(fh2022, r2021)   # usa plantilla 2021
r2023 <- tabla_a_raster(fh2023, r2021)
r2024 <- tabla_a_raster(fh2024, r2021)

# Unir en un solo SpatRaster con 4 capas
fh_stack <- c(r2021, r2022, r2023, r2024)
names(fh_stack) <- c("fh2021", "fh2022", "fh2023", "fh2024")

plot(fh_stack)

# Raster con el total 2021–2024
fh_total <- sum(fh_stack, na.rm = TRUE)  # suma capa a capa
names(fh_total) <- "fh_total_2021_2024"

# 3. Reescalamos fh_total a 1x1 grado, recortamos y exportamos
lonMin <- -150
lonMax <- -71
latMin <- -29
latMax <-  33

# Raster plantilla 1 x 1 grado
r_1deg <- rast(extent = c(lonMin, lonMax, latMin, latMax),
  res = 1,
  crs = "EPSG:4326")

values(r_1deg) <- NA  # opcional

fh_total_1deg <- resample(x = fh_total, y = r_1deg,
  method = "bilinear")

plot(fh_total)
plot(fh_total_1deg)

# Polígono para recorte (usamos el mismo polígono)
polig <- readRDS(file.path(ruta_data, "poligono.rds"))

# Convertir matriz de coords a SpatVector de polígonos
polig_v <- terra::vect(polig, type = "polygons", crs  = "EPSG:4326")

# Recorte + máscara al polígono
fh_total_1deg_crop <- terra::crop(fh_total_1deg, polig_v)
fh_total_1deg_crop <- terra::mask(fh_total_1deg_crop, polig_v)

# plot(fh_total_1deg_crop, main = "fh_total 1x1 recortado")

# Exportar reescalado + TIF
tif_file <- file.path(ruta_data, "fh_total_1deg_recortado.tif")

terra::writeRaster(fh_total_1deg_crop,
  tif_file, overwrite = TRUE)
