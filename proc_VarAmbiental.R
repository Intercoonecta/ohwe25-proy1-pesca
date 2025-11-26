## Procesamiento de variables ambientales descargadas de Copernicus

# Los límites geográficos usados al moemnto de la descarga fueron:
#   Longitud: -150 a -71
#   Latitud: -29 a 33
# Resolución temporal: mensual, de enero de 2021 a dicimebre de 2024
# Base de datos: GLOBAL_MULTIYEAR_PHY_001_030 (https://doi.org/10.48670/moi-00021)
#
# En este ejemplo se usa un archivo nc con una sola variable, pero puede contener más de una
#
# 1. Importar archivo nc con paquete 'satin'
# 2. Calcular climatología y guardar el promedio
# 3. Crear un raster vacío con la extensión y resolución deseada (1 grado) para 
#    que coincida con la resolución de la base de la CIAT
# 4. Reescalar la variable ambiental al raster vacío usando el paquete 'terra'
# 5. Exportar la variable ambiental reescalada en formato .asc con la función 'satin2asc'


# paquetes
 library(satin)
 library(terra)

# Función satin2asc
 satin2asc <- function(X, slice = 2, file){
   if (!inherits(X, "satin")) 
     stop("need object of class 'satin'")
   
   # Header
   sz <- X@lat[2] - X@lat[1]
   z <- X@data[ , , slice] 
   dims <- dim(z)
   nc <- dims[2] 
   nr <- dims[1]
   head <- data.frame(
     nom = c("NCOLS", "NROWS", "XLLCORNER", "YLLCORNER", 
             "CELLSIZE", "NODATA_value"), 
     val = c(nc, nr, min(X@lon) - sz/2, min(X@lat) - sz/2, sz, -999))
   write.table(head, file, sep = " ", col.names = FALSE, 
               row.names = FALSE, quote = FALSE)
   
   # Data. Usually the average, therefore "slice = 2", because we expect
   # X was obtained from climatology().
   z[is.na(z)] <- -999
   z <- z[nrow(z):1, ]
   write.table(z, file, sep = " ", append = TRUE, col.names = FALSE, 
               row.names = FALSE)
   cat("file", file, "has been written to", getwd(), "folder.")
 }
 
# 1. Importar archivo nc con paquete 'satin' 
 
# Variable ambiental descargada de Copernicus
 ruta <- "D:/ohwe2025/nc/"
 nc <- "cmems_mod_glo_phy_my_0.083deg_P1M-m_1764099561632.nc"

 va <- read.cmems(paste(ruta, nc, sep =""))
 vn <- va@attribs$name

# 2. Calcular climatología y guardar el promedio 
 va.c <- climatology(va) 
 va.m <- va.c
 va.m@data <- va.c@data[ , , 2]
 dim(va.m@data) <- c(dim(va.m@data), 1)
 rm(va.c)
 # saveRDS(va.m, paste(ruta, vn, ".rds", sep ="")) # opcional

# 3. Crear un raster vacío con la extensión (tomada de base) y resolución deseada
 lonMin <- -149.5
 lonMax <- -71.5
 latMin <- -28.5
 latMax <- 32.5
 
 r <- rast(extent = c(lonMin, lonMax, latMin, latMax), res = 1)
 # Asignar valores NA
 values(r) <- NA
 
# 4. Reescalar la variable ambiental al raster vacío usando el paquete 'terra'
 # copia de la estructura para guardar datos reescalados
 va.r <- va.m
 va.r@lon <- seq(lonMin, lonMax, 1)
 va.r@lat <- seq(latMin, latMax, 1)
 
 # extent
 xt2 <- ext(c(range(va.r@lon), range(va.r@lat)))
 # reescalar
 a <- va.r@data[ , , 1]
 a <- rast(a, extent = xt2)
 b <- resample(x = a, y = r)
 va.r@data <- as.matrix(b, wide = TRUE)

 dim(va.r@data) <- c(dim(va.r@data), 1)

 plot(va.m) # original
 plot(va.r) # reescalado

 saveRDS(va.r, paste(ruta, vn, "_res_1x1.rds", sep ="")) # opcional
 satin2asc(va.r, slice = 1, file = paste(ruta, vn, ".asc", sep = "")) 
 