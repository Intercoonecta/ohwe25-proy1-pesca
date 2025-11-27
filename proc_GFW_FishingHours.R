fh <- read.csv("D:/ohwe2025/fh_2021_2024.csv")

# En el data.frame fh, generar columnas lat.e y lon.e a partir de Lat y Lon pero descartando decimales. Poner en formato de matriz los datos de Apparent.Fishing. Hours de cuardo con lon.e (en columnas) y lat.e (en filas). Graficar como imagen

fh$lat.e <- floor(fh$Lat)
fh$lon.e <- floor(fh$Lon)
library(reshape2)
fh_matrix <- tapply(fh$Apparent.Fishing.Hours, list(fh$lon.e, fh$lat.e), mean, na.rm = TRUE)

image(fh_matrix)

# quitar renglones y columnas extra
dim(fh_matrix)
fh_mat <- fh_matrix[2:79, 2:62]

# Función matrix2asc
matrix2asc <- function(X, file){
  if (!inherits(X, "matrix")) 
    stop("need object of class 'matrix'")
  
  lat <- as.numeric(colnames(X))
  lon <- as.numeric(rownames(X))
  
  # Header
  sz <- lat[2] - lat[1]
  z <- t(X[order(1:nrow(X), decreasing = FALSE), ])
  dims <- dim(z)
  nc <- dims[2] 
  nr <- dims[1]
  head <- data.frame(
    nom = c("NCOLS", "NROWS", "XLLCORNER", "YLLCORNER", 
            "CELLSIZE", "NODATA_value"), 
    val = c(nc, nr, min(lon) - sz/2, min(lat) - sz/2, sz, -999))
  write.table(head, file, sep = " ", col.names = FALSE, 
              row.names = FALSE, quote = FALSE)
  
  # 
  z[is.na(z)] <- -999
  z <- z[nrow(z):1, ]
  write.table(z, file, sep = " ", append = TRUE, col.names = FALSE, 
              row.names = FALSE)
  cat("file", file, "has been written to", getwd(), "folder.")
}


matrix2asc(fh_mat, "fh_apparent_fishing_hours.asc")
