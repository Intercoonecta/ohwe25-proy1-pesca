# Cargar librerías necesarias
library(dplyr)
library(tidyr)

# Cargar los datos
datos <- read.csv("Tiburones depurados.csv", sep = ";")

# Transformar los datos - desagregar especies y filtrar solo PRESENCIAS
datos_largos <- datos %>%
  pivot_longer(
    cols = c(BSHn, CCLn, FALn, OCSn, SEPNn, SPLn, SPZn),
    names_to = "Especie",
    values_to = "Valor"
  ) %>%
  # Filtrar solo los registros donde hay presencia (valor > 0)
  filter(Valor > 0) %>%
  # Seleccionar y ordenar las columnas como quieres
  select(Especie, Year, Month, Flag, Lat, Lon, Valor)

# Ver los resultados
head(datos_largos)
print(paste("Número de registros con presencia:", nrow(datos_largos)))

# Guardar en CSV - SOLO PRESENCIAS
write.csv(datos_largos, "presencias.csv", row.names = FALSE)
