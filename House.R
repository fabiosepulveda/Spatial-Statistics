# Cargar las librerías
library(spgwr)
library(GWmodel)
library(sp)
library(sf)
library(dplyr)
library(devtools)
library(EDAWR)
library(mapview)
library(viridis)

setwd("C:/Users/USUARIO/Documents/Shape.Bucaramanga/Delitos/Seminario")
datos <- read.csv("House_limpio.csv")
head(datos)

# Seleccionar solo las columnas que vamos a usar
datos_gwr <- datos %>%
  select(price, bedrooms, bathrooms, sqft_living, lat, long) %>%
  na.omit() # Eliminar cualquier fila con valores faltantes

# Muestra
set.seed(123)  # Para reproducibilidad
muestra <- datos_gwr[sample(1:nrow(datos_gwr), size = 1000), ]

head(muestra)
summary(muestra)

# Convertir el dataframe a un objeto espacial (SpatialPointsDataFrame)
coordenadas <- muestra %>%
  select(long, lat)

datos_sp <- SpatialPointsDataFrame(
  coords = coordenadas,
  data = muestra,
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

#Visualización
mapview(datos_sp)
mapview(datos_sp, zcol = "sqft_living", legend = TRUE)

# Proyectar a UTM Zona 10N para distancias correctas
datos_utm <- spTransform(datos_sp, CRS("+proj=utm +zone=10 +datum=WGS84 +units=m"))
class(datos_utm)
summary(datos_utm)

# --- Correr el modelo GWR --- #
# Determinar el ancho de banda óptimo usando el criterio AICc
ancho_banda <- gwr.sel(
  formula = price ~ sqft_living + bedrooms + bathrooms,
  data = datos_utm,
  gweight = gwr.Gauss,
  adapt = T
)

# Ajustar el modelo GWR
modelo_gwr <- gwr(
  formula = price ~ sqft_living + bedrooms + bathrooms,
  data = datos_utm,
  adapt = ancho_banda,
  gweight = gwr.Gauss
  )

# --- Visualizar e interpretar los resultados --- #
# Añadir los coeficientes del GWR a tu dataframe espacial
datos_utm@data <- cbind(datos_utm@data, as.data.frame(modelo_gwr$SDF))

# Convertir a 'sf' para una visualización más fácil
datos_sf <- st_as_sf(datos_utm)

# Mapear el coeficiente de 'sqft_living' para ver su variación espacial
mapview(datos_sf, zcol = "sqft_living.1", legend = TRUE,
          layer.name = "Coeficientes locales sqft_living",
          col.regions = viridis::viridis(100))



