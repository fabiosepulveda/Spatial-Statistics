library(tidyverse)
library(sf)
library(sp)
library(tmap)
library(tmaptools)
library(grid)
library(gridExtra)
library(ggplot2)
library(spdep)
library(spgwr)


setwd("C:\\Users\\USUARIO\\Documents\\Econo. Espacial")

#Importar Datos
Census.Data <-read.csv("Censo_Camden.csv")
head(Census.Data)

#Estadísticos Descriptivos
summary(Census.Data)
hist(Census.Data$Unemployed,breaks=20, col= "blue", main="% in full-time employment", xlab="Percentage")

#Importar Mapa
Output.Areas2 <- read_sf("Camden_oa11.shp")

#Merge
OA.Census <- merge(Output.Areas2, Census.Data, by.x="OA11CD", by.y="OA")

#Transformar a objeto sf
OA.Census2 <- st_as_sf(OA.Census)
plot(OA.Census2)

#Tarea: Visualizar la distribución espacial de las variables


#--------------Regresión---------------#

#Modelo convencional
#comprender la relación global 
#entre las variables en el área de estudio.

model <- lm(Qualification ~ Unemployed + White_British, data = Census.Data)
summary(model)

#Analisis de los residuales
#Moran's I, Ho:no existe autocorrelación espacial
resids <- residuals(model)

#Agregarlo
map.resids <- cbind(OA.Census, resids)
head(map.resids)

nb <- poly2nb(map.resids, queen = TRUE) # queen shares point or border
nbw <- nb2listw(nb, style = "W")

gmoran <- moran.test(map.resids$resids, nbw,
                     alternative = "two.sided")
gmoran

#Tarea: Hacer el mapa distribucion espacial de los residuos

#Modelo Geograficamente ponderado: GWR
#calcular el Ancho de banda
X1 <- as(OA.Census2, "Spatial")
GWRbandwidth <- gwr.sel(Qualification ~ Unemployed + White_British, data = X1, adapt = T)

gwr.model = gwr(Qualification ~ Unemployed + White_British,
                data = X1,
                adapt=GWRbandwidth,
                hatmatrix=TRUE,
                se.fit=TRUE) 

gwr.model

#Crear una dataframe con los resultados
results <- as.data.frame(gwr.model$SDF)
names(results)

gwr.map <- cbind(X1, as.matrix(results))
gwr.map2 <- st_as_sf(gwr.map)

#Visualización del R2
ggplot(gwr.map2) +
  geom_sf(aes(fill = localR2), color = NA) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(title = "Proporción de varianza explicada en cada ubicación",
       subtitle = "Valores del R2",
       fill = "R2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )

ggplot(data = gwr.map2) +
  geom_sf(aes(fill = localR2)) +
  scale_color_viridis_c(option = "plasma") +
  labs(title = "Proporción de varianza explicada en cada ubicación",
       color = "R2") +
  theme_minimal()

qtm(gwr.map2, fill = "localR2")

#Visualización coeficientes locales de White_British
ggplot(gwr.map2) +
  geom_sf(aes(fill = White_British.1), color = NA) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(title = "Mapa de White_British",
       subtitle = "Coeficientes locales",
       fill = "White_British") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )


