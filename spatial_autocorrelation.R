#---------Autocorrelación Espacial-------------#

#---Directorio de trabajo----#
#Por defecto
getwd()

#Cambiar directorio de trabajo
setwd("C:/Users/USUARIO/Documents/Documentos/Curso Estadística Espacial/Ejemplo_AEDS_area_1")

#---Cargar Librerías-------#
library(spdep)
#library(maptools)
library(spData)
library(sf)
library(mapview)

#----Importar datos------#
X= read_sf("SA_classdata.shp")

head(X$PPOVERTY)

#---Otra Forma----#
X1 = st_as_sf(X)
ggplot(X1) + geom_sf(aes(fill=PPOVERTY))+ annotation_scale(style="ticks")+
  annotation_north_arrow(location='tl') + xlab("Longitud") + ylab("Latitud") + 
  ggtitle("Distribución Espacial de la Tasa de Pobreza")

mapview(X1, zcol = "PPOVERTY")

#----------- 1. Estructura de Vecindad --------------#
#
#---- a. Tipo Reina -----#
nb <- spdep::poly2nb(X1, queen = TRUE) # Construir lista de vecinos
plot(st_geometry(X1), border = "lightgray", main="Mapa de contactos usando criterio de contigüidad tipo reina")
plot.nb(nb, st_geometry(X1), add = TRUE)

#Izquierda: Mapa de vecinos basado en la contigüidad.
#Derecha: Mapa de vecinos del área 20 basado en la contigüidad.

id <- 20 # area id
X1$neighbors <- "other"
X1$neighbors[id] <- "area"
X1$neighbors[nb[[id]]] <- "neighbors"
ggplot(X1) + geom_sf(aes(fill = neighbors)) + theme_bw() +
  scale_fill_manual(values = c("gray30", "gray", "white"))


#---- b. k-vecinos (k = 3) -----#
coo <- st_centroid(X1)
nb <- knn2nb(knearneigh(coo, k = 3)) # k number nearest neighbors
plot(st_geometry(X1), border = "lightgray",main = "Mapa de contacto usando criterio de contigüidad 3 vecinos más cercanos")
plot.nb(nb, st_geometry(X1), add = TRUE)


#----------- 2. Construcción de la matriz de ponderaciones espacial  --------------#

#---- a. Tipo Reina -----#
nb <- poly2nb(X1, queen = TRUE)
nbw <- spdep::nb2listw(nb, style = "W")
nbw$weights[1:3]

#----------- 3. Prueba de autocorrelación espacial  --------------#
nb <- poly2nb(X1, queen = TRUE) # queen shares point or border
nbw <- nb2listw(nb, style = "W")

# Global Moran's I
gmoran <- moran.test(X1$PPOVERTY, nbw,
                     alternative = "two.sided")
gmoran

#Scatterplot
moran.plot(X1$PPOVERTY, nbw)
