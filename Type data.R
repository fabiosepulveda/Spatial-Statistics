#------Visualización---------#

# Librerias
library(sp)
library(ggplot2)
library(GWmodel)
library(mapview)
library(leaflet)
library(hpiR)
library(dplyr)
library(rgdal)
library(sf)
library(ggspatial)
library(RColorBrewer)
library(classInt)


#Puntos
data(meuse)
coordinates(meuse) = c("x", "y")
proj4string(meuse) <- CRS("+init=epsg:28992")
plot(meuse, pch=1, main="Puntos",col="red") 

methods(fortify)
m = as(meuse, "data.frame")
class(m)
ggplot(m, aes(x, y)) + geom_point() + coord_equal()

ggplot(m, aes(x,y)) + geom_point(aes(color = zinc), size = 2, alpha = 3/4) +
  ggtitle("Zinc concentration (ppm)") + coord_equal() + theme_bw()

#Líneas
cc = coordinates(meuse)
m.sl = SpatialLines(list(Lines(list(Line(cc)), "line1")))
plot(m.sl, main="Líneas") 

#Área
data(meuse.riv)
meuse.lst = list(Polygons(list(Polygon(meuse.riv)), "meuse.riv")) 
meuse.pol = SpatialPolygons(meuse.lst) 
plot(meuse.pol, col = "grey",main="Área")

data(DubVoter)
plot(Dub.voter, main="Área")

setwd("C:/Users/USUARIO/Documents/Shape.Bucaramanga")
Y <- st_read("Perimetro_Bucara.shp")
plot(Y, main="Perímetro de Bucaramanga")
X = st_read("Comunas.shp")
plot(X,main="Comunas de Bucaramanga")

#Visualización Combinada y aplicaciones
plot(meuse,pch=1)
plot(meuse.pol, col = "royalblue", add = TRUE)

################################
#--------Aplicaciones----------#
################################

mapview(meuse['zinc'])
spplot(meuse,"zinc",main="Distribución Espacial del Zinc")

#-------Salud Pública-------#
setwd("C:/Users/USUARIO/Documents/Shape.Bucaramanga/Delitos")
Datos = read.csv("Delitos_en_Bucaramanga_enero_2010_a_diciembre_de_2021.csv")
Datos = na.omit(Datos)
Datos$LATITUD = as.numeric(Datos$LATITUD)
Datos$LONGITUD = as.numeric(Datos$LONGITUD)
Datos = na.omit(Datos)
Homi = subset(Datos, CONDUCTA=="HOMICIDIO")

m <- leaflet(Homi) %>% addTiles() %>% setView(lng = -73.119, lat = 7.113, zoom = 12)%>%
  addCircles(lng = ~LONGITUD, lat = ~LATITUD)
m

#-----Economía:Precio Hedónico------#
data(ex_sales)
muestramia <- ex_sales %>%  sample_n(size=300,replace=FALSE)
coordinates(muestramia) = c("longitude","latitude")
wgs84 = "+proj=longlat +datum=WGS84 +no_defs"
proj4string(muestramia) <- CRS(wgs84)
plot(muestramia,pch=1)
mapview(muestramia['sale_price'])

#-----PIB--------#
tmp <- tempdir()
tmp <- tempdir()
url <- "http://personal.tcu.edu/kylewalker/data/mexico.zip"
file <- basename(url)
download.file(url, file)
unzip(file, exdir = tmp)
setwd("C:/Users/USUARIO/Documents/Shape.Bucaramanga/Delitos/mexico")
mexico <- st_read("mexico.shp")
mapview(mexico['gdp08'])

mexico1 = st_as_sf(mexico)
intervalo.mdm.16 = classIntervals(c(min(mexico1$gdp08) - .00001, mexico1$gdp08), n = 4, style="quantile")
new_shape.cuts.16 = mutate(mexico1, gdp08_cut = cut(gdp08, intervalo.mdm.16$brks))
ggplot(new_shape.cuts.16) + geom_sf(aes(fill=gdp08_cut)) + scale_fill_brewer(palette = "Blues")+ 
  xlab("Longitud") + ylab("Latitud") + ggtitle("PIB per-cápita en miles de pesos, 2008")+annotation_scale(style="ticks")+
  annotation_north_arrow(location='tr')

#-------CV-------------#
setwd("C:\\Users\\USUARIO\\Documents\\Documentos\\Curso Estadística Espacial\\Parciales")
D = read.csv("Indicador.csv")
coordinates(D)=~x+y
#proj4string(D) <- CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs")
mapview(D['Indicador'])

