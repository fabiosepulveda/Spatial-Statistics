#---------Estadísticos Resumen GP-----------#

#Instalar librerías
install.packages("GWmodel")
install.packages("RColorBrewer")

#Cargar Librerías
library(GWmodel)
library(RColorBrewer)

#Cargar datos
data("DubVoter")
?Dub.voter
class(Dub.voter)
head(Dub.voter@data)
dim(Dub.voter@data)

#Estimación de los estadísticos resumen GP
Estadísticos = gwss(Dub.voter, vars = c("GenEl2004", "LARent", "Unempl"), kernel = "bisquare", adaptive = TRUE, bw = 48)
Estadísticos

#Visualización de las estimaciones
map.na = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(329000, 261500), scale = 4000, col = 1)
map.scale.1 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(326500, 217000), scale = 5000, col = 1, fill = c("transparent", "blue"))
map.scale.2 = list("sp.text", c(326500, 217900), "0", cex = 0.9, col = 1)
map.scale.3 = list("sp.text", c(331500, 217900), "5km", cex = 0.9, col = 1)
map.layout = list(map.na, map.scale.1, map.scale.2, map.scale.3)
mypalette.1 = brewer.pal(8, "Reds")
mypalette.2 = brewer.pal(6, "Greens")

#Media geográficamente ponderada:
spplot(Estadísticos$SDF, "LARent_LM", key.space = "right", col.regions = mypalette.1, cuts = 7, main = "Media GP para LARent", sp.layout = map.layout)

#Desviación estándar geográficamente ponderada:
spplot(Estadísticos$SDF, "GenEl2004_LSD", key.space = "right", col.regions = mypalette.1, cuts = 7, main = "Desviación estándar GP para GenEl2004", sp.layout = map.layout)

#Correlación geográficamente ponderada entre GenEl2004 and LARent
spplot(Estadísticos$SDF, "Corr_LARent.Unempl", key.space = "right", col.regions = mypalette.2, at = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1), main = "Correlación GP entre LARent y Unempl", sp.layout = map.layout)
