#NDVI#


install.packages("rgdal")
install.packages("raster")
install.packages("sp")

# load the raster, sp, and rgdal packages


library(sp)
library(rgdal)
library(raster)

# set working directory to data folder


install.packages("RStoolbox")
install.packages("raster")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("terra")

library(RStoolbox)
library(raster)
library(ggplot2)
library(gridExtra)

setwd("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 2/NDVI/NDVI José Tomas/3_04_2022/Bandas NDVI")

# Lectura y apilado de las bandas del sensor Sentinel 2 -------------------


dir <- list.files(pattern = "T19HBB",full.names = T)

#Ingreso de coordenadas para el corte desdee qgis
Corte<-raster(file.choose(), header=T)

Sentinel2 <- stack(dir)

#Corte del stack

sentinel2<-crop(Sentinel2,Corte)


names(sentinel2)<- c("Blue", "Green", "Red", "NIR")

# Cálculo del NDVI --------------------------------------------------------

setwd("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 2/NDVI/NDVI José Tomas/3_04_2022/RESULTADOS")

nir <- sentinel2$NIR # Infrarojo cercano
red <- sentinel2$Red # Rojo

ndvi <- (nir-red)/(nir+red) # NDVI

ndvi[ndvi>1] <- 1; ndvi[ndvi< (-1)] <- -1 #Reescalado para evitar outliers


# Visualización del NDVI e imagen falso color -----------------------------

# Visualización NDVI guardada en el objeto ndvi_plot

ndvi_plot <- ggR(ndvi, geom_raster = TRUE,alpha = 1)+
  scale_fill_gradientn(colours = rev(terrain.colors(100)), 
                       name = "NDVI") + 
  theme(legend.positio = "bottom")

# Visualización falso color guardada en el objecto falso_color

falso_color <- ggRGB(sentinel2, r= "NIR" , g="Green" , b="Blue", stretch = "lin")

# Representación final

jpeg(filename="NDVI_FC_MANDV.jpg",width=13000,height=5000,units="px",res=700,pointsize=12,quality=100)

grid.arrange(ndvi_plot, falso_color, ncol=2)

dev.off()

#Cluster

nr <- getValues(ndvi)
str(nr)

set.seed(99)#Semilla CONJUNTO DE TODOS LOS NÚMEROS ALEATORIOS QUE PUEDEN EXISTIR

# Queremos crear 7 cúmulos, permitir 500 iteraciones, comenzar con 7 conjuntos aleatorios usando el método de "Lloyd"
kmncluster <- kmeans(na.omit(nr), centers = 7, iter.max = 500, nstart = 7, algorithm="Lloyd")

# kmeans devuelve un objeto de clase "kmeans"
str(kmncluster)

# Usar el objeto ndvi para fijar los valores del cúmulo a un nuevo raster
knr <- setValues(ndvi, kmncluster$cluster)

# También puedes hacerlo así
knr <- raster(ndvi)
values(knr) <- kmncluster$cluster
knr

# Definir un vector de color para 9 cúmulos (aprender más sobre la configuración del color más tarde)
#Coloresmycolor <- c("#fef65b","#ff0000", "#daa520","#0000ff","#00ff00","#cbbeb5","#c3ff5b", "#ff7373", "#808080")

mycolor <- c("#fef65b","#ff0000", "#daa520","#0000ff","#00ff00","#cbbeb5","#c3ff5b")

jpeg(filename="NDVI_CL_MANV.jpg",width=13000,height=5000,units="px",res=700,pointsize=12,quality=100)

par(mfrow = c(1,2))
plot(ndvi, col = rev(terrain.colors(10)), main = 'Landsat-NDVI')
plot(knr, main = 'Unsupervised classification', col = mycolor )

dev.off()

writeRaster( knr,filename=file.path("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 2/NDVI/NDVI José Tomas/3_04_2022/RESULTADOS/MANDV_CL.tif"),format="GTiff", overwrite=TRUE)
writeRaster( ndvi,filename=file.path("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 2/NDVI/NDVI José Tomas/3_04_2022/RESULTADOS/MANDV_NDVI.tif"),format="GTiff", overwrite=TRUE)
writeRaster( sentinel2,filename=file.path("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 2/NDVI/NDVI José Tomas/3_04_2022/RESULTADOS/MANDV_FC.tif"),format="GTiff", overwrite=TRUE)

kmncluster


# Cálculo del SAVI -------------------------------------------------------------------------------------------------------------------------------------------------------

nir <- sentinel2$NIR # Infrarojo cercano
red <- sentinel2$Red # Rojo

ndvi <- ((nir-red)/(nir+red+1))*(1+1) # NDVI

ndvi[ndvi>1] <- 1; ndvi[ndvi< (-1)] <- -1 #Reescalado para evitar outliers


# Visualización del NDVI e imagen falso color -----------------------------

# Visualización NDVI guardada en el objeto ndvi_plot

ndvi_plot <- ggR(ndvi, geom_raster = TRUE,alpha = 1)+
  scale_fill_gradientn(colours = rev(terrain.colors(100)), 
                       name = "NDVI") + 
  theme(legend.positio = "bottom")

# Visualización falso color guardada en el objecto falso_color

falso_color <- ggRGB(sentinel2, r= "NIR" , g="Green" , b="Blue", stretch = "lin")

# Representación final

jpeg(filename="SAVI_FC_MANDV.jpg",width=13000,height=5000,units="px",res=700,pointsize=12,quality=100)

grid.arrange(ndvi_plot, falso_color, ncol=2)

dev.off()

#Cluster

nr <- getValues(ndvi)
str(nr)

set.seed(99)#Semilla CONJUNTO DE TODOS LOS NÚMEROS ALEATORIOS QUE PUEDEN EXISTIR

# Queremos crear 7 cúmulos, permitir 500 iteraciones, comenzar con 7 conjuntos aleatorios usando el método de "Lloyd"
kmncluster <- kmeans(na.omit(nr), centers = 7, iter.max = 500, nstart = 7, algorithm="Lloyd")

# kmeans devuelve un objeto de clase "kmeans"
str(kmncluster)

# Usar el objeto ndvi para fijar los valores del cúmulo a un nuevo raster
knr <- setValues(ndvi, kmncluster$cluster)

# También puedes hacerlo así
knr <- raster(ndvi)
values(knr) <- kmncluster$cluster
knr

# Definir un vector de color para 9 cúmulos (aprender más sobre la configuración del color más tarde)
#Coloresmycolor <- c("#fef65b","#ff0000", "#daa520","#0000ff","#00ff00","#cbbeb5","#c3ff5b", "#ff7373", "#808080")

mycolor <- c("#fef65b","#ff0000", "#daa520","#0000ff","#00ff00","#cbbeb5","#c3ff5b")

jpeg(filename="SAVI_CL_MANV.jpg",width=13000,height=5000,units="px",res=700,pointsize=12,quality=100)

par(mfrow = c(1,2))
plot(ndvi, col = rev(terrain.colors(10)), main = 'Landsat-NDVI')
plot(knr, main = 'Unsupervised classification', col = mycolor )

dev.off()

writeRaster( knr,filename=file.path("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 2/NDVI/NDVI José Tomas/3_04_2022/RESULTADOS/MANDV_CL.tif"),format="GTiff", overwrite=TRUE)
writeRaster( ndvi,filename=file.path("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 2/NDVI/NDVI José Tomas/3_04_2022/RESULTADOS/MANDV_SAVI.tif"),format="GTiff", overwrite=TRUE)
writeRaster( sentinel2,filename=file.path("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 2/NDVI/NDVI José Tomas/3_04_2022/RESULTADOS/MANDV_FC.tif"),format="GTiff", overwrite=TRUE)

kmncluster

