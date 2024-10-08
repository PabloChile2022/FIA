pacman::p_load(sf, terra, ggplot2, geosphere, tidyverse, openxlsx, tidyterra,ggspatial, tmap)

###El último proyecto, fue con un EPSG:32719
#Rutina exige crear directorio de trabajo previo en la secuencia año/Mes/Nombre Proyecto/Shape o Raster o producto

setwd("C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/")

YY<-"2024" #Define Año en el directorio
Mes<-"Octubre" #Define el mes de trabajo en la carpeta directorio
Nombre<-"Aquist" #Define el nombre del proyecto

ruta_perfil <- file.path(YY, Mes, Nombre, "SHAPE", "Perfiles.shp")
ruta_DEM<-file.path(YY, Mes, Nombre, "Raster", "DEM.tif")

Perfil<-vect(ruta_perfil)
DEM<-rast(ruta_DEM)
crs(DEM)<-"epsg:32719"
crs(Perfil)<-"epsg:32719"
Perfil<-sf::st_as_sf(Perfil, crs=st_crs(32719)) %>% arrange(Name)
Perfil1<-Perfil[1,]
Perfil2<-Perfil[2,]
Perfil3<-Perfil[3,]
Perfil4<-Perfil[4,]
#Perfil5<-Perfil[5,]

# Función para generar puntos muestreados y extraer coordenadas y elevaciones
generar_puntos_muestreados <- function(Perfil, rev = FALSE, N_ESTACAS = 60, DEM, mod_id = 10) {
  # Calcula el delta y la secuencia de puntos
  DELT <- 1 / (N_ESTACAS - 1)
  Seq_Delta <- seq(DELT, (1 - DELT), by = DELT)
  Seq_Delta_P <- c(0, Seq_Delta, 1)
  
  # Muestra los puntos en el perfil
  puntos_muestreados <- st_line_sample(Perfil, sample = Seq_Delta_P)
  puntos_muestreados <- st_cast(puntos_muestreados, "POINT")
  puntos_muestreados <- st_as_sf(puntos_muestreados)
  
  # Extrae la elevación del DEM en los puntos
  DEM_PERFIL <- terra::extract(DEM, vect(puntos_muestreados))
  Coord <- puntos_muestreados %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    dplyr::rename(Lon = X, Lat = Y)
  
  # Crea el dataframe con ID, coordenadas y elevación
  Coord <- cbind(Coord, DEM_PERFIL$DEM) %>% as_tibble()
  Coord$ID <- seq_along(Coord$Lon)
  Coord <- Coord[, c(4, 1, 2, 3)]
  colnames(Coord) <- c("ID", "Lon", "Lat", "DEM")
  
  # Procesa puntos muestreados
  puntos_muestreados <- st_cast(puntos_muestreados, "POINT") %>% st_as_sf()
  puntos_muestreados$ID <- seq_along(puntos_muestreados$x)
  puntos_muestreados <- puntos_muestreados[, c(2, 1)]
  
  if (rev) {
    puntos_muestreados$ID <- rev(puntos_muestreados$ID)
  }
  
  # Filtra los puntos para la representación
  PLOT <- puntos_muestreados %>% filter(ID %% mod_id == 0 | ID == 1)
  
  # Convierte a objetos spatVector y asigna CRS
  puntos_muestreados <- vect(puntos_muestreados)
  crs(puntos_muestreados) <- "epsg:32719"
  
  PLOT <- vect(PLOT)
  crs(PLOT) <- "epsg:32719"
  
  return(list(Coord_P1 = Coord, Puntos1 = puntos_muestreados, Plot = PLOT))
}

Perfil1 <- generar_puntos_muestreados(Perfil1, rev = FALSE, N_ESTACAS = 60, DEM, mod_id = 10)
Perfil2 <- generar_puntos_muestreados(Perfil2, rev = TRUE, N_ESTACAS = 40, DEM, mod_id = 10)
Perfil3 <- generar_puntos_muestreados(Perfil3, rev = FALSE, N_ESTACAS = 34, DEM, mod_id = 10)
Perfil4 <- generar_puntos_muestreados(Perfil4, rev = FALSE, N_ESTACAS = 60, DEM, mod_id = 10)
#Perfil5 <- generar_puntos_muestreados(Perfil5, rev = FALSE, N_ESTACAS = 60, DEM, mod_id = 10)

Coord_P1<-Perfil1[[1]]
Coord_P2<-Perfil2[[1]]
Coord_P3<-Perfil3[[1]]
Coord_P4<-Perfil4[[1]]
#Coord_P5<-Perfil5[[1]]

puntos_muestreados1<-Perfil1[[2]]
puntos_muestreados2<-Perfil2[[2]]
puntos_muestreados3<-Perfil3[[2]]
puntos_muestreados4<-Perfil4[[2]]
#puntos_muestreados5<-Perfil5[[2]]

PLOT_P1<-Perfil1[[3]]
PLOT_P2<-Perfil2[[3]]
PLOT_P3<-Perfil3[[3]]
PLOT_P4<-Perfil4[[3]]
#PLOT_P5<-Perfil5[[3]]

P1<-st_transform(sf::st_as_sf(puntos_muestreados1, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
P2<-st_transform(sf::st_as_sf(puntos_muestreados2, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
P3<-st_transform(sf::st_as_sf(puntos_muestreados3, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
P4<-st_transform(sf::st_as_sf(puntos_muestreados4, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
#P5<-st_transform(sf::st_as_sf(puntos_muestreados5, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()

dist1<-head(c(0,geosphere::distGeo(P1)),-1)
dist2<-head(c(0,geosphere::distGeo(P2)),-1)
dist3<-head(c(0,geosphere::distGeo(P3)),-1)
dist4<-head(c(0,geosphere::distGeo(P4)),-1)
#dist5<-head(c(0,geosphere::distGeo(P5)),-1)


tmap_mode("view")

tm_basemap("Esri.WorldImagery")+
  tm_shape(puntos_muestreados1)+
  tm_dots(shape = 24, col = "red", size = 0.5, fill="red")+
  tm_shape(puntos_muestreados2)+
  tm_dots(shape = 24, col = "green", size = 0.5, fill="green")+
  tm_shape(puntos_muestreados3)+
  tm_dots(shape = 24, col = "purple", size = 0.5, fill="purple")+
  tm_shape(puntos_muestreados4)+
  tm_dots(shape = 24, col = "blue", size = 0.5, fill="blue")
  
  #tm_shape(puntos_muestreados5)+
  #tm_dots(shape = 24, col = "orange", size = 0.5, fill="orange")



#Producto

Perfil1_txt<-file.path(YY, Mes, Nombre, "PRODUCTOS", "Perfil1.txt")
Perfil2_txt<-file.path(YY, Mes, Nombre, "PRODUCTOS", "Perfil2.txt")
Perfil3_txt<-file.path(YY, Mes, Nombre, "PRODUCTOS", "Perfil3.txt")
Perfil4_txt<-file.path(YY, Mes, Nombre, "PRODUCTOS", "Perfil4.txt")
Perfil5_txt<-file.path(YY, Mes, Nombre, "PRODUCTOS", "Perfil5.txt")

write.table(Coord_P1 %>% mutate(Distancia=dist1), file = Perfil1_txt, sep = ",", quote = FALSE, row.names = FALSE)
write.table(Coord_P2 %>% mutate(Distancia=dist2), file = Perfil2_txt, sep = ",", quote = FALSE, row.names = FALSE)
write.table(Coord_P3 %>% mutate(Distancia=dist3), file = Perfil3_txt, sep = ",", quote = FALSE, row.names = FALSE)
write.table(Coord_P4 %>% mutate(Distancia=dist4), file = Perfil4_txt, sep = ",", quote = FALSE, row.names = FALSE)
#write.table(Coord_P5 %>% mutate(Distancia=dist5), file = Perfil5_txt, sep = ",", quote = FALSE, row.names = FALSE)

#Producto

Puntos1<-file.path(YY, Mes, Nombre, "PRODUCTOS", "Puntos1.shp")
Puntos2<-file.path(YY, Mes, Nombre, "PRODUCTOS", "Puntos2.shp")
Puntos3<-file.path(YY, Mes, Nombre, "PRODUCTOS", "Puntos3.shp")
Puntos4<-file.path(YY, Mes, Nombre, "PRODUCTOS", "Puntos4.shp")
#Puntos5<-file.path(YY, Mes, Nombre, "PRODUCTOS", "Puntos5.shp")

writeVector(PLOT_P1,Puntos1, overwrite=TRUE)
writeVector(PLOT_P2,Puntos2, overwrite=TRUE)
writeVector(PLOT_P3,Puntos3, overwrite=TRUE)
writeVector(PLOT_P4,Puntos4, overwrite=TRUE)
#writeVector(PLOT_P5,Puntos5, overwrite=TRUE)


##Ubicación_region

Mapa_regiones<-vect("C:/Users/Pablo/Desktop/Procesamiento/Capas shp/Regiones/Regiones_UTM.shp")
Mapa_regiones<-st_as_sf(Mapa_regiones)

# Crear una variable que indique si es la "Región de Antofagasta"
Mapa_regiones$Highlight <- ifelse(Mapa_regiones$Region == "Región de Valparaíso", "Región de Valparaíso", NA)

tmap_mode("plot")

# Plotear con tmap
Regiones<-tm_shape(Mapa_regiones) +
  tm_polygons("Highlight", 
              palette = c("Región de Valparaíso" = "yellow", "NA" = "grey"), 
              title = "Región") +
  tm_grid(lines = TRUE, labels.size = 1.5,labels.fontface = "bold", crs = "epsg:32719", labels.show=c(0,1)) +  # Agregar grilla en UTM
  tm_compass(position = c("left", "top"),size=1) +  # Agregar la rosa de los vientos en la esquina superior izquierda
  tm_layout(legend.position = c("left", "bottom"),
            fontface = "bold",
            legend.title.size = 2,  # Ajustar el tamaño del título
            legend.title.color = "black",  # Cambiar el color del título de la leyenda a blanco
            legend.title.fontface = "bold",
            legend.text.color = "black",
            legend.text.size = 2,
            legend.text.fontface = "bold")

##Ubicación_Comuna

Mapa_Comuna<-vect("C:/Users/Pablo/Desktop/Procesamiento/Capas shp/Comunas/COMUNAS_UTM.shp")
Mapa_Comuna<-st_as_sf(Mapa_Comuna)
Mapa_Comuna<-Mapa_Comuna %>% filter(Region=="Región de Valparaíso")

##Puntos

Puntos<-vect("C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Octubre/Aquist/SHAPE/Ubicación.shp")
Puntos<-st_as_sf(Puntos)
Puntos<-Puntos[,-c(2:11)]
colnames(Puntos)<-c("id", "geometry")

Vect<-c("Cuncumén", "La Floresta", "Los Escalones")
Puntos$id<-Vect

Cuncumén<-Puntos %>% filter(id=="Cuncumén")
Proyecto<-Puntos[c(2,3),]


# Crear una variable que indique si es la "Región de Antofagasta"
Mapa_Comuna$Highlight <- ifelse(Mapa_Comuna$Comuna == "San Antonio", "San Antonio", NA)

# Plotear con tmap
Comuna<-tm_shape(Mapa_Comuna) +
  tm_polygons("Highlight", 
              palette = c("San Antonio" = "yellow", "NA" = "grey"), 
              title = "Comuna")+
  tm_grid(lines = TRUE, labels.size = 0.8,labels.fontface = "bold", crs = "epsg:32719") +  # Agregar grilla en UTM
  tm_compass(position = c("left", "top"), size=1) +  # Agregar la rosa de los vientos en la esquina superior izquierda
  tm_layout(legend.position = c("right", "top"),
            axis.text.size = 0.5, 
            fontface = "bold",           
            legend.title.size = 0.5,  # Ajustar el tamaño del título
            legend.title.color = "black",  # Cambiar el color del título de la leyenda a blanco
            legend.title.fontface = "bold",
            legend.text.color = "black",
            legend.text.size = 0.5,
            legend.text.fontface = "bold",
            axis.x.show = FALSE)


Ubica<-tm_shape(Mapa_Comuna %>% filter(Comuna=="San Antonio")) +
  tm_polygons("Highlight", 
              palette = c("San Antonio" = "yellow", "NA" = "grey"), 
              title = "Comuna")+  
  tm_grid(lines = TRUE, labels.size = 1.5,labels.fontface = "bold", crs = "epsg:32719") +  # Agregar grilla en UTM
  tm_shape(Cuncumén)+
  tm_dots(shape = 24, col = "red", size = 0.5, fill="red") +
  tm_text("id", size = 1.5, col ="black", ymod = 0.6,xmod=-1, fontface = "bold", NA.show = FALSE)+  # Mostrar texto
  tm_shape(Proyecto)+
  tm_dots(shape = 24, col = "red", size = 0.5, fill="red") +
  tm_text("id", size = 1.5, col ="black", ymod = 0.6,xmod=-2, fontface = "bold", NA.show = FALSE)+  # Mostrar texto
  tm_layout(legend.position = c("left", "bottom"),
            legend.title.size = 1,  # Ajustar el tamaño del título
            legend.title.color = "black",  # Cambiar el color del título de la leyenda a blanco
            legend.title.fontface = "bold",
            legend.text.color = "black",
            legend.text.size = 1,
            legend.text.fontface = "bold",
            axis.x.show = FALSE)

Ubica

jpeg(filename="C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Octubre/Aquist/PRODUCTOS/Regiones.jpg",width=7480,height=6000,units="px",res=1000)

Regiones

dev.off()

jpeg(filename="C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Octubre/Aquist/PRODUCTOS/Comuna.jpg",width=2000,height=3000,units="px",res=1000)

Comuna

dev.off()

jpeg(filename="C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Octubre/Aquist/PRODUCTOS/Ubicación.jpg",width=7480,height=6000,units="px",res=1000)

Ubica

dev.off()







