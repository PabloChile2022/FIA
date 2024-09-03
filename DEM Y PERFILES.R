pacman::p_load(sf, terra, ggplot2, geosphere, tidyverse, openxlsx, tidyterra,ggspatial, tmap)

###El último proyecto, fue con un EPSG:32718

Perfil<-vect("C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Pan de Azucar/Perfiles.shp")
DEM<-rast("C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Pan de Azucar/DEM.tif")
crs(DEM)<-"epsg:32719"
crs(Perfil)<-"epsg:32719"
Perfil<-sf::st_as_sf(Perfil, crs=st_crs(32719)) %>% arrange(Name)
Perfil1<-Perfil[1,]
Perfil2<-Perfil[2,]
Perfil3<-Perfil[3,]
Perfil4<-Perfil[4,]
Perfil5<-Perfil[5,]

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
Perfil2 <- generar_puntos_muestreados(Perfil2, rev = FALSE, N_ESTACAS = 60, DEM, mod_id = 10)
Perfil3 <- generar_puntos_muestreados(Perfil3, rev = TRUE, N_ESTACAS = 58, DEM, mod_id = 10)
Perfil4 <- generar_puntos_muestreados(Perfil4, rev = TRUE, N_ESTACAS = 59, DEM, mod_id = 10)
Perfil5 <- generar_puntos_muestreados(Perfil5, rev = FALSE, N_ESTACAS = 60, DEM, mod_id = 10)

Coord_P1<-Perfil1[[1]]
Coord_P2<-Perfil2[[1]]
Coord_P3<-Perfil3[[1]]
Coord_P4<-Perfil4[[1]]
Coord_P5<-Perfil5[[1]]

puntos_muestreados1<-Perfil1[[2]]
puntos_muestreados2<-Perfil2[[2]]
puntos_muestreados3<-Perfil3[[2]]
puntos_muestreados4<-Perfil4[[2]]
puntos_muestreados5<-Perfil5[[2]]

PLOT_P1<-Perfil1[[3]]
PLOT_P2<-Perfil2[[3]]
PLOT_P3<-Perfil3[[3]]
PLOT_P4<-Perfil4[[3]]
PLOT_P5<-Perfil5[[3]]

P1<-st_transform(sf::st_as_sf(puntos_muestreados1, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
P2<-st_transform(sf::st_as_sf(puntos_muestreados2, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
P3<-st_transform(sf::st_as_sf(puntos_muestreados3, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
P4<-st_transform(sf::st_as_sf(puntos_muestreados4, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
P5<-st_transform(sf::st_as_sf(puntos_muestreados5, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()

dist1<-head(c(0,geosphere::distGeo(P1)),-1)
dist2<-head(c(0,geosphere::distGeo(P2)),-1)
dist3<-head(c(0,geosphere::distGeo(P3)),-1)
dist4<-head(c(0,geosphere::distGeo(P4)),-1)
dist5<-head(c(0,geosphere::distGeo(P5)),-1)

write.table(Coord_P1 %>% mutate(Distancia=dist1), file = "C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Pan de Azucar/Perfil1.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.table(Coord_P2 %>% mutate(Distancia=dist2), file = "C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Pan de Azucar/Perfil2.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.table(Coord_P3 %>% mutate(Distancia=dist3), file = "C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Pan de Azucar/Perfil3.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.table(Coord_P4 %>% mutate(Distancia=dist4), file = "C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Pan de Azucar/Perfil4.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.table(Coord_P5 %>% mutate(Distancia=dist5), file = "C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Pan de Azucar/Perfil5.txt", sep = ",", quote = FALSE, row.names = FALSE)

writeVector(PLOT_P1,"C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Pan de Azucar/Puntos1.shp", overwrite=TRUE)
writeVector(PLOT_P2,"C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Pan de Azucar/Puntos2.shp", overwrite=TRUE)
writeVector(PLOT_P3,"C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Pan de Azucar/Puntos3.shp", overwrite=TRUE)
writeVector(PLOT_P4,"C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Pan de Azucar/Puntos4.shp", overwrite=TRUE)
writeVector(PLOT_P5,"C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Pan de Azucar/Puntos5.shp", overwrite=TRUE)

tmap_mode("view")

tm_basemap("Esri.WorldImagery")+
  tm_shape(puntos_muestreados1)+
  tm_dots(shape = 24, col = "red", size = 0.5, fill="red")+
  tm_shape(puntos_muestreados2)+
  tm_dots(shape = 24, col = "green", size = 0.5, fill="green")+
  tm_shape(puntos_muestreados3)+
  tm_dots(shape = 24, col = "purple", size = 0.5, fill="purple")+
  tm_shape(puntos_muestreados4)+
  tm_dots(shape = 24, col = "blue", size = 0.5, fill="blue")+
 tm_shape(puntos_muestreados5)+
 tm_dots(shape = 24, col = "orange", size = 0.5, fill="orange")

