pacman::p_load(sf, terra, ggplot2, geosphere, tidyverse, openxlsx, tidyterra,ggspatial, tmap)


###El último proyecto, fue con un EPSG:32718

Perfil<-vect("C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Agrícola San Pablo/Perfiles.shp")
DEM<-rast("C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Agrícola San Pablo/DEM.tif")
crs(DEM)<-"epsg:32719"
crs(Perfil)<-"epsg:32719"
Perfil<-sf::st_as_sf(Perfil, crs=st_crs(32719)) %>% arrange(Name)
Perfil1<-Perfil[1,]
#Perfil2<-Perfil[2,]
#Perfil3<-Perfil[3,]
#Perfil4<-Perfil[4,]
#Perfil5<-Perfil[5,]



##N_ESTACAS

N_ESTACAS<-60
DELT<-1/(N_ESTACAS-1)
Seq_Delta<-seq(DELT,(1-DELT), by=DELT)
Seq_Delta_P<-c(0,Seq_Delta,1)

puntos_muestreados1 <- st_line_sample(Perfil1, sample=Seq_Delta_P)

#N_ESTACAS<-60
#DELT<-1/(N_ESTACAS-1)
#Seq_Delta<-seq(DELT,(1-DELT), by=DELT)
#Seq_Delta_P<-c(0,Seq_Delta,1)

#puntos_muestreados2 <- st_line_sample(Perfil2, sample=Seq_Delta_P)

#N_ESTACAS<-37
#DELT<-1/(N_ESTACAS-1)
#Seq_Delta<-seq(DELT,(1-DELT), by=DELT)
#Seq_Delta_P<-c(0,Seq_Delta,1)

#puntos_muestreados3 <- st_line_sample(Perfil3, sample=Seq_Delta_P)

#N_ESTACAS<-60
#DELT<-1/(N_ESTACAS-1)
#Seq_Delta<-seq(DELT,(1-DELT), by=DELT)
#Seq_Delta_P<-c(0,Seq_Delta,1)

#puntos_muestreados4 <- st_line_sample(Perfil4, sample=Seq_Delta_P)

#N_ESTACAS<-90
#DELT<-1/(N_ESTACAS-1)
#Seq_Delta<-seq(DELT,(1-DELT), by=DELT)
#Seq_Delta_P<-c(0,Seq_Delta,1)

#puntos_muestreados5 <- st_line_sample(Perfil5, sample=Seq_Delta_P)


 
Puntos2<-st_cast(puntos_muestreados2, "POINT")
Puntos2<-st_as_sf(Puntos2)
DEM_PERFIL2<-terra::extract(DEM, vect(Puntos2))
Coord_P2<-Puntos2 %>% st_coordinates() %>% as.data.frame() %>% rename(Lon=X, Lat= Y)
Coord_P2<-cbind(Coord_P2,DEM_PERFIL2$DEM)
Coord_P2<-Coord_P2 %>% as_tibble()
Coord_P2$ID <- seq_along(Coord_P2$Lon)
Coord_P2<-Coord_P2[,c(4,1,2,3)]
colnames(Coord_P2)<-c("ID","Lon", "Lat", "DEM")


puntos_muestreados2<-st_cast(puntos_muestreados2, "POINT") %>% st_as_sf()
puntos_muestreados2$ID <- seq_along(puntos_muestreados2$x)
puntos_muestreados2<-puntos_muestreados2[,c(2,1)]
puntos_muestreados2$ID<-(puntos_muestreados2$ID)##AGREGAR REV() EN CASO DE SER NECESARIO
PLOT_P2<-puntos_muestreados2 %>% filter(ID %% 10==0 | ID==1) ##Representa puntos muestreados, ID %%6==0 EVALUA SI EL RESULTADO ES MULTIPLO DE 6 Y EL ID==1 AGREGA EL ID1
puntos_muestreados2<-vect(puntos_muestreados2)
crs(puntos_muestreados2)<-"epsg:32719"
PLOT_P2<-vect(PLOT_P2)
crs(PLOT_P2)<-"epsg:32719"


Puntos3<-st_cast(puntos_muestreados3, "POINT")
Puntos3<-st_as_sf(Puntos3)
DEM_PERFIL3<-terra::extract(DEM, vect(Puntos3))
Coord_P3<-Puntos3 %>% st_coordinates() %>% as.data.frame() %>% rename(Lon=X, Lat= Y)
Coord_P3<-cbind(Coord_P3,DEM_PERFIL3$DEM)
Coord_P3<-Coord_P3 %>% as_tibble()
Coord_P3$ID <- seq_along(Coord_P3$Lon)
Coord_P3<-Coord_P3[,c(4,1,2,3)]
colnames(Coord_P3)<-c("ID","Lon", "Lat", "DEM")


puntos_muestreados3<-st_cast(puntos_muestreados3, "POINT") %>% st_as_sf()
puntos_muestreados3$ID <- seq_along(puntos_muestreados3$x)
puntos_muestreados3<-puntos_muestreados3[,c(2,1)]
puntos_muestreados3$ID<-(puntos_muestreados3$ID)##AGREGAR REV() EN CASO DE SER NECESARIO
PLOT_P3<-puntos_muestreados3 %>% filter(ID %% 10==0 | ID==1) ##Representa puntos muestreados, ID %%6==0 EVALUA SI EL RESULTADO ES MULTIPLO DE 6 Y EL ID==1 AGREGA EL ID1
puntos_muestreados3<-vect(puntos_muestreados3)
crs(puntos_muestreados3)<-"epsg:32719"
PLOT_P3<-vect(PLOT_P3)
crs(PLOT_P3)<-"epsg:32719"


#Puntos4<-st_cast(puntos_muestreados4, "POINT")
#Puntos4<-st_as_sf(Puntos4)
#DEM_PERFIL4<-terra::extract(DEM, vect(Puntos4))
#Coord_P4<-Puntos4 %>% st_coordinates() %>% as.data.frame() %>% rename(Lon=X, Lat= Y)
#Coord_P4<-cbind(Coord_P4,DEM_PERFIL4$DEM)
#Coord_P4<-Coord_P4 %>% as_tibble()
#Coord_P4$ID <- seq_along(Coord_P4$Lon)
#Coord_P4<-Coord_P4[,c(4,1,2,3)]
#colnames(Coord_P4)<-c("ID","Lon", "Lat", "DEM")


#puntos_muestreados4<-st_cast(puntos_muestreados4, "POINT") %>% st_as_sf()
#puntos_muestreados4$ID <- seq_along(puntos_muestreados4$x)
#puntos_muestreados4<-puntos_muestreados4[,c(2,1)]
#puntos_muestreados4$ID<-(puntos_muestreados4$ID)##AGREGAR REV() EN CASO DE SER NECESARIO
#PLOT_P4<-puntos_muestreados4 %>% filter(ID %% 10==0 | ID==1) ##Representa puntos muestreados, ID %%6==0 EVALUA SI EL RESULTADO ES MULTIPLO DE 6 Y EL ID==1 AGREGA EL ID1
#puntos_muestreados4<-vect(puntos_muestreados4)
#crs(puntos_muestreados4)<-"epsg:32719"
#PLOT_P4<-vect(PLOT_P4)
#crs(PLOT_P4)<-"epsg:32719"


#Puntos5<-st_cast(puntos_muestreados5, "POINT")
#Puntos5<-st_as_sf(Puntos5)
#DEM_PERFIL5<-terra::extract(DEM, vect(Puntos5))
#Coord_P5<-Puntos5 %>% st_coordinates() %>% as.data.frame() %>% rename(Lon=X, Lat= Y)
#Coord_P5<-cbind(Coord_P5,DEM_PERFIL5$DEM)
#Coord_P5<-Coord_P5 %>% as_tibble()
#Coord_P5$ID <- seq_along(Coord_P5$Lon)
#Coord_P5<-Coord_P5[,c(4,1,2,3)]
#colnames(Coord_P5)<-c("ID","Lon", "Lat", "DEM")


#puntos_muestreados5<-st_cast(puntos_muestreados5, "POINT") %>% st_as_sf()
#puntos_muestreados5$ID <- seq_along(puntos_muestreados5$x)
#puntos_muestreados5<-puntos_muestreados5[,c(2,1)]
#puntos_muestreados5$ID<-rev(puntos_muestreados5$ID)##AGREGAR REV() EN CASO DE SER NECESARIO
#PLOT_P5<-puntos_muestreados5 %>% filter(ID %% 10==0 | ID==1) ##Representa puntos muestreados, ID %%6==0 EVALUA SI EL RESULTADO ES MULTIPLO DE 6 Y EL ID==1 AGREGA EL ID1
#puntos_muestreados5<-vect(puntos_muestreados5)
#crs(puntos_muestreados5)<-"epsg:32719"
#PLOT_P5<-vect(PLOT_P5)
#crs(PLOT_P5)<-"epsg:32719"

tmap_mode("view")

tm_basemap("Esri.WorldImagery")+
tm_shape(puntos_muestreados1)+
tm_dots(shape = 24, col = "red", size = 0.5, fill="red")+
tm_shape(puntos_muestreados2)+
tm_dots(shape = 24, col = "green", size = 0.5, fill="green")+
tm_shape(puntos_muestreados3)+
tm_dots(shape = 24, col = "purple", size = 0.5, fill="purple")

#+
#tm_shape(puntos_muestreados4)+
#tm_dots(shape = 24, col = "blue", size = 0.5, fill="blue")+
#tm_shape(puntos_muestreados5)+
#tm_dots(shape = 24, col = "orange", size = 0.5, fill="orange")



P1<-st_transform(sf::st_as_sf(puntos_muestreados1, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
P2<-st_transform(sf::st_as_sf(puntos_muestreados2, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
P3<-st_transform(sf::st_as_sf(puntos_muestreados3, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
#P4<-st_transform(sf::st_as_sf(puntos_muestreados4, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
#P5<-st_transform(sf::st_as_sf(puntos_muestreados5, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()

Line1<-st_transform(Perfil1, crs="+init=EPSG:4326") %>% st_coordinates
Line1<-Line1[,-3]

dist1<-head(c(0,geosphere::distGeo(P1)),-1)
dist2<-head(c(0,geosphere::distGeo(P2)),-1)
dist3<-head(c(0,geosphere::distGeo(P3)),-1)
#dist4<-head(c(0,geosphere::distGeo(P4)),-1)
#dist5<-head(c(0,geosphere::distGeo(P5)),-1)


write.table(Coord_P1 %>% mutate(Distancia=dist1), file = "C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Bolsico/Perfil1.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.table(Coord_P2 %>% mutate(Distancia=dist2), file = "C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Bolsico/Perfil2.txt", sep = ",", quote = FALSE, row.names = FALSE)
write.table(Coord_P3 %>% mutate(Distancia=dist3), file = "C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Bolsico/Perfil3.txt", sep = ",", quote = FALSE, row.names = FALSE)
#write.table(Coord_P4 %>% mutate(Distancia=dist4), file = "C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Bolsico/Perfil4.txt", sep = ",", quote = FALSE, row.names = FALSE)
#write.table(Coord_P5 %>% mutate(Distancia=dist5), file = "C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Bolsico/Perfil5.txt", sep = ",", quote = FALSE, row.names = FALSE)

writeVector(PLOT_P1,"C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Agrícola San Pablo/Puntos1.shp", overwrite=TRUE)
writeVector(PLOT_P2,"C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Agrícola San Pablo/Puntos2.shp", overwrite=TRUE)
writeVector(PLOT_P3,"C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Agrícola San Pablo/Puntos3.shp", overwrite=TRUE)
#writeVector(PLOT_P4,"C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Bolsico/Puntos4.shp", overwrite=TRUE)
#writeVector(PLOT_P5,"C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Bolsico/Puntos5.shp", overwrite=TRUE)










###DISTANCIAS GEOSPHERE

pacman::p_load(raster, sf, sp, ggplot2, tmap, geosphere)

cortar<-sf::st_bbox(Perfil1)

poligono <- sf::st_as_sfc(cortar, crs = st_crs(perfil1))

DEM1<-raster("C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/Chillan/DEM.tif")
crs="+proj=longlat"
DEM1<-projectRaster(DEM1,crs=crs)

P1<-st_transform(Perfil1, crs=st_crs(DEM1))


alturas<-raster::extract(DEM1, P1, along=TRUE, cellnumbers=TRUE)

alturas_df<-purrr::map_df(alturas,as.data.frame, .id="ID")
coor_rec=raster::xyFromCell(DEM1, alturas_df$cell)
par_dist<-geosphere::distGeo((coor_rec)) [-nrow(coor_rec)]
alturas_df$dist=c(0,cumsum(par_dist))
names(alturas_df)[3]<-"altura"

ggplot(data=alturas_df, aes(x=dist, y=altura)) +
       geom_area(position="identity", col="red", size=2,
                 fill="chocolate4", alpha=0.8) +
       coord_fixed(ratio=4/1, ylim=c(200,400), expand=F) +
       theme_minimal()+ labs(title="Perfil de Elevación") +
       theme(panel.background = element_rect(fill="azure"))

###ST

# Calcula distancias
distances <- st_line_locate_point(puntos_muestreados1, Perfil1)
accumulated_distances <- cumsum(distances)


####LEAFLET


pacman::p_load(tidyverse,tidyterra, terra, sf, leaflet, magrittr, sf, geojsonio, htmltools, htmlwidgets, stringi, 
               RColorBrewer, leafpop,leafem, magick,htmltools,mapedit, mapview, stars, openxlsx)

setwd("C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/")

Pozos_sf<-read.xlsx("Agrosuper/La Compañia/Coordenadas Pozos.xlsx") %>% 
          st_as_sf(coords = c("Lon", "Lat"), crs=st_crs(32719)) %>%
          st_transform(crs = st_crs("+init=EPSG:4326")) 
       
Pozos_lf<-Pozos_sf %>% st_coordinates() %>% as.data.frame() %>% cbind(st_drop_geometry(Pozos_sf)) %>%
          rename(Lon=X, Lat=Y)



Perfil<-vect("Agrosuper/La Compañia/MAPA_COMPAÑIA_OK_pd.dxf")
crs(Perfil)<-"epsg:32719"
Perfil<-st_as_sf(Perfil)
Perfil<-Perfil[,-c(2:6)]
Raster_Img<-rast("Agrosuper/La Compañia/profundidad.tif")
Corte<-vect("Agrosuper/La Compañia/Corte.shp")
Raster_Img<-terra::crop(Raster_Img, Corte, mask=TRUE)

L1<- Perfil %>% dplyr::filter(Layer=="L1")
L2<- Perfil %>% dplyr::filter(Layer=="L2")
L3<- Perfil %>% dplyr::filter(Layer=="L3")
L4<- Perfil %>% dplyr::filter(Layer=="L4")
L5<- Perfil %>% dplyr::filter(Layer=="L5")


L1_DF<-L1 %>% st_transform(crs = st_crs("+init=EPSG:4326")) %>% st_coordinates() %>% as.data.frame() %>% rename(Lon=X, Lat= Y, Obj=L1)
L2_DF<-L2 %>% st_transform(crs = st_crs("+init=EPSG:4326")) %>% st_coordinates() %>% as.data.frame() %>% rename(Lon=X, Lat= Y, Obj=L1)
L3_DF<-L3 %>% st_transform(crs = st_crs("+init=EPSG:4326")) %>% st_coordinates() %>% as.data.frame() %>% rename(Lon=X, Lat= Y, Obj=L1)
L4_DF<-L4 %>% st_transform(crs = st_crs("+init=EPSG:4326")) %>% st_coordinates() %>% as.data.frame() %>% rename(Lon=X, Lat= Y, Obj=L1)
L5_DF<-L5 %>% st_transform(crs = st_crs("+init=EPSG:4326")) %>% st_coordinates() %>% as.data.frame() %>% rename(Lon=X, Lat= Y, Obj=L1)


L1<-image_read_pdf("Agrosuper/La Compañia/L1.pdf", pages=1)
L2<-image_read_pdf("Agrosuper/La Compañia/L2.pdf", pages=1)
L3<-image_read_pdf("Agrosuper/La Compañia/L3.pdf", pages=1)
L4<-image_read_pdf("Agrosuper/La Compañia/L4.pdf", pages=1)
L5<-image_read_pdf("Agrosuper/La Compañia/L5.pdf", pages=1)



image_write(L1, path="Agrosuper/La Compañia/L1_IMG.png", format="png")
image_write(L2, path="Agrosuper/La Compañia/L2_IMG.png", format="png")
image_write(L3, path="Agrosuper/La Compañia/L3_IMG.png", format="png")
image_write(L4, path="Agrosuper/La Compañia/L4_IMG.png", format="png")
image_write(L5, path="Agrosuper/La Compañia/L5_IMG.png", format="png")

file1<-"Agrosuper/La Compañia/L1_IMG.png"
file2<-"Agrosuper/La Compañia/L2_IMG.png"
file3<-"Agrosuper/La Compañia/L3_IMG.png"
file4<-"Agrosuper/La Compañia/L4_IMG.png"
file5<-"Agrosuper/La Compañia/L5_IMG.png"

MARKL1<-st_as_sf(L1_DF, coords = c("Lon", "Lat"), crs=st_crs("+init=EPSG:4326"))
MARKL1<-MARKL1[1,0]

MARKL2<-st_as_sf(L2_DF, coords = c("Lon", "Lat"), crs=st_crs("+init=EPSG:4326"))
MARKL2<-MARKL2[1,0]

MARKL3<-st_as_sf(L3_DF, coords = c("Lon", "Lat"), crs=st_crs("+init=EPSG:4326"))
MARKL3<-MARKL3[1,0]

MARKL4<-st_as_sf(L4_DF, coords = c("Lon", "Lat"), crs=st_crs("+init=EPSG:4326"))
MARKL4<-MARKL4[1,0]

MARKL5<-st_as_sf(L5_DF, coords = c("Lon", "Lat"), crs=st_crs("+init=EPSG:4326"))
MARKL5<-MARKL5[1,0]


m <- leaflet() %>%
     addProviderTiles("Esri.WorldImagery")  %>% 
      setView(lng = -70.68219883, lat = -34.06541587, zoom = 16)



pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(Raster_Img),
                    na.color =  c("#00000000"))

Compañia_lft<-m %>% addPolylines(data=L1_DF %>% filter(Obj== 1), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L1_DF %>% filter(Obj== 2), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1, ) %>%
             addPolylines(data=L1_DF %>% filter(Obj== 3), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L1_DF %>% filter(Obj== 4), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L1_DF %>% filter(Obj== 5), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L1_DF %>% filter(Obj== 6), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L1_DF %>% filter(Obj== 7), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L1_DF %>% filter(Obj== 8), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L1_DF %>% filter(Obj== 9), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L1_DF %>% filter(Obj== 10), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L1_DF %>% filter(Obj== 11), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L1_DF %>% filter(Obj== 12), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>% 
             addPolylines(data=L2_DF %>% filter(Obj== 1), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L2_DF %>% filter(Obj== 2), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L2_DF %>% filter(Obj== 3), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L2_DF %>% filter(Obj== 4), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L2_DF %>% filter(Obj== 5), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L2_DF %>% filter(Obj== 6), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L2_DF %>% filter(Obj== 7), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L2_DF %>% filter(Obj== 8), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L2_DF %>% filter(Obj== 9), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L2_DF %>% filter(Obj== 10), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1)  %>% 
             addPolylines(data=L3_DF %>% filter(Obj== 1), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L3_DF %>% filter(Obj== 2), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L3_DF %>% filter(Obj== 3), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L3_DF %>% filter(Obj== 4), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L3_DF %>% filter(Obj== 5), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L3_DF %>% filter(Obj== 6), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L3_DF %>% filter(Obj== 7), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L3_DF %>% filter(Obj== 8), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L3_DF %>% filter(Obj== 9), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L4_DF %>% filter(Obj== 1), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L4_DF %>% filter(Obj== 2), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L4_DF %>% filter(Obj== 3), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L4_DF %>% filter(Obj== 4), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L4_DF %>% filter(Obj== 5), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L4_DF %>% filter(Obj== 6), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L4_DF %>% filter(Obj== 7), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>% 
             addPolylines(data=L5_DF %>% filter(Obj== 1), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 2), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 3), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 4), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 5), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 6), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 7), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 8), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 9), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 10), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 11), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 12), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 )  %>%
             addPolylines(data=L5_DF %>% filter(Obj== 13), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 14), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%
             addPolylines(data=L5_DF %>% filter(Obj== 15), lng=~Lon, lat=~Lat, group="Perfiles", color="red", weight=2, opacity =1 ) %>%             
             addCircleMarkers(data = MARKL1, group = "Perfil1") %>%
             addPopupImages(file1, width = 500, height = 300, group="Perfil1") %>%
             addCircleMarkers(data = MARKL2, group = "Perfil2") %>%
             addPopupImages(file2, width = 500, height = 300, group="Perfil2") %>%
             addCircleMarkers(data = MARKL3, group = "Perfil3") %>%
             addPopupImages(file3, width = 500, height = 300, group="Perfil3") %>%
             addCircleMarkers(data = MARKL4, group = "Perfil4") %>%
             addPopupImages(file4, width = 500, height = 300, group="Perfil4") %>%
             addCircleMarkers(data = MARKL5, group = "Perfil5") %>%
             addPopupImages(file5, width = 500, height = 300, group="Perfil5") %>%
             addCircles(data=Pozos_lf, radius = 40,
             color = "",  opacity = 1,
             fillColor = c("#a500a5","#a500a5", "#a500a5", "#a500a5", "red"), fillOpacity = 0.45, group="Pozos") %>%
             addLabelOnlyMarkers(data=Pozos_lf, label=~Pozos_lf$Caudal,labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T), group="Pozos") %>%
             addStarsImage(st_as_stars(Raster_Img),colors=pal, opacity = 0.8, na.color="transparent", group="Profundidad") %>%
             addLayersControl(baseGroups = c("Mapa base"),overlayGroups = c("Perfiles", "Perfil1","Perfil2", "Perfil3", "Perfil4", "Perfil5", "Profundidad", "Pozos"),position = "topright") 
             addLegend(colors = c("red", "#a500a5"), labels=c("Pozo existente", "Pozo Propuesto"), group="Pozos", title="Pozos")


            
Compañia_lft


leaflet() %>% addTiles() %>% addCircles(data=Pozos_lf, radius = 40,
                                        color = "",  opacity = 1,
                                        fillColor = "#a500a5", fillOpacity = 0.45, group=Pozos) %>%
                           addLabelOnlyMarkers(data=Pozos_lf, label=~Pozos_lf$Caudal,
                                               labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T), group=Pozos)
                             

saveas <- function(map, file){
  class(map) <- c("saveas",class(map))
  attr(map,"filesave")=file
  map
}

print.saveas <- function(x, ...){
  class(x) = class(x)[class(x)!="saveas"]
  htmltools::save_html(x, file=attr(x,"filesave"))
}



saveas(Compañia_lft, "Agrosuper/La Compañia/LC.html")


pacman::p_load(sf, terra, ggplot2, geosphere, tidyverse, openxlsx, tidyterra,ggspatial, tmap)


###El último proyecto, fue con un EPSG:32718

Perfil<-vect("C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Fritíucola Bellavista/Perfiles.shp")
DEM<-rast("C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Fritíucola Bellavista/DEM.tif")
crs(DEM)<-"epsg:32719"
crs(Perfil)<-"epsg:32719"
Perfil<-sf::st_as_sf(Perfil, crs=st_crs(32719)) %>% arrange(Name)
Perfil1<-Perfil[1,]


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


##Perfil1

Perfil1 <- generar_puntos_muestreados(Perfil1, rev = TRUE, N_ESTACAS = 60, DEM, mod_id = 10)
Coord_P1<-Perfil1[[1]]
puntos_muestreados1<-Perfil1[[2]]
PLOT_P1<-Perfil1[[3]]
P1<-st_transform(sf::st_as_sf(puntos_muestreados1, crs=st_crs(32719)), crs= "+init=EPSG:4326") %>% st_coordinates()
dist1<-head(c(0,geosphere::distGeo(P1)),-1)
write.table(Coord_P1 %>% mutate(Distancia=dist1), file = "C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Bolsico/Perfil1.txt", sep = ",", quote = FALSE, row.names = FALSE)
writeVector(PLOT_P1,"C:/Users/Pablo/Desktop/Pablo/FIA/Pozos/2024/Sembrador Capital/Agrícola San Pablo/Puntos1.shp", overwrite=TRUE)


tmap_mode("view")

tm_basemap("Esri.WorldImagery")+
tm_shape(puntos_muestreados1)+
tm_dots(shape = 24, col = "red", size = 0.5, fill="red")

