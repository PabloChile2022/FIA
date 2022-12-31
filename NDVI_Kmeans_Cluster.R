library(sp)
library(raster)
library(rgdal)


setwd("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 2/NDVI/NDVI José Tomas/FIA/105")

EX1.6b <- stack("EX1_6band.tif")
names(EX1.6b)<- c("Blue", "Green", "Red", "RedEdge", "NIR","0")
nir <- EX1.6b$NIR # Infrarojo cercano
red <- EX1.6b$Red # Rojo
ndvi <- (nir-red)/(nir+red) # NDVI

ndvi_2<-ndvi
ndvi_2[ndvi_2<0.5] <- NA

Corte<-shapefile("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 2/NDVI/NDVI José Tomas/FIA/105/Corte") #Cargar shape para el corte
RR<-mask(ndvi_2,Corte)
RR<-crop(RR,Corte)

nr <- getValues(RR) #Convirtiendo el raster a una matriz de datos
str(nr)#Propiedades nr

i <- !is.na(nr)
set.seed(99)

kmncluster <- kmeans(nr[i], centers = 5, iter.max = 500, nstart = 5, algorithm="Lloyd")

nr[i] <- kmncluster$cluster
knr <- setValues(RR, nr)

knr[knr==4] <- 6
knr[knr==5] <- 7
knr[knr==2] <- 8
knr[knr==3] <- 9
knr[knr==1] <- 10

knr[knr==6] <- 1
knr[knr==7] <- 2
knr[knr==8] <- 3
knr[knr==9] <- 4
knr[knr==10] <- 5




library(tidyverse)
library(patchwork)
library(RStoolbox)
library(ggspatial)
library(sf)
library(openxlsx)
.rs.unloadPackage("tidyr")
COORD<-read.xlsx("COORD.xlsx") %>% st_as_sf(coords=c(4,5), crs=st_crs(5361))      
COORD$ndvi<-extract(ndvi,COORD)
COORD$cluster<-extract(knr, COORD)
COORD$Rango<-c("alto", "alto", "medio", "medio", "bajo", "bajo")

ndvi_mask_sp <- RR %>% as("SpatialPixelsDataFrame") %>% as.data.frame() 

plot_ndvi<- ggplot() + geom_tile(data=ndvi_mask_sp, aes(x=x, y=y, fill=layer)) +
  scale_fill_gradientn(colours= rev(terrain.colors(5)), name="ndvi") +
  geom_sf(data=COORD, aes(color=Punto), size=5) +
  annotation_north_arrow(location='tl', 
                         width=unit(1,"cm"),
                         height=unit(1,"cm"),
                         style=north_arrow_fancy_orienteering(text_size=10)) +
                         theme_dark()

plot_ndvi1<- ggplot() + geom_tile(data=ndvi_mask_sp, aes(x=x, y=y, fill=layer)) +
  scale_fill_gradientn(colours= rev(terrain.colors(5)), name="ndvi") +
  geom_sf(data=COORD, aes(color=Rango), size=5) +
  annotation_north_arrow(location='tl', 
                         width=unit(1,"cm"),
                         height=unit(1,"cm"),
                         style=north_arrow_fancy_orienteering(text_size=10)) +
                         theme_dark()

Resumen<-(plot_ndvi|plot_ndvi1)
Resumen_ERT<-Resumen + plot_annotation(tag_levels = 'A')

ggsave(filename="Figura_4.png",
       plot = Resumen_ERT,
       dpi=300)

area<-res(knr)[1] * res(knr)[2]/10000
area

knr_tbl<-rasterToPoints(knr) %>% as_tibble()


nrow_knr1<-knr_tbl %>% filter(layer==1) %>% nrow()
nrow_knr2<-knr_tbl %>% filter(layer==2) %>% nrow()
nrow_knr3<-knr_tbl %>% filter(layer==3) %>% nrow()
nrow_knr4<-knr_tbl %>% filter(layer==4) %>% nrow()
nrow_knr5<-knr_tbl %>% filter(layer==5) %>% nrow()
sum(nrow_knr1, nrow_knr2, nrow_knr3, nrow_knr4, nrow_knr5)

Est<-data.frame(Cluster=c("1", "2", "3", "4", "5"),
                Valor=c(0.91, 0.71, 0.79, 0.54, 0.63),
                pixeles=c(nrow_knr1, nrow_knr2, nrow_knr3, nrow_knr4, nrow_knr5),
                Area= c(nrow_knr1*area, nrow_knr2*area, nrow_knr3*area, nrow_knr4*area, nrow_knr5*area)) %>%
                mutate(Porcentaje=c(nrow_knr1*area*100/sum(Area), nrow_knr2*area*100/sum(Area), nrow_knr3*area*100/sum(Area), nrow_knr4*area*100/sum(Area), nrow_knr5*area*100/sum(Area)))

write.csv(Est, "Estadística_Raster.csv")

Sector<-st_read("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 2/NDVI/NDVI José Tomas/FIA/105/Sector.shp")
Sector<-Sector %>% mutate(Sector= c(1.1, 1.2, 2.1, 2.2))

S1_1<-Sector %>% filter(Sector==1.1)
knr1_1<-mask(knr, S1_1)
knr1_1<-crop(knr1_1, S1_1)

S1_2<-Sector %>% filter(Sector==1.2)
knr1_2<-mask(knr, S1_2)
knr1_2<-crop(knr1_2, S1_2)

S2_1<-Sector %>% filter(Sector==2.1)
knr2_1<-mask(knr, S2_1)
knr2_1<-crop(knr2_1, S2_1)

S2_2<-Sector %>% filter(Sector==2.2)
knr2_2<-mask(knr, S2_2)
knr2_2<-crop(knr2_2, S2_2)

knr1_1tbl<-rasterToPoints(knr1_1) %>% as_tibble()
nrow1_S1_1<-knr1_1tbl %>% filter(layer==1) %>% nrow()
nrow2_S1_1<-knr1_1tbl %>% filter(layer==2) %>% nrow()
nrow3_S1_1<-knr1_1tbl %>% filter(layer==3) %>% nrow()
nrow4_S1_1<-knr1_1tbl %>% filter(layer==4) %>% nrow()
nrow5_S1_1<-knr1_1tbl %>% filter(layer==5) %>% nrow()

knr1_2tbl<-rasterToPoints(knr1_2) %>% as_tibble()
nrow1_S1_2<-knr1_2tbl %>% filter(layer==1) %>% nrow()
nrow2_S1_2<-knr1_2tbl %>% filter(layer==2) %>% nrow()
nrow3_S1_2<-knr1_2tbl %>% filter(layer==3) %>% nrow()
nrow4_S1_2<-knr1_2tbl %>% filter(layer==4) %>% nrow()
nrow5_S1_2<-knr1_2tbl %>% filter(layer==5) %>% nrow()

knr2_1tbl<-rasterToPoints(knr2_1) %>% as_tibble()
nrow1_S2_1<-knr2_1tbl %>% filter(layer==1) %>% nrow()
nrow2_S2_1<-knr2_1tbl %>% filter(layer==2) %>% nrow()
nrow3_S2_1<-knr2_1tbl %>% filter(layer==3) %>% nrow()
nrow4_S2_1<-knr2_1tbl %>% filter(layer==4) %>% nrow()
nrow5_S2_1<-knr2_1tbl %>% filter(layer==5) %>% nrow()

knr2_2tbl<-rasterToPoints(knr2_2) %>% as_tibble()
nrow1_S2_2<-knr2_2tbl %>% filter(layer==1) %>% nrow()
nrow2_S2_2<-knr2_2tbl %>% filter(layer==2) %>% nrow()
nrow3_S2_2<-knr2_2tbl %>% filter(layer==3) %>% nrow()
nrow4_S2_2<-knr2_2tbl %>% filter(layer==4) %>% nrow()
nrow5_S2_2<-knr2_2tbl %>% filter(layer==5) %>% nrow()

Sector1_1<-data.frame(Cluster=c("1", "2", "3", "4", "5"),
                Valor=c(0.91, 0.71, 0.79, 0.54, 0.63),
                pixeles=c(nrow1_S1_1, nrow2_S1_1, nrow3_S1_1, nrow4_S1_1, nrow5_S1_1),
                Area= c(nrow1_S1_1*area, nrow2_S1_1*area, nrow3_S1_1*area, nrow4_S1_1*area, nrow5_S1_1*area)) %>%
                mutate(Porcentaje=c(nrow1_S1_1*area*100/sum(Area), nrow2_S1_1*area*100/sum(Area), nrow3_S1_1*area*100/sum(Area), nrow4_S1_1*area*100/sum(Area), nrow5_S1_1*area*100/sum(Area)))

Sector1_2<-data.frame(Cluster=c("1", "2", "3", "4", "5"),
                      Valor=c(0.91, 0.71, 0.79, 0.54, 0.63),
                      pixeles=c(nrow1_S1_2, nrow2_S1_2, nrow3_S1_2, nrow4_S1_2, nrow5_S1_2),
                      Area= c(nrow1_S1_2*area, nrow2_S1_2*area, nrow3_S1_2*area, nrow4_S1_2*area, nrow5_S1_2*area)) %>%
                      mutate(Porcentaje=c(nrow1_S1_2*area*100/sum(Area), nrow2_S1_2*area*100/sum(Area), nrow3_S1_2*area*100/sum(Area), nrow4_S1_2*area*100/sum(Area), nrow5_S1_2*area*100/sum(Area)))

Sector2_1<-data.frame(Cluster=c("1", "2", "3", "4", "5"),
                      Valor=c(0.91, 0.71, 0.79, 0.54, 0.63),
                      pixeles=c(nrow1_S2_1, nrow2_S2_1, nrow3_S2_1, nrow4_S2_1, nrow5_S2_1),
                      Area= c(nrow1_S2_1*area, nrow2_S2_1*area, nrow3_S2_1*area, nrow4_S2_1*area, nrow5_S2_1*area)) %>%
                      mutate(Porcentaje=c(nrow1_S2_1*area*100/sum(Area), nrow2_S2_1*area*100/sum(Area), nrow3_S2_1*area*100/sum(Area), nrow4_S2_1*area*100/sum(Area), nrow5_S2_1*area*100/sum(Area)))

Sector2_2<-data.frame(Cluster=c("1", "2", "3", "4", "5"),
                      Valor=c(0.91, 0.71, 0.79, 0.54, 0.63),
                      pixeles=c(nrow1_S2_2, nrow2_S2_2, nrow3_S2_2, nrow4_S2_2, nrow5_S2_2),
                      Area= c(nrow1_S2_2*area, nrow2_S2_2*area, nrow3_S2_2*area, nrow4_S2_2*area, nrow5_S2_2*area)) %>%
                      mutate(Porcentaje=c(nrow1_S2_2*area*100/sum(Area), nrow2_S2_2*area*100/sum(Area), nrow3_S2_2*area*100/sum(Area), nrow4_S2_2*area*100/sum(Area), nrow5_S2_2*area*100/sum(Area)))

Resumen_Sector<-rbind(Sector1_1, Sector1_2, Sector2_1, Sector2_2) %>% 
  mutate(Sec_riego=c(1.1,1.1,1.1,1.1,1.1,
                     1.2,1.2,1.2,1.2,1.2,
                     2.1,2.1,2.1,2.1,2.1,
                     2.2,2.2,2.2,2.2,2.2))

write.csv(Resumen_Sector, "Resumen_Sector.csv")

sp1_1 <- knr1_1 %>% as("SpatialPixelsDataFrame") %>% as.data.frame()
sp1_2 <- knr1_2 %>% as("SpatialPixelsDataFrame") %>% as.data.frame()
sp2_1 <- knr2_1 %>% as("SpatialPixelsDataFrame") %>% as.data.frame()
sp2_2 <- knr2_2 %>% as("SpatialPixelsDataFrame") %>% as.data.frame()

plot1_1<- ggplot() + geom_tile(data=sp1_1, aes(x=x, y=y, fill=layer)) +
          scale_fill_gradientn(colours= rev(terrain.colors(5)), name="Cluster") +
          annotation_north_arrow(location='tl', 
                                  width=unit(1,"cm"),
                                  height=unit(1,"cm"),
                                  style=north_arrow_fancy_orienteering(text_size=10)) +
        theme_dark()

plot1_2<- ggplot() + geom_tile(data=sp1_2, aes(x=x, y=y, fill=layer)) +
          scale_fill_gradientn(colours= rev(terrain.colors(5)), name="Cluster") +
          annotation_north_arrow(location='tl', 
                         width=unit(1,"cm"),
                         height=unit(1,"cm"),
                         style=north_arrow_fancy_orienteering(text_size=10)) +
          theme_dark()

plot2_1<- ggplot() + geom_tile(data=sp2_1, aes(x=x, y=y, fill=layer)) +
          scale_fill_gradientn(colours= rev(terrain.colors(5)), name="Cluster") +
          annotation_north_arrow(location='tl', 
                         width=unit(1,"cm"),
                         height=unit(1,"cm"),
                         style=north_arrow_fancy_orienteering(text_size=10)) +
          theme_dark()
                                
plot2_2<- ggplot() + geom_tile(data=sp2_2, aes(x=x, y=y, fill=layer)) +
          scale_fill_gradientn(colours= rev(terrain.colors(5)), name="Cluster") +
          annotation_north_arrow(location='tl', 
                         width=unit(1,"cm"),
                         height=unit(1,"cm"),
                         style=north_arrow_fancy_orienteering(text_size=10)) +
          theme_dark()


GG_SECTOR<-(plot1_1|plot1_2)/(plot2_1|plot2_2)
Cluster_Plot<-GG_SECTOR + plot_annotation(tag_levels = 'A')

ggsave(filename="Figura_5.png",
       plot = Cluster_Plot,
       dpi=300,
       units="in",
       width= 8,
       height=6)
