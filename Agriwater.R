#SINGLE VALUES OF AGROMETEOROLOGICAL DATA
#SENTINEL-2

#'
#' Crop coefficient (ETa / ET0) using Sentinel-2 images with single agrometeorological data.
#'
#' @param doy   is the Day of Year (DOY)
#' @param RG  is the global solar radiation
#' @param Ta  is the average air temperature
#' @param a  is one of the regression coefficients of SAFER algorithm
#' @param b  is one of the regression coefficients of SAFER algorithm
#' @export
#' @import raster
#' @import sp
#' @import rgdal
#' @importFrom utils read.csv
#'
#' @return It returns in raster format (.tif) the Surface Albedo at 24h scale ("Alb_24"), NDVI, Surface Temperature ("LST"), Crop Coefficient ("kc") and net radiation ("Rn_MJ").
#' @examples
#' library(agriwater)
#'
#' # dependencies of package 'agriwater'
#' library(sp)
#' library(raster)
#' library(rgdal)
#' rgdal_show_exportToProj4_warnings"="none"
#'
#'
#' # Using a temporary folder to run example
#' wd <- tempdir()
#' initial = getwd()
#' setwd(wd)
#'
#' #Exiting temporary folder and returning to previous workspace
#' setwd(initial)


library("tidyverse")
library("agriwater")
library("sp")
library("rgdal")
library("raster")
library("sf")
library("patchwork")
library("openxlsx")
library("data.table")


doy = 3
a = 1.8
b = -0.008
RG = 25.36
Ta = 22.67
ET0=5.56

setwd("C:/Users/Pablo Diaz/Desktop/Procesamiento_R")
dir <- list.files(pattern = "T19HBB",full.names = T)
names(dir)<- c("B2", "B3", "B4", "B8")
mask_utm<-readOGR(file.choose()) 
Sentinel2 <- stack(dir)

names(Sentinel2)<- c("B2", "B3", "B4", "B8")
b2<- Sentinel2$B2 # B2
b3<- Sentinel2$B3 # B3
b4<- Sentinel2$B4 # B4
b8<- Sentinel2$B8 # B5


sentinel2<-crop(Sentinel2,mask_utm)
sentinel2<-mask(sentinel2,mask_utm)

names(sentinel2)<- c("b2_mascara", "b3_mascara", "b4_mascara", "b8_mascara")

b2_mascara<- sentinel2$b2_mascara/10000 # B2_MASK
crs="+proj=longlat"
b2_mascara<-projectRaster(b2_mascara,crs=crs)

b3_mascara<- sentinel2$b3_mascara/10000 # B3_MASK
crs="+proj=longlat +datum=WGS84"
b3_mascara<-projectRaster(b3_mascara,crs=crs)

b4_mascara<- sentinel2$b4_mascara/10000 # B4_MASK
crs="+proj=longlat +datum=WGS84"
b4_mascara<-projectRaster(b4_mascara,crs=crs)

b8_mascara<- sentinel2$b8_mascara/10000 # B8_MASK
crs="+proj=longlat +datum=WGS84"
b8_mascara<-projectRaster(b8_mascara,crs=crs)

Alb_Top = b2_mascara*0.32+b3_mascara*0.26+b4_mascara*0.25+b8_mascara*0.17

Alb_sur = 0.6054*Alb_Top + 0.0797

Alb_24 =  1.0223*Alb_sur + 0.0149


writeRaster(Alb_24, "Alb_24", format = "GTiff", overwrite=TRUE)

NDVI =(b8_mascara-b4_mascara)/(b8_mascara+b4_mascara)

writeRaster(NDVI, "NDVI", format = "GTiff", overwrite=TRUE)

mask_long_lat<-spTransform(mask_utm, CRS("+proj=longlat"))
lati <- long <- b2_mascara
xy <- coordinates(b2_mascara)
long[] <- xy[, 1]
long <- crop(long, extent(mask_long_lat))
lati[] <- xy[, 2]
lati <- crop(lati, extent(mask_long_lat))

map1 <- (long/long)*((2*pi)/365)*(doy-1)


Et <- (0.000075+0.001868*cos(map1)-0.032077*sin(map1)-0.014615*cos(2*map1)-0.04089*sin(2*map1))

LAT <- (13+(4*long/60)+(Et/60))

Dec <- 0.006918-0.399912*cos(map1)+0.070257*sin(map1)+0.006758*cos(2*map1)+0.000907*sin(2*map1)-0.002697*cos(3*map1)+0.00148*sin(3*map1)

W <- 15*(LAT-12)*(pi/180)

cos_zwn <- sin(lati*pi/180)*sin(Dec)+cos(lati*pi/180)*cos(Dec)*cos(W)

E0 <- (1.00011+0.034221*cos(map1)+0.00128*sin(map1)+0.000719*cos(2*map1)+0.000077*sin(2*map1))

Ws = acos(((-1)*tan(lati*pi/180))*tan(Dec))

R =(Ws*sin(lati*pi/180)*sin(Dec))+(cos(lati*pi/180)*cos(Dec)*sin(Ws))

RsTOP_aux =(1367/pi)*E0*R

RsTOP = resample(RsTOP_aux, b2_mascara, method="bilinear")

Transm =(RG*11.6)/RsTOP

Rn_coeff =6.99*Ta-39.99

Rn =((1-Alb_24)*(RG*11.6))-(Rn_coeff*Transm)

Rn_MJ =Rn/11.6

writeRaster(Rn_MJ, "Rn_MJ", format = "GTiff", overwrite=TRUE)

slope =(4098*(0.6108*exp((17.27*(Ta))/((Ta)+237.3)))/((Ta)+237.3)^2)

LEeq = (slope*Rn)/(slope+0.066)

rm(b2, b3, b4, b8, b2_mascara, b3_mascara, b4_mascara, b8_mascara, slope, Rn_coeff, RsTOP, RsTOP_aux, R, Ws, E0, cos_zwn, W, Dec, LAT, Et, map1, lati, long)

RR =Alb_24*RG

Emiss_atm = 0.9364*(((-1)*log(Transm))^0.1135)

Emiss_atm[Emiss_atm > 1] <- 1

RLdown_wm2 =(Emiss_atm*5.67*(10^(-8))*((Ta +273.15)^4))

RL_down =RLdown_wm2/11.6

RL_up =(RG-RR+RL_down-Rn_MJ)

Esurf_r1 <- NDVI

Esurf_r1[NDVI < 0] <- 1

Esurf_r1[NDVI >= 0] <- NA

Esurf_r2 <- 1.0035+0.0589*log(NDVI)

Esurf <- merge(Esurf_r1, Esurf_r2)

TS24 =((RL_up*11.6)/((Esurf*5.67)*(10^(-8))))^(0.25)

TS24[TS24 < 273.15] = NA

writeRaster(TS24, "LST", format = "GTiff", overwrite=TRUE)

NDVI[NDVI <= 0] = NA

kc=exp((a)+(b*((TS24-273.15)/(Alb_24*NDVI))))

writeRaster(kc, "kc", format = "GTiff", overwrite=TRUE)

plot(kc)

ET<-ET0*kc

plot(ET)

#'Energy balance using Sentinel-2 images with single agrometeorological data.
#'@param doy is the Day of Year (DOY)
#'@param RG is the global solar radiation
#'@param Ta is the average air temperature
#'@param ET0  is the reference evapotranspiration
#'@param a is one of the regression coefficients of SAFER algorithm
#'@param b is one of the regression coefficients of SAFER algorithm
#'@export
#'@import raster
#' @import sp
#' @import rgdal
#' @importFrom utils read.csv
#'
#' @return It returns in raster format (.tif) the Surface Albedo at 24h scale ("Alb_24"), NDVI, Surface Temperature ("LST"), Crop Coefficient ("kc"), Actual Evapotranspiration (evapo), latent heat flux "LE_MJ"), net radiation ("Rn_MJ"), ground heat flux ("G_MJ") and the sensible heat flux ("H_MJ").
#' @examples
#' library(agriwater)
#'
#' # dependencies of package 'agriwater'
#' library(sp)
#' library(raster)
#' library(rgdal)
#' rgdal_show_exportToProj4_warnings"="none"
#'
#'
#' # Using a temporary folder to run example
#' wd <- tempdir()
#' initial = getwd()
#' setwd(wd)

LE_MJ =ET*2.45

writeRaster(LE_MJ, "LE_MJ", format = "GTiff", overwrite=TRUE)

G_Rn =3.98*exp(-25.47*Alb_24)

G_MJ =G_Rn*Rn_MJ

writeRaster(G_MJ, "G_MJ", format = "GTiff", overwrite=TRUE)

H_MJ =Rn_MJ-LE_MJ-G_MJ

writeRaster(H_MJ, "H_MJ", format = "GTiff", overwrite=TRUE)

plot(LE_MJ)
plot(G_MJ)
plot(H_MJ)

Equipo_2010<-readOGR(file.choose()) 
Sector<-c("1","1", "2","2","3","4","4","5","5","6")
Equipo_2010<-cbind(Equipo_2010, Sector)
names(Equipo_2010)<-c("Sub_Unidad", "LAYER", "KML_STYLE", "tessellate", "Sector", "geometry")
Equipo_2010<-st_as_sf(Equipo_2010)

st_crs(Equipo_2010)<-st_crs(kc)

ggplot() + geom_sf(data=Equipo_2010, aes(fill=Sector), alpha=0.5)


kc1<-kc %>% as("SpatialPixelsDataFrame") %>% as.data.frame()
LE1<-LE %>% as("SpatialPixelsDataFrame") %>% as.data.frame()
ET1<-ET %>% as("SpatialPixelsDataFrame") %>% as.data.frame()

ggplot() + geom_tile(data=kc1, aes(x=x, y=y, fill=layer))

plot_kc<-ggplot() + geom_tile(data=kc1, aes(x=x, y=y, fill=layer))+
  scale_fill_gradientn(colours= rev(terrain.colors(100)), name="Kc")+
  geom_sf(data=Equipo_2010, aes(color=Sector), alpha=0, size=1.5)

plot_LE<-ggplot() + geom_tile(data=kc1, aes(x=x, y=y, fill=layer))+
  scale_fill_gradientn(colours= rev(terrain.colors(100)), name="LE")+
  geom_sf(data=Equipo_2010, aes(color=Sector), alpha=0, size=1.5)

plot_ET<-ggplot() + geom_tile(data=kc1, aes(x=x, y=y, fill=layer))+
  scale_fill_gradientn(colours= rev(terrain.colors(100)), name="ET") +
  geom_sf(data=Equipo_2010, aes(color=Sector), alpha=0, size=1.5)


Data<-stack(NDVI, kc, ET, LE_MJ, H_MJ, G_MJ)
names(Data)<-c("ndvi", "kc", "ET", "LE","H", "G")
plot(Data[[2]])
vles_mean<-raster::extract(Data, Equipo_2010, fun=mean, na.rm= TRUE, df=TRUE)
vles_max<-raster::extract(Data, Equipo_2010, fun=max, na.rm= TRUE, df=TRUE)
vles_min<-raster::extract(Data, Equipo_2010, fun=min, na.rm= TRUE, df=TRUE)
vles<-cbind(Equipo_2010,vles_mean)
head(vles)

sector_riego<-ggplot() + geom_sf(data=Equipo_2010, aes(fill=Sector), alpha=0.5)
ndvi_sector<-ggplot() + geom_sf(data=vles, aes(fill=ndvi))
kc_sector<-ggplot()+ geom_sf(data=vles, aes(fill=kc))
ET_sector<-ggplot() + geom_sf(data=vles, aes(fill=ET))

Resumen<-(sector_riego|ndvi_sector)/(kc_sector|ET_sector)
Resumen

Riego<-read.xlsx("Riego.xlsx")
class(Riego)

vles1<-cbind(vles, Riego)
vles2<-as.data.table(vles1)
vles2<-vles2[,.(KC=max(kc), ETc=max(ET), pp=mean(PP)), by=Sector]
vles2<-vles2[ , horas := (ETc/0.9/pp) ]
vles2<-vles2[, FR := pp*5/(ETc/0.9)]
vles3<-as.data.table(vles1)
vles3<-vles3[ ,.(kc, ET, PP), by=.(Sector, Sub_Unidad) ]
vles3<-vles3[ , hh := (ET/0.9/PP) ]
vles3

