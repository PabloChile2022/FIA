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
RG = 28.4
Ta = 18.6
ET0=6.45

setwd("C:/Users/Pablo Diaz/Desktop/Pablo/FIA/Etapa 3/Cherry Sweet/Procesamiento R")


dir <- list.files(pattern = "T19HBB",full.names = T)
names(dir)<- c("B2", "B3", "B4", "B8")
mask_utm<-shapefile("Mask_UTM.shp") 
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

#Estadísticas por sector

Equipo_riego <- st_read("Equipos_Cherry_Sweet.shp")
Equipo_riego <-st_as_sf(Equipo_riego)
Riego <- read.xlsx("Riego.xlsx")


SectorA<-c("1","1","2","2","3","4","4","5","5","6", "7")
SectorB<-c("1","1","1","2","2","3","3","4","4","5","6","6")
SectorC<-c("1","1","2","2","2","3","3","3","4","4","4","5", "5", "5")

Sector<-c(SectorA, SectorB, SectorC)

Equipos<-cbind(Equipo_riego, Sector)

names(Equipos)<-c("Sub_Unidad", "LAYER", "KML_STYLE", "Equipo", "tessellate", "Sector", "geometry")

st_crs(Equipos)<-st_crs(kc)


Data<-stack(NDVI, kc, ET, LE_MJ, H_MJ, G_MJ)
names(Data)<-c("ndvi", "kc", "ET", "LE","H", "G")
plot(Data[[2]])
vles_mean<-raster::extract(Data, Equipos, fun=mean, na.rm= TRUE, df=TRUE)
vles_max<-raster::extract(Data, Equipos, fun=max, na.rm= TRUE, df=TRUE)
vles_min<-raster::extract(Data, Equipos, fun=min, na.rm= TRUE, df=TRUE)
vles<-cbind(Equipos,vles_mean)

Riego<-Riego %>% dplyr::select(Sub_Unidad, Plantas,Ha, PP, HR)

class(Riego)

vles<-merge(vles, Riego)



FINAL<-vles %>% group_by(Equipo, Sector) %>% summarise (
  ndvi=mean(ndvi), Kc=mean(kc), ET= mean(ET), pp=mean(PP)) %>%
  dplyr::mutate (HR=ET/0.85/pp) %>%
  dplyr::select(Equipo, Sector, ndvi, Kc, ET, pp, HR, geometry)


