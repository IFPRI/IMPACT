##########################Cleaned version 2S ##############################################


#this code is to be run only ones to produce the necessary spatial layers for the soil pathway
#this code prepares the burkina faso soil data
# all layers are in WGS 84 unprojected (long/lat) 
#AFSIS have to be reprojected before
#optimal in terms of time is batch reprojection in qgis



#clearing all memory 
rm(list=ls(all=TRUE))

#library(maptools)
library(raster)
library(igraph)
library( plotrix)
library(maptools)
setwd('D:/Dropbox/CLEANED - IMPACT')
# IPCC definition 
# species<-'dairyCows'
# #Africa, Asia, Latin America
# region='Africa'
# #base layers 
# #area definition
# 
sarea<-readShapePoly('1-input/spatial/TZ.shp')

lu<-raster('1-input/spatial/lu/lu.tif')
rr<-raster('1-input/spatial/rr.tif')
rain<-raster('1-input/spatial/rain.tif')
proj<-projection(rr)


ff <- c('af_NTO_T__M_xd2_250m', 'af_NTO_T__M_xd1_250m')
nms <- c('SN2', 'SN1')
ff <- paste0('../CleanedS/0-raw input/spatial/soil/', ff, '.tif')
s <- stack(ff)
names(s) <- nms

SN2<-raster('../CleanedS/0-raw input/spatial/soil/af_NTO_T__M_xd2_250m.tif')
#SN2<-projectRaster(SN2,crs=proj)
SN1<-raster('../CleanedS/0-raw input/spatial/soil/af_NTO_T__M_xd1_250m.tif')
#SN1<-projectRaster(SN1,crs=proj)
BD1<-raster('../CleanedS/0-raw input/spatial/soil/af_BLD_T__M_sd1_250m.tif')
#BD1<-projectRaster(BD1,crs=proj)
BD2<-raster('../CleanedS/0-raw input/spatial/soil/af_BLD_T__M_sd2_250m.tif')
#BD2<-projectRaster(BD2,crs=proj)
BD3<-raster('../CleanedS/0-raw input/spatial/soil/af_BLD_T__M_sd3_250m.tif')
#BD3<-projectRaster(BD3,crs=proj)
SL1<-raster('../CleanedS/0-raw input/spatial/soil/af_SLTPPT_T__M_sd1_250m.tif')
#SL1<-projectRaster(SL1,crs=proj)
SL2<-raster('../CleanedS/0-raw input/spatial/soil/af_SLTPPT_T__M_sd2_250m.tif')
#SL2<-projectRaster(SL2,crs=proj)
SL3<-raster('../CleanedS/0-raw input/spatial/soil/af_SLTPPT_T__M_sd3_250m.tif')
#SL3<-projectRaster(SL3,crs=proj)
CL1<-raster('../CleanedS/0-raw input/spatial/soil/af_CLYPPT_T__M_sd1_250m.tif')
#CL1<-projectRaster(CL1,crs=proj)
CL2<-raster('../CleanedS/0-raw input/spatial/soil/af_CLYPPT_T__M_sd2_250m.tif')
#CL2<-projectRaster(CL1,crs=proj)
CL3<-raster('../CleanedS/0-raw input/spatial/soil/af_CLYPPT_T__M_sd3_250m.tif')
#CL3<-projectRaster(CL1,crs=proj)
C1<-raster('../CleanedS/0-raw input/spatial/soil/af_ORCDRC_T__M_sd1_250m.tif')
#C1<-projectRaster(C1,crs=proj)
C2<-raster('../CleanedS/0-raw input/spatial/soil/af_ORCDRC_T__M_sd2_250m.tif')
#C2<-projectRaster(C2,crs=proj)
C3<-raster('../CleanedS/0-raw input/spatial/soil/af_ORCDRC_T__M_sd3_250m.tif')
#C3<-projectRaster(C3,crs=proj)
CF1<-raster('../CleanedS/0-raw input/spatial/soil/af_CRFVOL_T__M_sd1_250m.tif')
#CF1<-projectRaster(CF1,crs=proj)
CF2<-raster('../CleanedS/0-raw input/spatial/soil/af_CRFVOL_T__M_sd2_250m.tif')
#CF2<-projectRaster(CF2,crs=proj)
CF3<-raster('../CleanedS/0-raw input/spatial/soil/af_CRFVOL_T__M_sd3_250m.tif')
#CF3<-projectRaster(CF3,crs=proj)
TEX<-raster('../CleanedS/0-raw input/spatial/soil/af_TEXMHT_T__M_sd2_250m.tif')
#TEX<-projectRaster(TEX,crs=proj)
LS<-raster('../CleanedS/0-raw input/spatial/soil/ls_afr250.tif')
R<-raster('../CleanedS/0-raw input/spatial/soil/R_EI30rescaled_TMPA1998-2012_Africa_geotiff.tif')
projection (R)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
lai<-raster('../CleanedS/0-raw input/spatial/soil/LAI_min_2010.tif')
projection(lai)<-CRS("+init=epsg:4326")
lai<-projectRaster(lai,crs=proj)
SD1<-raster('../CleanedS/0-raw input/spatial/soil/af_SNDPPT_T__M_sd1_250m.tif')
SD2<-raster('../CleanedS/0-raw input/spatial/soil/af_SNDPPT_T__M_sd2_250m.tif')
SD3<-raster('../CleanedS/0-raw input/spatial/soil/af_SNDPPT_T__M_sd3_250m.tif')
pixel<-read.csv('1-input/parameter/pixel.csv')
pixel<-pixel$x
SN2<-resample(SN2,rr)
SN1<-resample(SN1,rr)
BD1<-resample(BD1,rr)
BD2<-resample(BD2,rr)
BD3<-resample(BD3,rr)
CL1<-resample(CL1,rr)
CL2<-resample(CL2,rr)
CL3<-resample(CL3,rr)
SL1<-resample(SL1,rr)
SL2<-resample(SL2,rr)
SL3<-resample(SL3,rr)
CF1<-resample(C1,rr)
CF2<-resample(C2,rr)
CF3<-resample(C3,rr)
SD1<-resample(SD1,rr)
SD2<-resample(SD2,rr)
SD3<-resample(SD3,rr)
TEX<-resample(TEX,rr)
C1<-resample(C1,rr)
C2<-resample(C2,rr)
C3<-resample(C3,rr)
R<-resample(R,rr)
LS<-resample(LS,rr,method='ngb')
lai<-resample(lai,rr)



SN2<-mask(SN2,sarea)
SN1<-mask(SN1,sarea)
BD1<-mask(BD1,sarea)
BD2<-mask(BD2,sarea)
BD3<-mask(BD3,sarea)
CL1<-mask(CL1,sarea)
CL2<-mask(CL2,sarea)
CL3<-mask(CL3,sarea)
SL1<-mask(SL1,sarea)
SL2<-mask(SL2,sarea)
SL3<-mask(SL3,sarea)
CF1<-mask(CF1,sarea)
CF2<-mask(CF2,sarea)
CF3<-mask(CF3,sarea)
TEX<-mask(TEX,sarea)
SD1<-mask(SD1,sarea)
SD2<-mask(SD2,sarea)
SD3<-mask(SD3,sarea)
C1<-mask(C1,sarea)
C2<-mask(C2,sarea)
C3<-mask(C3,sarea)
R<-mask(R,sarea)
LS<-mask(LS,sarea)
lai<-mask(lai,sarea)


fun<-function(a,b,c){(a+b+c)/3}
CLavg<-overlay(CL1,CL2,CL3, fun=mean)
BDavg<-overlay(BD1,BD2,BD3, fun=fun)
SLavg<-overlay(SL1,SL2,SL3,fun=fun)
SDavg<-overlay(SD1,SD2,SD3,fun=fun)
Cavg<-overlay(C1,C2,C3,fun=fun)
CFavg<-overlay(CF1,CF2,CF3,fun=fun)
CLavg<-overlay(CL1,CL2,CL3,fun=fun)
plot(SLavg)

#creation of Nsoil used in L computaion 
fun<-function(x,y){x*0.001*y*0.2*(pixel*pixel/10000)}
Nsoil<-overlay(SN1,BDavg,fun=fun)

#this is required in L compuation
m<-c(0,35,0,35.0001,55,1,55.0001,100,0)
rclmat<-matrix(m,ncol=3,byrow=T)
CLclass2<-reclassify(CLavg,rclmat)

m<-c(0,35,0,35.0001,55,0,55.0001,100,1)
rclmat<-matrix(m,ncol=3,byrow=T)
CLclass3<-reclassify(CLavg,rclmat)


fun=function(x,y){(0.0138*x+1.4615)*y}
RFc2<-overlay(rain,CLclass2,fun=fun)

fun=function(x,y){(0.0066*x+6.2538)*y}
RFc3<-overlay(rain,CLclass3,fun=fun)

#atmospheric deposition
Apara=0.14*(pixel*pixel/10000) #per pixel 
fun<-function(x){Apara*x^0.5}
A<-overlay(rain, fun=fun)

writeRaster(A,'1-input/spatial/soil/A.tif',overwrite=T)
writeRaster(SN2,'1-input/spatial/soil/SN2.tif',overwrite=T)
writeRaster(SN1,'1-input/spatial/soil/SN1.tif',overwrite=T)
writeRaster(BD1,'1-input/spatial/soil/BD1.tif',overwrite=T)
writeRaster(BD2,'1-input/spatial/soil/BD2.tif',overwrite=T)
writeRaster(BD3,'1-input/spatial/soil/BD3.tif',overwrite=T)
writeRaster(CL1,'1-input/spatial/soil/CL1.tif',overwrite=T)
writeRaster(CL2,'1-input/spatial/soil/CL2.tif',overwrite=T)
writeRaster(CL3,'1-input/spatial/soil/CL3.tif',overwrite=T)
writeRaster(SL1,'1-input/spatial/soil/SL1.tif',overwrite=T)
writeRaster(Nsoil,'1-input/spatial/soil/Nsoil.tif',overwrite=T)
writeRaster(CLclass2,'1-input/spatial/soil/CLclass2.tif',overwrite=T)
writeRaster(CLclass3,'1-input/spatial/soil/CLclass3.tif',overwrite=T)
writeRaster(RFc2,'1-input/spatial/soil/RFc2.tif',overwrite=T)
writeRaster(RFc3,'1-input/spatial/soil/RFc3.tif',overwrite=T)
writeRaster(CLavg,'1-input/spatial/soil/CLavg.tif',overwrite=T)

#remove(SL1,SL2,SL3,C1,C2,C3,CF1,CF2,CF3,SD1,SD2,SD3, CL1,CL2,CL3)

########################################RUSLE##########################################
####################################################################
# E = R * K * C * LS * P   
# 
# Where
# 
# E:  Annual average soil loss (t ha-1 yr-1),
# R: Rainfall Erosivity factor (MJ mm ha-1 h-1 yr-1),
# K: Soil Erodibility factor (t ha h ha-1 MJ-1 mm-1),
# C: Cover-Management factor (dimensionless),
# LS:  Slope Length and Slope Steepness factor (dimensionless),
# P: Support practices factor (dimensionless).



#####################################################################
#compute K 

# K = [(2.1 x 10-4 M1.14 (12 - OM) +3.25 (s -2) + 2.5 (p - 3) ) / 100 ] * 0.1317      (1)
# http://esdac.jrc.ec.europa.eu/themes/soil-erodibility-europe

# compute M  = (msilt +  mvfs) * (100 - mc );
fun<-function(x,y,z){(x+y)*(100-z)}
M<-overlay(SLavg,SDavg,CLavg, fun=fun)
#compute SOM*0.58 = soil carbon 
#/10 is because OM is in per mille
t=1/0.58/10
fun<-function(x){x*t}
SOM<-overlay(Cavg, fun=fun)
gc()

#compute s
# we are making two classes in stead of 4
# see table 1 in http://www.sciencedirect.com/science/article/pii/S0048969714001727
#15% is from FAO guide ftp://ftp.fao.org/agl/agll/docs/guidel_soil_descr.pdf p21, everything between 0-15% is common so 15% of CF = fine gets value 1
# all the rest gets value 2.5 as average between between the other classes (4-1)/2 +1 

m<-c(0,15,1,15.0001,100,2.5)
rclmat<-matrix(m,ncol=3,byrow=T)
s<-reclassify(CFavg,rclmat)

m<-c(12,1,11,2,9,2,7,3,8,3,6,4,4,4,5,5,3,5,1,6,2,6,10,3)
rclmat<-matrix(m,ncol=2,byrow=T)
p<-reclassify(TEX,rclmat)
plot(p)
# compute permeability 
#use here table 2 in http://www.sciencedirect.com/science/article/pii/S0048969714001727
#allocate based on the texture soil class
fun<-function(a,b,c,d){((2.1*10^-4*a^1.14*(12-b)+3.25*(c-2)+2.5*(d-3))/100)*0.1317}
K<-overlay(M,SOM,s,p,fun=fun)
gc()
########################################################################
#compute C and P
#create a layer based on Smaling C factors
#crop get 0.3 
#grazing land 0.05
#rest get 0.01 forest and others 

lai[lai@data@values==0]<-NaN
fun<-function(x){1/x}
ilai<-calc(lai,fun=fun)

#normalize C
ilai_min<-cellStats(ilai,stat=min )
ilai_max<-cellStats(ilai,stat=max )
fun<-function(x){(x-ilai_min)/(ilai_max-ilai_min)}
C<-overlay(ilai,fun=fun)

#compute E =>E = R * K * C * LS * (P) #convert erosion into kg/pixel
fun<-function(a,b,c,d){(a*b*c*d)}#
E<-overlay(R,K,C,LS,fun=fun, filename='1-input/spatial/soil/E.tif', overwrite=TRUE)
plot(E)

# if the above fails because of memory limitations, change 'rasterOptions chunksize, maxmemory
# but using the filename= argument in overlay (instead of writeRaster afterwards), may have fixed it
# alternative
#E <- prod(stack(R,K,C,LS))
#E <- prod(R,K,C,LS)



