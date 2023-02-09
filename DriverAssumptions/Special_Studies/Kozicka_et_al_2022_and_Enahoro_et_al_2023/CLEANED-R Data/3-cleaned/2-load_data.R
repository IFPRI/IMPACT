# this code loads all the data into the memory 
# it needs to be run before the feed basket 

# read libraries
library(raster)
library(maptools)
library(RColorBrewer)
library(rgdal)
setwd(path)
#read parameter
para<-read.csv('1-input/parameter/para.csv')
para$value<-as.numeric(para$value)

dim<-dim(para)
obs<-dim [1]

for(i in seq_along(para$name))
{
  assign(as.character(para$name[i]), para$value[i])
}


#read necessary spatial data
#from water
sarea<-readOGR('1-input/spatial', layer='TZ')
#proj4string(sarea)<-' +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 '
rain<-raster('1-input/spatial/rain.tif')
y_maize<-raster('1-input/spatial/y_maize.tif')
y_pulse<-raster('1-input/spatial/y_pulse.tif')
y_rice<-raster('1-input/spatial/y_rice.tif')
ET_gras<-raster('1-input/spatial/ET_gras.tif')
ET_grasl<-raster('1-input/spatial/ET_grasl.tif')
ET_maize<-raster('1-input/spatial/ET_maize.tif')
ET_pulse<-raster('1-input/spatial/ET_pulse.tif')
ET_rice<-raster('1-input/spatial/ET_pulse.tif')


# from soil 

#lu<-raster('D:/Dropbox/Cleaned version 2/1-input/spatial/lu/lu.tif')
rr<-raster('D:/Dropbox/Cleaned version 2/1-input/spatial/rr.tif')
SN2<-raster('1-input/spatial/soil/SN2.tif')
SN1<-raster('1-input/spatial/soil/SN1.tif')
BD1<-raster('1-input/spatial/soil/BD1.tif')
BD2<-raster('1-input/spatial/soil/BD2.tif')
BD3<-raster('1-input/spatial/soil/BD3.tif')
CL1<-raster('1-input/spatial/soil/CL1.tif')
CL2<-raster('1-input/spatial/soil/CL2.tif')
CL3<-raster('1-input/spatial/soil/CL3.tif')
Nsoil<-raster('1-input/spatial/soil/Nsoil.tif')
CLavg<-raster('1-input/spatial/soil/CLavg.tif')
#BDavg<-raster('1-input/spatial/soil/BDavg.tif')
E<-raster('1-input/spatial/soil/E.tif')

pixel<-read.csv('1-input/parameter/pixel.csv')
pixel<-pixel$x
A<-raster('1-input/spatial/soil/A.tif')


# from ghg pathway
#read spatial layer
tempr<-raster('1-input/spatial/tempr.tif')
#livdist is here a layer of 1 
livdist <-raster('1-input/spatial/dum.tif')
#climate<-raster('1-input/spatial/climate.tif')

soil<-raster('1-input/spatial/soil.tif')
soilref<-raster('1-input/spatial/soilref.tif')
Flu_c<-raster('1-input/spatial/Flu_c.tif')
Flu_pc<-raster('1-input/spatial/Flu_pc.tif')
Flu_rice<-raster('1-input/spatial/Flu_rice.tif')
Flu_sa<-raster('1-input/spatial/Flu_sa.tif')
clim_wtmoist<-raster('1-input/spatial/ipcc_climate/clim_wtmoist.tif')
clim_wtdry<-raster('1-input/spatial/ipcc_climate/clim_wtdry.tif')
clim_ctmoist<-raster('1-input/spatial/ipcc_climate/clim_ctmoist.tif')
clim_ctdry<-raster('1-input/spatial/ipcc_climate/clim_ctdry.tif')
clim_tr_mont<-raster('1-input/spatial/ipcc_climate/clim_tr_mont.tif')
clim_tr_wet<-raster('1-input/spatial/ipcc_climate/clim_tr_wet.tif')
clim_tr_moist<-raster('1-input/spatial/ipcc_climate/clim_tr_moist.tif')
clim_tr_dry<-raster('1-input/spatial/ipcc_climate/clim_tr_dry.tif')
graz_clim<-raster('1-input/spatial/graz_clim.tif')
forest_clim<-raster('1-input/spatial/forest_clim.tif')
y_maize<-raster('1-input/spatial/y_maize.tif')
y_pulse<-raster('1-input/spatial/y_pulse.tif')
y_grass<-raster('1-input/spatial/y_grass.tif')


l10<-raster('1-input/spatial/lu/l10.tif')
l11<-raster('1-input/spatial/lu/l11.tif')
l12<-raster('1-input/spatial/lu/l12.tif')
l20<-raster('1-input/spatial/lu/l20.tif')
l30<-raster('1-input/spatial/lu/l30.tif')
l40<-raster('1-input/spatial/lu/l40.tif')
l50<-raster('1-input/spatial/lu/l50.tif')
l60<-raster('1-input/spatial/lu/l60.tif')
l61<-raster('1-input/spatial/lu/l61.tif')
l62<-raster('1-input/spatial/lu/l62.tif')
l70<-raster('1-input/spatial/lu/l70.tif')
l71<-raster('1-input/spatial/lu/l71.tif')
l72<-raster('1-input/spatial/lu/l72.tif')
l80<-raster('1-input/spatial/lu/l80.tif')
l81<-raster('1-input/spatial/lu/l81.tif')
l82<-raster('1-input/spatial/lu/l82.tif')
l90<-raster('1-input/spatial/lu/l90.tif')
l100<-raster('1-input/spatial/lu/l100.tif')
l110<-raster('1-input/spatial/lu/l110.tif')
l120<-raster('1-input/spatial/lu/l120.tif')
l121<-raster('1-input/spatial/lu/l121.tif')
l122<-raster('1-input/spatial/lu/l122.tif')
l130<-raster('1-input/spatial/lu/l130.tif')
l140<-raster('1-input/spatial/lu/l140.tif')
l150<-raster('1-input/spatial/lu/l150.tif')
l152<-raster('1-input/spatial/lu/l152.tif')
l153<-raster('1-input/spatial/lu/l153.tif')
l160<-raster('1-input/spatial/lu/l160.tif')
l170<-raster('1-input/spatial/lu/l170.tif')
l180<-raster('1-input/spatial/lu/l180.tif')
l190<-raster('1-input/spatial/lu/l190.tif')
l200<-raster('1-input/spatial/lu/l200.tif')
l201<-raster('1-input/spatial/lu/l201.tif')
l202<-raster('1-input/spatial/lu/l210.tif')
l220<-raster('1-input/spatial/lu/l220.tif')
disttocrop<-raster('1-input/spatial/disttocrop.tif')
disttocrop2<-raster('1-input/spatial/disttocrop2.tif')
#nearestcrop<-raster('1-input/spatial/nearestcrop.tif')

suit_c<-raster( '1-input/spatial/suit_c.tif')

# cropland<-l10+l20+l30+l40+l11+l12
# grazland<-l130+l120+l121+l122+l110+l100+l180



mms_lagoon_mcf <- raster('1-input/spatial/mms_lagoon_mcf.tif')
mms_liquidslurry_mcf <- raster('1-input/spatial/mms_liquidslurry_mcf.tif')
mms_solidstorage_mcf <- raster('1-input/spatial/mms_solidstorage_mcf.tif')
mms_drylot_mcf <- raster('1-input/spatial/mms_drylot_mcf.tif')
mms_pasture_mcf <- raster( '1-input/spatial/mms_pasture_mcf.tif')
mms_dailyspread_mcf <- raster( '1-input/spatial/mms_dailyspread_mcf.tif')
mms_digester_mcf <- raster( '1-input/spatial/mms_digester_mcf.tif')
mms_burned_mcf <- raster( '1-input/spatial/mms_burned_mcf.tif')
mms_other_mcf <- raster( '1-input/spatial/mms_other_mcf.tif')

ef_sg <- raster( '1-input/spatial/ef_sg.tif')
ef_pg <- raster( '1-input/spatial/ef_pg.tif')
ef_ch <- raster( '1-input/spatial/ef_ch.tif')



lucs<-0

