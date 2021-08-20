
# this is preparation code for CLEANED -IMPACT 

# now spatial data code extract the overall ESA 300m map, each category is extracted as 0/1 layer
# this is the grouping code that make land cover talk CLEANED categories
# and computes the distance to crop for the land cover change code. 
path<-'D:/Dropbox/CLEANED - IMPACT'
setwd(path)

library(raster)
library(igraph)
#here we need to define 
 ## what is cropland 
 ## what is grazland
 ## which cells can be converted and which cannot 
 ## create the distance to crop map that is needed for the LUC module. 
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
l202<-raster('1-input/spatial/lu/l202.tif')
l210<-raster('1-input/spatial/lu/l210.tif')
l220<-raster('1-input/spatial/lu/l220.tif')
parea<-raster('1-input/spatial/parea.tif')
suit_c<-raster( '1-input/spatial/suit_c.tif')
sarea<-readOGR('1-input/spatial', layer='TZ')

cropland<-l10+l20+l30+l11+l12+l40
# cropland <- l10 +l20+0.70*l30+0.25*l40
# grazland <- l130+0.10*l30+0.25*l40+0.25*l100+0.75*l110+0.1*l150+0.1*l153
# croplandB <- l10 +l20+l30+l40
# grazlandB <- l130+l30+l40+l100+l110+l150+l153
# nochange <- l220+l210+l202+l201+l200+l190

    
# assumptions 
    # l30  => 0.7=cropland, 0.1=herbaceous, 0.1 shrub, 0.1 tree
    # l40  => 0.25 cropland, 0.25 each of the other 3 
    # l100 => herbaceous=0.25, tree=0.75
    # l110 => herbaceous=0.75, tree=0.25
    # l150 & l153 heraceous =0.1


nochange <- l220+l210+l202+l201+l200+l190
parea[is.na(parea)]<-0
nochange_prot<-l220+l210+l202+l201+l200+l190+parea
nochange_prot[nochange_prot>0]<-1
#create the distance from original cropland layer
#create a layer with crop =1 and non changeable cell =2
fun=function(v,w){v+w*2}
temp<-overlay(cropland,nochange,fun=fun)
disttocrop<-gridDistance(temp,origin=1, omit=2 )
disttocrop<-mask(disttocrop,sarea)
writeRaster(disttocrop,'1-input/spatial/disttocrop.tif', overwrite=T)
writeRaster(nochange_prot,'1-input/spatial/nochange_prot.tif', overwrite=T)


#create the distance from original cropland layer with protected forest (at this stage assumed to be l50)
#create a layer with crop =1 and non changeable cell =2
fun=function(u,v){u+v*2}
temp<-overlay(l30+l40,nochange_prot,fun=fun)
disttol3040<-gridDistance(temp,origin=1, omit=2 )
disttol3040<-mask(disttol3040,sarea)
writeRaster(disttol3040,'1-input/spatial/disttol3040.tif', overwrite=T)


fun=function(u,v){u+v*2}
temp<-overlay(l10,nochange_prot,fun=fun)
disttol10<-gridDistance(temp,origin=1, omit=2 )
disttol10<-mask(disttocrop,sarea)
writeRaster(disttol10,'1-input/spatial/disttol10.tif', overwrite=T)

#create a layer that says which of the two layer has the min distance

nearestcrop<-ifelse(disttol10<disttol3040,1,2)
writeRaster(nearestcrop,'1-input/spatial/nearestcrop.tif', overwrite=T)


