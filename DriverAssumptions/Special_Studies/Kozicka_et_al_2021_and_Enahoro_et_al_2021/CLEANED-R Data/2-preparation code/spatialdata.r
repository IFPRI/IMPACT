###########################Cleaned version IMPACT #############################################

#this code is to be run only ones to produce the necessary spatial layers

  # all layers are in WGS 84 unprojected (long/lat)
  # this code prepares layers that links to impact and uses ESA map (in raw data)
  
  #clearing all memory 
  rm(list=ls(all=TRUE))
  
  library(maptools)
  library(raster)
  library(igraph)
  
  setwd('D:/Dropbox/CLEANED - IMPACT')
  # IPCC definition 
  # species<-'dairyCows'
  # #Africa, Asia, Latin America
  # region='Africa'
  # #base layers 
  # #area definition
  
  sarea<-readShapePoly('1-input/spatial/TZ.shp')
  lu<-raster('0-raw_data/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif')
  lu<-crop(lu,sarea)
  lu<-mask(lu,sarea)
  rr<-raster(xmn=lu@extent@xmin, xmx=lu@extent@xmax,ymn=lu@extent@ymin,ymx=lu@extent@ymax, res=res(lu))
  ext<-extent(rr)
  ext@xmax<-ext@xmax+0.1
  ext@xmin<-ext@xmin-0.1
  ext@ymax<-ext@ymax+0.1
  ext@ymin<-ext@ymin-0.1
  rr2<-raster(xmn=lu@extent@xmin, xmx=lu@extent@xmax,ymn=lu@extent@ymin,ymx=lu@extent@ymax, res=11*res(lu))

  #defining the clipping extend slightly bigger than the area extent, to make sure we do not
  writeRaster(lu,'1-input/spatial/lu/lu.tif',overwrite=T)
  writeRaster(rr,'1-input/spatial/rr.tif',overwrite=T)
  #pixel size in m 
  pixel=300
  write.csv(pixel,'1-input/parameter/pixel.csv')
  species="dairyCows"
  ###########################################################################
  #This section of the code defines the different land use and therefore has to be adjusted for each country


# for IMPACT we are using ESA 300m map from climate change initiative
  
  
#prepare the land use layer

#cropland<-mask(cropland, sarea)
#plot(acrop)
#writeRaster(batis,'1-input/spatial/lu/batis.tif',overwrite=T)

# extracting Culture cerealiere
  #base line for copy pasting 
rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))

  
rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l10 <- reclassify(lu, rcl=rcl)
writeRaster(l10,'1-input/spatial/lu/l10.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l11 <- reclassify(lu, rcl=rcl)
writeRaster(l11,'1-input/spatial/lu/l11.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l12 <- reclassify(lu, rcl=rcl)
writeRaster(l12,'1-input/spatial/lu/l12.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l20 <- reclassify(lu, rcl=rcl)
writeRaster(l20,'1-input/spatial/lu/l20.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l30 <- reclassify(lu, rcl=rcl)
writeRaster(l30,'1-input/spatial/lu/l30.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l40 <- reclassify(lu, rcl=rcl)
writeRaster(l40,'1-input/spatial/lu/l40.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l50 <- reclassify(lu, rcl=rcl)
writeRaster(l50,'1-input/spatial/lu/l50.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l60 <- reclassify(lu, rcl=rcl)
writeRaster(l60,'1-input/spatial/lu/l60.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l61 <- reclassify(lu, rcl=rcl)
writeRaster(l61,'1-input/spatial/lu/l61.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l62 <- reclassify(lu, rcl=rcl)
writeRaster(l62,'1-input/spatial/lu/l62.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l70 <- reclassify(lu, rcl=rcl)
writeRaster(l70,'1-input/spatial/lu/l70.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l71 <- reclassify(lu, rcl=rcl)
writeRaster(l71,'1-input/spatial/lu/l71.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l72 <- reclassify(lu, rcl=rcl)
writeRaster(l72,'1-input/spatial/lu/l72.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l80 <- reclassify(lu, rcl=rcl)
writeRaster(l80,'1-input/spatial/lu/l80.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l81 <- reclassify(lu, rcl=rcl)
writeRaster(l81,'1-input/spatial/lu/l81.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l82 <- reclassify(lu, rcl=rcl)
writeRaster(l82,'1-input/spatial/lu/l82.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l90 <- reclassify(lu, rcl=rcl)
writeRaster(l90,'1-input/spatial/lu/l90.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l100 <- reclassify(lu, rcl=rcl)
writeRaster(l100,'1-input/spatial/lu/l100.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l110 <- reclassify(lu, rcl=rcl)
writeRaster(l110,'1-input/spatial/lu/l110.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l120 <- reclassify(lu, rcl=rcl)
writeRaster(l120,'1-input/spatial/lu/l120.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l121 <- reclassify(lu, rcl=rcl)
writeRaster(l121,'1-input/spatial/lu/l121.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l122 <- reclassify(lu, rcl=rcl)
writeRaster(l122,'1-input/spatial/lu/l122.tif',overwrite=T)


rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l130 <- reclassify(lu, rcl=rcl)
writeRaster(l130,'1-input/spatial/lu/l130.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l <- reclassify(lu, rcl=rcl)
writeRaster(l,'1-input/spatial/lu/l140.tif',overwrite=T)


rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l <- reclassify(lu, rcl=rcl)
writeRaster(l,'1-input/spatial/lu/l150.tif',overwrite=T)


rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l <- reclassify(lu, rcl=rcl)
writeRaster(l,'1-input/spatial/lu/l152.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0))
l <- reclassify(lu, rcl=rcl)
writeRaster(l,'1-input/spatial/lu/l153.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0))
l <- reclassify(lu, rcl=rcl)
writeRaster(l,'1-input/spatial/lu/l160.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0))
l <- reclassify(lu, rcl=rcl)
writeRaster(l,'1-input/spatial/lu/l170.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0))
l <- reclassify(lu, rcl=rcl)
writeRaster(l,'1-input/spatial/lu/l180.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0))
l <- reclassify(lu, rcl=rcl)
writeRaster(l,'1-input/spatial/lu/l190.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0))
l <- reclassify(lu, rcl=rcl)
writeRaster(l,'1-input/spatial/lu/l200.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0))
l <- reclassify(lu, rcl=rcl)
writeRaster(l,'1-input/spatial/lu/l201.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0))
l <- reclassify(lu, rcl=rcl)
writeRaster(l,'1-input/spatial/lu/l202.tif',overwrite=T)

rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0))
l <- reclassify(lu, rcl=rcl)
writeRaster(l,'1-input/spatial/lu/l210.tif',overwrite=T)



# rcl<- cbind(c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,130,140,150,152,153,160,170,180,190,200,201,202,210,220),
#             c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1))
# l <- reclassify(lu, rcl=rcl)
# writeRaster(l,'1-input/spatial/lu/l220.tif',overwrite=T)

# now the land cover classification and distance to crop are run in luc_trans.r 

#yield
y_maize<-raster('../Cleaned version 2/0-raw input/GAEZ/act2000_r_mze_2000_yld.tif')
y_pulse<-raster('../Cleaned version 2/0-raw input/GAEZ/act2000_r_pls_2000_yld.tif')
y_rice<-raster('../Cleaned version 2/0-raw input/GAEZ/act2000_t_rcw_2000_yld.tif')
y_grass<-raster('0-raw_data/res02_crav6190l_misc150b_yld.tif')
y_maize<-crop(y_maize,ext)
y_pulse<-crop(y_pulse,ext)
y_rice<-crop(y_rice,ext)
y_grass<-crop(y_grass,ext)
y_maize<-resample(y_maize,rr)
y_pulse<-resample(y_pulse,rr)
y_rice<-resample(y_rice,rr)
y_grass<-resample(y_grass,rr)
y_maize<-mask(y_maize,sarea)
y_pulse<-mask(y_pulse,sarea)
y_grass<-mask(y_grass,sarea)

#adjust yield to new sample size from t/ha to kg/pixel : *1000 to get kg (1ton =1000kg),/10000 to get ha into m2
#1 pixel is 900 m2
fun=function(x){x*1000/10000*pixel^2}
y_maize<-overlay(y_maize,fun=fun)
y_pulse<-overlay(y_pulse,fun=fun)
y_rice<-overlay(y_rice, fun=fun)
writeRaster(y_maize,'1-input/spatial/y_maize.tif',overwrite=T)
writeRaster(y_pulse,'1-input/spatial/y_pulse.tif',overwrite=T)
writeRaster(y_rice,'1-input/spatial/y_rice.tif',overwrite=T)
writeRaster(y_grass,'1-input/spatial/y_grass.tif',overwrite=T)



# evapotranspiration
ET_maize1<-raster('../Cleaned version 2/0-raw input/GAEZ/res02_crav6190l_lmze150b_eta.tif')
ET_maize2<-raster('../Cleaned version 2/0-raw input/GAEZ/res02_crav6190l_hmai150b_eta.tif')
ET_maize3<-raster('../Cleaned version 2/0-raw input/GAEZ/res02_crav6190l_tmze150b_eta.tif')
ET_pulse<-raster('../Cleaned version 2/0-raw input/GAEZ/res02_crav6190l_puls150b_eta.tif')
ET_gras <-raster('../Cleaned version 2/0-raw input/GAEZ/res02_crav6190l_gras150b_eta.tif')
ET_grasl<-raster('../Cleaned version 2/0-raw input/GAEZ/res02_crav6190l_lmze150b_eta.tif')
ET_rice<-raster('../Cleaned version 2/0-raw input/GAEZ/res02_crav6190i_ricw000a_eta.tif')

ET_maize1<-crop(ET_maize1,ext)
ET_maize2<-crop(ET_maize2,ext)
ET_maize3<-crop(ET_maize3,ext)
ET_pulse<-crop(ET_pulse,ext)
ET_gras<-crop(ET_gras,ext)
ET_grasl<-crop(ET_grasl,ext)
ET_rice<-crop(ET_rice,ext)

fun=function(x,y,z){x+y+z}
ET_maize<-overlay(ET_maize1,ET_maize2,ET_maize3,fun=fun )
plot(ET_maize)
ET_maize<-resample(ET_maize,rr)
ET_pulse<-resample(ET_pulse,rr)
ET_gras<-resample(ET_gras,rr)
ET_grasl<-resample(ET_grasl,rr)
ET_rice<-resample(ET_rice,rr)

ET_maize<-mask(ET_maize,sarea)
ET_pulse<-mask(ET_pulse,sarea)
ET_gras<-mask(ET_gras,sarea)
ET_grasl<-mask(ET_grasl,sarea)
ET_rice<-mask(ET_rice,sarea)

writeRaster(ET_maize,'1-input/spatial/ET_maize.tif',overwrite=T)
writeRaster(ET_pulse,'1-input/spatial/ET_pulse.tif',overwrite=T)
writeRaster(ET_gras,'1-input/spatial/ET_gras.tif',overwrite=T)
writeRaster(ET_grasl,'1-input/spatial/ET_grasl.tif',overwrite=T)
writeRaster(ET_rice,'1-input/spatial/ET_rice.tif',overwrite=T)

rain<-raster("../Cleaned version 2/0-raw input/GAEZ/res01_prc_crav6190.tif")
rain<-crop(rain,ext)
rain<-resample(rain,rr)
rain<-mask(rain,sarea)
writeRaster(rain,'1-input/spatial/rain.tif',overwrite=T)

tempr<-raster('../Cleaned version 2/0-raw input/IPCC/temp.tif')
tempr<-crop(tempr,ext)
tempr<-resample(tempr,rr)
tempr<-mask(tempr,sarea)
writeRaster(tempr,'1-input/spatial/tempr.tif',overwrite=T)

soil<-raster('../Cleaned version 2/0-raw input/IPCC/soil_ipcc.tif')
soil<-crop(soil,ext)
soil<-resample(soil,rr,method='ngb')
soil<-mask(soil,sarea)
writeRaster(soil,'1-input/spatial/soil.tif',overwrite=T)


climate<-raster('../Cleaned version 2/0-raw input/IPCC/climatezone.tif')
climate<-crop(climate,ext)
climate<-resample(climate,rr,method='ngb')
climate<-mask(climate,sarea)
writeRaster(climate,'1-input/spatial/ipcc_climate/climate.tif',overwrite=T)

#create a layer tha reproduce soil reference carbon
fun3<-function (x,y){10*x+y}
temp<-overlay(climate,soil, fun=fun3)
src<-read.csv('1-input/parameter/ghg_soil_reference_carbon.csv')
rcl<- cbind(c(12,13,14,15,16,17,
              22,23,24,25,26,27,
              32,33,34,35,36,37,
              42,43,44,45,46,47,
              72,73,74,75,76,77,
              82,83,84,85,86,87,
              92,93,94,95,96,97,
              102,103,104,105,106,107,
              112,113,114,115,116,117,
              122,123,124,125,126,127),
            c(src[8,4],src[8,7], src[8,6],src[8,5],src[8,2],src[8,3],
              src[7,4],src[7,7], src[7,6],src[7,5],src[7,2],src[7,3],
              src[5,4],src[5,7], src[5,6],src[5,5],src[5,2],src[5,3],
              src[4,4],src[4,7], src[4,6],src[4,5],src[4,2],src[4,3],
              src[2,4],src[2,7], src[2,6],src[2,5],src[2,2],src[2,3],
              src[1,4],src[1,7], src[1,6],src[1,5],src[1,2],src[1,3],
              src[13,4],src[13,7], src[13,6],src[13,5],src[13,2],src[13,3],
              src[12,4],src[12,7], src[12,6],src[12,5],src[12,2],src[12,3],
              src[11,4],src[11,7], src[11,6],src[11,5],src[11,2],src[11,3],
              src[10,4],src[10,7], src[10,6],src[10,5],src[10,2],src[10,3]))
soilref<-reclassify(temp,rcl=rcl)

writeRaster(soilref,'1-input/spatial/soilref.tif',overwrite=T)


# create a dummy raster
dum<-raster(xmn=lu@extent@xmin, xmx=lu@extent@xmax,ymn=lu@extent@ymin,ymx=lu@extent@ymax, res=res(lu),vals=1)
dum<-mask(dum,sarea)
writeRaster(dum,'1-input/spatial/dum.tif',overwrite=T)




#creation of a soil carbon stock change factor map  related to land use (Flu)
# this is used in ghg pathway emmissions from soil

test<-data.frame(levels(as.factor(climate)))
test$re<-ifelse(test$ID==1,1,0)
rcl<-matrix(c(test$ID,test$re),ncol=2)
clim_wtmoist<-reclassify(climate, rcl)
writeRaster(clim_wtmoist,'1-input/spatial/ipcc_climate/clim_wtmoist.tif',overwrite=T)

test$re<-ifelse(test$ID==2,1,0)
rcl<-matrix(c(test$ID,test$re),ncol=2)
clim_wtdry<-reclassify(climate, rcl)
writeRaster(clim_wtdry,'1-input/spatial/ipcc_climate/clim_wtdry.tif',overwrite=T)

test$re<-ifelse(test$ID==3,1,0)
rcl<-matrix(c(test$ID,test$re),ncol=2)
clim_ctmoist<-reclassify(climate, rcl)
writeRaster(clim_ctmoist,'1-input/spatial/ipcc_climate/clim_ctmoist.tif',overwrite=T)

test$re<-ifelse(test$ID==4,1,0)
rcl<-matrix(c(test$ID,test$re),ncol=2)
clim_ctdry<-reclassify(climate, rcl)
writeRaster(clim_ctdry,'1-input/spatial/ipcc_climate/clim_ctdry.tif',overwrite=T)

test$re<-ifelse(test$ID==9,1,0)
rcl<-matrix(c(test$ID,test$re),ncol=2)
clim_tr_mont<-reclassify(climate, rcl)
writeRaster(clim_tr_mont,'1-input/spatial/ipcc_climate/clim_tr_mont.tif',overwrite=T)

test$re<-ifelse(test$ID==10,1,0)
rcl<-matrix(c(test$ID,test$re),ncol=2)
clim_tr_wet<-reclassify(climate, rcl)
writeRaster(clim_tr_wet,'1-input/spatial/ipcc_climate/clim_tr_wet.tif',overwrite=T)

test$re<-ifelse(test$ID==11,1,0)
rcl<-matrix(c(test$ID,test$re),ncol=2)
clim_tr_moist<-reclassify(climate, rcl)
writeRaster(clim_tr_moist,'1-input/spatial/ipcc_climate/clim_tr_moist.tif',overwrite=T)

test$re<-ifelse(test$ID==12,1,0)
rcl<-matrix(c(test$ID,test$re),ncol=2)
clim_tr_dry<-reclassify(climate, rcl)
writeRaster(clim_tr_dry,'1-input/spatial/ipcc_climate/clim_tr_dry.tif',overwrite=T)

#create a layer from soil carbon stock change factor related to land use (Flu)
#read necessary parameters 
ssc<-read.csv('../Cleaned version 2/1-input/parameter/ghg_soil_stock_change.csv')


#create climate map for cropland
col=3
fun=function(a,b,c,d,e,f,g,h) {
  ((a*ssc[6,col])+(b*ssc[5,col])+(c*ssc[4,col])+(d*ssc[3,col])+(e*ssc[10,col])+(f*ssc[9,col])+(g*ssc[8,col])+(h*ssc[7,col]))
}
Flu_c<-overlay(clim_wtmoist,clim_wtdry,clim_ctmoist,clim_ctdry,  clim_tr_mont,clim_tr_wet,clim_tr_moist,clim_tr_dry,fun=fun)
writeRaster(Flu_c,'1-input/spatial/Flu_c.tif',overwrite=T)

#perenial crop
col=5
Flu_pc<-overlay(clim_wtmoist,clim_wtdry,clim_ctmoist,clim_ctdry,  clim_tr_mont,clim_tr_wet,clim_tr_moist,clim_tr_dry,fun=fun)
# in Lushoto not modeled, note that here one would still need to multiply with the right land use, fun=fun2
writeRaster(Flu_pc,'1-input/spatial/Flu_pc.tif',overwrite=T)

#set asside
col=6
Flu_sa<-overlay(clim_wtmoist,clim_wtdry,clim_ctmoist,clim_ctdry,  clim_tr_mont,clim_tr_wet,clim_tr_moist,clim_tr_dry,fun=fun)
writeRaster(Flu_sa,'1-input/spatial/Flu_sa.tif',overwrite=T)


#paddy rice 
col=4
Flu_rice<-overlay(clim_wtmoist,clim_wtdry,clim_ctmoist,clim_ctdry,  clim_tr_mont,clim_tr_wet,clim_tr_moist,clim_tr_dry,fun=fun)
writeRaster(Flu_rice,'1-input/spatial/Flu_rice.tif',overwrite=T)

#reference vegetation for grazing land 
col=14
fun=function(a,b,c,d,e,f,g,h) {
  ((a*ssc[6,col])+(b*ssc[5,col])+(c*ssc[4,col])+(d*ssc[3,col])+(e*ssc[10,col])+(f*ssc[9,col])+(g*ssc[8,col])+(h*ssc[7,col]))
}
graz_clim<-overlay(clim_wtmoist,clim_wtdry,clim_ctmoist,clim_ctdry,  clim_tr_mont,clim_tr_wet,clim_tr_moist,clim_tr_dry,fun=fun)
writeRaster(graz_clim,'1-input/spatial/graz_clim.tif',overwrite=T)

#reference vegetation for forest 
col=15
fun=function(a,b,c,d,e,f,g,h) {
  ((a*ssc[6,col])+(b*ssc[5,col])+(c*ssc[4,col])+(d*ssc[3,col])+(e*ssc[10,col])+(f*ssc[9,col])+(g*ssc[8,col])+(h*ssc[7,col]))}
forest_clim<-overlay(clim_wtmoist,clim_wtdry,clim_ctmoist,clim_ctdry,  clim_tr_mont,clim_tr_wet,clim_tr_moist,clim_tr_dry,fun=fun)
plot(forest_clim)
writeRaster(forest_clim,'1-input/spatial/forest_clim.tif',overwrite=T)


# compute methane conversion factors for specific manure management systems

mms_mcf <- read.csv('1-input/parameter/MMSspeciesTemp.csv') # reads the parameter 
mms_mcf<-subset(mms_mcf,mms_mcf$Species==species)
rcl<-matrix(c(mms_mcf$Temperatures ,mms_mcf$Temperatures+1 ,mms_mcf$Lagoon ), ncol=3, byrow=F)
mms_lagoon_mcf <- reclassify(tempr, rcl, include.lowest=T , right=NA )
writeRaster( mms_lagoon_mcf,'1-input/spatial/mms_lagoon_mcf.tif',overwrite=T)

rcl<-matrix(c(mms_mcf$Temperatures ,mms_mcf$Temperatures +1,mms_mcf$Liquid.slurry ), ncol=3, byrow=F)
mms_liquidslurry_mcf <-  reclassify(tempr, rcl, include.lowest=T , right=NA ) 
writeRaster(mms_liquidslurry_mcf ,'1-input/spatial/mms_liquidslurry_mcf.tif',overwrite=T)

rcl<-matrix(c(mms_mcf$Temperatures ,mms_mcf$Temperatures +1,mms_mcf$Solid.storage ), ncol=3, byrow=F)  
mms_solidstorage_mcf <-   reclassify(tempr, rcl, include.lowest=T , right=NA )
writeRaster(mms_solidstorage_mcf ,'1-input/spatial/mms_solidstorage_mcf.tif',overwrite=T)

rcl<-matrix(c(mms_mcf$Temperatures,mms_mcf$Temperatures +1 ,mms_mcf$Drylot ), ncol=3, byrow=F)
mms_drylot_mcf <-   reclassify(tempr, rcl, include.lowest=T , right=NA )
writeRaster(mms_drylot_mcf ,'1-input/spatial/mms_drylot_mcf.tif',overwrite=T)

rcl<-matrix(c(mms_mcf$Temperatures,mms_mcf$Temperatures +1 ,mms_mcf$Pasture ), ncol=3, byrow=F)
mms_pasture_mcf <-   reclassify(tempr, rcl, include.lowest=T , right=NA )
writeRaster( mms_pasture_mcf,'1-input/spatial/mms_pasture_mcf.tif',overwrite=T)

rcl<-matrix(c(mms_mcf$Temperatures,mms_mcf$Temperatures +1 ,mms_mcf$Daily.spread ), ncol=3, byrow=F)
mms_dailyspread_mcf <-   reclassify(tempr, rcl, include.lowest=T , right=NA )
writeRaster( mms_dailyspread_mcf,'1-input/spatial/mms_dailyspread_mcf.tif',overwrite=T)

rcl<-matrix(c(mms_mcf$Temperatures,mms_mcf$Temperatures +1 ,mms_mcf$Digester ), ncol=3, byrow=F)
mms_digester_mcf <-   reclassify(tempr, rcl, include.lowest=T , right=NA )
writeRaster( mms_digester_mcf,'1-input/spatial/mms_digester_mcf.tif',overwrite=T)

rcl<-matrix(c(mms_mcf$Temperatures,mms_mcf$Temperatures +1 ,mms_mcf$Burned.for.fuel ), ncol=3, byrow=F)
mms_burned_mcf <-   reclassify(tempr, rcl, include.lowest=T , right=NA )
writeRaster(mms_burned_mcf ,'1-input/spatial/mms_burned_mcf.tif',overwrite=T)

rcl<-matrix(c(mms_mcf$Temperatures,mms_mcf$Temperatures +1 ,mms_mcf$Other ), ncol=3, byrow=F)
mms_other_mcf <-   reclassify(tempr, rcl, include.lowest=T , right=NA )
writeRaster(mms_other_mcf ,'1-input/spatial/mms_other_mcf.tif',overwrite=T)


# create the layers for goat and sheep and monogatrics
# goat and sheep table 10.15
rcl<-matrix(c(0,15,25,15,25 ,250, (0.10+0.11)/2,(0.15+0.17)/2,(0.2+0.22)/2), ncol=3, byrow=F)
ef_sg <-   reclassify(tempr, rcl, include.lowest=T , right=NA )
writeRaster(ef_sg ,'1-input/spatial/ef_sg.tif',overwrite=T)

# poultry table 10.15
rcl<-matrix(c(0,15,25,15,25 ,250, 0.01,0.02,0.02), ncol=3, byrow=F)
ef_ch <-   reclassify(tempr, rcl, include.lowest=T , right=NA )
writeRaster(ef_ch ,'1-input/spatial/ef_ch.tif',overwrite=T)

# swine table 10.14
rcl<-matrix(c(0,13,28,13,28 ,250, 0,1,2), ncol=3, byrow=F)
ef_pg <-   reclassify(tempr, rcl, include.lowest=T , right=NA )
writeRaster(ef_pg ,'1-input/spatial/ef_pg.tif',overwrite=T)




suit_c <-raster ("../Cleaned version 2/0-raw input/GAEZ/res03_crav6190l_sxlr_cer.tif")
suit_c<-crop(suit_c,sarea)
suit_c<-resample(suit_c,rr)
suit_c<-fun=function(x){(x-min)/range}
mask(suit_c,sarea)
max <- maxValue(suit_c)
min<- minValue(suit_c)
range<-max-min 
suit_c<-overlay(suit_c, fun=fun)


# introducing the protected ara

parea<-readShapePoly('0-raw_data/protected area.shp')
parea<-rasterize(parea,rr)
parea[parea > 0]<- 1
writeRaster(parea ,'1-input/spatial/parea.tif',overwrite=T)
