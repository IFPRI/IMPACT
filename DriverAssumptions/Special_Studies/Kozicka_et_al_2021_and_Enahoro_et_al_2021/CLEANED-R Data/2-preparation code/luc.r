################################land use change module##############################

#CLEANED version 2
#Code by Catherine Pfeifer, c.pfeifer@cgiar.org


#this code allows to create a crop change layer, i.e. a layer that indicated which cells in the landscape
#will be converted to cropland based on a land use change model 

#this code results in the  "addcrop"layer, which can be requested from 0-userdefinition to activate a 
# land use change based scenario 

#set path to cleaned tool
path<-'D:/Dropbox/Cleaned Tanzania'
#clearing all memory 
setwd(path)
# read libraries
library(raster)
library(maptools)
library(RColorBrewer)
setwd(path)


#read parameter
para<-read.csv('1-input/parameter/para.csv')
para$value<-as.numeric(para$value)
dim<-dim(para)
obs<-dim [1]

for(i in seq_along(para$name))
{assign(as.character(para$name[i]), para$value[i])}
source('3-cleaned/2-load_data.r')

source('3-cleaned/2-feedbasket_nonlinear2.r')


#set the persentage of additional land required 

add = 0
diff = 1000

#set here the yield objective in tons Tanzania, 
#tot biomass
biomass<- fw_rc+fw_rl+fw_pf+ fwg_hay+ fwg_sil 
objyield= biomass * add
start<-Sys.time()
while(diff > biomass*0.005| diff< -biomass*0.005 ) {
    #######################computing the ranking of cells ######################

    fun<-function(x,y){x*y}
    dist_suit_c<-overlay(disttocrop2,suit_c,fun=fun)
    dist_suit_c[]<-rank(dist_suit_c[],ties.method="random",na.last=T)


    ncellc <-cellStats(cropland, stat='sum', na.rm=TRUE)

    tres<-add * ncellc
    m<-c(0, tres,1,tres+0.0000001,maxValue(dist_suit_c),0)
    rcl<-matrix(m,ncol=3,byrow=T)
    addcrop<-reclassify(dist_suit_c,rcl=rcl)
    # name<-paste(add,'addcrop_area.tif',sep='_')
    # name<-paste(path,'1-input/spatial/landuse_scenario',sep='/')
    # writeRaster(addcrop,name,overwrite=T)

    #compute the yield on the new cell 
    #calculate the production on the new land 
    fun=function(w,x,y){w*(ar_rc/(ar_rc+ar_rl+ar_pf)*x*rfm+ar_rl/(ar_rc+ar_rl+ar_pf)*y*rfl+fy_pf*1000/10000*pixel^2*rfpf*ar_pf/(ar_rc+ar_rl+ar_pf))}
    yield<-overlay(addcrop,y_maize,y_pulse,fun=fun)

    addyield <-cellStats(yield, stat='sum', na.rm=TRUE)/1000 #/1000 to have tons
    addyield

    diff=objyield-addyield
    print(diff) 
    add<-ifelse(diff < 0, add-0.01, add+0.01)
    print(add)} #end of while loop 




end<-Sys.time()

end-start
    
