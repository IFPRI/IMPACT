#this codes computed the initial numbers of livestock based on the pgis 



library(raster)
library(maptools)
path<-'D:/Dropbox/Cleaned - Burkina' 
setwd(path)
# read the relevant data 


sarea<-readShapePoly('1-input/spatial/study_area.shp')
pop<-raster("G:/new gis/new world pop/2017.2.17continent/AFR_PPP_2010_adj_v2.tif")
pop2<-crop(pop,sarea)
pop<-mask(pop2,sarea)
dhs<-read.csv('D:/Dropbox/DHS-CRP/0_RawData/DHS_Merged_Clusters.csv')
dhs<-dhs[dhs$HV024=='hauts basins',]
dhs$hhs<-dhs$HV009/dhs$h_cnt
hhs<-mean(dhs$hhs)
hh<-pop/hhs
plot(hh)
plot(sarea,add=T)
zs<-extract(hh, sarea, fun=sum, na.rm=TRUE, df=TRUE)
zs2<-extract(pop, sarea, fun=sum, na.rm=TRUE, df=TRUE)


#menage Bama  11737 in 2006 and 7424 in Padema, pop Bama 69631 and padema 50957
#number of troupeau in the study ara 
tot<-11737+7424/2
tot2<-zs[1,2]+zs[2,2]

ttd2<-70/zs[2,2]*tot2 *120
tld2<-150/zs[2,2]*tot2*20
tptd2<-180/zs[2,2]*tot2*120


tt2<-70/zs[2,2]*tot2
tl2<-150/zs[2,2]*tot2
tpt2<-180/zs[2,2]*tot2

tt<-70/11737*tot
tl<-150/11737*tot
tpt<-180/11737*tot

ttd<-70/11737*tot*120
tld<-150/11737*tot*20
tptd<-180/11737*tot*120

#getting the fattening (4 is the mode, but is it unclear if it is the whole year round usaid report suggest only half of the year)
f<-0.8*(zs[1,2]+zs[2,2])*4/2
f<-0.8*tot*4
f2<-0.8*tot2*4

#getting the specalised dairy 
#in bobo 2 of 46 farm where type c  http://revues.cirad.fr/index.php/cahiers-agricultures/article/view/30745/30505
# from report we know 68 houshold in bama have dairy on the 11737 computed here
dd<-86/11737
d<-dd*tot*10 # 15 was workshop but Salifou thinks 10
d2<-dd*tot2*10
###draft animals
da2=tot2*mean(dhs$land_h_prop)*2 

#comparison with national statistics
cat<-raster('H:/GIS data/global/livestock_data/livestock_CATTLE/CATTLE/Glb_Cattle_CC2006_AD.tif')
cat<-crop(cat,sarea)
cat<-mask(cat,sarea)
zs3<-extract(cat, sarea, fun=sum, na.rm=TRUE, df=TRUE)

catnum<-zs3[1,2]+zs3[2,2]
#computed
d+f+ttd+tptd+tld
#if fattening is the cattle the goes on small transhumance but does not go on the long transhumance
d+f+ttd+tld
d+ttd+tptd+tld+f

d2+f2+ttd2+tptd2+tld2+da2
d2+f2+ttd2+tld2
d2+ttd2+tptd2+tld2

#compare with dhs
summary(dhs$Ct_h_sum)

#total animal extrapolated from dhs
tot*mean(dhs$Ct_hs)*mean(dhs$Ct_h_prop)
tot2*mean(dhs$Ct_hs)*mean(dhs$Ct_h_prop)



