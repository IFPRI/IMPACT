#########################################biodiverity pathway Burkina Faso ##################################
#cleaned version Burkina Faso

#this code computes the biodiversity pathway 

library(raster)
library(maptools)
library(RColorBrewer)
setwd(path)


#read parameter
tspecies<-read.csv('1-input/parameter/threatenedspecies.csv')
tspecies[is.na(tspecies)]<-0

#run the landuse change scenario if relevant
if (lucs==1) {
  source('3-cleaned/2-luccomp.r')
}

bio_base<-read.csv('1-input/base_run/bio_ind.csv', row.names = NULL)
#bio_base<-bio_base[ ,2:3 ]
bio_map<-raster('1-input/base_run/bio_map.tif')

############compute biodiversity index###########
tspecies["dm"]<-1
num<-sum(tspecies$dm)
ncforet<-sum(tspecies$foret.claire)
nccrop<-sum(tspecies$Culture.cerealiere)
nicrop<-sum(tspecies$Culture.irriguee)
nriz<-sum(tspecies$riz)
nroche<-sum(tspecies$roche)
ngforet<-sum(tspecies$foret.galerie)
npforet<-sum(tspecies$plantation.forestiere)
nasava<-sum(tspecies$savanne.arboree)
narsava<-sum(tspecies$savanne.arbustive)
nhsava<-sum(tspecies$savanne.herbeuse)
nastep<-sum(tspecies$steppe.arboree)
narstep<-sum(tspecies$steppe.arbustive)
nhstep<-sum(tspecies$steppe.herbeuse)
nautre<-sum(tspecies$autre)

#we modified the original crop with cropland variable, this needs to be readjusted here

modcrop<-cropland-ccrop-icrop
ccrop<-modcrop+ccrop

fun=function (x) {nccrop*x/num}
bi_ccrop<-calc(ccrop,fun=fun)

fun=function (x) {nicrop*x/num}
bi_icrop<-calc(icrop,fun=fun)

fun=function (x) {nriz*x/num}
bi_riz<-calc(riceland,fun=fun)

fun=function (x) {ncforet*x/num}
bi_cforet<-calc(cforet,fun=fun)

fun=function (x) {ngforet*x/num}
bi_gforet<-calc(gforet,fun=fun)

fun=function (x) {npforet*x/num}
bi_pforet<-calc(pforet,fun=fun)

fun=function (x) {nasava*x/num}
bi_asava<-calc(asava,fun=fun)

fun=function (x) {narsava*x/num}
bi_arsava<-calc(arsava,fun=fun)

fun=function (x) {nhsava*x/num}
bi_hsava<-calc(hsava,fun=fun)

fun=function (x) {nastep*x/num}
bi_astep<-calc(astep,fun=fun)

fun=function (x) {narstep*x/num}
bi_arstep<-calc(arstep,fun=fun)

fun=function (x) {nhstep*x/num}
bi_hstep<-calc(hstep,fun=fun)

fun=function (x) {nautre*x/num}
bi_autre<-calc(autre,fun=fun)



fun=function (a,b,c,d,e,f,g,h,i,j,k,l,m) {a+b+c+d+e+f+g+h+i+j+k+l+m}
bio_index<-overlay(bi_ccrop,bi_icrop,bi_riz,bi_cforet,bi_gforet, bi_pforet,bi_asava, bi_arsava,
                   bi_hstep,bi_hsava,bi_astep,bi_arstep,bi_autre,fun=fun)

#plot(bio_index)


###########number of endangered species that have lost their habitat

if (lucs==1) {
  
  #computation for the base run 
  tspecies$dmt<-tspecies$dm*tspecies$critical
  numt<-sum(tspecies$dmt*tspecies$critical)
  ncforet<-sum(tspecies$foret.claire*tspecies$critical)
  nccrop<-sum(tspecies$Culture.cerealiere*tspecies$critical)
  nicrop<-sum(tspecies$Culture.irriguee*tspecies$critical)
  nriz<-sum(tspecies$riz*tspecies$critical)
  nroche<-sum(tspecies$roche*tspecies$critical)
  ngforet<-sum(tspecies$foret.galerie*tspecies$critical)
  npforet<-sum(tspecies$plantation.forestiere*tspecies$critical)
  nasava<-sum(tspecies$savanne.arboree*tspecies$critical)
  narsava<-sum(tspecies$savanne.arbustive*tspecies$critical)
  nhsava<-sum(tspecies$savanne.herbeuse*tspecies$critical)
  nastep<-sum(tspecies$steppe.arboree*tspecies$critical)
  narstep<-sum(tspecies$steppe.arbustive*tspecies$critical)
  nhstep<-sum(tspecies$steppe.herbeuse*tspecies$critical)
  nautre<-sum(tspecies$autre*tspecies$critical)
  

  
  #computing habitat change for endangered species due to scenario conversion to annual cropland
  # note it is overwriting variables from the biodiversity index 
 
  # fun=function (x) {nccrop*x/num}
  # bis_ccrop<-calc(lostccrop,fun=fun)
  # 
  # fun=function (x) {nicrop*x/num}
  # bis_icrop<-calc(losticrop,fun=fun)
  
  fun=function (x) {nriz*x/num}
  bis_riz<-calc(lostriz,fun=fun)
  
  fun=function (x) {ncforet*x/num}
  bis_cforet<-calc(lostcforet,fun=fun)
  
  fun=function (x) {ngforet*x/num}
  bis_gforet<-calc(lostgforet,fun=fun)
  
  fun=function (x) {npforet*x/num}
  bis_pforet<-calc(lostpforet,fun=fun)
  
  fun=function (x) {nasava*x/num}
  bis_asava<-calc(lostasava,fun=fun)
  
  fun=function (x) {narsava*x/num}
  bis_arsava<-calc(lostarsava,fun=fun)
  
  fun=function (x) {nhsava*x/num}
  bis_hsava<-calc(losthsava,fun=fun)
  
  fun=function (x) {nastep*x/num}
  bis_astep<-calc(lostastep,fun=fun)
  
  fun=function (x) {narstep*x/num}
  bis_arstep<-calc(lostarstep,fun=fun)
  
  fun=function (x) {nhstep*x/num}
  bis_hstep<-calc(hstep,fun=fun)
  
  fun=function (x) {nautre*x/num}
  bis_autre<-calc(lostautre,fun=fun)
  
  
  
  fun=function (c,d,e,f,g,h,i,j,k,l,m) {c+d+e+f+g+h+i+j+k+l+m}
  esp_sc<-overlay(bis_riz,bis_cforet,bis_gforet, bis_pforet,bis_asava, bis_arsava,
                  bis_hstep,bis_hsava,bis_astep,bis_arstep,bis_autre,fun=fun)
  
  #esp_sc<-zoom(esp_sc, ext=addcrop@extent)
 # plot(esp_sc)
  
}
  

##################landscape level indicators ######### 
#average biodiversity index

bio_index_avg<- round(cellStats(bio_index,stat='mean'),2)

if (lucs==1) {
  
  esp_sc_max<- round(cellStats(esp_sc,stat='max'),2)
} else {esp_sc_max<- NA }

 


bio_ind<-data.frame(bio_index_avg, esp_sc_max)
bio_ind_diff<-bio_ind-bio_base
bio_ind_perc<-round(bio_ind_diff/bio_base*100, digits = 1) 
bio_map<-bio_index-bio_map
bio_ind2<-rbind(bio_ind,bio_ind_diff,bio_ind_perc)
names(bio_ind2)<-c("avg richness index", "nbr of species loosing critical habitat")
bio_ind2<-data.frame(t(bio_ind2))
colnames(bio_ind2)<-c('result','diffence','percent')

bio_ind_val<-read.csv('1-input/parameter/bio_val.csv')
bio_ind2$evaluation <- ifelse(bio_ind2$percent <= bio_ind_val$peu, 'low', ifelse((bio_ind2$percent >= bio_ind_val$peu) & (bio_ind2$percent <= bio_ind_val$peu),'medium','high'))





title1<-paste('Biodiversity: richness index', name, sep=" ")
title2<-paste('Biodiversity: critically affected species', name, sep=" ")
title3<-paste('Biodiversity: difference of richness index', name, sep=" ")

col<-colorRampPalette(brewer.pal(9,'YlGn'))(100)
col2<-colorRampPalette(rev(brewer.pal(9,'Reds')))(100)
col3<-colorRampPalette((brewer.pal(9,'Reds')))(100)


par(mfrow=c(1,3),mar=c(2, 4.5, 2, 6))
#pdf(paste(name, "biodiversity_pathway.pdf", sep='-'))

plot(bio_index, legend.width=1, legend.shrink=0.45,col=col) 
plot(sarea, add=TRUE) 
title(title1) 

plot(bio_map, legend.width=1, legend.shrink=0.45,col=col2) 
plot(sarea, add=TRUE) 
title(title3) 


if (lucs==1) {
  plot(esp_sc, legend.width=1, legend.shrink=0.45,col=col3)
  plot(sarea, add=TRUE)
  title(title2)}


#grid.table(bio_ind2)







#####################################################################
#extract the maps for final user
setwd("4-output")

title1<-paste('Biodiversity pathway : richness index', name, sep=" ")
title2<-paste('Biodiversity pathway : critically affected species', name, sep=" ")
title3<-paste('Biodiversity pathway : difference of richness index', name, sep=" ")

col<-colorRampPalette(brewer.pal(9,'YlGn'))(100)
col2<-colorRampPalette(rev(brewer.pal(9,'Reds')))(100)
col3<-colorRampPalette((brewer.pal(9,'Reds')))(100)



pdf(paste(name, "biodiversity_pathway.pdf", sep='-'))

plot(bio_index, legend.width=1, legend.shrink=0.75,col=col) 
plot(sarea, add=TRUE) 
title(title1) 

plot(bio_map, legend.width=1, legend.shrink=0.75,col=col2) 
plot(sarea, add=TRUE) 
title(title3) 


if (lucs==1) {
  plot(esp_sc, legend.width=1, legend.shrink=0.75,col=col3)
  plot(sarea, add=TRUE)
  title(title2)}


plot(NA, xlim=c(0,100), ylim=c(0,10), bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
grid.table(bio_ind2)


dev.off() 

# tiff(paste(name,"biodiversity_pathway.tiff", sep="-"))
# 
# plot(bio_index, legend.width=1, legend.shrink=0.75,col=col)
# plot(sarea, add=TRUE)
# title(title1)
# 
# dev.off()




if (lucs==1) {
  outputname<-paste(name,"biodiversity_pathway2.tiff", sep="-")
  tiff(outputname)

  plot(esp_sc, legend.width=1, legend.shrink=0.75,col=col)
  plot(sarea, add=TRUE)
  title(title2)
  dev.off()
  
  writeRaster(esp_sc,paste(name,"bioES_map.tif", sep="-"), overwrite=TRUE)
  
  }#end of if

writeRaster(bio_index,paste(name,"bio_map.tif", sep="-"),overwrite =TRUE)
write.csv(bio_ind,paste(name,"bio_ind.csv", sep="-"),row.names = F)

