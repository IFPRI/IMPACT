
setwd(path)

############################################computation resulting from land use change#########



################################################################################################
#extracting data for scenarios 
# here we will need to adjust the reclassification depending on the input layer 
cropland<-cropland+addcrop


rcl2<- cbind(c(0,1,2 ),
             c(0,1,1))
cropland <- reclassify(cropland, rcl=rcl2)


### make a loop for all other land cover 

varname<-c(l11,l12,l30,l40 ,l50 ,l60 , l61,l62 ,l70 ,l71 ,l72 ,l80 ,l81 ,l82 ,l90 ,l100 ,l110 ,l120 ,l121 ,
           l122,l130 ,l140 ,l150 , l152,l153,l160,l170,l180,l190,l200,l201,l202,l220) 
varname2<-varname
rcl10<- cbind(c(0,1 ),
              c(0,2))
addcrop<-reclassify(addcrop, rcl=rcl10)


lc<-stack(l11,l12,l30,l40 ,l50 ,l60 , l61,l62 ,l70 ,l71 ,l72 ,l80 ,l81 ,l82 ,l90 ,l100 ,l110 ,l120 ,l121 ,
           l122,l130 ,l140 ,l150 , l152,l153,l160,l170,l180,l190,l200,l201,l202,l220) 
varname<-names(lc)
lc2<-lc


# lost cells 
for (i in 1:length(names(lc))) {
    temp<-lc@layers[[i]]-addcrop
    rcl3<- cbind(c(-2,-1,0,1 ),
                 c(0,1,0,0))
    temp2<- reclassify(temp, rcl=rcl3)
    lc@layers[[i]]<-temp2
    names(lc@layers[[i]])<-varname[i]
}

ll11<- lc@layers[[1]]
ll12<-lc@layers[[2]]
ll30<-lc@layers[[3]]
ll40<-lc@layers[[4]]
ll50<-lc@layers[[5]]
ll60<-lc@layers[[6]]
ll61<-lc@layers[[7]]
ll62<-lc@layers[[8]]
ll70<-lc@layers[[9]]
ll71<-lc@layers[[10]]
ll72<-lc@layers[[11]]
ll80<-lc@layers[[12]]
ll81<-lc@layers[[13]]
ll82<-lc@layers[[14]]
ll90<-lc@layers[[15]]
ll100<-lc@layers[[16]]
ll110<-lc@layers[[17]]
ll120<-lc@layers[[18]]
ll121<-lc@layers[[19]]
ll122<-lc@layers[[20]]
ll130<-lc@layers[[21]]
ll140<-lc@layers[[22]]
ll150<-lc@layers[[23]]
ll152<-lc@layers[[24]]
ll153<-lc@layers[[25]]
ll160<-lc@layers[[26]]
ll170<-lc@layers[[27]]
ll180<-lc@layers[[28]]
ll190<-lc@layers[[29]]
ll200<-lc@layers[[30]]
ll201<-lc@layers[[31]]
ll202<-lc@layers[[32]]
ll220<-lc@layers[[33]]


lostgraz<-ll130+ll120+ll121+ll122+ll110+ll100
lostforest<-ll50+ll60+ll61+ll62+ll70+ll71+ll72+ll80+ll81+ll82+ll90

rm(list = ls()[grep("ll", ls())])
rm(lc)

#create the new land use layer


for (i in 1:length(names(lc2))) {
    temp<-lc2@layers[[i]]-addcrop
    rcl3<- cbind(c(-2,-1,0,1 ),
                 c(0,0,0,1))
    temp2<- reclassify(temp, rcl=rcl3)
   lc2@layers[[i]]<-temp2
   names(lc2@layers[[i]])<-varname[i]
}

l11<-lc2@layers[[1]]
l12<-lc2@layers[[2]]
l30<-lc2@layers[[3]]
l40<-lc2@layers[[4]]
l50<-lc2@layers[[5]]
l60<-lc2@layers[[6]]
l61<-lc2@layers[[7]]
l62<-lc2@layers[[8]]
l70<-lc2@layers[[9]]
l71<-lc2@layers[[10]]
l72<-lc2@layers[[11]]
l80<-lc2@layers[[12]]
l81<-lc2@layers[[13]]
l82<-lc2@layers[[14]]
l90<-lc2@layers[[15]]
l100<-lc2@layers[[16]]
l110<-lc2@layers[[17]]
l120<-lc2@layers[[18]]
l121<-lc2@layers[[19]]
l122<-lc2@layers[[20]]
l130<-lc2@layers[[21]]
l140<-lc2@layers[[22]]
l150<-lc2@layers[[23]]
l152<-lc2@layers[[24]]
l153<-lc2@layers[[25]]
l160<-lc2@layers[[26]]
l170<-lc2@layers[[27]]
l180<-lc2@layers[[28]]
l190<-lc2@layers[[29]]
l200<-lc2@layers[[30]]
l201<-lc2@layers[[31]]
l202<-lc2@layers[[32]]
l220<-lc2@layers[[33]]


rm(lc2)

### this is the the part we adjust the cropycorr layers 
# there are several option 
 # 1 is all is 100% cropland, 2 all is mosaik so 50%, 3 it depends which land use is nearest

# if (corrOption==1) {
#      cropycorr<-cropycorr+ addcrop
#      
# } else  {
#      cropycorr<-cropycorr+ addcrop*mosc1
# } 

grazland<-l130+l120+l121+l122+l110+l100
grazlandHarv<-l130+l120+l121+l122+l110+l100+l30+l40
# grazycorr<-l130+l120*mosg1+l121*mosg1+l122*mosg1+l110*mosg2+l100*mosg2+l30*(1-mosc1)+l40*(1-mosc2)

fun=function(x,y,z){x*(1+pgc)*y*z}
y_maizec<-overlay(y_maize,cropland,cropycorr,fun=fun)
# max_c<-cellStats(y_maizec,stat='sum')
# 
y_pulsec<-overlay(y_pulse,cropland,cropycorr,fun=fun)
# max_l<-cellStats(y_pulsec,stat='sum')

fy_rcc<-overlay(fy_rc,cropland,cropycorr,fun=fun)

# max_rc<-cellStats(fy_rcc,stat='sum')
# 
fy_rlc<-overlay(fy_rl,cropland,cropycorr, fun=fun)
# max_rl<-cellStats(fy_rlc,stat='sum')


fy_rcc[fy_rcc==0]<-NaN
avg_rc<-cellStats(fy_rcc,stat='mean')
# compute maize grain needed
y_maizec[y_maizec==0]<-NaN
# avg_cer<-cellStats(y_maizec*(1+pgc),stat='mean')
# cropProdI<- cropProd[, YEAR]*1000/ 10000 *pixel^2 #kg/m2 
#pgc<-(cropProdI-avg_cer)/cropProdI

avg_cer<-cellStats(y_maizec,stat='mean')




avg_cer<-cellStats(y_maizec,stat='mean')
avg_rc<-cellStats(fy_rcc,stat='mean')


#area required 
ar_rc1<-fw_rc/(avg_rc) *pixel^2/100000  # number pixel * adjustement in km2
ar_cer<-fw_cer/(avg_cer)*pixel^2/100000

#We select the area which is used most or for residue or for cereal 
ar_rc<-max(ar_rc1,ar_cer)
# 
# 
# 
# #total cropland area required
# ar<-(ar_rc+ar_rl+ar_pf) # it is already in km2
# 
# general_yield<-avg_rc *ar_rc/ar*(1+pgc)+avg_rl *ar_rl/ar+(fy_pf*1000/0.01*rfpf)*ar_pf/ar
# 
