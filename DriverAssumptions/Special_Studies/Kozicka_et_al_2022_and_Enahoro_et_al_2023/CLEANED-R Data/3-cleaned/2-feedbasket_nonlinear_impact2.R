
### this code is ajusted to fit impact
# to run it for TZ impact, only use fattening,specialized milk and draft
# draft animals have been ajusted to be followers
# cons1 is becoming maize grain consumption

# adjust land cover
library(gridExtra)
setwd(path)
#read parameter
para<-read.csv('1-input/parameter/para.csv')
para$value<-as.numeric(para$value)
dim<-dim(para)
obs<-dim [1]
for(i in seq_along(para$name))
{assign(as.character(para$name[i]), para$value[i])}
prod_base<-read.csv('1-input/base_run/prod_ind.csv', row.names = NULL)
#prod_base<-prod_base[,2:18]
#now overwrite variables with preset

cropland<-l10+l20+l30+l11+l12+l40
cropycorr<-l10+l11+l12+l20+l30*mosc1+l40*mosc2 
if (corrOption==2) {
    cropycorr[cropycorr==0]<-mosc1
} else {cropycorr[cropycorr==0]<-1
}

grazland<-l130+l120+l121+l122+l110+l100 # by moving this, it computes is with the luc
ncellgraz<-cellStats(grazland,stat='sum') 
grazlandHarv<-l130+l120+l121+l122+l110+l100+l30+l40
grazycorr<-l130+l120*mosg1+l121*mosg1+l122*mosg1+l110*mosg2+l100*mosg2+l30*(1-mosc1)+l40*(1-mosc2)

setwd(path)
#now overwrite variables with preset
if(preset==1){
    # get the parameter of the dairy system
    temp<-scenario[ ,c('Dvar', D)]
    names(temp)[2]<-'ppara'
    for(i in  which(! temp$Dvar== ''))
    {
        assign(as.character(temp$Dvar[i]), temp$ppara[i])
    }}



if(preset==1){
    # get the parameter of the fattening system
    temp<-scenario[ , c('Fvar',Fa)]
    names(temp)[2]<-'ppara'
    for(i in  which(! temp$Fvar== ''))
    {
        assign(as.character(temp$Fvar[i]), temp$ppara[i])
    }}

if(preset==1){
    # get the parameter of the draft system
    temp<-scenario[ , c('Ovar', O )]
    names(temp)[2]<-'ppara'
    for(i in  which(! temp$Tvar== ''))
    {
        assign(as.character(temp$Tvar[i]), temp$ppara[i])
    }}
if(preset==1){
    # get the parameter crop system 
    temp<-scenario[ , c('Cvar',Ca)]
    names(temp)[2]<-'ppara'
    for(i in  which(! temp$Cvar== ''))
    {
        assign(as.character(temp$Cvar[i]), temp$ppara[i])
    }}



if(preset==1){
     # get the parameter of the crop 
     temp<-scenario[ , c('Cvar',Ca)]
     names(temp)[2]<-'ppara'
     for(i in  which(! temp$Cvar== ''))
     {
          assign(as.character(temp$Cvar[i]), temp$ppara[i])
     }}


if(preset==1){
    # get the parameter of the SHEEP AND GOAT  
    temp<-scenario[ , c('SGvar',SG)]
    names(temp)[2]<-'ppara'
    for(i in  which(! temp$Cvar== ''))
    {
        assign(as.character(temp$SGvar[i]), temp$ppara[i])
    }}

if(preset==1){
    # get the parameter of PIG
    temp<-scenario[ , c('Pvar',P)]
    names(temp)[2]<-'ppara'
    for(i in  which(! temp$Pvar== ''))
    {
        assign(as.character(temp$Pvar[i]), temp$ppara[i])
    }}

if(preset==1){
    # get the parameter of the CHICKEN 
    temp<-scenario[ , c('CHvar',CH)]
    names(temp)[2]<-'ppara'
    for(i in  which(! temp$CHvar== ''))
    {
        assign(as.character(temp$CHvar[i]), temp$ppara[i])
    }}




# end of else loop


# check if ther e is a land use change required and run the code that will yield at add crop

# cropland is changes in luc
# grazlandHarv is used to compute the harvested land, it includes the the mosaic land,
# grazland is the origninal grazland, is the one used for land use change, it does not
# include the mosaic land 
# creating the yield correction layers, these are based on land cover. Mosaic land is assumed to be 50% 
# crop and 50% grazing


###################computation of the number of animals in the landscape##########
#numbers of cow in the area


numcow<-numcow_d+numcow_f+numcow_da

############################these are the computation for cattle############################
# for any other animal, this has to be adjusted based on IPCC guidlines 


#total energy requirement per average cow per year per system

#maintenance energy eq 10.3, and table 10.4


er_msis<-0.370*lwsis^0.75
er_mis<-0.386*lwis^0.75
er_mda<-0.370*lwda^0.75
er_msg<-0.217*lwsg^0.75
#activity energy equ 10.4 table 10.5
er_amsis<-0*er_mis 
er_amis<-0.1*er_mis # assumtion these animal still graze a bit  so the value is between 0 and 0.17
er_amda<-0.15*er_mda
er_amsg<-0.0107 *lwsg   # aaumption flat pasture as an average for the contry
#lactation energy eq 10.8 assuming milk fat content of 3.5%, this equation is per year 
er_lsis<-0 
er_lis<-myis*(1.47+0.4*3.5)
er_lda<-0
#net energy requirement per cow per day
erc_sis<-(er_msis+er_amsis)+ er_lsis/365 
erc_is<-(er_mis+er_amis)+ er_lis/365 
#erc_da<-(er_mda+er_amda)+ er_lda/365 +er_mda*0.1*1#draft energy 
erc_da<-(er_mda+er_amda)+ er_lda/365 + (22.02*(lwda/(lwsis*1)^0.75*0.0015))  #take draft energy out and put growth energy for follower
erc_sg<-er_msg+er_amsg

# 0.0041868 to convert cal to mj 
#total energy requirement from ruminants 
erc<-erc_is+erc_sis+erc_da+erc_sg # this is wrong but unnecessary as we do not run the GHG
##############################################################################################
#calculate energy requirement in by season  from each feed source 

#total energy required from each source of food
ng<-(sfng1/100*numcow_f*erc_sis + ifng1/100 * numcow_d*erc_is + dafng1/100 * (numcow_da*erc_da)+ sgfng1/100 * (num_sg*erc_sg))*365
rc<- ( sfrc1/100*numcow_f*erc_sis + ifrc1/100 * numcow_d*erc_is+ dafrc1/100 *(numcow_da*erc_da)+ sgfrc1/100 * (num_sg*erc_sg))*365
rr<- (sfrr1/100*numcow_f*erc_sis + ifrr1/100 * numcow_d*erc_is+ dafrr1/100 * (numcow_da*erc_da)+ sgfrr1/100 * (num_sg*erc_sg))*365
rl<-(sfrl1/100*numcow_f*erc_sis + ifrl1/100 * numcow_d*erc_is+ dafrl1/100 *(numcow_da*erc_da)+ sgfrl1/100 * (num_sg*erc_sg))*365
pf<-(sfpf1/100*numcow_f*erc_sis + ifpf1/100 * numcow_d*erc_is+ dafpf1/100 *(numcow_da*erc_da)+ sgfpf1/100 * (num_sg*erc_sg))*365
conc<-(sfconc1/100*numcow_f*erc_sis + ifconc1/100 * numcow_d*erc_is+ dafconc1/100 *( numcow_da*erc_da)+ sgfconc1/100 * (num_sg*erc_sg))*365
conos<-(sfconos1/100*numcow_f*erc_sis + ifconos1/100 * numcow_d*erc_is+ dafconos1/100 *(numcow_da*erc_da)+ sgfconos1/100 * (num_sg*erc_sg))*365

# fraction of each fodder at landscape scale over the year step. 
fng<- ng/(ng+rc+rl+pf+conc+conos+rr)
frc<- rc/(ng+rc+rl+pf+conc+conos+rr)
frr<- rr/(ng+rc+rl+pf+conc+conos+rr)
frl<-rl/(ng+rc+rl+pf+conc+conos+rr)
fpf<-pf/(ng+rc+rl+pf+conc+conos+rr)
fconc<-conc/(ng+rc+rl+pf+conc+conos+rr)
fconos<-conos/(ng+rc+rl+pf+conc+conos+rr)


# calculating the fresh weight of feed in basket 
# corrected for the fact that meg (metabolizing energy)
fw_g<-ng/(meg)
fw_rc<-rc/(merc)
fw_rl<-rl/(merl)
fw_pf<-pf/(mepf)
fw_cer_rum<-conc/(meconc) # so this is in kg
fw_conos<-conos/(meconos)
num_ch<-(num_ch_e+num_ch_m)
fw_cer_mono<-(1753.199136/100*num_ch)+ (num_pg*1461.000755/5)
fw_cer<-fw_cer_rum+fw_cer_mono
fw_rr<-0
ncellrice<-0
BNrice<-0
y_ricec<-0

#ratio of net energy available in the diet for maintenance to digestible energy consumed (REM)
#first compute digestibiliy of the landscape level feed
#de<-(ng*d_g+rc*d_rc+rl*d_rl+pf*d_pf+conc*d_conc+conos*d_conos+rr*d_rr)*100 # this seems to be wrong 11.3.2017
de<-(fng*d_g+frc*d_rc+frl*d_rl+fpf*d_pf+fconc*d_conc+fconos*d_conos+frr*d_rr)*100
#REM = ratio of net energy available in a diet for maintenance to digestible energy consumed
rem=(1.123-(4.092*10^-3*de)+(1.126*10^-5*de^2)-(25.4/de))

#yearly gross energy requirement at landscape scale
gerc<-(erc/rem)/(de/100) # this is now missing pigs and chicken



# calculate production 
# milk production in tons of litre 
milk<- numcow_d * myis 

#meat production in kg tons
# here we actually account for the total meat produced regarless that the animals are not always in the area
meat_sis <- (numcow_f *dsis)
# also dairy cows here do not contribute to meat , and it is the potential meat production, assume we cull them all
# this is now adressed and dairy animal contribute to meat as in impact 
meat_is<-(numcow_d*dsis*1/5) # 1/5 is a globlium assumption
# we changes the dressing percentage to yield per animal... 
meat<-meat_sis+meat_is
# area needed for the feed production (with adjustement to pass from ha to km2, and tons to kg)
#first we need to compute the fodder yield. This is done by multiplying the crop yield with the
# residue factor, which is the part that is used as feed. This is computed in the water pararmeter
#excel sheet and accounts for post harvest loss
fun=function(x){x*rfm} # residue factor to account for what is consummed by livestock only
fy_rc<-overlay(y_maize,fun=fun)

fun=function(x){x*rfl}
fy_rl<-overlay(y_pulse,fun=fun)

fun=function(x,y,z){x*y*z}
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

# max_rl<-cellStats(fy_rlc,stat='sum')
###check from here in principle one should not work with the area but the produce... 
#getting biomass from grass
 
y_grass2<-y_grass* 0.001/ dm_g *grazlandHarv * grazycorr# this is in kg DM /ha * for0.001 to tons /dm to get freshweight and we need to transform
 # now this is in kg/pixel fresh weight
y_grass2[y_grass2==0]<-NaN
fy_g<-cellStats(y_grass2,stat='mean') # per tons per ha 

#we assume that every cropland pixel produces the basket so we need to correct the total production
#from the area to avoid double planting 

#compute the percentage of each feed on cropland
#so we need to compute the area used for each crop (in term of pixels, so in a 900m2)
fy_rcc[fy_rcc==0]<-NaN
avg_rc<-cellStats(fy_rcc,stat='mean')
# compute maize grain needed
y_maizec[y_maizec==0]<-NaN
avg_cer<-cellStats(y_maizec,stat='mean')
cropProdI<- cropProd[, YEAR]*1000/ 10000 *pixel^2 #kg/m2 
pgc<-(cropProdI-avg_cer)/cropProdI

if(nopgc==1){pgc<-0.01157012} # change this with the base run pcg

#avg_cer<-cellStats(y_maizec*(1+pgc),stat='mean')
#avg_rc<-cellStats(fy_rcc*(1+pgc),stat='mean')

fun=function(x,y,z){x*(1+pgc)*y*z}
y_maizec<-overlay(y_maize,cropland,cropycorr,fun=fun)
y_maizec[y_maizec==0]<-NaN

# max_c<-cellStats(y_maizec,stat='sum')
# 
fy_rcc<-overlay(fy_rc,cropland,cropycorr,fun=fun)
fy_rcc[fy_rcc==0]<-NaN

# max_rc<-cellStats(fy_rcc,stat='sum')

avg_cer<-cellStats(y_maizec,stat='mean')
avg_rc<-cellStats(fy_rcc,stat='mean')


#area required 
ar_rc1<-fw_rc/(avg_rc) *pixel^2/100000  # number pixel * adjustement in km2
ar_cer<-fw_cer/(avg_cer)*pixel^2/100000

#We select the area which is used most or for residue or for cereal 
ar_rc<-max(ar_rc1,ar_cer)

fy_rlc[fy_rlc==0]<-NaN
avg_rl<-cellStats(fy_rlc,stat='mean')
ar_rl<-fw_rl/avg_rl *pixel^2/100000
ar_pf<-fw_pf/(fy_pf*1000/0.01*rfpf)

riceland<-cropland
riceland[]<-0
avg_rr<-0
ar_rr<-0
ricearea<-1
#total cropland area required
ar<-(ar_rc+ar_rl+ar_pf) # it is already in km2

general_yield<-avg_rc *ar_rc/ar+avg_rl *ar_rl/ar+(fy_pf*1000/0.01*rfpf)*ar_pf/ar

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

if (add > 0){
    source('3-cleaned/2-luc2test.r')}
# compute the new land cover layers
if (add>0) {
   source('3-cleaned/2-luccomp2.r')
}

grazland<-l130+l120+l121+l122+l110+l100 # by moving this, it computes is with the luc
ncellgraz<-cellStats(grazland,stat='sum') 
grazlandHarv<-l130+l120+l121+l122+l110+l100+l30+l40
grazycorr<-l130+l120*mosg1+l121*mosg1+l122*mosg1+l110*mosg2+l100*mosg2+l30*(1-mosc1)+l40*(1-mosc2)


# by moving this, it computes is with the luc
#ncellgraz<-cellStats(grazland,stat='sum') 
grazlandHarv<-l130+l120+l121+l122+l110+l100+l30+l40
#cropycorr<-l10+l11+l12+l20+l30*mosc1+l40*mosc2 

# the percentage computation are now in the feedbasket 
ncellcrop<-cellStats(cropland,stat='sum')


#calculate the production of planted fodder under cropland 
croparea<-cellStats(cropland,stat='sum')*pixel^2/1000000 #area in km

#compute the biomass per km2  
bio_km2<- (fw_rc+fw_rl+fw_pf+fw_cer )/ar_rc
biomass<- (fw_rc+fw_rl+fw_pf+fw_cer)

diff_ar<-croparea-ar   # ar already in km2
if (diff_ar>0){
     import_c = 0
     totmaize= (avg_cer*(ar_rc+ diff_ar) - fw_cer)*0.001 #fw_cer is in kg *0.001 to tons 
     #ar_rc1 is crop residue only, and ar_rc is the max area from ar_rc or ar_cer
} else {import_c= -diff_ar*bio_km2

     totmaize= avg_cer*ar_rc*0.001- fw_cer*0.001  #fw_cer is in kg
     #ar_rc1 is crop residue only, and ar_rc is the max area from ar_rc or ar_cer
} # now imports are in area in km2 
diff_ar<-round(diff_ar, digit=0)
#calculate the production under grazing land 


grazarea<-cellStats(grazlandHarv,stat='sum')*(pixel^2/1000000) #area in km2
ar_g<-fw_g/(fy_g*1000/0.01 )#fy_g is is in ton /ha so we need to transform into kg/km2


diff_g <- grazarea-ar_g
if (diff_g>0){
  import_g = 0
} else {import_g<- -diff_g*(fy_g*1000/0.01 )}
diff_g <- round(diff_g, digit=0)


####################################moving the manure management computation here as they are used in soil and ghg 
species<-'dairyCows'
species2<- 'Othercattle'
species3<- 'OtherCattle'
species4<- 'Cattle'

#Africa, Asia, Latin America
region='Africa'
country='United Republic of Tanzania' # for global forest ressource
#for dairy 
# extracting the IPCC parameter for each management type depending on temperature 
mms_lagoon<-(is_lagoon_perc*numcow_d)
mms_liquidslurry<-(is_liquidslurry_perc*numcow_d)
mms_solidstorage<-(is_solidstorage_perc*numcow_d)
mms_drylot<-(is_drylot_perc*numcow_d)
mms_pasture<-(is_pasture_perc*numcow_d)
mms_dailyspread<-(is_dailyspread_perc*numcow_d)
mms_digester<-(is_digester_perc*numcow_d)
mms_fuel<-(is_fuel_perc*numcow_d)
mms_other<-(is_other_perc*numcow_d)
mms_total<-mms_lagoon+mms_liquidslurry+mms_solidstorage+mms_drylot+mms_pasture+mms_dailyspread+mms_fuel+mms_other

mms_lagoon_perc<-mms_lagoon/mms_total*100
mms_liquidslurry_perc<- mms_liquidslurry/mms_total*100
mms_solidstorage_perc<- mms_solidstorage/mms_total*100
mms_drylot_perc<- mms_drylot/mms_total*100
mms_pasture_perc<- mms_pasture/mms_total*100
mms_dailyspread_perc<- mms_dailyspread/mms_total*100
mms_digester_perc<- mms_digester/mms_total*100
mms_fuel_perc<- mms_fuel/mms_total*100
mms_other_perc<- mms_other/mms_total*100

mms_params1 <- read.csv('1-input/parameter/MMSparams.csv')
mms_params <-subset(mms_params1 , mms_params1$Species==species&mms_params1$Region==region)
B0<-mms_params$B0

if(ipcc==1){
  mms_lagoon_perc<-mms_params$lagoon_perc
  mms_liquidslurry_perc<-mms_params$Liquid.slurry_perc
  mms_solidstorage_perc<-mms_params$Solid.storage_perc
  mms_drylot_perc<-mms_params$Dry.lot_perc
  mms_pasture_perc<-mms_params$Pasture_perc
  mms_dailyspread_perc<-mms_params$Daily.spread_perc
  mms_digester_perc<-mms_params$Digester_perc
  mms_fuel_perc<-mms_params$Burned.for.fuel_perc
  mms_other_perc<-mms_params$Other_perc}


mms_params <-subset(mms_params1 , mms_params1$Species==species2 &mms_params1$Region==region)
B02<-mms_params$B0

mms_lagoon2<-(sis_lagoon_perc*numcow_f+da_lagoon_perc*numcow_da)
mms_liquidslurry2<-(sis_liquidslurry_perc*numcow_f+da_liquidslurry_perc*numcow_da)
mms_solidstorage2<-(sis_solidstorage_perc*numcow_f+da_solidstorage_perc*numcow_da)
mms_drylot2<-(sis_drylot_perc*numcow_f+da_drylot_perc*numcow_da)
mms_pasture2<-(sis_pasture_perc*numcow_f+da_pasture_perc*numcow_da)
mms_dailyspread2<-(sis_dailyspread_perc*numcow_f)
mms_digester2<-(sis_digester_perc*numcow_f+da_digester_perc*numcow_da)
mms_fuel2<-(sis_fuel_perc*numcow_f+da_fuel_perc*numcow_da)
mms_other2<-(sis_other_perc*numcow_f+da_other_perc*numcow_da)
mms_total2<-mms_lagoon2+mms_liquidslurry2+mms_solidstorage2+mms_drylot2+mms_pasture2+mms_dailyspread2+mms_fuel2+mms_other2

mms_lagoon_perc2<-mms_lagoon2/mms_total2*100
mms_liquidslurry_perc2<- mms_liquidslurry2/mms_total2*100
mms_solidstorage_perc2<- mms_solidstorage2/mms_total2*100
mms_drylot_perc2<- mms_drylot2/mms_total2*100
mms_pasture_perc2<- mms_pasture2/mms_total2*100
mms_dailyspread_perc2<- mms_dailyspread2/mms_total2*100
mms_digester_perc2<- mms_digester2/mms_total2*100
mms_fuel_perc2<- mms_fuel2/mms_total2*100
mms_other_perc2<- mms_other2/mms_total2*100

if(ipcc==1){
  mms_lagoon_perc2<-mms_params$lagoon_perc
  mms_liquidslurry_perc2<-mms_params$Liquid.slurry_perc
  mms_solidstorage_perc2<-mms_params$Solid.storage_perc
  mms_drylot_perc2<-mms_params$Dry.lot_perc
  mms_pasture_perc2<-mms_params$Pasture_perc
  mms_dailyspread_perc2<-mms_params$Daily.spread_perc
  mms_digester_perc2<-mms_params$Digester_perc
  mms_fuel_perc2<-mms_params$Burned.for.fuel_perc
  mms_other_perc2<-mms_params$Other_perc}

ch_meat<-round((num_ch)*dch, digit=0)
eggs<-round(num_ch_e*egg,digit=0)
pg_meat<-round(num_pg*dpg, digit=0)
rum_meat<-round(num_sg*dsg, digit=0)
milk<-round(milk,digits=0)
meat<-round(meat,digits=0)
totmaize<-round(totmaize,digits=0)
import_c<-round(import_c,digits=0)
import_g<-round(import_g,digits=0)
croparea<-round(croparea,digits=0)
grazarea<-round(grazarea,digits=0)
ar_g<-round(ar_g,digits=1)
ar_rc<-round(ar_rc,digits=1)


ch_meat_pop<-round(ch_meat/pop*1000, digit=2)
eggs_pop<-round(eggs/pop*1000,digit=2)
pg_meat_pop<-round(pg_meat/pop*1000, digit=2)
rum_meat_pop<-round(rum_meat/pop*1000, digit=2)
milk_pop<-round(milk/pop*1000,digits=2)
meat_pop<-round(meat/pop*1000,digits=2)
totmaize_pop<-round(totmaize/pop*1000,digits=2)
biomass2<-round(biomass*0.001,digits=0)

prod_ind<-data.frame( meat, milk , rum_meat,  ch_meat, eggs, pg_meat,totmaize, croparea,grazarea, ar_rc, ar_g ,import_c*0.001, import_g*0.001 , ch_meat_pop,eggs_pop,pg_meat_pop,rum_meat_pop,milk_pop, meat_pop,totmaize_pop,biomass2)
prod_ind_diff<-prod_ind-prod_base
prod_ind_per<-round(prod_ind_diff/prod_base*100, digit=4)
prod_ind_per<-ifelse(is.na(prod_ind_per),0,prod_ind_per)
prod_ind2<-rbind(prod_ind,prod_ind_diff)
prod_ind2<-rbind(prod_ind2, prod_ind_per)
names(prod_ind2)<-c("cattle meat produced","milk produced", "goat and sheep meat", "chicken meat","eggs","pig meat", "maize for consumption", 'total area available for crop', 'total area available for pasture',  "crop area used", 'pasture area used',  "import crop", 'import pasture', 'chick meat per person',   'eggs per person', 'pig meat per person', 'small ruminant meat per pers', "milk per person" , 'beef meat per person' , "maize per person","biomass")
prod_ind2<-data.frame(t(prod_ind2))
colnames(prod_ind2)<-c('base','diff','percent')
##steve edits
# prod_ind_val<-read.csv('1-input/parameter/productivity_val.csv')
# prod_ind2$evaluation <- ifelse(prod_ind2$percent <= prod_ind_val$peu, 'low', ifelse((prod_ind2$percent >= prod_ind_val$peu) & (prod_ind2$percent <= prod_ind_val$peu),'medium','high'))

proposed_add<- general_yield*import_c*1000/(fw_rc+fw_rl+fw_pf+fw_cer) # in kg
proposed_add2<- general_yield*prod_ind_diff$ar_rc*1000/(fw_rc+fw_rl+fw_pf+fw_cer) # in kg


setwd ("4-output")
pdf(paste(name,"productivity.pdf", sep="-"))
#par(mfrow=(c(2,1)))

plot(NA, xlim=c(0,100), ylim=c(0,10), bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
 grid.table(prod_ind2)


dev.off()

write.csv(prod_ind,paste(name,"prod_ind.csv", sep="-"),row.names = F)

setwd(path)


