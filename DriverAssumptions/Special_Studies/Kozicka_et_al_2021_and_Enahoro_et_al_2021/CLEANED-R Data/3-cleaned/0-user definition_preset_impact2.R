####################################CLEANED IMPact Tanzania ################################################
# cleaned version 2
# code by Catherine Pfeifer, ILRI, c.pfeifer@cgiar.org
#code created on 20.6.2016
#clearing all memory 

# in this code conc1 is becoming maize as a grain
# so now we need to check on whether maize or crop is biggest and how we do account for the 
# maize as food security indicator
# we are also introducing shoat and chicken
# the environmental impacts have to be disconnected 

##############################land use driven scenarios####################################CLEANED IMPact Tanzania ################################################
# cleaned version 2
# code by Catherine Pfeifer, ILRI, c.pfeifer@cgiar.org
#code created on 20.6.2016
#clearing all memory 
rm(list=ls(all=TRUE))
#set path to cleaned tool


path<-'D:/Dropbox/CLEANED - IMPACT'
setwd(path)

# in this code conc1 is becoming maize as a grain
# so now we need to check on whether maize or crop is biggest and how we do account for the 
# maize as food security indicator
# we are also introducing shoat and chicken
# the environmental impacts have to be disconnected 
###################################This sheet defines all user defined variables#################
# set selection from IMPACT here 

IMPscen <- 'TZNPesDemand' # 'TZNBaseDemand', 'TZNOptDemand','TZNPesDemand' ,'TZNAltDemand'this one is not working 

# SSP2-NoCC
YEAR<- "X2030"

##############################land use driven scenarios
library(raster)

#do we run a land use change driven scenario? then we need to run the land use change module first 
# and read here the file indicating the pixels that have changed

#add the path to the changes in land use rasters
#path to changing cropland i.e. cropland change layer
# enter here the percentage of additional biomass required (i.e. inducing the land use change)
add<-0  # in percent, will be devided by 100 in the  luc code
corrOption<- 1# 1=cropland only 2= newland is agro-forestry 
#give your scanario a name 

nopgc<-1 # if ==1 then no crop productivity gain pcg from base run 


name<-'pes_np'
#add<- add+add_cc
# AMT-Beef
# AMT-Pork
# AMT-Lamb
# AMT-Poultrypr
# AOT-Eggs
# AOT-Dairy



### read in IMPACT DATA 
animals<-read.csv('1-input/parameter/AnmAgg.csv')
animals<-animals[animals$Scenario==IMPscen , ]
yield<-read.csv('1-input/parameter/YielAgg.csv')
yield<-yield[yield$Scenario==IMPscen  , ]
#expansion<-read.csv('1-input/parameter/cropExpansion.csv',header=TRUE ) 
#expansion<-expansion[expansion$Scenario==IMPscen & expansion$type=='CER', ]
#add_cc<-expansion[,YEAR]  #in mtrons now only relevant for SSP2-HGEM-HiYld2 scenario 


cropProd<-read.csv('1-input/parameter/cropProd.csv',header=TRUE ) 
cropProd<-cropProd[cropProd$Scenario==IMPscen & cropProd$Commodity=='CER', ] 



# in the  animals (27015712 total 2016 FAOSTAT)
numcow_d <-  animals[animals$Commodity=="AOT-Dairy", YEAR]*1000  # dairy 

#  meat (37193)
numcow_f<- animals[animals$Commodity=="AMT-Beef", YEAR]*1000    # fattening 

# followers (40885)
numcow_da <- 0 # follower 

# shoat
num_sg<- 	animals[animals$Commodity=="AMT-Lamb", YEAR] *1000   # (goat and sheep FAO stat 2016)

# chicken 
#for meat
num_ch_m<- animals[animals$Commodity=="AMT-Poultry", YEAR] *1000 
#for egg
num_ch_e<- animals[animals$Commodity=="AOT-Eggs", YEAR] *1000 

     
# pig 
num_pg<-animals[animals$Commodity=="AMT-Pork", YEAR] *1000 


# presets 
preset <-1 #if 1 the interface uses the presets if 0 or other it uses the manual input below. 
scenario<-read.csv('1-input/parameter/preset.csv',  skip=2)

# select from the preset scenario
D  <- 'DBR'  #options are (DBR, D1, D2)
Fa <- 'FBR'  #options are (FBR, F1, F2)
O  <- 'OBR'  #options are (OBR, O1,O2)
SG <- 'SGBR' #options are (SGBR, SG1,SG2)
P <- 'PBR' #options are (PBR, P1,P2)
CH <- 'CHBR' #options are (CHBR, CH1,CH2)

Ca<- 'CBR' #options are (CBR, C1)



#DEFINE THE DIFFERENT SYSTEM 
#################################

####################################### meat category ###################
popdata<-read.csv('1-input/parameter/pop.csv',  skip=0)
popdata$Scenario<-as.character(popdata$Scenario)
pop<-popdata[popdata$Scenario==IMPscen,YEAR]*1000000 

#define liveweigtht in kg for the the breed in the semi intensive system (250)
lwsis=125 
#define milk yield (kg/cow/year) for the breed in the semi intensive system (0)
mysis=0

#yield per animal
dsis = yield[yield$Commodity=="AMT-Beef", YEAR]

#feed basket for dairy wet season
#natural grass in percent (15)
sfng1<-48
#crop residues cerals (40)
sfrc1<-0
#crop residue from rice
sfrr1<-0
#crop residue legumes (30)
sfrl1<-0
#planted fodder ()
sfpf1<-0
# concentrates cereal (maize grain) (5)
sfconc1<- 0
# concentrates oilseed cake (10)
sfconos1<- 40  



# manure management in the fattening system in percent (100%)
#(if ipcc=1, then no need to adjust this )
sis_lagoon_perc<- 00
sis_liquidslurry_perc<-00
sis_solidstorage_perc<-100

#feed basket  for fattening  system season wet

#natural grass in percent (15)
sfng1<-40
#crop residues cereals (40)
sfrc1<-20
#crop residue from rice
sfrr1<- 0
#crop residue legumes (30)
sfrl1<-0
#planted fodder ()
sfpf1<-0 


sis_drylot_perc<-00
sis_pasture_perc<-00
sis_dailyspread_perc<-00
sis_digester_perc<-00
sis_fuel_perc<-00
sis_other_perc<-00


################################# dairy category ############


#define liveweigtht in kg for the the breed in dairy system (250)
lwis=125

#define milk yield (ton/cow/year) for the breed in the dairy system 
myis=yield[yield$Commodity=="AOT-Dairy", YEAR] 

#dressing percentage for the milking cow meat per cow 
#dis = 0.85 # unnecessary in this version



#feed basket for dairy wet season
#natural grass in percent (15)
ifng1<-48
#crop residues cerals (40)
ifrc1<-0
#crop residue from rice
ifrr1<-0
#crop residue legumes (30)
ifrl1<-0
#planted fodder ()
ifpf1<-0
# concentrates cereal (maize bran) (5)
ifconc1<- 0
# concentrates oilseed cake (10)
ifconos1<- 52


# manure management in the semi-intensive system in percent (100%)
#(if ipcc=1, then no need to adjust this )
is_lagoon_perc<- 00
is_liquidslurry_perc<-00
is_solidstorage_perc<-100
is_drylot_perc<-00
is_pasture_perc<-00
is_dailyspread_perc<-00
is_digester_perc<-00
is_fuel_perc<-00
is_other_perc<-00

################################# followers  ############


#define liveweigtht in kg for the the breed in dairy system (250)
lwda=125

#define milk yield (kg/cow/year) for the breed in the dairy system (1000)
myda=0 

#feed basket for dairy wet season
#natural grass in percent (15)
dafng1<-40
#crop residues cerals (40)
dafrc1<-20
#crop residue from rice
dafrr1<-0
#crop residue legumes (30)
dafrl1<-0
#planted fodder ()
dafpf1<-0
# concentrates cereal (maize bran) (5)
dafconc1<- 0
# concentrates oilseed cake (10)
dafconos1<- 40

# manure management in the semi-intensive system in percent (100%)
#(if ipcc=1, then no need to adjust this )
da_lagoon_perc<- 00
da_liquidslurry_perc<-00
da_solidstorage_perc<-100
da_drylot_perc<-00
da_pasture_perc<-00
da_dailyspread_perc<-00
da_digester_perc<-00
da_fuel_perc<-00
da_other_perc<-00


#################################shoat############################
#define liveweigtht in kg for ta shoat (20)
lwsg=25

#define milk yield (kg/cow/year) for the breed in the dairy system (141)
mysg=0 
#yield  percentage for shoat meat 
dsg = yield[yield$Commodity== "AMT-Lamb", YEAR] 


#feed basket for shoat
#natural grass in percent ()
sgfng1<-80
#crop residues cerals ()
sgfrc1<-20
#crop residue from rice
sgfrr1<-0
#crop residue legumes ()
sgfrl1<-0
#planted fodder ()
sgfpf1<-0
# maize grain ()
sgfconc1<- 0
# concentrates oilseed cake ()
sgfconos1<- 0




# manure management in the semi-intensive system in percent (100%)
#(if ipcc=1, then no need to adjust this )
sg_lagoon_perc<- 00
sg_liquidslurry_perc<-00
sg_solidstorage_perc<-100
sg_drylot_perc<-00
sg_pasture_perc<-00
sg_dailyspread_perc<-00
sg_digester_perc<-00
sg_fuel_perc<-00
sg_other_perc<-00



#################################chicken############################
#define liveweigtht in kg for ta chicken (20)
lwch= 2.5

# number of eggs per chicken per year
# note that egg production is not linked to energy requirement for chicken, so this is a fully exogenous
egg<- yield[yield$Commodity=="AOT-Eggs", YEAR]  

# yield percentage for chicken meat per cow 
dch = yield[yield$Commodity=="AMT-Poultry", YEAR] 
# percentage of feed from agricultural waste
pagwch<-0

#################################pig ############################
#define liveweigtht in kg for  pig 
lwpg= 50

#yield percentage 
dpg = yield[yield$Commodity=="AMT-Pork", YEAR] 


#percent of feed from agricultural waste
pagwpg<-0

#######################################################################################
#global variable definition
#ipcc= 1 the code will use ipcc tier 2 standards for manure storage in stead of the user defined one
ipcc= 1 

#########################parmeters specific to the soil pathway##################


#linking the manure availability to the production system 

mprod_f<- 3 #manure production from a cow in the fattening system per day
mprod_d<- 3 #manure production from a cow in the dairy system per day
mprod_da<- 3 #manure production from a cow in the dairy system per day
mprodn_sg<- 48.02385*0.1 # globium rate
mprodn_ch<-20.454*0.01
mprodn_pg<- 20.545*0.25

#percent of stored manure applied to the different crop
#cereal (mprod_c *% to this crop for linking with production )
manc<-0.4
# legumes  (mprod_c *% to this crop for linking with production )
manl<-0
#planted fodder  (mprod_c *% to this crop for linking with production )
manpf<- 0
#rice  (mprod_r *% to this crop for linking with production )
manr<- 0.4 
#grazing land  (mprod_r *% to this crop for linking with production )
mangraz<-0 

# application of slurry kg/ha
#cereal ()
sluc<-0
# legumes (0)
slul<-0
#planted fodder ()
slupf<-0
#grazing land
slugraz<-0
#rice land
slur<-0


slurryconv<- 0.001 #conversion rate between slurry (NPK) and Nitrogen
#we need a source here What about compost and other manure. 

#inorganic fertilizer application in kg per hectare

#cereal (50 is recommended)
fertc<- 0
#rice (50 is recommended)
fertr<- 0
# legumes (0)
fertl<- 0
#planted fodder 
fertpf<- 0
#grazing land
fertgraz<- 0

Fertconv<- 0.2 #conversion rate between fertilizer (NPK) and Nitrogen, depends on the locally available ferilizer, +/- 20%
# from impact lit we know that DAP is most commonly used - Joanne is looking for conversion rates


#exogenous yield productivity gain in percentage of yield
#crop
pgc= 0.0
#legumes
pgl=0.0
#planted fodder
pgpf=0.0
#grassland
pgg= 0.0


#############soil management option on cropland (ghg)
perc_til= 0 #percentage of cropland that is tilled 
perc_redtil = 100 #percentage of cropland that is on reduced till
perc_notil = 0 #percentage of cropland that is on no till

perc_inlow = 100  #percentage of land with low input 
perc_inmedium = 0  #percentage of land with medium input 
perc_inhighnoman =0  #percentage of land with high input no manure 
perc_inhighman =0   #percentage of land with high input with manure



#####define the correction factor for each land type
mosc1<-0.5 # on land l30
mosc2<-0.5 #on land l40 
mosg1<- 0.8 # on shrubland
mosg2<-0.5 # on mozaik without crop 


#####reading some r info
setwd(path)
pixel<-read.csv('1-input/parameter/pixel.csv')
pixel<-as.numeric(pixel[2])


start<-Sys.time()
# end of else loop
setwd(path)
#ar_rc1 is crop residue only, and ar_rc is the max area from ar_rc or ar_cer
source('3-cleaned/2-load_data.r')
setwd(path)

source('3-cleaned/2-feedbasket_nonlinear_impact2.r')
# check if there is a land use change required and run the code that will yield at add crop
# compute minimal land use change : replace the number after biomass with diff biomass in the output table
import_c*0.001 /(biomass2-prod_ind2[21,2])*100


setwd(path)
source('3-cleaned/1-water.r')

setwd(path)
source('3-cleaned/1-ghg.r')

# setwd(path)
# source('3-cleaned/1-biodiv.r')


rm(Coe_map,Coe_map_diff,COe_l,csluc,ef_ch,ef_pg,ef_sg,efc_d,efc_c, efg_d, forest_clim, dist_suit_c,efg_c, ET_gras,ET_grasl,ET_maize,ET_pulse,ET_rice,Flu_c,Flu_pc,Flu_rice)
rm(Fmg_c,Flu_tot, disttocrop,disttocrop2,Fi_c, Flu_sa,clim_ctdry,clim_ctmoist,clim_tr_dry,Coe,Coe_manure, lostgraz,lostforest, manure_CH4_year_herd_c,manure_CH4_year_herd_d, N2O_t_year_herd_map, N2O_t_year_herd_map_c,N2O_t_year_herd_map_g, temp,temp2)

setwd(path)
source('3-cleaned/1-soil.r')

end<-Sys.time()

end-start


