####################################CLEANED version 2 for Burkina Faso ################################################
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

# in the  animals (27015712 total 2016 FAOSTAT)
numcow_d <-  27015712*0.3  # dairy 

#  meat (37193)
numcow_f<- 27015712 *0.4 # fattening 

# followers (40885)
numcow_da <- 27015712*0.4 # follower 

# shoat
num_sg<- 	18721705+ 6225812 # (goat and sheep FAO stat 2016)

# chicken 
num_ch<- 37260*1000
     
# pig 
num_pg<-516912


# presets 
preset <-1 #if 1 the interface uses the presets if 0 or other it uses the manual input below. 
scenario<-read.csv('1-input/parameter/preset.csv',  skip=2)

# select from the preset scenario

M<- 'MBR'  #options are (MBR, M1, M2)
Fa<- 'FBR'  #options are (FBR, F1, F2)
Tr<- 'TBR'  #options are (TBR, T1)


Ca<- 'CBR' #options are (CBR, C1)

#give your scanario a name 
name<-"base run"

#DEFINE THE DIFFERENT SYSTEM 
#################################

####################################### meat category ###################



#define liveweigtht in kg for the the breed in the semi intensive system (250)
lwsis=125 
#define milk yield (kg/cow/year) for the breed in the semi intensive system (0)
mysis=0

#dressing percentage http://www.dpi.nsw.gov.au/__data/assets/pdf_file/0006/103992/dressing-percentages-for-cattle.pdf
dsis = 0.64

#feed basket  for fattening  system season wet

#natural grass in percent (15)
sfng1<-42.367298
#crop residues cereals (40)
sfrc1<-17.193793
#crop residue from rice
sfrr1<- 0
#crop residue legumes (30)
sfrl1<-0
#planted fodder ()
sfpf1<-0 
# concentrates cereal (maize grain) (5)
sfconc1<- 0
# concentrates oilseed cake (10)
sfconos1<- 40.438905  

# manure management in the fattening system in percent (100%)
#(if ipcc=1, then no need to adjust this )
sis_lagoon_perc<- 00
sis_liquidslurry_perc<-00
sis_solidstorage_perc<-100
sis_drylot_perc<-00
sis_pasture_perc<-00
sis_dailyspread_perc<-00
sis_digester_perc<-00
sis_fuel_perc<-00
sis_other_perc<-00


################################# dairy category ############


#define liveweigtht in kg for the the breed in dairy system (250)
lwis=125

#define milk yield (kg/cow/year) for the breed in the dairy system (141)
myis=141 

#dressing percentage for the milking cow meat per cow 
dis = 0.85



#feed basket for dairy wet season
#natural grass in percent (15)
ifng1<-47.223208
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
ifconos1<- 52.776792


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
dafng1<-42.60399
#crop residues cerals (40)
dafrc1<-17.284697
#crop residue from rice
dafrr1<-0
#crop residue legumes (30)
dafrl1<-0
#planted fodder ()
dafpf1<-0
# concentrates cereal (maize bran) (5)
dafconc1<- 0
# concentrates oilseed cake (10)
dafconos1<- 40.111305

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
#dressing percentage for the milking cow meat per cow 
dsg = 0.85

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
lwch=1.5

#dressing percentage for the milking cow meat per cow 
dch = 0.7


#################################pig ############################
#define liveweigtht in kg for  pig (20)
lwpg= 30

#dressing percentage 
dpg = 0.7


#######################################################################################
#global variable definition
#ipcc= 1 the code will use ipcc tier 2 standards for manure storage in stead of the user defined one
ipcc= 1 

#########################parmeters specific to the soil pathway##################


#linking the manure availability to the production system 

mprod_f<- 3 #manure production from a cow in the fattening system per day
mprod_d<- 3 #manure production from a cow in the dairy system per day
mprod_da<- 3 #manure production from a cow in the dairy system per day


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


#####reading some r info
setwd(path)
pixel<-read.csv('1-input/parameter/pixel.csv')
pixel<-as.numeric(pixel[2])

##############################land use driven scenarios
library(raster)

#do we run a land use change driven scenario? then we need to run the land use change module first 
# and read here the file indicating the pixels that have changed

#add the path to the changes in land use rasters
#path to changing cropland i.e. cropland change layer

add<-0 # in percent, will be devided by 100 in the luc code


setwd(path)
#now overwrite variables with preset
if(preset==1){
  # get the parameter of the pastoral extensive system
  temp<-scenario[ ,c('Avar', Ap)]
  names(temp)[2]<-'ppara'
  for(i in  which(! temp$Avar== ''))
  {
    assign(as.character(temp$Avar[i]), temp$ppara[i])
  }}

if(preset==1){  
  # get the parameter of the pastoral dairy system
  temp<-scenario[ , c('Lvar',L)]
  names(temp)[2]<-'ppara'
  for(i in  which(! temp$Lvar== ''))
  {
    assign(as.character(temp$Lvar[i]), temp$ppara[i])
  }}

if(preset==1){
  # get the parameter of the  dairy system
  temp<-scenario[ , c('Mvar',M)]
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
  temp<-scenario[ , c('Tvar',Tr)]
  names(temp)[2]<-'ppara'
  for(i in  which(! temp$Tvar== ''))
  {
    assign(as.character(temp$Tvar[i]), temp$ppara[i])
  }}
if(preset==1){
  # get the parameter of the draft system
  temp<-scenario[ , c('Cvar',Ca)]
  names(temp)[2]<-'ppara'
  for(i in  which(! temp$Cvar== ''))
  {
    assign(as.character(temp$Cvar[i]), temp$ppara[i])
  }}

# end of else loop
setwd(path)
#ar_rc1 is crop residue only, and ar_rc is the max area from ar_rc or ar_cer
source('3-cleaned/2-load_data.r')
setwd(path)

source('3-cleaned/2-feedbasket_nonlinear_impact.r')


end<-Sys.time()

end-start
# #########################################run CLEANED##################################
# 
# #run the water pathway 
# setwd(path)
# source('3-cleaned/1-water.r')
# #run the greenhouse gas pathway
# setwd(path)  
# source('3-cleaned/1-ghg.r')
# #run the biodiversity pathway
# setwd(path)
# source('3-cleaned/1-biodiv.r')
#   
# setwd(path)
# source('3-cleaned/1-soil.r')
# 
# # #ouput maps can be found in the 4- ouput map folder 

