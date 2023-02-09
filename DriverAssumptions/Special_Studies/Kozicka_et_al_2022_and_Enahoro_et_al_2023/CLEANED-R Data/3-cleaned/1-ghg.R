######################### greenhouse gases  pathway impact TZA ##############
#by Catherine Pfeifer
#21.6.2016
# this version is modified on 23.1.2018 thus after the workshop
# changes are 
#1. ge is not divied by 365 because gerc is daily, not true in this version gerc is annual so it was corrected back 
#2.  N_loss$N_loss has been adjusted as in the old version the subset did not pick any number 



#clearing all memory 

# read libraries

#setting the species for querries the IPCC tables 
#for manure, choice are  : dairyCows, otherCattle, Market swine, Breeding swine
ncellrice<-0
setwd(path)
Coe_base<-read.csv('1-input/base_run/ghg_ind.csv', row.names = NULL)
#Coe_base<-Coe_base[ ,2:7]
Coe_map<-raster('1-input/base_run/ghg_map.tif')

########################################enteric fermetation ########################################


#livestock distribution 

#####calculating percentage of DM total
dmi<-((fw_g*dm_g)+(fw_rc*dm_rc)+(fw_rl*dm_rl)+(fw_pf*dm_pf)+(fw_cer_rum*dm_conc)+(fw_conos*dm_conos)+(fw_rr*dm_rr))
perc_dm_g<-(fw_g*dm_g)/dmi
perc_dm_rc<-(fw_rc*dm_rc)/dmi
perc_dm_rl<-(fw_rl*dm_rl)/dmi
perc_dm_pf<-(fw_pf*dm_pf)/dmi
perc_dm_conc<-(fw_cer_rum*dm_conc)/dmi
perc_dm_conos<-(fw_conos*dm_conos)/dmi
perc_dm_rr<-(fw_rr*dm_rr)/dmi


# this computation if for dairy system as proposed by Gerber (2011)
#calculating LCIDE and gama m (ym)
#Average digestibilities assumed to be: Pasture 0.66, crop residue 0.526, fodder 0.567, Concentrate 0.8
lcide <- (perc_dm_g*d_g)+(perc_dm_rc*d_rc)+(perc_dm_rl*d_rl)+(perc_dm_pf*d_pf)+(perc_dm_conc*d_conc)+(perc_dm_conos*d_conos)+(perc_dm_rr*d_rr)
# methane conversion factor, constants are taken from (tier 2 estimation) Gerber et al 2011
ym<- (9.75 - 0.05 * lcide) # SF report page 11 
#gross energy intake, constant is default value IPCC, IPCC guideline p 10.21

#ge <- (dmi*18.45)
ge<-gerc/numcow/365 #gerc is per cow
#we now compute the full energy requirement equations so we do not rely on ipcc shortcut average anymore. 
# when digestibility increases then entiric fermentation decreases
# with our base run feed basket, on digestibility is lower than ipcc standards

#emissions (per animal) per day 
rum_ch4_day <- (ge*(ym/100))/55.65
# IPCC default value IPCC guideline 10.31
rum_ch4_year <- rum_ch4_day*365
#computation of the CO2 equivalent IPCC guideline, 100 years

rum_co2e_yeartot <- rum_ch4_year*25*numcow

# adding goat and sheep with tier 1 
rum_co2e_yeartot <-rum_co2e_yeartot + 5*num_sg



#spatially allocation of the impact

sumlivdist<-cellStats(livdist,stat='sum',na.rm=TRUE)
fun<-function(x){x*rum_co2e_yeartot/sumlivdist}
rum_co2e_map<-overlay(livdist,fun=fun)


###################################################emissions from manure management#################################

#volatile solids 
#ash = mineral content, this is an IPCC value (2006-) but can be customized 
#ge is  per cow
vs <- (ge*(1-lcide)+(0.04*ge))*((1-ash)/18.45) #IPCC
#0.04=urinary energy default
#vs <- ((dmi) * (1.04 - lcide))*(1-ash) #LEAP guidelines gives the same answer

#the percentage comuptation are now it in the feedbasket 

# differentiate between pasture = grazing land, solid storage, slurry and daily spread= cropland, other =uniform)

fun <-function (a,b,c,d,f,g,h,i ) {vs*365*B0*0.67 * ((a/100*mms_lagoon_perc/100)+
                                     (b/100*mms_liquidslurry_perc/100)+
                                     (c/100*mms_solidstorage_perc/100)+
                                     (d/100*mms_drylot_perc/100)+
                                     (f/100*mms_dailyspread_perc/100)+
                                     (g/100*mms_digester_perc/100)+
                                     (h/100*mms_fuel_perc/100)+
                                     (i/100*mms_other_perc/100))}

efc_d <- overlay (mms_lagoon_mcf, mms_liquidslurry_mcf, mms_solidstorage_mcf ,mms_drylot_mcf,mms_dailyspread_mcf,mms_digester_mcf,mms_burned_mcf,mms_other_mcf, fun=fun )


### for other cattle 


# differentiate between pasture = grazing land, solid storage, slurry and daily spread= cropland, other =uniform)
# current modeling assumes all uniform. 

fun <-function (a,b,c,d,f,g,h,i ) {vs*365*B02*0.67 * ((a/100*mms_lagoon_perc2/100)+
                                     (b/100*mms_liquidslurry_perc2/100)+
                                     (c/100*mms_solidstorage_perc2/100)+
                                     (d/100*mms_drylot_perc2/100)+
                                     (f/100*mms_dailyspread_perc2/100)+
                                     (g/100*mms_digester_perc2/100)+
                                     (h/100*mms_fuel_perc2/100)+
                                     (i/100*mms_other_perc2/100))}

efc_c <- overlay (mms_lagoon_mcf, mms_liquidslurry_mcf, mms_solidstorage_mcf ,mms_drylot_mcf,mms_dailyspread_mcf,mms_digester_mcf,mms_burned_mcf,mms_other_mcf, fun=fun )



###add here monogastic/sheep and goat tier one 
fun1<-function(a,b,c,d,e,f){(a*(numcow_d)+b*(numcow_f+numcow_da)+e*num_ch+f*num_pg)/(ncellcrop+ncellrice)*(c+d)}
manure_CH4_year_herd_c<-overlay(efc_d,efc_c,cropland, riceland,ef_ch,ef_pg, fun=fun1)
#CH4 , pig, chicken assigned to cropland







#we assume that the proportion of cows on pastures is equivalent of the manure on pastures, so here we have all the others, so 1-percent pasture


#plot(manure_CH4_year_herd_c)

#CH4 estimates
fun <-function (e ) {(vs*365*B0*0.67 *(e/100*mms_pasture_perc/100))*(numcow_d)}
efg_d <- overlay (mms_pasture_mcf, fun=fun )

fun <-function (e ) {(vs*365*B02*0.67 *(e/100*mms_pasture_perc2/100))*(numcow_f+numcow_da)}
efg_c <- overlay (mms_pasture_mcf, fun=fun )


# add all goats here 
fun<- function(a,b,c,e){(a+b+(e*num_sg))/ncellgraz*c}
manure_CH4_year_herd_g <- overlay (efg_c,efg_d,grazland,ef_sg, fun=fun )

#total
fun=function(x,y) {x+y}
manure_CH4_year_herd_tot <-overlay(manure_CH4_year_herd_g, manure_CH4_year_herd_c, fun=fun)
#plot(manure_CH4_year_herd_tot)


#N2O emissions

NrateLook <- read.csv('1-input/parameter/MMS_Nrate.csv')
MMS_N_EF <- read.csv('1-input/parameter/MMSn2oEF.csv')
MMS_N_EFmean <- mean(MMS_N_EF$EF, na.rm=TRUE)

#for dairy
Nrate <- NrateLook$ExcretionRate[NrateLook$Species==species & NrateLook$Region==region]
Nrate2 <- NrateLook$ExcretionRate[NrateLook$Species==species3 & NrateLook$Region==region]
Nrate3<-NrateLook$ExcretionRate[NrateLook$Species=='Swine' & NrateLook$Region==region]
Nrate4<-NrateLook$ExcretionRate[NrateLook$Species=='Goats' & NrateLook$Region==region]
Nrate5<-NrateLook$ExcretionRate[NrateLook$Species=='Sheep' & NrateLook$Region==region]

Nex <- Nrate*(lwis*numcow_d)*365 #for all dairy animals 
Nex2 <- Nrate2*((lwsis*numcow_f+lwda*numcow_da))*365 # for all other cattle
Nex3 <- Nrate3*((lwpg*num_pg))*365 # for all other cattle
Nex4 <- (Nrate4+Nrate5)/2*(lwsg*num_sg)*365 # for all other cattle



#Direct
#dairy
N2O_d_year_herd_d <- sum(Nex*mms_lagoon_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="UncoveredAnerobic"] , 
                         Nex*mms_liquidslurry_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="Liquid"],
                         Nex*mms_solidstorage_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="SolidStorage"],
                         Nex*mms_drylot_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="DryLot"],
                         Nex*mms_dailyspread_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="DailySpread"],
                         Nex*mms_digester_perc/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="AnerobicDigester"],
                         Nex*mms_other_perc/100 * MMS_N_EFmean)*(44/28) 
#44/28 = conversion of (N2O-N)(mm) emissions to N2O(mm) emissions

#other cattle 
N2O_d_year_herd_c <- sum(Nex2*mms_lagoon_perc2/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="UncoveredAnerobic"] , 
                         Nex2*mms_liquidslurry_perc2/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="Liquid"],
                         Nex2*mms_solidstorage_perc2/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="SolidStorage"],
                         Nex2*mms_drylot_perc2/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="DryLot"],
                         Nex2*mms_dailyspread_perc2/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="DailySpread"],
                         Nex2*mms_digester_perc2/100 * MMS_N_EF$EF[MMS_N_EF$MMS=="AnerobicDigester"],
                         Nex2*mms_other_perc2/100 * MMS_N_EFmean)*(44/28) 

N2O_d_year_herd<-N2O_d_year_herd_d+N2O_d_year_herd_c+ (Nex3+Nex4)*(44/28) 


# Pasture (grazed and deposited) calculated is now calculted here 

#Direct and indirect N2O on pastures
Ndeposit <- (Nex+Nex2)*(mms_pasture_perc/100)+Nex4 #Equation 11.5 IPCC

EF_CPP <- 0.02 #Default emission factor for cattle, pigs and poultry
#EF_SO <- 0.01 #Default emission factor for sheep and others
N20deposit_direct <- (Ndeposit * EF_CPP)*(44/28) #Equation 11.1 IPCC

FracGASM <- 0.2 #Default value for fraction of applied organic N that volatilises as NH3 and NOx
EF_indirectN2Opasture <- 0.01 # Default emission factor
N2Odeposit_indirect <- (Ndeposit * FracGASM * EF_indirectN2Opasture)*(44/28) #Equation 11.9 IPCC


N_loss <- read.csv('1-input/parameter/MMS_Nloss.csv')
N_loss$N_loss<-N_loss$N_loss/100
N_loss <-N_loss[N_loss$Species==species,]
N_lossmean <- mean(N_loss$N_loss[N_loss$Species==species], na.rm=TRUE)

#Indirect (from ammonia)
N_vol_herd_d <- sum(Nex*mms_lagoon_perc/100 * N_loss$N_loss[ N_loss$MMS=="UncoveredAnerobic"] , 
             Nex*mms_liquidslurry_perc/100 * N_loss$N_loss[ N_loss$MMS=="Liquid"],
             Nex*mms_solidstorage_perc/100 * N_loss$N_loss[N_loss$MMS=="SolidStorage"],
             Nex*mms_drylot_perc/100 * N_loss$N_loss[N_loss$MMS=="DryLot"],
             Nex*mms_dailyspread_perc/100 * N_loss$N_loss[N_loss$MMS=="DailySpread"],
             Nex*mms_other_perc/100 * N_lossmean)
N_loss <- read.csv('1-input/parameter/MMS_Nloss.csv')
N_loss$N_loss<-N_loss$N_loss/100
N_loss <-N_loss[N_loss$Species==species4,]
N_lossmean2 <- mean(N_loss$N_loss[N_loss$Species==species4], na.rm=TRUE)
N_vol_herd_c <- sum(Nex2*mms_lagoon_perc2/100 * N_loss$N_loss[ N_loss$MMS=="UncoveredAnerobic"] , 
             Nex2*mms_liquidslurry_perc2/100 * N_loss$N_loss[ N_loss$MMS=="Liquid"],
             Nex2*mms_solidstorage_perc2/100 * N_loss$N_loss[N_loss$MMS=="SolidStorage"],
             Nex2*mms_drylot_perc2/100 * N_loss$N_loss[ N_loss$MMS=="DryLot"],
             Nex2*mms_dailyspread_perc2/100 * N_loss$N_loss[N_loss$MMS=="DailySpread"],
             Nex2*mms_other_perc2/100 * N_lossmean2)

N2O_i_year_herd <-((N_vol_herd_c+N_vol_herd_d) *0.01)*(44/28)
# now add here tier 1 for ggoat sheep, pig chicken
N2O_i_year_herd <-((N_vol_herd_c+N_vol_herd_d)+(Nex3*31.5+(Nex4)/2*18.5)) *0.01*(44/28)

#0.01 is the default EF for emission factor for N2O emissions from atmospheric deposition of nitrogen on soils and water surfaces
#44/28 density of N2O 
#assigning to cropland only

fun<-function(x,y){((N2O_i_year_herd+N2O_d_year_herd)/ncellcrop)*(x+y)} 
N2O_t_year_herd_map_c <- overlay(cropland, riceland,fun=fun )
fun<-function(x){((N20deposit_direct+N2Odeposit_indirect)/ncellgraz)*x} 
N2O_t_year_herd_map_g <- overlay(grazland,fun=fun )
fun<-function(x,y){x+y}
N2O_t_year_herd_map <- overlay(N2O_t_year_herd_map_c,N2O_t_year_herd_map_g,fun=fun )


#CO2 equ from manure 
fun <- function(x,y){(x*298)+(y*23)}
#298 conversion factor to Co2 equ for 100 year horizon (gwp) for N2O, and 23 for CH4
co2e_manure<-overlay(N2O_t_year_herd_map,manure_CH4_year_herd_tot,fun=fun)

##############################emissions from feed and fodder production  ########################

#ghg soil stock change
fun<-function(a,b){a*b}
Flu_c<-overlay(Flu_c,cropland, fun=fun) 

ssc<-read.csv('1-input/parameter/ghg_soil_stock_change.csv')

ssc$avgtil<-perc_til/100 * ssc$Full+perc_redtil/100 * ssc$Reduced + perc_notil/100* ssc$No.till
fun=function(a,b,c,d,e,f,g,h) {
  ((a*ssc[6,col])+(b*ssc[5,col])+(c*ssc[4,col])+(d*ssc[3,col])+(e*ssc[10,col])+(f*ssc[9,col])+(g*ssc[8,col])+(h*ssc[7,col]))
}
col=16
Fmg_c<-overlay(clim_wtmoist,clim_wtdry,clim_ctmoist,clim_ctdry,  clim_tr_mont,clim_tr_wet,clim_tr_moist,clim_tr_dry,fun=fun)
fun2<-function(x,y){x*y}
Fmg_c<-overlay(Fmg_c,cropland, fun=fun2) 
#plot(Fmg_c)

#create a layer from soil carbon stock change factor related to inputs (Fi)
ssc$input<-perc_inlow/100 * ssc$Low+perc_inmedium/100 * ssc$Medium + perc_inhighnoman/100* ssc$High.without.manure  + perc_inhighman/100*ssc$High.with.manure
col=17
Fi_c<-overlay(clim_wtmoist,clim_wtdry,clim_ctmoist,clim_ctdry,  clim_tr_mont,clim_tr_wet,clim_tr_moist,clim_tr_dry,fun=fun)
Fi_c<-overlay(Fi_c,cropland, fun=fun2) 
#plot(Fi_c)



#make a total Flu map, adjust here with the new land uses xxwhere necessary 

fun<-function(a,b){a*b}
Flu_rice<-overlay(Flu_rice,riceland, fun=fun) 
#fun3<-fun(x,y) {x+y}
#Flu_tot<-(Flu_c, Flu_rice, fun=fun3)
Flu_tot<-Flu_c +Flu_rice

fun4<-function(a,b,c,d){a*b*c*d}
SOCa<-overlay(soilref, Flu_tot, Fmg_c, Fi_c, fun=fun4)

fun<-function(x,y){x*y}
SOCr<-overlay(soilref, Flu_c,fun=fun)

#soil carbon balance on cropland
fun5=function(x,y){(x-y)*3.664*1/20}
scb<-overlay(SOCr,SOCa,fun=fun5)
scb<-overlay(scb,cropland, fun=fun)
#plot(scb)
#emissions from  land use change 
csluc<-raster(ext=cropland@extent, res=res(cropland),vals=0)
if (add>0 ){
   
gfrs<-read.csv('1-input/parameter/GlobalForestResourcesAssessment2010.csv')
gfrdom<-gfrs$Dead.organic.matter.forests[gfrs$Country==country] 
fun3<-function(x,y) {(x*y)*(3.664*1/20)*gfrdom}
rvc_forest<-overlay(forest_clim,lostforest, fun=fun3)
fun3<-function(x,y) {x*y*3.664*0.05}
rvc_gras<-overlay(graz_clim,lostgraz, fun=fun3)

fun=function(x,y){x+y}
csluc<-overlay(rvc_gras,rvc_forest,fun=fun )}




#############################################################################
# adding up the 4 indicators
fun=function(w,x,y,z){w+x+y+z}
COe<-overlay(rum_co2e_map,co2e_manure,scb,csluc,fun=fun)
#plot(COe)

#landscape level indicator
COe_l<-round(cellStats(COe,stat='sum'), 0)  # total CO2 producted 
co2e_manure_l<-round(cellStats(co2e_manure,stat='sum'),0) # total CO2 producted from manure 
co2cow<-round(COe_l/numcow,0)
co2milk<-round(COe_l/(milk),0)
co2meat<-round(COe_l/(meat), 0 )
rum_co2e_yeartot<-round(rum_co2e_yeartot,0)
Coe_ind<-data.frame(COe_l,co2e_manure_l,rum_co2e_yeartot,co2cow,co2milk,co2meat)


#indifference computation 
Coe_ind_diff<-Coe_ind-Coe_base
Coe_ind_perc<-round(Coe_ind_diff/Coe_base*100, digits = 1)
Coe_ind2<-rbind(Coe_ind,Coe_ind_diff,Coe_ind_perc)
names(Coe_ind2)<-c("CO2 emmissions", "CO2 from manure", "CO2 from interic fermetation","CO2 per cow", "CO2 per tonnes of milk", "CO2 per tonnes of meat" )
Coe_ind2<-data.frame(t(Coe_ind2))
colnames(Coe_ind2)<-c('result','difference','percent')

##steve edits
Coe_ind_val<-read.csv('1-input/parameter/ghg_val.csv')
Coe_ind2$evaluation <- ifelse(Coe_ind2$percent <= Coe_ind_val$peu, 'low', ifelse((Coe_ind2$percent >= Coe_ind_val$peu) & (Coe_ind2$percent <= Coe_ind_val$peu),'medium','high'))



Coe_map_diff<-COe-Coe_map


title1<-paste('GHG : CO2 equivalent', name, sep=" ")
title2<-paste('GHG : CO2 equ from manure only', name, sep=" ")
title3<-paste('GHG : CO2 difference from baseline', name, sep=" ")

col<-colorRampPalette(brewer.pal(9,'YlOrRd'))(100)
col2<-colorRampPalette(rev(brewer.pal(9,'Reds')))(100)

#pdf(paste(name, "ghg_pathway.pdf", sep='-')) 
par(mfrow=c(1,3),mar=c(1, 2.25, 1, 3))
plot(COe, legend.width=1, legend.shrink=0.45,col=col) 
plot(sarea, add=TRUE)
title(title1)

plot(co2e_manure, legend.width=1, legend.shrink=0.45,col=col)
plot(sarea, add=TRUE)
title(title2)

plot(Coe_map_diff, legend.width=1, legend.shrink=0.45,col=col2)
plot(sarea, add=TRUE)
title(title3)

# plot(NA, xlim=c(0,100), ylim=c(0,10), bty='n',
#      xaxt='n', yaxt='n', xlab='', ylab='')
#grid.table(Coe_ind2)










#####################################################################
#extract the maps for final user
setwd("4-output")

title1<-paste('Greenhouse gas pathway : CO2 equivalent', name, sep=" ")
title2<-paste('Greenhouse gas pathway : CO2 equ from manure only', name, sep=" ")
title3<-paste('Greenhouse gas pathway : CO2 difference from baseline', name, sep=" ")

col<-colorRampPalette(brewer.pal(9,'YlOrRd'))(100)
col2<-colorRampPalette(rev(brewer.pal(9,'Reds')))(100)

pdf(paste(name, "ghg_pathway.pdf", sep='-')) 
plot(COe, legend.width=1, legend.shrink=0.75,col=col) 
plot(sarea, add=TRUE)
title(title1)
plot(co2e_manure, legend.width=1, legend.shrink=0.75,col=col)
plot(sarea, add=TRUE)
title(title2)
plot(co2e_manure, legend.width=1, legend.shrink=0.75,col=col)
plot(sarea, add=TRUE)
title(title2)
plot(Coe_map_diff, legend.width=1, legend.shrink=0.75,col=col2)
plot(sarea, add=TRUE)
title(title3)
plot(NA, xlim=c(0,100), ylim=c(0,10), bty='n', 
     xaxt='n', yaxt='n', xlab='', ylab='')
grid.table(Coe_ind2)

dev.off()


writeRaster(COe,paste(name,"ghg_map.tif", sep="-"), overwrite=TRUE)
#writeRaster(co2e_manure,paste(name,"ghg_co2e_manure.tif", sep="-"),overwrite=TRUE)
write.csv(Coe_ind,paste(name,"ghg_ind.csv", sep="-"),row.names = F)


