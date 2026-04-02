$ONTEXT
This code is used to create .csv files of the assumptions included in the IMPACT model. The IMPACT model version used
to inform these outputs is v4.1.4. The SSP2-NoCC-NoCC scenario is used.
Author: Faaiqa Hartley
Date: April 2026
The following folders in DriverAssumptions are pending updates
- Nutrition_Hunger
- Nutrition_Nutrients
- Shapefiles
- Special_Studies
- WaterImpacts_Yield
$OFFTEXT

$setglobal gdx  SSP2-NoCC-NoCC.gdx

*Generation of README*----------------------------------------------------------------------------------------------------------------
File results / ..\README.txt /;
put results;
put "International Model for Policy Analysis of Agricultural Commodities and Trade (IMPACT)"/;
put " "/;
put "Repository for IFPRI's IMPACT model driver assumptions"/;
put " "/;
put "April 2026"/;
put " "/;
put "See https://www.ifpri.org/project/ifpri-impact-model/ for further information."/;
put "The model documentation at https://hdl.handle.net/10568/148953 will hopefully answer many questions."/;
put " "/;
put "Please use this before contacting IFPRI-Impact-Model@cgiar.org with any issues."/;
put " "/;
put "We welcome suggestions with documentation for improvements."/;
putclose;

*Generation of .csv files--------------------------------------------------------------------------------------------------------------
*BaseYearData
$call gdxdump %gdx% symb=BaseCTY format=csv Output=..\DriverAssumptions\BaseYearData\BaseCTY_Crops+Livestock.csv
$call gdxdump %gdx% symb=BaseCTY_Fish format=csv Output=..\DriverAssumptions\BaseYearData\BaseCTY_Fish.csv
$call gdxdump %gdx% symb=BaseFPU format=csv Output=..\DriverAssumptions\BaseYearData\BaseFPU_Crops.csv
$call gdxdump %gdx% symb=BaseFPUL format=csv Output=..\DriverAssumptions\BaseYearData\BaseFPU_Livestock.csv

*ClimateImpacts_Yield
$call gdxdump %gdx% symb=RdInCCdelta format=csv Output=..\DriverAssumptions\ClimateImpacts_Yield\CCDelta.csv

*Elasticities_Demand
$call gdxdump %gdx% symb=BFDmdPElas format=csv Output=..\DriverAssumptions\Elasticities_Demand\BFDmdPElas.csv
$call gdxdump %gdx% symb=ExogFDDmdElasH format=csv Output=..\DriverAssumptions\Elasticities_Demand\ExogFDDmdElasH.csv
$call gdxdump %gdx% symb=ExogIncDmdElasH format=csv Output=..\DriverAssumptions\Elasticities_Demand\ExogIncDmdElasH.csv
$call gdxdump %gdx% symb=FdElasHX0 format=csv Output=..\DriverAssumptions\Elasticities_Demand\FdElasH.csv
$call gdxdump %gdx% symb=FeedElas format=csv Output=..\DriverAssumptions\Elasticities_Supply\FeedElas.csv
$call gdxdump %gdx% symb=IncDmdElasHX0 format=csv Output=..\DriverAssumptions\Elasticities_Demand\IncDmdElasH.csv
$call gdxdump %gdx% symb=OthDmdIElas format=csv Output=..\DriverAssumptions\Elasticities_Demand\OthDmdIElas.csv
$call gdxdump %gdx% symb=OthDmdPElas format=csv Output=..\DriverAssumptions\Elasticities_Demand\OthDmdPElas.csv

*Elasticities_Supply
$call gdxdump %gdx% symb=AnmlFeedElas format=csv Output=..\DriverAssumptions\Elasticities_Supply\AnmlFeedElas.csv
$call gdxdump %gdx% symb=AreaElas format=csv Output=..\DriverAssumptions\Elasticities_Supply\AreaElas.csv
$call gdxdump %gdx% symb=AnmlElas format=csv Output=..\DriverAssumptions\Elasticities_Supply\LvstElas.csv
$call gdxdump %gdx% symb=YldElas format=csv Output=..\DriverAssumptions\Elasticities_Supply\YldElas.csv
$call gdxdump %gdx% symb=YldElasWF format=csv Output=..\DriverAssumptions\Elasticities_Supply\YldElasWF.csv
$call gdxdump %gdx% symb=QSElas format=csv Output=..\DriverAssumptions\Elasticities_Supply\QSElas.csv
$call gdxdump %gdx% symb=QSElasC format=csv Output=..\DriverAssumptions\Elasticities_Supply\QSElasC.csv
$call gdxdump %gdx% symb=WFElas format=csv Output=..\DriverAssumptions\Elasticities_Supply\WFElas.csv

* Nutrition_Children
$call gdxdump %gdx% symb=pop05 format=csv Output=..\DriverAssumptions\Nutrition_Children\pop05.csv

* Nutrition_Hunger
*To be added later if needed

* Nutrition_Nutrients
*To be added later if needed

*ProductionGrowth_Area
$call gdxdump %gdx% symb=areagr format=csv Output=..\DriverAssumptions\ProductionGrowth_Area\areagr.csv
$call gdxdump %gdx% symb=anmlnumgrx0 format=csv Output=..\DriverAssumptions\ProductionGrowth_Area\anmlgr.csv
$call gdxdump %gdx% symb=lndgr format=csv Output=..\DriverAssumptions\ProductionGrowth_Area\lndgr.csv

*ProductionGrowth_Yield
$call gdxdump %gdx% symb=yldgr format=csv Output=..\DriverAssumptions\ProductionGrowth_Yield\yldgr.csv
$call gdxdump %gdx% symb=anmlyldgrx0 format=csv Output=..\DriverAssumptions\ProductionGrowth_Yield\anmlyieldgr.csv
$call gdxdump %gdx% symb=yldbumpx0 format=csv Output=..\DriverAssumptions\ProductionGrowth_Yield\yldgrbump.csv

*Socioeconomic
$call gdxdump %gdx% symb=RdInGDPSSP format=csv Output=..\DriverAssumptions\Socioeconomic\gdp.csv
$call gdxdump %gdx% symb=GDPSSPgr format=csv Output=..\DriverAssumptions\Socioeconomic\gdpIMPgr.csv
$call gdxdump %gdx% symb=RdInPOPSSP format=csv Output=..\DriverAssumptions\Socioeconomic\pop.csv
$call gdxdump %gdx% symb=POPSSPgr format=csv Output=..\DriverAssumptions\Socioeconomic\popIMPgr.csv

*Trade_Parameters
$call gdxdump %gdx% symb=RdInCSE format=csv Output=..\DriverAssumptions\Trade_Parameters\CSE.csv
$call gdxdump %gdx% symb=PW00 format=csv Output=..\DriverAssumptions\Trade_Parameters\IMPACT_PW.csv
$call gdxdump %gdx% symb=RdInMM format=csv Output=..\DriverAssumptions\Trade_Parameters\MM.csv
$call gdxdump %gdx% symb=RdInMME format=csv Output=..\DriverAssumptions\Trade_Parameters\MME_1.csv
$call gdxdump %gdx% symb=RdInMMJ format=csv Output=..\DriverAssumptions\Trade_Parameters\MMJ_1.csv
$call gdxdump %gdx% symb=RdInMMM format=csv Output=..\DriverAssumptions\Trade_Parameters\MMM_1.csv
$call gdxdump %gdx% symb=RdInPSE format=csv Output=..\DriverAssumptions\Trade_Parameters\PSE.csv
$call gdxdump %gdx% symb=TE format=csv Output=..\DriverAssumptions\Trade_Parameters\TE.csv
$call gdxdump %gdx% symb=TM format=csv Output=..\DriverAssumptions\Trade_Parameters\TM.csv

*Demand
$call gdxdump %gdx% symb=WasteInt format=csv Output=..\DriverAssumptions\Demand\Waste.csv
$call gdxdump %gdx% symb=RdInFeedReq format=csv Output=..\DriverAssumptions\Demand\FeedRequirement.csv
$call gdxdump %gdx% symb=QBFInt2 format=csv Output=..\DriverAssumptions\Demand\BiofuelFeedstockDemandGrowth.csv
$call gdxdump %gdx% symb=QOthrInt2 format=csv Output=..\DriverAssumptions\Demand\OtherDemandGrowth.csv

*Supply
*To be added later if needed

