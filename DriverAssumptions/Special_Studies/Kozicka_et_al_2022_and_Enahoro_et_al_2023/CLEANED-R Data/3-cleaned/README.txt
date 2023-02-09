File description for CLEANED IMPACT

some logics in the file names
0 = user input
1 = core cleaned code
2 = processing code required by cleaned code 
3 = post-cleaned processing

impact = adjustement for linking IMPACT and CLEANED 

numbers in the name refer to the version number when mistakes are found, so always use the file with the highest number

File description 

0-user definition_preset : the file with user definition that is able to read pre-sets. Presets are define in 0-user definition_preset\preset.csv

1- core cleaned
if ending with F = French tables for the french verions
1- biodiv = biodiversity impact
1- ghg = greenhouse gaz emission
1- water = water impact 
1- soil = soil balance


2-processing cleaned (common to all 4 impacts) 

2-load data = loads data in active memory 
2-feedbasket = poductivity computation 


CLEANED_INTERFACE_WITH_PRESETS_FOR_BURKINA2017 = the interface, open and press run app to open the interface
note you will need to have a recent version of R and install the shiny and shinydashboard package. 

If you need to update you R, cut and copy all your packages to the new package folder and in r studio press up-date all. 