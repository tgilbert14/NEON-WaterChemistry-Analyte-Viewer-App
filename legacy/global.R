#---global--------------------------------------------------  
# this section of script will load in libraries needed and water chemistry file
# containing all site water chem data for mlr model- may change to just pull
# form each site and not load in file... 
#install.packages("shinydashboardPlus")

## Loading libraries
library(neonUtilities)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(tidyverse)       
library(DT)
library(shinycssloaders)
library(RColorBrewer)
library(mlr)    
library(data.table) 
library(gganimate)
library(gifski)
library(shinyalert)
library(shinyBS)
library(plotly)
library(forecast)
library(dplyr)
library(lubridate)
library(broom)
library(daymetr)
library(fresh)

## 'new' data for mlr model
data1<- read_csv("data1.csv")
## data from 2016 to end of 2020 for mlr model... need to update?? Large file!
fit_data<- read_csv("mlr_chem_data.csv")
## fit_data<- read_csv("all_water_chemData.csv")
## surface water temp data from 2016-2021 for all site
#temp_data<- ("")

neon_sites<- c("SYCA - Sycamore Creek, AZ [Desert Southwest]" = 'SYCA', "ARIK - Arikaree, CO [Central Plains]"="ARIK",
               "BARC - Barco Lake, FL [Southeast]"="BARC","BIGC - Upper Big Creek, CA [Pacific Southwest]"='BICG',
               "BLDE - Blacktail Deer Creek, WY [Northern Rockies]"="BLDE","BLUE - Blue River, OK [Southern Plains]"="BLUE",
               "BLWA - Black Warrior River, AL [Ozarks Complex]"="BLWA","CARI - Caribou Creek, AK [Taiga]"="CARI",
               "COMO - Como Creek, CO [Southern Rockies and Coloado Plateau]"="COMO","CRAM - Cramton Lake, WI [Great Lakes]"="CRAM",
               "CUPE - Rio Cupeyes, PR [Atlantic Neotropical]"="CUPE","FLNT - Flint River, GA [Southeast]"="FLNT",
               "GUIL - Rio Guilarte, PR [Atlantic Neotropical]"="GUIL","HOPB - Hop Brook, MA [Northeast]"="HOPB",
               "KING - Kings Creek, KA [Prairie Peninsula]"="KING","LECO - LeConte Creek, TN [Appalachians and Cumberland Plateau]"="LECO",
               "LEWI - Lewis Run, VA [Mid-Atlantic]"="LEWI","LIRO - Little Rock Lake, WI [Great Lakes]"="LIRO",
               "MART - Martha Creek, WA [Pacific Northwest]"="MART","MAYF - Mayfield Creek, AL [Ozarks Complex]"="MAYF",
               "MCDI - McDiffett Creek, KA [Prairie Peninsula]"="MCDI","MCRA - McRae Creek, OR [Pacific Northwest]"="MCRA","OKSR - Oksrukuyik, AK [Tundra]"="OKSR",
               "POSE - Posey Creek, VA [Mid-Atlantic]"="POSE","PRIN - Pringle Creek, TX [Southern Plains]"="PRIN",
               "PRLA - Prairie Lake, ND [Northern Plains]"="PRLA","PRPO - Prairie Pothole, ND [Northern Plains]"="PRPO",
               "REDB - Red Butte Creek, UT [Great Baisn]"="REDB","SUGG - Suggs Lake, FL [Southeast]"="SUGG",
               "TECR - Teakettle 2 Creek, CA [Pacific Southwest]"="TECR","TOMB - Tombigbee River, AL [Ozarks Complex]"="TOMB",
               "TOOK - Toolik Lake, AK [Tundra]"="TOOK","WALK - Walker Branch, TN [Appalachians and Cumberland Plateau]"="WALK",
               "WLOU - West St Louis Creek, CO [Rockies and Colorado Plateau]"="WLOU")


analyte_list<- c(
  'Acid Neutralizing Capacity(ANC)'='ANC',
  'Bicarbonate Concentration(Br)'='Br',
  'Calcium Concentration(Ca)'='Ca',
  'Chlorine Concentration(Cl)'='Cl',
  'Carbonate Concentration(CO3)'='CO3',
  #'Conductivity'='specificConductance', #Not in mlr data sheet- got rid of
  'Dissolved Inorganic Carbon(DIC)'='DIC',
  'Dissolved Organic Carbon(DOC)'='DOC',
  'Fluorine Concentration(F)'='F',
  'Iron Concentration(Fe)'='Fe',
  'Bicarbonate Concentration(HCO3)'='HCO3',
  'Potassium Concentration(K)'='K',
  'Magnesium Concentration(Mg)'='Mg',
  'Manganese Concentration(Mn)'='Mn',
  'Sodium Concentration(Na)'='Na',
  'Ammonium Concentration(NH4)'='NH4 - N',
  'Nitrogen Dioxide(NO2-N)'='NO2 - N',
  'Nitrate(NO3+NO2-N)'='NO3+NO2 - N',
  'Orthophosphate Concentration(Ortho-P)'='Ortho - P',
  'pH(pH)'='pH','Silica Concentration(Si)'='Si',
  'Sulfate Concentrations(SO4)'='SO4',
  'Total Dissolved Nitrogen(TDN)'='TDN',
  'Total Dissolved Phosphorus(TDP)'='TDP',
  'Total Dissolved Solids(TDS)'='TDS',
  'Total Nitrogen(TN)'='TN',
  'Total Organic Carbon(TOC)'='TOC',
  'Total Phosphorus(TP)'='TP',
  'Total Particulate Carbon(TPC)'='TPC',
  'Total Particulate Nitrogen(TPN)'='TPN',
  'Total Suspended Solids(TSS)'='TSS',
  'Dry Mass Suspended Solids(TSS-D)'='TSS - Dry Mass',
  'UV Absorbance (250 nm)',
  'UV Absorbance (280 nm)',
  'Surface Water Temperature(Field)'='waterTemp',
  'Specific Conductance(Field)'='specificConductanceField',
  'Dissolved Oxygen(Field)'='dissolvedOxygenField'#,
  #'Dissolved Oxygen*(Sensor)'='dissolvedOxygen',
  #'Chlorophyll*(Sensor)'='chlorophyll',
  #'Fluorescent dissolved organic matter(fDOM)*(Sensor)'='fDOM',
  #'Turbidity*(Sensor)'='turbidity'
)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "aqua"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "DarkCyan",
    dark_hover_bg = "aquamarine",
    dark_color = "aqua"
  ),
  adminlte_global(
    content_bg = "lightblue",
    box_bg = "cyan", 
    info_box_bg = "#D8DEE9"
  )
)

## testing QC'd file for mlr model...
## fit_data<- read_csv("all_water_chemData_QC.csv")

## neonUtilities package won't let Shinyapp be published for shiny.io
## library(neonUtilities)  ##Loading Libraries##


## for publishing application need to run for certain packages...
# library(BiocManager)
# options(repos = BiocManager::repositories())
# options('repos')
