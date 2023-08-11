# ## place to mess with scripts
# library(neonUtilities)
# library(tidyverse)
# 
# # #testing code
# select.analyte<- 'pH'
# select.analyteB<- 'Br'
# select.analyte<- 'waterTemp'
# select.analyte<- 'dissolvedOxygen'
# 
# select.analyteB<- 'dissolvedOxygen'
# 
# ## -
# dpid <- "DP1.20093.001"   # Surface Water Chemistry DPID
# data.swc<- loadByProduct(dpID = dpid, site = 'SYCA', startdate = "2018-01",
#                           enddate = "2019-05", check.size = F)
# External.data<- data.swc$swc_externalLabDataByAnalyte
# 
# analyte_data<- External.data %>%
#   filter(analyte == select.analyte) %>%
#   select(analyte, analyteConcentration, analyteUnits, collectDate) %>%
#   arrange(collectDate)
# analyte_data$collectDate<- substr(analyte_data$collectDate, 1, 10)
# 
# 
# ## -
# dpid <- "DP1.20288.001"   # Water Quality DPID
# data.sen<- loadByProduct(dpID = dpid, site = 'SYCA', startdate = "2018-01",
#                          enddate = "2019-05", check.size = F)
# sensor.sen<- data.sen$waq_instantaneous
# 
# if (select.analyteB == 'dissolvedOxygen') {
#   #dissolved oxygen*
#   dissolved_o <- sensor.sen %>%
#     filter(!is.na(dissolvedOxygen)) %>%
#     select(startDateTime,dissolvedOxygen,dissolvedOxygenExpUncert) %>%
#     #only want to keep data within 95% certainty
#     mutate(confidenceL = (dissolvedOxygenExpUncert/dissolvedOxygen)*100) %>%
#     filter(confidenceL <= 5)
#   dissolved_o$startDateTime<- substr(dissolved_o$startDateTime, 1, 10)
#   ## organizing data
#   analyte_dataB<- dissolved_o %>%
#     group_by(startDateTime) %>%
#     summarise(mean(dissolvedOxygen)) %>%
#     mutate(collectDate = startDateTime) %>%
#     mutate(analyteConcentration = `mean(dissolvedOxygen)`) %>%
#     select(-startDateTime, -`mean(dissolvedOxygen)`) %>%
#     mutate(analyte = 'Dissolved Oxygen') %>%
#     mutate(analyteUnits = 'milligramsPerLiter') %>%
#     arrange(collectDate)
# 
#   ## only include analyte_data that has same dates to join
#   analyte_dates<- analyte_data %>%
#     select(collectDate)
#   analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
# }
# 
# 
# #specific conductance*
# specific_c <- sensor.sen %>%
#   filter(!is.na(specificConductance)) %>%
#   select(startDateTime,specificConductance,specificConductanceExpUncert) %>%
#   #only want to keep data within 95% certainty
#   mutate(confidenceL = (specificConductanceExpUncert/specificConductance)*100) %>%
#   filter(confidenceL <= 5)
# specific_c$startDateTime<- substr(specific_c$startDateTime, 1, 10)
# ## organizing data
# analyte_dataB<- specific_c %>%
#   group_by(startDateTime) %>%
#   summarise(mean(specificConductance)) %>%
#   mutate(collectDate = startDateTime) %>%
#   mutate(analyteConcentration = `mean(specificConductance)`) %>%
#   select(-startDateTime, -`mean(specificConductance)`) %>%
#   mutate(analyte = 'Specific Conductance') %>%
#   mutate(analyteUnits = 'microsiemensPerCentimeter') %>%
#   arrange(collectDate)
# ## only include analyte_data that has same dates to join
# analyte_dates<- analyte_data %>%
#   select(collectDate)
# analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
# 
# 
# #pH*
# pH_data <- sensor.sen %>%
#   filter(!is.na(pH)) %>%
#   select(startDateTime,pH,pHExpUncert) %>%
#   #only want to keep data within 95% certainty
#   mutate(confidenceL = (pHExpUncert/pH)*100) %>%
#   filter(confidenceL <= 5)
# pH_data$startDateTime<- substr(pH_data$startDateTime, 1, 10)
# ## organizing data
# analyte_dataB<- pH_data %>%
#   group_by(startDateTime) %>%
#   summarise(mean(pH)) %>%
#   mutate(collectDate = startDateTime) %>%
#   mutate(analyteConcentration = `mean(pH)`) %>%
#   select(-startDateTime, -`mean(pH)`) %>%
#   mutate(analyte = 'pH') %>%
#   mutate(analyteUnits = 'pH') %>%
#   arrange(collectDate)
# ## only include analyte_data that has same dates to join
# analyte_dates<- analyte_data %>%
#   select(collectDate)
# analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
# 
# ##chlorophyll*
# chlor <- sensor.sen %>%
#   filter(!is.na(chlorophyll)) %>%
#   select(startDateTime,chlorophyll,chlorophyllExpUncert) %>%
#   #only want to keep data within 95% certainty
#   mutate(confidenceL = (chlorophyllExpUncert/chlorophyll)*100) %>%
#   filter(confidenceL <= 5)
# chlor$startDateTime<- substr(chlor$startDateTime, 1, 10)
# ## organizing data
# analyte_dataB<- chlor %>%
#   group_by(startDateTime) %>%
#   summarise(mean(chlorophyll)) %>%
#   mutate(collectDate = startDateTime) %>%
#   mutate(analyteConcentration = `mean(chlorophyll)`) %>%
#   select(-startDateTime, -`mean(chlorophyll)`) %>%
#   mutate(analyte = 'chlorophyll') %>%
#   mutate(analyteUnits = 'microgramsPerLiter') %>%
#   arrange(collectDate)
# ## only include analyte_data that has same dates to join
# analyte_dates<- analyte_data %>%
#   select(collectDate)
# analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
# 
# ##turbidity*
# turb <- sensor.sen %>%
#   filter(!is.na(turbidity)) %>%
#   select(startDateTime,turbidity,turbidityExpUncert) %>%
#   #only want to keep data within 95% certainty
#   mutate(confidenceL = (turbidityExpUncert/turbidity)*100) %>%
#   filter(confidenceL <= 5)
# turb$startDateTime<- substr(turb$startDateTime, 1, 10)
# ## organizing data
# analyte_dataB<- turb %>%
#   group_by(startDateTime) %>%
#   summarise(mean(turbidity)) %>%
#   mutate(collectDate = startDateTime) %>%
#   mutate(analyteConcentration = `mean(turbidity)`) %>%
#   select(-startDateTime, -`mean(turbidity)`) %>%
#   mutate(analyte = 'turbidity') %>%
#   mutate(analyteUnits = 'formazinNephelometricUnit') %>%
#   arrange(collectDate)
# ## only include analyte_data that has same dates to join
# analyte_dates<- analyte_data %>%
#   select(collectDate)
# analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
# 
# #fDOM*
# fDOM_data <- sensor.sen %>%
#   filter(!is.na(fDOM)) %>%
#   select(startDateTime,fDOM,fDOMExpUncert) %>%
#   #only want to keep data within 95% certainty
#   mutate(confidenceL = (fDOMExpUncert/fDOM)*100) %>%
#   filter(confidenceL <= 5)
# fDOM_data$startDateTime<- substr(fDOM_data$startDateTime, 1, 10)
# ## organizing data
# analyte_dataB<- fDOM_data %>%
#   group_by(startDateTime) %>%
#   summarise(mean(fDOM)) %>%
#   mutate(collectDate = startDateTime) %>%
#   mutate(analyteConcentration = `mean(fDOM)`) %>%
#   select(-startDateTime, -`mean(fDOM)`) %>%
#   mutate(analyte = 'fDOM') %>%
#   mutate(analyteUnits = 'quinineSulfateUnit') %>%
#   arrange(collectDate)
# ## only include analyte_data that has same dates to join
# analyte_dates<- analyte_data %>%
#   select(collectDate)
# analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
# 
# 
# 
# 
# 
# 
# 
# 
# ## -
# ## change analyte_data to only have dates and bind!
# analyte_dates<- analyte_data %>%
#   select(collectDate)
# ## bind data so only graphs data that intersects!! TEST PLEASE
# analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
# 
# 
# 
# 
# 
# 
# # Shiny app example --
# # runExample <- function() {
# #   appDir <- system.file("examples", "demo", package = "shinyalert")
# #   shiny::runApp(appDir, display.mode = "normal")
# # }
# # runExample()
