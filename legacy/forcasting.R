## forcasting--

library(neonUtilities)
library(tidyverse)

dpid <- "DP1.20093.001"
site.pick <- 'POSE'
start_d <- '2017-01'
end_d <- '2019-12'
data.swc<- loadByProduct(dpid, site = site.pick, startdate = start_d,
                         enddate = end_d, check.size = F)
# SWC data
External.data<- data.swc$swc_externalLabDataByAnalyte

# Adding parent data - hand measurements at collection- 1/21/21
parent_data<- data.swc$swc_fieldSuperParent
pdata<- parent_data %>%
  select(collectDate, waterTemp, dissolvedOxygen, specificConductance) %>%
  mutate(dissolvedOxygenField = dissolvedOxygen) %>%
  mutate(specificConductanceField = specificConductance) %>%
  select(-dissolvedOxygen,-specificConductance)
External.data<- left_join(External.data, pdata, by='collectDate')

View(External.data)

#field D.O.*
do <- External.data %>%
  filter(!is.na(dissolvedOxygenField)) %>%
  select(collectDate,dissolvedOxygenField)
do$collectDate<- substr(do$collectDate, 1, 10)
do_data<- (unique(do))
#do_data
## organizing data
analyte_dataB<- do_data %>%
  mutate(analyte = 'dissolvedOxygenField') %>%
  mutate(analyteUnits = 'milligramsPerLiter') %>%
  mutate(analyteConcentration = dissolvedOxygenField) %>%
  select(-dissolvedOxygenField) %>%
  arrange(collectDate)
#analyte_dataB

#water temp*
wt <- External.data %>%
  filter(!is.na(waterTemp)) %>%
  select(collectDate,waterTemp)
wt$collectDate<- substr(wt$collectDate, 1, 10)
wt_data<- (unique(wt))
## organizing data
analyte_dataC<- wt_data %>%
  mutate(analyte = 'surfaceWaterTemp') %>%
  mutate(analyteUnits = 'celsius') %>%
  mutate(analyteConcentration = waterTemp) %>%
  select(-waterTemp) %>%
  arrange(collectDate)
#analyte_dataC

analyteC_dates<- analyte_dataB %>%
  select(collectDate)
## same dates
analyte_dataB<- left_join(analyteC_dates, analyte_dataB, by= 'collectDate')

#View(analyte_dataB)

## End of SWC parent data------------------------------------------
analytes<- left_join(analyte_dataB, analyte_dataC, by= 'collectDate')

#View(analytes_f)
analytes_f<- analytes %>%
  filter(!is.na(analyteConcentration.y))

data<- analytes_f %>%
  select(collectDate, analyteConcentration.x, analyteConcentration.y)
## x is dissolvedOxygen, y is surface water temp

## only DO
data1<- data %>%
  select(collectDate, analyteConcentration.x)

View(data1)
## Dissolved oxygen negatively correlated w/ surfaceWaterTemp(high)..
## sometimes other low correleations.. DIC,specificConductance, ect
## depending on creeks chosen

#install.packages("fpp2")
library(fpp2)
#View(Y)
View(data1)

## only want same number of readings per year!!


## time series data decoration...
Y <- ts(data1[2], start = c(2017,1), frequency = 26)
## time plot
autoplot(Y) +
  ggtitle('Dissolved Oxygen over time')+
  ylab("analyte concentration")
## strong trend? Investigate...
DY<- diff(Y)
DY
## change in data
autoplot(DY) +
  ggtitle('Dissolved Oxygen over time')+
  ylab("analyte concentration")







## seasonality??
ggseasonplot(DY)+ggtitle('Seasonal Plot: Change in daily concentration')+
  ylab('concentration')
# does not look seasonal!!
# but some sort of pattern

## seasonal subseries plot
ggsubseriesplot(DY)
