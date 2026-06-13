# # #
# library(neonUtilities)
# library(mlr)
# library(tidyverse)
# # #
# # #
# # #
#  ## Lets try it----
#  {
#    site.pick<- c("SYCA", "ARIK","BARC","BIGC","BLDE","BLUE","BLWA","CARI","COMO","CRAM","CUPE","FLNT","GUIL","HOPB","KING","LECO","LEWI","LIRO","MART","MAYF","MCDI","MCRA","OKSR","POSE","PRIN","PRLA","PRPO","REDB","SUGG","TECR","TOMB","TOOK","WALK","WLOU")
# 
#    ## NEON data
#    # SWC
#    dpid <- "DP1.20093.001"   # surface Water Chemistry DPID
#    x=1
#    data.swc<- loadByProduct(dpid, site = site.pick[x], startdate = '2016-01', check.size = F)
# 
#    External.data<- data.swc$swc_externalLabDataByAnalyte
#    External.data<- External.data %>%
#      select(collectDate, siteID, analyte, analyteConcentration) %>%
#      mutate(collectDate = substr(collectDate,0,10))
#    ##list of the analytes from external lab SWC
#    analytes<- unique(External.data$analyte)
# 
#    i=1
#    exData<- External.data %>%
#      filter(analyte == analytes[i]) %>%
#      filter(!is.na(analyteConcentration)) %>%
#      select(-analyte) %>%
#      group_by(collectDate, siteID) %>%
#      summarize(analyteConcentrationAVG = mean(analyteConcentration, na.rm = TRUE))
#    names(exData)<- c('collectDate', 'siteID', analytes[i])
# 
#    i=2
#    ex<- list()
# 
#    while (i < length(analytes)+1) {
#      ex[[i]]<- External.data %>%
#        filter(analyte == analytes[i]) %>%
#        filter(!is.na(analyteConcentration)) %>%
#        select(-analyte) %>%
#        group_by(collectDate, siteID) %>%
#        summarize(analyteConcentrationAVG = mean(analyteConcentration, na.rm = F))
#      names(ex[[i]])<- c('collectDate', 'siteID', analytes[i])
# 
#      exData<- left_join(exData, ex[[i]], by = c('collectDate', 'siteID'))
#      i=i+1
#    }
# 
#    externalTib<- as_tibble(exData)
#    
#    parent_data<- data.swc$swc_fieldSuperParent
#    pdata<- parent_data %>% 
#      select(collectDate,siteID, waterTemp, dissolvedOxygen, specificConductance) %>% 
#      mutate(dissolvedOxygenField = dissolvedOxygen) %>%
#      mutate(specificConductanceField = specificConductance) %>% 
#      select(-dissolvedOxygen,-specificConductance) %>% 
#      mutate(collectDate = substr(collectDate,0,10))
#    
#    new_data<- left_join(externalTib, pdata, by= c('collectDate', 'siteID'))
# 
#    e_data <- new_data %>%
#      select(-collectDate) %>%
#      select(-siteID) %>%
#      select(-specificConductance)
# 
#    w=1
#    while (w < length(e_data)+1) {
#      e_data<- e_data %>%
#        filter(!is.na(e_data[w]))
#      w=w+1
#    }
# 
#    all_data<- e_data
# 
#    x=2
#    while (x < length(site.pick)+1) {
# 
#      data.swc<- loadByProduct(dpid, site = site.pick[x], startdate = '2016-01', check.size = F)
# 
#      External.data<- data.swc$swc_externalLabDataByAnalyte
#      External.data<- External.data %>%
#        select(collectDate, siteID, analyte, analyteConcentration) %>%
#        mutate(collectDate = substr(collectDate,0,10))
#      analytes<- unique(External.data$analyte)
# 
#      i=1
#      exData<- External.data %>%
#        filter(analyte == analytes[i]) %>%
#        filter(!is.na(analyteConcentration)) %>%
#        select(-analyte) %>%
#        group_by(collectDate, siteID) %>%
#        summarize(analyteConcentrationAVG = mean(analyteConcentration, na.rm = TRUE))
#      names(exData)<- c('collectDate', 'siteID', analytes[i])
# 
#      i=2
#      ex<- list()
# 
#      while (i < length(analytes)+1) {
#        ex[[i]]<- External.data %>%
#          filter(analyte == analytes[i]) %>%
#          filter(!is.na(analyteConcentration)) %>%
#          select(-analyte) %>%
#          group_by(collectDate, siteID) %>%
#          summarize(analyteConcentrationAVG = mean(analyteConcentration, na.rm = TRUE))
#        names(ex[[i]])<- c('collectDate', 'siteID', analytes[i])
# 
#        exData<- left_join(exData, ex[[i]], by = c('collectDate', 'siteID'))
#        i=i+1
#      }
#      
#      externalTib<- as_tibble(exData)
#      
#      parent_data<- data.swc$swc_fieldSuperParent
#      pdata<- parent_data %>% 
#        select(collectDate,siteID, waterTemp, dissolvedOxygen, specificConductance) %>% 
#        mutate(dissolvedOxygenField = dissolvedOxygen) %>%
#        mutate(specificConductanceField = specificConductance) %>% 
#        select(-dissolvedOxygen,-specificConductance) %>% 
#        mutate(collectDate = substr(collectDate,0,10))
#      
#      new_data<- left_join(externalTib, pdata, by= c('collectDate', 'siteID'))
# 
#      e_data <- new_data %>%
#        select(-collectDate) %>%
#        select(-siteID) %>%
#        select(-specificConductance)
# 
#      w=1
#      while (w < length(e_data)+1) {
#        e_data<- e_data %>%
#          filter(!is.na(e_data[w]))
#       w=w+1
#      }
# 
#      all_data<- union(all_data, e_data)
#      x=x+1
#    }
#    write.csv(all_data, "mlr_chem_data.csv")
#  }
# 
# fit_data<- all_data %>% mutate_if(is.numeric, round, 2)
# fit_data<- rename(fit_data,
#                 'TSSD' = 'TSS - Dry Mass',
#                 'UV280' = 'UV Absorbance (280 nm)',
#                 'UV250' = 'UV Absorbance (250 nm)',
#                 'NO3NO2' = 'NO3+NO2 - N',
#                 'NH4' = 'NH4 - N',
#                 'NO2' = 'NO2 - N',
#                 'OrthoP' = 'Ortho - P',
#                 'PH' = 'pH')
# write.csv(fit_data, "mlr_chem_data.csv")
# 
# ## Save 'all_data'
# 
# 
# 
# # #
# # #
# # #
# # #
# # #
# # #
# # #   # choosing less analytes- most relevant from AnalyteApp for ANC-
# # #   #dont need everthing- can use this instead
# # #   f_data<- fit_data %>%
# # #     select(HCO3, TDS, Ca, Mg, Na)
# # #
# # #   # define a task (what want to achieve) - Target is variable trying to predict
# # #   ANCTask <- makeRegrTask(data = fit_data, target = 'ANC')
# # #
# # #   lm <- makeLearner('regr.lm', predict.type = 'response')
# # #
# # #   # must define task, learner, and train model
# # #   lmModel<- train(lm, ANCTask)
# # #   #knnModel
# # #   #nrow(fit_data)
# # #   # Testing!!
# # #   # Cross validation (split data to training set and other so can test on 'new' data
# # #   # Hold-out takes out 1/3 of data or K-fold (split data into diff folds and test other folds)
# # #   # k-fold more robust!! Can do lots of folds (10), and check multiple times (50)
# # #
# # #   kFold<- makeResampleDesc("RepCV", fold = 10, reps = 50) # repeat cross validation ('RepCV')
# # #   kFoldCV<- resample(learner = knn, task = ANCTask,
# # #                      resampling = kFold)
# # #   # shows percent of misidentified classes (~10%)
# # #
# # # }
# # #
# # #
# # # # can use Predict function to predict data w/model:
# # # pred<- predictLearner(.learner = lm, .model = lmModel, .newdata = )
# # #
# # # #-----------
# #
# #
# # # #   #Getting rid of outliers
# # # #   Q1= 25th percentile
# # # #
# # # #   Q3= 75th percentile
# # # #
# # # #   Then, IQR= Q3 - Q1
# # # #
# # # #   And an outlier would be a point below [Q1- (1.5)IQR] or above [Q3+(1.5)IQR].
# # #
# #
# # data<- read_csv('all_water_chemData.csv')
# # names(data)
# #
# #
# #
# # data[24,]
# #
# # '-' %in% data[24,]
# # length(data)
# #
# # i=1
# # while (i < 4) {
# #   data <- data %>%
# #     filter(data[i] > 0)
# #   i=1+i
# # }
# #
# #
# #
# # View(data)
# # rowMeans(data, na.rm = TRUE)
# #
# #
# # ## look at data by row..
# #   if (data[[i]])
# #
# # q1<- quantile(data[[i]])
# # q1
# # q1[[4]] #75% quantile
# # data[[i]]
# #
# #
# # q2<- quantile(data[[2]])
# # q2
# # View(data[2])
# # plot(data[2])
# # ave(data[[1]])
# # head(data[[1]])
# # head(data[1])
# 
# 
# # 
# # ## TEMP DATA
# # #getTimeIndex('DP1.20053.001')
# # #library(neonUtilities)
# # #library(tidyverse)
# # swc<- "DP1.20093.001"
# # # ## doesn't work!!
# # temp<- loadByProduct(dpID = swc, site= 'SYCA', startdate = '2019-01', check.size = F)
# # parent_data<- temp$swc_fieldSuperParent
# # pdata<- parent_data %>%
# #   select(collectDate, waterTemp, dissolvedOxygen, specificConductance)
# # f<- left_join(ex, pdata, by='collectDate')
# # 
# # 
# # ex<- temp$swc_externalLabDataByAnalyte
# # 
# # 
# # parent_data<- temp$waq_instantaneous
# # View(parent_data)
# # pdata<- parent_data %>%
# #   select(collectDate, waterTemp, dissolvedOxygen, specificConductance)
# # f<- left_join(ex, pdata, by='collectDate')
# # 
# # 
# # 
# # View(f)
# # 
# # View(ex)
# # View(t)
# # ##surface water temp*
# #    s_temp <- sensor.sen %>%
# #      filter(!is.na(turbidity)) %>%
# #      select(startDateTime,turbidity,turbidityExpUncert) %>%
# #      #only want to keep data within 95% certainty
# #      mutate(confidenceL = (turbidityExpUncert/turbidity)*100) %>%
# #      filter(confidenceL <= 5)
# #    turb$startDateTime<- substr(turb$startDateTime, 1, 10)
# #    ## organizing data by day
# #    analyte_dataB<- turb %>%
# #      group_by(startDateTime) %>%
# #      summarise(mean(turbidity)) %>%
# #      mutate(collectDate = startDateTime) %>%
# #      mutate(analyteConcentration = `mean(turbidity)`) %>%
# #      select(-startDateTime, -`mean(turbidity)`) %>%
# #      mutate(analyte = 'turbidity') %>%
# #      mutate(analyteUnits = 'formazinNephelometricUnit') %>%
# #      arrange(collectDate)
# #    ## only include analyte_data that has same dates to join
# #    analyte_dates<- analyte_data %>%
# #      select(collectDate)
# #    analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
# #
