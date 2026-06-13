#---server--------------------------------------------------  
# Timothy Gilbert

server <- function(input, output, session) {

  ## Alert - creating alert for possible long loading times depending on
  ## analyte selection pulling from SWC and Water Quality dpids only when needed-----
  observeEvent(input, {
      shinyalert(
        title = "Water Chemisty Analyte Comparisons",
        text = "Select Site and Two Analytes - move down side tabs to compare analytes, check
        correlations between analytes, and try to predict analyte values with the machine learning tab",
        size = "l",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
  })
  ## End of Alert----

  # Start of analyte selections----
  Analyte_select <- reactive({
    
    # Update alert for longer loading time when downloading sensor data
    if (input$Select_B == 'dissolvedOxygen' || input$Select_B == 'chlorophyll' || input$Select_B == 'turbidity' || input$Select_B == 'fDOM') {
      
      createAlert(session, 'alert1', "exampleAlert", title = "LOADING DATA",
                  content = paste0("[",input$Select_A," finished]",'\n',
                                   input$Select_B,"* sensor data started"),
                  append = TRUE)
    }
    
    select.analyte<- input$Select_A # saving analyte 1 selection
    req(input$Select_A)
    External.data <- site_select()
    sensor.sen <- site_select2()

    ## Defining "analyte_data" depending on analytes selected
    ## Start of if statements for WQ/sensor data selection---------------
    if (select.analyte == 'dissolvedOxygen') {
      #dissolved oxygen*
      dissolved_o <- sensor.sen %>% 
        filter(!is.na(dissolvedOxygen)) %>% 
        select(startDateTime,dissolvedOxygen,dissolvedOxygenExpUncert) %>% 
        #only want to keep data within 95% certainty
        mutate(confidenceL = (dissolvedOxygenExpUncert/dissolvedOxygen)*100) %>% 
        filter(confidenceL <= 5)
      dissolved_o$startDateTime<- substr(dissolved_o$startDateTime, 1, 10)
      ## organizing data
      analyte_data<- dissolved_o %>%
        group_by(startDateTime) %>%
        summarise(mean(dissolvedOxygen)) %>%
        mutate(collectDate = startDateTime) %>%
        mutate(analyteConcentration = `mean(dissolvedOxygen)`) %>%
        select(-startDateTime, -`mean(dissolvedOxygen)`) %>%
        mutate(analyte = 'dissolvedOxygen') %>%
        mutate(analyteUnits = 'milligramsPerLiter') %>%
        arrange(collectDate)
      
      ## only include analyte_data that has same dates to join
      # analyte_dates<- analyte_data %>% 
      #   select(collectDate)
      # analyte_data<- left_join(analyte_dates, analyte_data, by= 'collectDate')
    }
    
    if (select.analyte == 'chlorophyll') {
      ##chlorophyll*
      chlor <- sensor.sen %>% 
        filter(!is.na(chlorophyll)) %>% 
        select(startDateTime,chlorophyll,chlorophyllExpUncert) %>% 
        #only want to keep data within 95% certainty
        mutate(confidenceL = (chlorophyllExpUncert/chlorophyll)*100) %>% 
        filter(confidenceL <= 5)
      chlor$startDateTime<- substr(chlor$startDateTime, 1, 10)
      ## organizing data
      analyte_data<- chlor %>%
        group_by(startDateTime) %>%
        summarise(mean(chlorophyll)) %>%
        mutate(collectDate = startDateTime) %>%
        mutate(analyteConcentration = `mean(chlorophyll)`) %>%
        select(-startDateTime, -`mean(chlorophyll)`) %>%
        mutate(analyte = 'chlorophyll') %>%
        mutate(analyteUnits = 'microgramsPerLiter') %>%
        arrange(collectDate)
      ## only include analyte_data that has same dates to join
      # analyte_dates<- analyte_data %>% 
      #   select(collectDate)
      # analyte_data<- left_join(analyte_dates, analyte_data, by= 'collectDate')
    }
    
    if (select.analyte == 'turbidity') {
      ##turbidity*
      turb <- sensor.sen %>% 
        filter(!is.na(turbidity)) %>% 
        select(startDateTime,turbidity,turbidityExpUncert) %>% 
        #only want to keep data within 95% certainty
        mutate(confidenceL = (turbidityExpUncert/turbidity)*100) %>% 
        filter(confidenceL <= 5)
      turb$startDateTime<- substr(turb$startDateTime, 1, 10)
      ## organizing data
      analyte_data<- turb %>%
        group_by(startDateTime) %>%
        summarise(mean(turbidity)) %>%
        mutate(collectDate = startDateTime) %>%
        mutate(analyteConcentration = `mean(turbidity)`) %>%
        select(-startDateTime, -`mean(turbidity)`) %>%
        mutate(analyte = 'turbidity') %>%
        mutate(analyteUnits = 'formazinNephelometricUnit') %>%
        arrange(collectDate)
      ## only include analyte_data that has same dates to join
      # analyte_dates<- analyte_data %>% 
      #   select(collectDate)
      # analyte_data<- left_join(analyte_dates, analyte_data, by= 'collectDate')
    }
    
    if (select.analyte == 'fDOM') {
      #fDOM*
      fDOM_data <- sensor.sen %>% 
        filter(!is.na(fDOM)) %>% 
        select(startDateTime,fDOM,fDOMExpUncert) %>% 
        #only want to keep data within 95% certainty
        mutate(confidenceL = (fDOMExpUncert/fDOM)*100) %>% 
        filter(confidenceL <= 5)
      fDOM_data$startDateTime<- substr(fDOM_data$startDateTime, 1, 10)
      ## organizing data
      analyte_data<- fDOM_data %>%
        group_by(startDateTime) %>%
        summarise(mean(fDOM)) %>%
        mutate(collectDate = startDateTime) %>%
        mutate(analyteConcentration = `mean(fDOM)`) %>%
        select(-startDateTime, -`mean(fDOM)`) %>%
        mutate(analyte = 'fDOM') %>%
        mutate(analyteUnits = 'quinineSulfateUnit') %>%
        arrange(collectDate)
      ## only include analyte_data that has same dates to join
      # analyte_dates<- analyte_data %>% 
      #   select(collectDate)
      # analyte_data<- left_join(analyte_dates, analyte_data, by= 'collectDate')
    }
    ## End of sensor data if statements-------------------------------
    
    ## Start of if statements for SWC data selection-------------------
    if (select.analyte == 'ANC' || select.analyte == 'Br' 
        || select.analyte == 'Ca' || select.analyte == 'Cl' 
        || select.analyte == 'CO3' || select.analyte == 'specificConductance'
        || select.analyte == 'DIC' || select.analyte == 'DOC' 
        || select.analyte == 'F' || select.analyte == 'Fe' 
        || select.analyte == 'HCO3' || select.analyte == 'K' 
        || select.analyte == 'Mg' || select.analyte == 'Mn' 
        || select.analyte == 'Na' || select.analyte == 'NH4 - N' 
        || select.analyte == 'NO2 - N' || select.analyte == 'NO3+NO2 - N' 
        || select.analyte == 'Ortho - P' || select.analyte == 'pH' 
        || select.analyte == 'Si' || select.analyte == 'SO4' 
        || select.analyte == 'TDN' || select.analyte == 'TDP' 
        || select.analyte == 'TDS' || select.analyte == 'TN' 
        || select.analyte == 'TOC' || select.analyte == 'TP' 
        || select.analyte == 'TPC' || select.analyte == 'TPN' 
        || select.analyte == 'TSS' || select.analyte == 'TSS - Dry Mass' 
        || select.analyte == 'UV Absorbance (250 nm)' 
        || select.analyte == 'UV Absorbance (280 nm)') {
      analyte_data<- External.data %>%
        filter(analyte == select.analyte) %>%
        select(analyte, analyteConcentration, analyteUnits, collectDate) %>%
        filter(!is.na(analyteConcentration)) %>% 
        arrange(collectDate)
      # fixing dates
      analyte_data$collectDate<- substr(analyte_data$collectDate, 1, 10)
    }
    
    if (select.analyte == 'waterTemp') {
      #water temp*
      wt <- External.data %>% 
        filter(!is.na(waterTemp)) %>% 
        select(collectDate,waterTemp)
      wt$collectDate<- substr(wt$collectDate, 1, 10)
      wt_data<- (unique(wt))
      ## organizing data
      analyte_data<- wt_data %>%
        mutate(analyte = 'waterTemp') %>%
        mutate(analyteUnits = 'celsius') %>%
        mutate(analyteConcentration = waterTemp) %>%
        select(-waterTemp) %>% 
        arrange(collectDate)
    }## End of if statement for SWC-------------------------------------
    
    if (select.analyte == 'dissolvedOxygenField') {
      #field D.O.*
      do <- External.data %>% 
        filter(!is.na(dissolvedOxygenField)) %>% 
        select(collectDate,dissolvedOxygenField)
      do$collectDate<- substr(do$collectDate, 1, 10)
      do_data<- (unique(do))
      ## organizing data
      analyte_data<- do_data %>%
        mutate(analyte = 'dissolvedOxygenField') %>%
        mutate(analyteUnits = 'milligramsPerLiter') %>%
        mutate(analyteConcentration = dissolvedOxygenField) %>%
        select(-dissolvedOxygenField) %>%
        arrange(collectDate)
    }## End of if statement for SWC-------------------------------------
    
    if (select.analyte == 'specificConductanceField') {
      #field specific conductance*
      sp <- External.data %>% 
        filter(!is.na(specificConductanceField)) %>% 
        select(collectDate,specificConductanceField)
      sp$collectDate<- substr(sp$collectDate, 1, 10)
      sp_data<- (unique(sp))
      ## organizing data
      analyte_data<- sp_data %>%
        mutate(analyte = 'specificConductanceField') %>%
        mutate(analyteUnits = 'microsiemensPerCentimeter') %>%
        mutate(analyteConcentration = specificConductanceField) %>%
        select(-specificConductanceField) %>%
        arrange(collectDate)
    }## End of if statement for SWC-------------------------------------
    ## End of defining "analyte_data"
    
    analyte_data

  })

  Analyte_selectB <- reactive({
    select.analyteB<- input$Select_B # saving analyte 2 selection
    req(input$Select_B)
    External.data <- site_select()
    sensor.sen <- site_select2()
    analyte_data<- Analyte_select()

    ## Defining "analyte_dataB" depending on analytes selected
    ## Start of if statements for WQ/sensor data selection---------------
    if (select.analyteB == 'dissolvedOxygen') {
      #dissolved oxygen*
      dissolved_o <- sensor.sen %>% 
        filter(!is.na(dissolvedOxygen)) %>% 
        select(startDateTime,dissolvedOxygen,dissolvedOxygenExpUncert) %>% 
        #only want to keep data within 95% certainty
        mutate(confidenceL = (dissolvedOxygenExpUncert/dissolvedOxygen)*100) %>% 
        filter(confidenceL <= 5)
      dissolved_o$startDateTime<- substr(dissolved_o$startDateTime, 1, 10)
      ## organizing data
      analyte_dataB<- dissolved_o %>%
        group_by(startDateTime) %>%
        summarise(mean(dissolvedOxygen)) %>%
        mutate(collectDate = startDateTime) %>%
        mutate(analyteConcentration = `mean(dissolvedOxygen)`) %>%
        select(-startDateTime, -`mean(dissolvedOxygen)`) %>%
        mutate(analyte = 'Dissolved Oxygen') %>%
        mutate(analyteUnits = 'milligramsPerLiter') %>%
        arrange(collectDate)
      
      ## only include analyte_data that has same dates to join
      analyte_dates<- analyte_data %>% 
        select(collectDate)
      analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
    }
    
    if (select.analyteB == 'chlorophyll') {
      ##chlorophyll*
      chlor <- sensor.sen %>% 
        filter(!is.na(chlorophyll)) %>% 
        select(startDateTime,chlorophyll,chlorophyllExpUncert) %>% 
        #only want to keep data within 95% certainty
        mutate(confidenceL = (chlorophyllExpUncert/chlorophyll)*100) %>% 
        filter(confidenceL <= 5)
      chlor$startDateTime<- substr(chlor$startDateTime, 1, 10)
      ## organizing data
      analyte_dataB<- chlor %>%
        group_by(startDateTime) %>%
        summarise(mean(chlorophyll)) %>%
        mutate(collectDate = startDateTime) %>%
        mutate(analyteConcentration = `mean(chlorophyll)`) %>%
        select(-startDateTime, -`mean(chlorophyll)`) %>%
        mutate(analyte = 'chlorophyll') %>%
        mutate(analyteUnits = 'microgramsPerLiter') %>%
        arrange(collectDate)
      ## only include analyte_data that has same dates to join
      analyte_dates<- analyte_data %>% 
        select(collectDate)
      analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
    }
    
    if (select.analyteB == 'turbidity') {
      ##turbidity*
      turb <- sensor.sen %>% 
        filter(!is.na(turbidity)) %>% 
        select(startDateTime,turbidity,turbidityExpUncert) %>% 
        #only want to keep data within 95% certainty
        mutate(confidenceL = (turbidityExpUncert/turbidity)*100) %>% 
        filter(confidenceL <= 5)
      turb$startDateTime<- substr(turb$startDateTime, 1, 10)
      ## organizing data
      analyte_dataB<- turb %>%
        group_by(startDateTime) %>%
        summarise(mean(turbidity)) %>%
        mutate(collectDate = startDateTime) %>%
        mutate(analyteConcentration = `mean(turbidity)`) %>%
        select(-startDateTime, -`mean(turbidity)`) %>%
        mutate(analyte = 'turbidity') %>%
        mutate(analyteUnits = 'formazinNephelometricUnit') %>%
        arrange(collectDate)
      ## only include analyte_data that has same dates to join
      analyte_dates<- analyte_data %>% 
        select(collectDate)
      analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
    }
    
    if (select.analyteB == 'fDOM') {
      #fDOM*
      fDOM_data <- sensor.sen %>% 
        filter(!is.na(fDOM)) %>% 
        select(startDateTime,fDOM,fDOMExpUncert) %>% 
        #only want to keep data within 95% certainty
        mutate(confidenceL = (fDOMExpUncert/fDOM)*100) %>% 
        filter(confidenceL <= 5)
      fDOM_data$startDateTime<- substr(fDOM_data$startDateTime, 1, 10)
      ## organizing data
      analyte_dataB<- fDOM_data %>%
        group_by(startDateTime) %>%
        summarise(mean(fDOM)) %>%
        mutate(collectDate = startDateTime) %>%
        mutate(analyteConcentration = `mean(fDOM)`) %>%
        select(-startDateTime, -`mean(fDOM)`) %>%
        mutate(analyte = 'fDOM') %>%
        mutate(analyteUnits = 'quinineSulfateUnit') %>%
        arrange(collectDate)
      ## only include analyte_data that has same dates to join
      analyte_dates<- analyte_data %>% 
        select(collectDate)
      analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
    }
    ## End of sensor data if statements-------------------------------
    
    ## Start of if statements for SWC data selection-------------------
    if (select.analyteB == 'ANC' || select.analyteB == 'Br'
        || select.analyteB == 'Ca' || select.analyteB == 'Cl' 
        || select.analyteB == 'CO3' || select.analyteB == 'specificConductance'
        || select.analyteB == 'DIC' || select.analyteB == 'DOC' 
        || select.analyteB == 'F' || select.analyteB == 'Fe' 
        || select.analyteB == 'HCO3' || select.analyteB == 'K' 
        || select.analyteB == 'Mg' || select.analyteB == 'Mn' 
        || select.analyteB == 'Na' || select.analyteB == 'NH4 - N' 
        || select.analyteB == 'NO2 - N' || select.analyteB == 'NO3+NO2 - N' 
        || select.analyteB == 'Ortho - P' || select.analyteB == 'pH' 
        || select.analyteB == 'Si' || select.analyteB == 'SO4' 
        || select.analyteB == 'TDN' || select.analyteB == 'TDP' 
        || select.analyteB == 'TDS' || select.analyteB == 'TN' 
        || select.analyteB == 'TOC' || select.analyteB == 'TP' 
        || select.analyteB == 'TPC' || select.analyteB == 'TPN' 
        || select.analyteB == 'TSS' || select.analyteB == 'TSS - Dry Mass' 
        || select.analyteB == 'UV Absorbance (250 nm)' 
        || select.analyteB == 'UV Absorbance (280 nm)') {
      analyte_dataB<- External.data %>%
        filter(analyte == select.analyteB) %>%
        select(analyte, analyteConcentration, analyteUnits, collectDate) %>%
        filter(!is.na(analyteConcentration)) %>% 
        arrange(collectDate)
      # fixing dates
      analyte_dataB$collectDate<- substr(analyte_dataB$collectDate, 1, 10)
    }
    
    if (select.analyteB == 'waterTemp') {
      #water temp*
      wt <- External.data %>% 
        filter(!is.na(waterTemp)) %>% 
        select(collectDate,waterTemp)
      wt$collectDate<- substr(wt$collectDate, 1, 10)
      wt_data<- (unique(wt))
      ## organizing data
      analyte_dataB<- wt_data %>%
        mutate(analyte = 'Surface Water Temp') %>%
        mutate(analyteUnits = 'celsius') %>%
        mutate(analyteConcentration = waterTemp) %>%
        select(-waterTemp) %>% 
        arrange(collectDate)
    }## End of if statement for SWC-------------------------------------
    
    if (select.analyteB == 'dissolvedOxygenField') {
      #field D.O.*
      do <- External.data %>% 
        filter(!is.na(dissolvedOxygenField)) %>% 
        select(collectDate,dissolvedOxygenField)
      do$collectDate<- substr(do$collectDate, 1, 10)
      do_data<- (unique(do))
      ## organizing data
      analyte_dataB<- do_data %>%
        mutate(analyte = 'Dissolved Oxygen (Field)') %>%
        mutate(analyteUnits = 'milligramsPerLiter') %>%
        mutate(analyteConcentration = dissolvedOxygenField) %>%
        select(-dissolvedOxygen) %>%
        arrange(collectDate)
    }## End of if statement for SWC-------------------------------------
    
    if (select.analyteB == 'specificConductanceField') {
      #field specific conductance*
      sp <- External.data %>% 
        filter(!is.na(specificConductanceField)) %>% 
        select(collectDate,specificConductanceField)
      sp$collectDate<- substr(sp$collectDate, 1, 10)
      sp_data<- (unique(sp))
      ## organizing data
      analyte_dataB<- sp_data %>%
        mutate(analyte = 'Specific Conductivity (Field)') %>%
        mutate(analyteUnits = 'microsiemensPerCentimeter') %>%
        mutate(analyteConcentration = specificConductanceField) %>%
        select(-specificConductance) %>%
        arrange(collectDate)
    }## End of if statement for SWC-------------------------------------
    ## End of defining "analyte_dataB"
    analyte_dataB
    
  }) # End of analyte selections----
  
  # Downloading SWC data-----
  site_select <- reactive({
    # Reactive variables from input
    site.pick<- input$Select
    req(input$Select)
    
    start_d<-format(input$dateRange[1]) # start date
    end_d<-format(input$dateRange[2])  # end date
    
    dpid <- "DP1.20093.001"   # surface Water Chemistry DPID
    # loading in neonScripts from neonUtilities packages needed
    #base::source('serverNEON.R', local=TRUE)   
    data.swc<- loadByProduct(dpid, site = site.pick, startdate = start_d, 
                             enddate = end_d, check.size = F)
    
    # data.swc<- loadByProduct(dpid, site = 'SYCA', startdate = '2019-03', 
    #                          enddate = '2019-10', check.size = F)
    # analyte_data<- 'ANC'
    # analyte_dataB<- 'Br'
    
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
  }) # End of SWC data download-----
 
  # Downloading Water Quality - Sensor data-----
  site_select2 <- reactive({
    site.pick<- input$Select
    req(input$Select)
    #select.analyteA<- input$Select_A
    select.analyteB<- input$Select_B

    ## not sure about specificConductance and pH for sensors..
    ## same name as SWC analyte??
    start_d<-format(input$dateRange[1]) # start date
    end_d<-format(input$dateRange[2])  # end date

    dpid <- "DP1.20288.001"   # Water Quality dpid

    if (input$Select_A == 'dissolvedOxygen' || input$Select_A == 'chlorophyll' || input$Select_A == 'turbidity' || input$Select_A == 'fDOM' ||
        input$Select_B == 'dissolvedOxygen' || input$Select_B == 'chlorophyll' || input$Select_B == 'turbidity' || input$Select_B == 'fDOM') {

      # loading in neonScripts from neonUtilities packages needed
      #base::source('serverNEON.R', local=TRUE)
      data.sen <- loadByProduct(dpid, site = site.pick, startdate = start_d,
                                enddate = end_d, check.size = F)


      # water quality sensor data
      sensor.sen <- data.sen$waq_instantaneous

    }
  })   # End of Sensor Data download-----

  # Reactive plot for pattern visualization
  plotInput <- reactive({
    # reactive variables
    analyte_data<- Analyte_select()
    analyte_dataB<- Analyte_selectB()

    # if main analytes has too many points from sensor data- only use same dates..
    if (input$Select_A == 'dissolvedOxygen' || input$Select_A == 'chlorophyll' || input$Select_A == 'turbidity' || input$Select_A == 'fDOM') {
      analyteB_dates<- analyte_dataB %>% 
        select(collectDate)
      analyte_data<- left_join(analyteB_dates, analyte_data, by= 'collectDate')
    }
    
    # if main analytes has too many points from sensor data- only use same dates..
    if (input$Select_A == 'waterTemp' || input$Select_A == 'dissolvedOxygenField' || input$Select_A == 'dissolvedOxygenField') {
      analyteB_dates<- analyte_dataB %>% 
        select(collectDate)
      analyte_data<- left_join(analyteB_dates, analyte_data, by= 'collectDate')
    }
    
    
    # making color pallete for data
    colourCount = length(unique(analyte_data$analyte))
    getPalette = colorRampPalette(brewer.pal(9, "Set3"))
    
    d.plot<- plot_ly() # plotting first analyte selcetion from SWC data
    d.plot<- d.plot %>% add_trace(data=analyte_data,
                                  type='scatter',
                                  mode='markers+lines',
                                  x=~collectDate,
                                  y=~analyteConcentration,
                                  name =~analyte,
                                  color=I("blue"),
                                  alpha = .9,
                                  #create custom hovertext
                                  text=~paste0("Analyte: ",analyte, '\n',"Analyte Concentration: ",analyteConcentration,' ',analyteUnits, '\n'," Collect Date: ",collectDate, '\n', "From Extrernal Lab Analysis"), 
                                  hoverinfo='text'
    )%>%
      layout(title='External Lab Analyte Concentrations',yaxis=list(title='Concentration'),xaxis=list(title='Collect Date'))

    # defining another y-axis
    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "Concentration"
    )
    # adding secondary analyte to opposite y-axis (y2)
    d.plot<- d.plot%>% add_trace(data=analyte_dataB, x=~collectDate,y=~analyteConcentration,yaxis = 'y2',name=~analyte, mode='lines+markers', inherit = F, type='scatter', text=~paste0("Analyte: ",analyte, '\n',"Collect Date: ", collectDate, '\n','Analyte Concentration: ', analyteConcentration,' ',analyteUnits), alpha=.5, 
                                 hoverinfo='text', colors =  getPalette(colourCount))
    # making look nicer
    d.plot<- d.plot%>% layout(
      title = "External Lab Analyte Concentrations", yaxis2 = ay,
      xaxis = list(title="Collect Date"))
    
  })
  output$Elab <- renderPlotly({
    print(plotInput())
  })
  
  
  
  
  
  forcast<- reactive({
    # reactive variables
    analyte_data<- Analyte_select()
    analyte_dataB<- Analyte_selectB()
    
    # if main analytes has too many points from sensor data- only use same dates..
    if (input$Select_A == 'dissolvedOxygen' || input$Select_A == 'chlorophyll' || input$Select_A == 'turbidity' || input$Select_A == 'fDOM') {
      analyteB_dates<- analyte_dataB %>% 
        select(collectDate)
      analyte_data<- left_join(analyteB_dates, analyte_data, by= 'collectDate')
    }
    
    # if main analytes has too many points from sensor data- only use same dates..
    if (input$Select_A == 'waterTemp' || input$Select_A == 'dissolvedOxygenField' || input$Select_A == 'dissolvedOxygenField') {
      analyteB_dates<- analyte_dataB %>% 
        select(collectDate)
      analyte_data<- left_join(analyteB_dates, analyte_data, by= 'collectDate')
    }
    

    ##mutate data to 1 point per month...
    data<- analyte_data %>% 
      group_by(Date = substr(collectDate, 1, 7)) %>% 
      summarize(MonthlyAvg = ave(analyteConcentration))
    
    m_data<- unique(data)
    months<- c(1:length(m_data$Date))

    ## set up analyte data to date format
    #months <- 1:240
    
    ## making noise
    noise <- rnorm(length(months), mean = 0, sd = 1)
    
    ## empty vector
    lag   <- vector() 
    #lag   <- vector(length = length(months)) 
    
    ## for length form 1 to end of months
    for(t in 1:length(months)){
      ## if first month (initial)
      if(t == 1){
        ## initial lag - 1st observation has no previous data
        lag[t]  <- rnorm(1, mean = 0, sd = 1)
      } else {
        ## autocorrelation with one month lag
        ## for t=2 all to end of 'months'
        lag[t]  <- (lag[t-1] + noise[t]) / 2
      }
    }
    ## divided by 48 just to make number more manageable -dont need it for real data i assume
    lag_trend <- lag + months
    #lag_trend
    
    
    ## for seasonal patterns - repeating pattern over period of 12months
    seasonal <- 2*sin(2*pi*months/12) + noise
    ## seasonal patterns and trend
    seasonal_trend <- seasonal + months
    #seasonal_trend
    #start1<- m_data$Date[1]
    #m_data$Date[1]
    #end1<- m_data$Date[(length(m_data$Date)+2)]
    #end1<- '2020-01'
    ## mapping data onto real world events -- makes assumptions
    all <- ts(data = data.frame(noise, lag, lag_trend, seasonal, seasonal_trend),
              start = 2016,
              end = 2022,
              ## frequency = monthly
              frequency = 12) 
    
    
    #plot(all)
    
    ## have time series and matrix - can apply functions to it
    #class(all)
    
    ## list of what you can do!
    #methods(class = 'ts')
    
    #class(mod_linear)
    #methods(class = 'lm')
    
    
    
    ## looking at lag at time (1month, 3month, 6m, and 9m)
    lag.plot(all, set.lags = c(1, 3, 6, 9))
    ## way to look at lags and trends for data
    
    
    ## estimate autocorrelation function -- 
    acf(all[,'noise'], xlab = 'Lag (years), noise')
    
    ## look at lag plot - 
    acf(all[,'lag'], xlab = 'Lag (years), lag 1')
    
    acf(all[,'lag'], xlab = 'Lag (years), lag 1 and trend')
    
    acf(all[,'seasonal'], xlab = 'Lag (years), seasonal')
    
    ## example - Normalized Fish weight
    plot(all[,"seasonal_trend"])
    ## whats casuing the wiggles other than 'error'
    ## can use decompose - breaks down to seasonal components using moving avgs
    dec <- decompose(all[,'seasonal_trend'])
    #class(dec) ## decomposed times seris
    #summary(dec)
    
    
    #plot(dec)
    ## built in trend of 1/48 from formula above
    
    ## looking at if something changed... if 4 twigs float by (r)
    #r <- rpois(n = length(dec$random), lambda = 4)
    
    #tidy(lm(dec$random ~ r))
    ## (small values- tells us P-value is high, so not sig)
    
    ## check trend
    #tidy(lm(dec$trend ~ months))
    ## model deosnt fit
    #length(dec$trend)
    #length(months)
    
    ## subset data to be same...
    tidy(lm(dec$trend ~ months[1:length(dec$trend)]))
    ## months do have affect - close to 1/48 (.02) (trend)
    
    
    
    ## modeling with smoothing!
    ## smooth out...
    seasonal_stl <- stl(all[,'seasonal_trend'], s.window = 6)
    plot(seasonal_stl)
    
    #fit <- lm(seasonal_stl$time.series[,'trend'] ~ months)
    fit <- lm(seasonal_stl$time.series[,'trend'] ~ months[1:length(seasonal_stl$time.series[,'trend'])])
    coef(fit)
    
    
    
    ## 'tslm' -- fitting data linear model (trend and seasonal component)
    ## before is looking at mean predictions only
    ts_fit <- tslm(all[,'seasonal_trend'] ~ trend + season)
    tidy(ts_fit)
    
    
    ## can add other predictiors too (h = future time steps)
    plot(forecast(ts_fit, h = 25))
    
    
    ## arima- AR uses previous predictions for next prediction- MA uses previous error to predict next value
    #auto_fit <- auto.arima(all[,'seasonal_trend'])
    
    #summary(auto_fit)


    # ## adding linear regression
    # ####          lm( mass ~ 1(intercept) + days(amount of days), and your data=data)
    # mod_linear <- lm(mass ~ 1 + days, data = linear_data)
    # 
    # ## look at results
    # tidy(mod_linear)
    # 
    # 
    # mod_linear
    # 
    # ## model statistics
    # summary(mod_linear)
    
    
    
    # # making color pallete for data
    # colourCount = length(unique(analyte_data$analyte))
    # getPalette = colorRampPalette(brewer.pal(9, "Set3"))
    # 
    # d.plot2<- plot_ly() # plotting first analyte selcetion from SWC data
    # d.plot2<- d.plot2 %>% add_trace(data=analyte_data,
    #                                 type='scatter',
    #                                 mode='markers+lines',
    #                                 x=~collectDate,
    #                                 y=~analyteConcentration,
    #                                 name =~analyte,
    #                                 color=I("blue"),
    #                                 alpha = .9,
    #                                 #create custom hovertext
    #                                 text=~paste0("Analyte: ",analyte, '\n',"Analyte Concentration: ",analyteConcentration,' ',analyteUnits, '\n'," Collect Date: ",collectDate, '\n', "From Extrernal Lab Analysis"), 
    #                                 hoverinfo='text'
    # )
    # 
    # d.plot2
    
  })
  output$per<- renderPlot({
    print(forcast())
  })
  
  
  
  # Analyte compare plot2
  plotInput2 <- reactive({
    # reactive variables
    analyte_data<- Analyte_select()
    analyte_dataB<- Analyte_selectB()

    analytes<- left_join(analyte_data, analyte_dataB, by= 'collectDate')
    
    analytes_f<- analytes %>% 
      filter(!is.na(analyteConcentration.y))

    sct_base<-ggplot(analytes_f,aes(y = analyteConcentration.y,x = analyteConcentration.x))
    d.plot<- sct_base+geom_point()+
      geom_smooth(method = "lm",se = F, color = "Purple", show.legend = T, formula = 'y ~ x', na.rm = F)+
      geom_smooth(color = "Pink", show.legend = T, inherit.aes = T)+
      theme_classic()+
      # labeling plot w/ analytes being evaluated
      ggtitle(paste0(analytes_f$analyte.y[1],' vs ',analytes_f$analyte.x[1]))+
      xlab(analytes_f$analyte.x[1])+
      ylab(analytes_f$analyte.y[1])
  })
  output$Dlab <- renderPlotly({
    print(plotInput2())
  })
  
  output$PvalueSUM <- renderPrint({
    # reactive variables
    analyte_data<- Analyte_select()
    analyte_dataB<- Analyte_selectB()
    
    analytes<- left_join(analyte_data, analyte_dataB, by= 'collectDate')
    
    analytes_f<- analytes %>% 
      filter(!is.na(analyteConcentration.y))
    
    mdl_1<-lm(analyteConcentration.y ~ analyteConcentration.x,data = analytes_f)
    summary(mdl_1)
    
  })
  
  # Rendering table of plotted values for regression analysis
  output$table <- renderDataTable({
    # reactive variables
    analyte_data<- Analyte_select()
    analyte_dataB<- Analyte_selectB()

    analytes<- left_join(analyte_data, analyte_dataB, by= 'collectDate')
    
    analytes_f<- analytes %>% 
      filter(!is.na(analyteConcentration.y))
    
  })
  
  ## Start of Correlation Table------
  c_check<- reactive({
    # reative varibles
    analyte_data<- Analyte_select()
    analyte_dataB<- Analyte_selectB()
    External.data <- site_select()
    sensor.sen <- site_select2()

    select.analyte <- input$Select_A
    select.analyteB <- input$Select_B
    
    #took out specificConductance from external lab data-
    # list of variable/analytes to go through
    alist<- c('ANC','Br','Ca','Cl','CO3','DIC','DOC','F','Fe','HCO3',
              'K','Mg','Mn','Na','NH4 - N','NO3+NO2 - N','Ortho - P','pH',
              'Si','SO4','TDN','TDP','TDS','TN','TOC','TP','TPC','TPN','TSS',
              'TSS - Dry Mass','UV Absorbance (250 nm)','UV Absorbance (280 nm)',
              'waterTemp','dissolvedOxygenField','specificConductanceField')
    # if selected sensor data- extend list to all sensor data
    if (select.analyteB == 'dissolvedOxygen' || select.analyteB == 'chlorophyll' || 
        select.analyteB == 'turbidity' || select.analyteB == 'fDOM' ||
        select.analyte == 'dissolvedOxygen' || select.analyte == 'chlorophyll' || 
        select.analyte == 'turbidity' || select.analyte == 'fDOM') {
      
      alist2<- c('dissolvedOxygen','chlorophyll','turbidity','fDOM')
      alist<- rbind(c(alist, alist2))
    }
    
    # empty Correlation Table to fill up w/ data
    correlation <- data.table(Analyte=character(),Correlation=numeric())
    
    i=1
    ## looking at one analyte at a time
    while (i < length(alist)+1 ) {
      
      ## if statement for Secondary Analyte- if SWC selection...
      ## Start of if statements for SWC data selection-------------------
      
      if (alist[i] == 'ANC' || alist[i] == 'Br' 
          || alist[i] == 'Ca' || alist[i] == 'Cl' 
          || alist[i] == 'CO3' || alist[i] == 'specificConductance'
          || alist[i] == 'DIC' || alist[i] == 'DOC' 
          || alist[i] == 'F' || alist[i] == 'Fe' 
          || alist[i] == 'HCO3' || alist[i] == 'K' 
          || alist[i] == 'Mg' || alist[i] == 'Mn' 
          || alist[i] == 'Na' || alist[i] == 'NH4 - N' 
          || alist[i] == 'NO2 - N' || alist[i] == 'NO3+NO2 - N' 
          || alist[i] == 'Ortho - P' || alist[i] == 'pH' 
          || alist[i] == 'Si' || alist[i] == 'SO4' 
          || alist[i] == 'TDN' || alist[i] == 'TDP' 
          || alist[i] == 'TDS' || alist[i] == 'TN' 
          || alist[i] == 'TOC' || alist[i] == 'TP' 
          || alist[i] == 'TPC' || alist[i] == 'TPN' 
          || alist[i] == 'TSS' || alist[i] == 'TSS - Dry Mass' 
          || alist[i] == 'UV Absorbance (250 nm)' 
          || alist[i] == 'UV Absorbance (280 nm)') {
        
        analyte_dataB<- External.data %>%
          filter(analyte == alist[i]) %>%
          select(analyte, analyteConcentration, analyteUnits, collectDate) %>% 
          arrange(collectDate)
        analyte_dataB$collectDate<- substr(analyte_dataB$collectDate, 1, 10)
      }
      ## End of if statement for SWC-------------------------------------
      
      ## if statement for Secondary Analyte- if WQ/Sensor selection...
      ## Start of if statements for WQ/sensor data selection---------------
      if (alist[i] == 'dissolvedOxygen') {
        #dissolved oxygen*
        dissolved_o <- sensor.sen %>% 
          filter(!is.na(dissolvedOxygen)) %>% 
          select(startDateTime,dissolvedOxygen,dissolvedOxygenExpUncert) %>% 
          #only want to keep data within 95% certainty
          mutate(confidenceL = (dissolvedOxygenExpUncert/dissolvedOxygen)*100) %>% 
          filter(confidenceL <= 5)
        dissolved_o$startDateTime<- substr(dissolved_o$startDateTime, 1, 10)
        ## organizing data
        analyte_dataB<- dissolved_o %>%
          group_by(startDateTime) %>%
          summarise(mean(dissolvedOxygen)) %>%
          mutate(collectDate = startDateTime) %>%
          mutate(analyteConcentration = `mean(dissolvedOxygen)`) %>%
          select(-startDateTime, -`mean(dissolvedOxygen)`) %>%
          mutate(analyte = 'dissolvedOxygen') %>%
          mutate(analyteUnits = 'milligramsPerLiter') %>%
          arrange(collectDate)
        
        ## only include analyte_data that has same dates to join
        analyte_dates<- analyte_data %>% 
         select(collectDate)
        analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
      }
      
      if (alist[i] == 'chlorophyll') {
        ##chlorophyll*
        chlor <- sensor.sen %>% 
          filter(!is.na(chlorophyll)) %>% 
          select(startDateTime,chlorophyll,chlorophyllExpUncert) %>% 
          #only want to keep data within 95% certainty
          mutate(confidenceL = (chlorophyllExpUncert/chlorophyll)*100) %>% 
          filter(confidenceL <= 5)
        chlor$startDateTime<- substr(chlor$startDateTime, 1, 10)
        ## organizing data
        analyte_dataB<- chlor %>%
          group_by(startDateTime) %>%
          summarise(mean(chlorophyll)) %>%
          mutate(collectDate = startDateTime) %>%
          mutate(analyteConcentration = `mean(chlorophyll)`) %>%
          select(-startDateTime, -`mean(chlorophyll)`) %>%
          mutate(analyte = 'chlorophyll') %>%
          mutate(analyteUnits = 'microgramsPerLiter') %>%
          arrange(collectDate)
        ## only include analyte_data that has same dates to join
        analyte_dates<- analyte_data %>%
          select(collectDate)
        analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
      }
      
      if (alist[i] == 'turbidity') {
        ##turbidity*
        turb <- sensor.sen %>% 
          filter(!is.na(turbidity)) %>% 
          select(startDateTime,turbidity,turbidityExpUncert) %>% 
          #only want to keep data within 95% certainty
          mutate(confidenceL = (turbidityExpUncert/turbidity)*100) %>% 
          filter(confidenceL <= 5)
        turb$startDateTime<- substr(turb$startDateTime, 1, 10)
        ## organizing data
        analyte_dataB<- turb %>%
          group_by(startDateTime) %>%
          summarise(mean(turbidity)) %>%
          mutate(collectDate = startDateTime) %>%
          mutate(analyteConcentration = `mean(turbidity)`) %>%
          select(-startDateTime, -`mean(turbidity)`) %>%
          mutate(analyte = 'turbidity') %>%
          mutate(analyteUnits = 'formazinNephelometricUnit') %>%
          arrange(collectDate)
        ## only include analyte_data that has same dates to join
        analyte_dates<- analyte_data %>%
          select(collectDate)
        analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
      }
      
      if (alist[i] == 'fDOM') {
        #fDOM*
        fDOM_data <- sensor.sen %>% 
          filter(!is.na(fDOM)) %>% 
          select(startDateTime,fDOM,fDOMExpUncert) %>% 
          #only want to keep data within 95% certainty
          mutate(confidenceL = (fDOMExpUncert/fDOM)*100) %>% 
          filter(confidenceL <= 5)
        fDOM_data$startDateTime<- substr(fDOM_data$startDateTime, 1, 10)
        ## organizing data
        analyte_dataB<- fDOM_data %>%
          group_by(startDateTime) %>%
          summarise(mean(fDOM)) %>%
          mutate(collectDate = startDateTime) %>%
          mutate(analyteConcentration = `mean(fDOM)`) %>%
          select(-startDateTime, -`mean(fDOM)`) %>%
          mutate(analyte = 'fDOM') %>%
          mutate(analyteUnits = 'quinineSulfateUnit') %>%
          arrange(collectDate)
        ## only include analyte_data that has same dates to join
        analyte_dates<- analyte_data %>%
          select(collectDate)
        analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
      }
      ## End of sensor data if statements-------------------------------
      
      ## Start of SWC parent data----------------------------------------
      if (alist[i] == 'waterTemp') {
        #water temp*
        wt <- External.data %>% 
          filter(!is.na(waterTemp)) %>% 
          select(collectDate,waterTemp)
        wt$collectDate<- substr(wt$collectDate, 1, 10)
        wt_data<- (unique(wt))
        ## organizing data
        analyte_dataB<- wt_data %>%
          mutate(analyte = 'waterTemp') %>%
          mutate(analyteUnits = 'celsius') %>%
          mutate(analyteConcentration = waterTemp) %>%
          select(-waterTemp) %>% 
          arrange(collectDate)
        ## only include analyte_data that has same dates to join
        analyte_dates<- analyte_data %>%
          select(collectDate)
        analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
      }## End of if statement for SWC-------------------------------------
      
      if (alist[i] == 'dissolvedOxygenField') {
        #field D.O.*
        do <- External.data %>% 
          filter(!is.na(dissolvedOxygenField)) %>% 
          select(collectDate,dissolvedOxygenField)
        do$collectDate<- substr(do$collectDate, 1, 10)
        do_data<- (unique(do))
        ## organizing data
        analyte_dataB<- do_data %>%
          mutate(analyte = 'dissolvedOxygenField') %>%
          mutate(analyteUnits = 'milligramsPerLiter') %>%
          mutate(analyteConcentration = dissolvedOxygenField) %>%
          #select(-dissolvedOxygen) %>%
          arrange(collectDate)
        analyte_dates<- analyte_data %>%
          select(collectDate)
        analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
      }## End of if statement for SWC-------------------------------------
      
      if (alist[i] == 'specificConductanceField') {
        #field specific conductance*
        sp <- External.data %>% 
          filter(!is.na(specificConductanceField)) %>% 
          select(collectDate,specificConductanceField)
        sp$collectDate<- substr(sp$collectDate, 1, 10)
        sp_data<- (unique(sp))
        ## organizing data
        analyte_dataB<- sp_data %>%
          mutate(analyte = 'specificConductanceField') %>%
          mutate(analyteUnits = 'microsiemensPerCentimeter') %>%
          mutate(analyteConcentration = specificConductanceField) %>%
          #select(-specificConductance) %>%
          arrange(collectDate)
        analyte_dates<- analyte_data %>%
          select(collectDate)
        analyte_dataB<- left_join(analyte_dates, analyte_dataB, by= 'collectDate')
      }## End of if statement for SWC-------------------------------------
      
      ## End of SWC parent data------------------------------------------
      analytes<- left_join(analyte_data, analyte_dataB, by= 'collectDate')
      
      analytes_f<- analytes %>% 
        filter(!is.na(analyteConcentration.y))

      correlation[[i,2]]<- cor(analytes_f$analyteConcentration.x, analytes_f$analyteConcentration.y)
      correlation[[i,1]]<- paste0(analytes_f$analyte.y[1])
      i=i+1
    }
    
    correlation<- correlation %>%
      arrange(desc(Correlation))
    
    i=1
    while (i < length(alist)+1 ) {
    if(is.na(correlation[[i,2]])) {
      correlation[[i,2]]<- ('Not enough data')
    }
      i=i+1
    }
    
    correlationA<- correlation %>%
      filter(Analyte != select.analyte)
  })
  ## End of Correlation Table-------
  
  # Render Correlation Table
  output$patterns<- renderDataTable({
    print(c_check())
  })
  
  # Start Modeling tab output (in progress)-------
  # Saving water chem file from global environment for mlr model
  mlr<- reactive({
    f_data <- fit_data
  })
  
  mlr_check <- reactive({
    
    #External.data <- site_select()
    select.analyte <- Analyte_select()
    f_data<- mlr()
    correlationA<- c_check()
    select.analyte<- input$Select_A
    
    ## Formating for MLR modeling-----
    if (select.analyte == 'TSS - Dry Mass') {
      select.analyte<- 'TSSD'
    }
    if (select.analyte == 'UV Absorbance (280 nm)') {
      select.analyte<- 'UV280'
    }
    if (select.analyte == 'UV Absorbance (250 nm)') {
      select.analyte<- 'UV250'
    }
    if (select.analyte == 'NO3+NO2 - N') {
      select.analyte<- 'NO3NO2'
    }
    if (select.analyte == 'NH4 - N') {
      select.analyte<- 'NH4'
    }
    if (select.analyte == 'Ortho - P') {
      select.analyte<- 'OrthoP'
    }
    if (select.analyte == 'pH') {
      select.analyte<- 'PH'
    }
    
    
    # fixing problematic formating
    find_me<- grep('TSS - Dry Mass', correlationA$Analyte, fixed = T)
    correlationA$Analyte[find_me]<- 'TSSD'
    find_me<- grep('UV Absorbance (280 nm)', correlationA$Analyte, fixed = T)
    correlationA$Analyte[find_me]<- 'UV280'
    find_me<- grep('UV Absorbance (280 nm)', correlationA$Analyte, fixed = T)
    correlationA$Analyte[find_me]<- 'UV250'
    find_me<- grep('NO3+NO2 - N', correlationA$Analyte, fixed = T)
    correlationA$Analyte[find_me]<- 'NO3NO2'
    find_me<- grep('NH4 - N', correlationA$Analyte, fixed = T)
    correlationA$Analyte[find_me]<- 'NH4'
    find_me<- grep('Ortho - P', correlationA$Analyte, fixed = T)
    correlationA$Analyte[find_me]<- 'OrthoP'
    find_me<- grep('pH', correlationA$Analyte, fixed = T)
    correlationA$Analyte[find_me]<- 'PH'
    ## end of name formatting-----
    
    # ## Prediciting value-
    # ## choose top 3 correlated values for MAIN analyte--
    f2_data<- f_data %>%
      select(correlationA[[1]][1],correlationA[[1]][2],correlationA[[1]][3], select.analyte)
    
    # define a task (what want to achieve) - Target is variable trying to predict
    ANCTask <- makeRegrTask(data = f2_data, target = select.analyte)
    # choose a Learner for model- listLearners("regr")$class
    lm <- makeLearner('regr.glm', predict.type = 'response')
    # must define task, learner, and train model
    lmModel<- train(lm, ANCTask)
    
    ##max
    max1<- max(f2_data[[1]])
    max2<- max(f2_data[[2]])
    max3<- max(f2_data[[3]])
    
    updateSelectizeInput(session, 'a1', correlationA$Analyte[1])
    updateSelectInput(session, 'a1', choices = c(0:max1))
    
    updateSelectizeInput(session, 'a2', correlationA$Analyte[2])
    updateSelectInput(session, 'a2', choices = c(0:max2))
    
    updateSelectizeInput(session, 'a3', correlationA$Analyte[3])
    updateSelectInput(session, 'a3', choices = c(0:max3))
    
    # testing
    # cross validation w/ 10 fold and 50 reps
    
    kFold<- makeResampleDesc("RepCV", fold = 10, reps = 30) # repeat cross validation ('RepCV')
    kFoldCV<- resample(learner = lm, task = ANCTask, resampling = kFold)
    
    mse.test.mean<- kFoldCV$aggr
    
  })

  output$predict <- renderText({
    print(mlr_check())
  })
  
  
  observeEvent(input$a1,{
    final_react<- reactive({
      ## value places for various analytes for pred()
      correlationA<- c_check()
      select.analyte <- Analyte_select()
      f_data<- mlr()
      correlationA<- c_check()
      select.analyte<- input$Select_A
      
      ## Formating for MLR modeling-----
      if (select.analyte == 'TSS - Dry Mass') {
        select.analyte<- 'TSSD'
      }
      if (select.analyte == 'UV Absorbance (280 nm)') {
        select.analyte<- 'UV280'
      }
      if (select.analyte == 'UV Absorbance (250 nm)') {
        select.analyte<- 'UV250'
      }
      if (select.analyte == 'NO3+NO2 - N') {
        select.analyte<- 'NO3NO2'
      }
      if (select.analyte == 'NH4 - N') {
        select.analyte<- 'NO3NO2'
      }
      if (select.analyte == 'Ortho - P') {
        select.analyte<- 'OrthoP'
      }
      if (select.analyte == 'pH') {
        select.analyte<- 'PH'
      }
      
      # fixing problematic formating
      find_me<- grep('TSS - Dry Mass', correlationA$Analyte, fixed = T)
      correlationA$Analyte[find_me]<- 'TSSD'
      find_me<- grep('UV Absorbance (280 nm)', correlationA$Analyte, fixed = T)
      correlationA$Analyte[find_me]<- 'UV280'
      find_me<- grep('UV Absorbance (280 nm)', correlationA$Analyte, fixed = T)
      correlationA$Analyte[find_me]<- 'UV250'
      find_me<- grep('NO3+NO2 - N', correlationA$Analyte, fixed = T)
      correlationA$Analyte[find_me]<- 'NO3NO2'
      find_me<- grep('NH4 - N', correlationA$Analyte, fixed = T)
      correlationA$Analyte[find_me]<- 'NO3NO2'
      find_me<- grep('Ortho - P', correlationA$Analyte, fixed = T)
      correlationA$Analyte[find_me]<- 'OrthoP'
      find_me<- grep('pH', correlationA$Analyte, fixed = T)
      correlationA$Analyte[find_me]<- 'PH'
      ## end of name formatting-----

      data2<- data1 %>%
        select(correlationA[[1]][1],correlationA[[1]][2],correlationA[[1]][3], select.analyte)
      
      data2[[1]]<- as.numeric(input$a1)
      data2[[2]]<- as.numeric(input$a2)
      data2[[3]]<- as.numeric(input$a3)

      # ## Prediciting value-
      # ## choose top 3 correlated values for MAIN analyte--
      f2_data<- f_data %>%
        select(correlationA[[1]][1],correlationA[[1]][2],correlationA[[1]][3], select.analyte)
      
      # define a task (what want to achieve) - Target is variable trying to predict
      ANCTask <- makeRegrTask(data = f2_data, target = select.analyte)
      # choose a Learner for model- listLearners("regr")$class
      lm <- makeLearner('regr.glm', predict.type = 'response')
      
      # must define task, learner, and train model
      lmModel<- train(lm, ANCTask)

      end_d<- predictLearner(.newdata =data2, .learner = lm, .model = lmModel)
      
      # empty Correlation Table to fill up w/ data
      end_data <- data.table(AnalyteEvaluated=character(),AnalytePrediction=numeric())

      end_data[[1,2]]<- as.numeric(end_d)
      end_data[[1,1]]<- paste0(select.analyte)
      end_data<- as.data.frame(end_data)
      
    })
    
    output$final<- renderDataTable({
      datatable(final_react())
    })
    
})  ## End of modeling tab-----



  
  
  
  #Readme - make into HTML
  output$appreadme<-renderUI({includeHTML('AppReadme.html')})
  
} #end of server