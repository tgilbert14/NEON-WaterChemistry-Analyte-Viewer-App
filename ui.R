#---user interface--------------------------------------------------  
# Timothy Gilbert 1/21/2021
# This script is the user interface of the shiny app

ui <- dashboardPage(skin = 'blue',
                        header=dashboardHeader(title='Analyte Compare (SWC)'),
  dashboardSidebar(
    useShinyalert(),
      selectInput("Select", "Type and Select Field Site:",
                  choices = c(neon_sites),
                  selected = F, multiple = T),
      dateRangeInput('dateRange',label='Select Date Range (YYYY-MM)',format = "yyyy-mm",start=Sys.Date()-(930),end=Sys.Date()-730, startview = "year"),
      selectInput("Select_A", "Main Analytes:",
                  choices = c(analyte_list),
                  selected = F, multiple = F),
      bsAlert('alert1'),
      selectInput("Select_B", "Secondary Analytes:",
                  choices = c(analyte_list),
                  selected = F, multiple = F),
      submitButton("Process Selection(s)"),
      menuItem('About',tabName='readme',icon=icon('info-circle')),
    ## Sidebar Menu
      sidebarMenu(
        
        menuItem('Comparison Plot',tabName = 'E',icon=icon('chart-area')),
        menuItem('Correlation Report',tabName = 'C',icon=icon('newspaper')),
        menuItem('Regression Analysis',tabName = 'D',icon=icon('laptop-code')),
        menuItem('Analyte Predictor',tabName = 'model', icon=icon('robot')),
        
        menuItem('Analyte Forcasting',tabName = 'predict', icon=icon('chart-line')),
        
        menuItem('Data Table',tabName = 'dtable',icon=icon('table'))
        
      )
    ),
  ## Body  
    dashboardBody("Choose a site and two analytes to compare!",
                  
                  use_theme(mytheme),
            tabItems(  
                ## Comparison plot
                tabItem(tabName = 'E',
                        fluidRow(
                          shinydashboard::box(title='Analyte Comparisons',
                              footer = 'Relationship through time between analytes taken from Water Chemistry Samples between selected dates
                              (An error above may indicate no data for selected dates above)',
                              status = 'info',
                              collapsible = T,
                              collapsed = F,
                              solidHeader = F,
                              height='650',
                              width='12',
                              column(12,withSpinner(plotlyOutput('Elab'), type = 8, color = 'purple', proxy.height = '500px'),
                                     ##old spinner - image = 'https://i.pinimg.com/originals/52/16/80/5216809ff35e0daf8bcada59fa04f3c4.gif',  
                                     ##image.height = '750px', image.width = '1000px',
                                     style='height:500px;overflow-y:scroll'
                                )
                              )
                            )
                          ),
                ## Correlations Table
                tabItem(tabName = 'C',
                        fluidRow(
                          shinydashboard::box(title='Correlation Report',
                              footer = 'Calculating correlations between main analyte to the others between selected dates
                              (An error above may indicate no data for selected dates above)',
                              status = 'info',
                              collapsible = T,
                              collapsed = F,
                              solidHeader = F,
                              height='650',
                              width='12',
                              column(12,withSpinner(dataTableOutput('patterns'), type = 4, color = 'purple', proxy.height = '500px'),
                                     ##image = 'https://i.pinimg.com/originals/52/16/80/5216809ff35e0daf8bcada59fa04f3c4.gif',  
                                     ##image.height = '750px', image.width = '1000px',
                                     style='height:500px;overflow-y:scroll'
                              )
                          )
                        )
                ),
                ## P-value Plot
                tabItem(tabName = 'D',
                        fluidRow(
                          shinydashboard::box(title='Analyte Statistical Analysis
                                        (An error above may indicate no data for selected dates above)',
                              footer = 'Linear Regression Plot',
                              status = 'info',
                              collapsible = T,
                              collapsed = F,
                              solidHeader = F,
                              height='550',
                              width='12',
                              column(12,withSpinner(plotlyOutput('Dlab'), image = 'https://i.pinimg.com/originals/52/53/35/52533552584f2c81e63ee15a2f4ee468.gif', image.height = '800px'),
                                     
                                     style='height:400px;overflow-y:scroll'
                              )
                          ),
                          
                          shinydashboard::box(title='Analyte P-value summary',
                              footer = 'Summary of P-value by Analyte Concentrations by Date
                                        (An error above may indicate no data for selected dates above)',
                              status = 'info',
                              collapsible = T,
                              collapsed = F,
                              solidHeader = F,
                              height='500',
                              width='12',
                              column(12,withSpinner(verbatimTextOutput('PvalueSUM'), image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif', image.height = '600px', image.width = '1200px', proxy.height = '300px'),
                                     
                                     style='height:400px;overflow-y:scroll'
                              )
                          ),
                        )
                ),
                ## Data Table
                tabItem(tabName = 'dtable',
                        fluidRow(
                          shinydashboard::box(title='Data Table',
                              footer = 'Data table of relationship being plotted for Analyte Analysis
                              (An error above may indicate no data for selected dates above)',
                              status = 'info',
                              collapsible = T,
                              collapsed = F,
                              solidHeader = F,
                              height='650',
                              width='12',
                              column(12,withSpinner(dataTableOutput('table'), image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',  image.height = '600px', image.width = '1000px', proxy.height = '1000px'),
                                     
                                     style='height:500px;overflow-y:scroll'
                              )
                          )
                        )
                ),
                ## MLR model - Analyte predictor
                tabItem(tabName = 'model',
                        fluidRow(
                          shinydashboard::box(title = 'Calculating Top 3 Correlated Analytes:',
                              selectInput('a1','', choices = NULL),
                              #numericInput('a1','analyte1',''),
                              selectInput('a2','', choices = NULL),
                              selectInput('a3','', choices = NULL),
                              submitButton('Update'),footer = 'Select analye values above [0 to maxValue] 
                              and "Update" to get predicted value of "Main Analyte" - based on machine 
                              learning model'),
                          shinydashboard::box(title='Mean Squared Error (MSE)',
                              footer = 'MSE is the average square of the difference between actual and 
                              estimated values, the lower the value the better (zero meaning perfect model). 
                              Machine Learning model using mlr package for choosen "main" analyte: Model = 
                              "regr.glm" [percent based on CrossValidation w/10 fold, 30 reps]',
                              status = 'info',
                              collapsible = T,
                              collapsed = F,
                              solidHeader = F,
                              height='287',
                              width='6',
                              column(6,withSpinner(textOutput('predict'),type = 4, color = 'purple',proxy.height = '200px')
                                     ##image = 'https://media.giphy.com/media/yGhIqFuOx84KY/source.gif',  
                                     ##image.height = '600px', image.width = '1000px',
                                     #style='height:500px;overflow-y:scroll'
                              )
                          ),
                          shinydashboard::box(title = 'Predicted Value for Main Analyte:',
                              status = 'info',
                              collapsible = T,
                              collapsed = F,
                              solidHeader = F,
                              height='150',
                              width='12',
                              column(3,withSpinner(dataTableOutput('final'),type = 4, color = 'purple',proxy.height = '200px'))
                          )
                        )
                ),
                
                ## Predict
                tabItem(tabName = 'predict',
                        fluidRow(
                          shinydashboard::box(title='Analyte Forcast',
                                              footer = 'Forcasting Main Analyte Through Time',
                                              status = 'info',
                                              collapsible = T,
                                              collapsed = F,
                                              solidHeader = F,
                                              height='650',
                                              width='12',
                                              column(12,withSpinner(plotOutput('per'), type = 8, color = 'purple', proxy.height = '500px'),
                                                     ##old spinner - image = 'https://i.pinimg.com/originals/52/16/80/5216809ff35e0daf8bcada59fa04f3c4.gif',  
                                                     ##image.height = '750px', image.width = '1000px',
                                                     style='height:500px;overflow-y:scroll'
                                              )
                          )
                        )
                ),
                
                ## Readme
                tabItem(tabName='readme',
                        htmlOutput('appreadme')
                )
                
            )
    )
)
