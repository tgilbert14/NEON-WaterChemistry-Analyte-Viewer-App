## USGS data - Sycamore Creek...
##### 
# Sites in this file include:
# USGS 09510200 SYCAMORE CREEK NEAR FORT MCDOWELL, AZ
# 
# Explanation of Parameter Code and ts_id used in the Statistics Data 
# parameter_cd	Parameter Name					ts_id	Location Name
# 00060		Discharge, cubic feet per second		5732	
#
# Data heading explanations.
# begin_yr_dt ... First complete water year of data of daily mean values for this day.
# end_yr_dt   ... Last complete water year of data of daily mean values for this day.
# max_va      ... Maximum of daily mean values for this day.
# min_va      ... Minimum of daily mean values for this day.
# mean_va     ... Mean of daily mean values for this day.
# p05_va      ... 05 percentile of daily mean values for this day.
# p10_va      ... 10 percentile of daily mean values for this day.
# p20_va      ... 20 percentile of daily mean values for this day.
# p25_va      ... 25 percentile of daily mean values for this day.
# p50_va      ... 50 percentile (median) of daily mean values for this day.
# p75_va      ... 75 percentile of daily mean values for this day.
# p80_va      ... 80 percentile of daily mean values for this day.
# p90_va      ... 90 percentile of daily mean values for this day.
# p95_va      ... 95 percentile of daily mean values for this day.
#####

#library(tidyverse)
usgs<- read_csv('usgs_data.csv',progress = T)
View(usgs)

# getting rid of 'extra' data
u<- usgs[-1,]
View(usgs)

usgs<- read_csv('usdsD2016.csv')
View(usgs)
u<- usgs

discharge<- u %>% 
  select(month_nu, day_nu, mean_va)

#class(discharge$month_nu)
discharge$month_nu<- as.numeric(discharge$month_nu)
discharge$day_nu<- as.numeric(discharge$day_nu)
discharge$mean_va<- as.numeric(discharge$mean_va)
#discharge[,1]

discharge
dates<- c('Jan','Feb','March','April','May','June','July','Aug','Sept','Oct','Nov','Dec')
i=1

while(i < length(dates)+1){
  discharge$month_nu[discharge$month_nu==i]<- dates[i]
  i=i+1
}

View(discharge)
#library(plotly)
#library(RColorBrewer)

# making color pallete for data
colourCount = length(unique(discharge$month_nu))
getPalette = colorRampPalette(brewer.pal(9, "Set3"))

d.plot<- plot_ly() # plotting first analyte selcetion from SWC data
d.plot<- d.plot %>% add_trace(data=discharge,
                              type='scatter',
                              mode='lines+markers',
                              x=~day_nu,
                              y=~mean_va,
                              name =~month_nu,
                              #color=I("blue"),
                              colors=getPalette(colourCount),
                              alpha = .8,
                              #create custom hovertext
                              text=~paste0("Discharge Daily Mean: ",mean_va, '\n',"Date: ",month_nu,' ',day_nu), 
                              hoverinfo='text'
)%>%
  layout(title=paste0('USGS Discharge by Month [',u$begin_yr[[1]],']'),yaxis=list(title='Discharge(cubic feet per second)'),xaxis=list(title='Day of Year'))
d.plot




usgs<- read_csv('usdsD2017.csv')
View(usgs)
u<- usgs

discharge2<- u %>% 
  select(month_nu, day_nu, mean_va)

#class(discharge$month_nu)
discharge2$month_nu<- as.numeric(discharge2$month_nu)
discharge2$day_nu<- as.numeric(discharge2$day_nu)
discharge2$mean_va<- as.numeric(discharge2$mean_va)
#discharge[,1]

discharge2
dates<- c('Jan','Feb','March','April','May','June','July','Aug','Sept','Oct','Nov','Dec')
i=1

while(i < length(dates)+1){
  discharge2$month_nu[discharge2$month_nu==i]<- dates[i]
  i=i+1
}



# adding secondary analyte to opposite y-axis (y2)
d.plot<- d.plot%>% add_trace(data=discharge2, x=~day_nu,y=~mean_va,name=~month_nu, mode='lines', inherit = T, type='scatter', alpha=.5, 
                             colors =  getPalette(colourCount))
d.plot
