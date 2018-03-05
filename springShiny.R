library(shiny)
library(RColorBrewer)
library(sf)
library(ggplot2)
library(showtext)
library(scales)
library(dplyr)
setwd('~/GitHub/spring_festival_tourism_market')
spri.data<-read.csv('F:/Administrator/Documents/GitHub/spring_festival_tourism_market/spring_data.csv')[,-1]
china.map<-st_read('china.geojson',stringsAsFactors = FALSE)
Encoding(china.map$name)<-'UTF-8'

sdm <- merge(spri.data,china.map,by.x = 'province',by.y = 'name',all = TRUE) %>% 
  st_sf(stringsAsFactors = FALSE,crs = 4326,sf_column_name = 'geometry')


sdm$income<-as.numeric(sdm$income)
sdm$gr.i<-as.numeric(sdm$gr.i)
sdm$vistor<-as.numeric(sdm$vistor)
sdm$gr.v<-as.numeric(sdm$gr.v)


pal.income <- brewer.pal(9,name = 'Reds')
pal.visitor<-brewer.pal(9,name = 'GnBu')

frame_1<-
  
  theme(legend.position="right", 
        
        axis.title = element_blank(),
        
        axis.text = element_blank(),
        
        panel.background = element_blank(),
        
        panel.grid = element_blank())

#shiny code
ui <- fluidPage(
  titlePanel("China's Tourism Market of 2018 Spring Festival"),
  
  sidebarLayout(
    sidebarPanel (
      helpText('Select a type to see what happened during  7-day spring festival holiday.'),
      br(),
      selectInput('type',
                         h3('type of graph'),
                         choices = list('Total Tourism Income',
                                        'Growth Rate of Total Tourism Income',
                                        'Visitor Number' ,
                                        'Growth Rate of Visitor Number' ),
                         selected = 'gr.i')
    ),
    mainPanel(
      plotOutput('map')
      
    )
  )
)

server<-function(input,output){
  
  
  
   output$map<-renderPlot({
     fill.data<-data.frame()
     fill.data<-switch(input$type,
                       'Total Tourism Income' = sdm$income,
                       'Growth Rate of Total Tourism Income' = sdm$gr.i,
                       'Visitor Number' = sdm$vistor,
                       'Growth Rate of Visitor Number' = sdm$gr.v)
     
     
     ggplot(sdm)+
     geom_sf(aes(fill = fill.data))+
     scale_fill_gradient(name='billion RMB',low = pal.income[1] , high = pal.income[9] ,
                         guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))+
       frame_1
     }) 
}

shinyApp(ui,server,options = 'test.mode')
