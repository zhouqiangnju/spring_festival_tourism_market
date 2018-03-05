library(shiny)
library(RColorBrewer)
library(sf)
library(ggplot2)
library(showtext)

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
      helpText('Select a type to see what happened during  7-day spring festival holiday.')
      br(),
      checkboxGroupInput('type',
                         h3('type of graph'),
                         choices = list('Total Tourism Income'= 'income',
                                        'Growth Rate of Total Tourism Income'= 'gr.i',
                                        'Visitor Number' = 'visitor',
                                        'Growth Rate of Visitor Number' = 'gr.v'),
                         selected = 'income')
    )
    mainPanel(
      plotOutput('map')
      
    )
  )
)

server<-function(input,output){
  
   output$map<-renderPlot({ggplot(sdm)+
     geom_sf(aes(fill = input$type))+
     scale_fill_gradient(name='billion RMB',low = pal.income[1] , high = pal.income[9] ,
                         guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))
     +frame_1}) 
}

shinyApp(ui,server)
