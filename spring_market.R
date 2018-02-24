setup<-function(){
  library(Rmisc)
  library("XML")
library(xml2)
library("stringr")
library("RCurl")
library("dplyr")
library("rvest")
library(sf)
library(ggplot2)
library(jsonlite)
library(httr)
library(rlist)
library(leaflet)
library(RColorBrewer)
library(ggthemes)
library('showtext')
library(scales)
library(grid)
library(Cairo)
}
#set environment.
# 1.load necessary packages
# 2.set textshow environment
# 3.pickup colors to use

setup()


showtext_auto()
font_add('SimSun','simsun.ttc')
font_add('fzxbs','C:/Windows/Fonts/方正小标宋_GBK.TTF')

setwd('~/GitHub/spring_festival_tourism_market')

pal.income <- brewer.pal(9,name = 'Reds')
pal.visitor<-brewer.pal(9,name = 'GnBu')

#don't need
display.brewer.all()
install.packages('Rmisc')

#prepare data
spri.data <- read.csv('spring_data.csv',stringsAsFactors = FALSE)[,-1]

#input mapfile
china.map<-st_read('china.geojson',stringsAsFactors = FALSE)
Encoding(china.map$name)<-'UTF-8'
capital<-st_read('省会.shp')
#merge files,sdm stands for spring-data-map
sdm <- merge(spri.data,china.map,by.x = 'province',by.y = 'name',all = TRUE) %>% 
       st_sf(stringsAsFactors = FALSE,crs = 4326,sf_column_name = 'geometry')


sdm$income<-as.numeric(sdm$income)
sdm$gr.i<-as.numeric(sdm$gr.i)
sdm$vistor<-as.numeric(sdm$vistor)
sdm$gr.v<-as.numeric(sdm$gr.v)
# visiualization


  theme_clean <- function(base_size=12){

    theme(
      
      axis.title = element_blank(),
      
      axis.text = element_blank(),
      
      panel.background = element_blank(),
      
      panel.grid = element_blank()
    )
    
  }
  
  frame_1<-theme_economist()+
                 theme(legend.position="right", 
                 
                 axis.title = element_blank(),
                 
                 axis.text = element_blank(),
                 
                 panel.background = element_blank(),
                 
                 panel.grid = element_blank())
           
    
           
  
total.income <-ggplot(sdm)+
               geom_sf(aes(fill = income))+
               scale_fill_gradient(name='单位:亿元',low = pal.income[1] , high = pal.income[9] ,
                                    guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))+
               ggtitle('2018年春节旅游市场',subtitle='旅游收入')
               

gr.i <-ggplot(sdm)+
       geom_sf(aes(fill=gr.i))+
       scale_fill_gradient(name='%',low = pal.income[1] , high = pal.income[9] ,
                      guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))+
       ggtitle('2018年春节旅游市场',subtitle='旅游收入增长率')

visitor<-ggplot(sdm)+
         geom_sf(aes(fill=as.numeric(sdm$vistor)))+
         scale_fill_gradient(name='万人次',low = pal.visitor[1] , high = pal.visitor[9] ,
                      guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))+
         ggtitle('2018年春节旅游市场',subtitle='旅游人次')
      
gr.v<-ggplot(sdm)+
      geom_sf(aes(fill=as.numeric(sdm$gr.v)))+
      scale_fill_gradient(name='%',low = pal.visitor[1] , high = pal.visitor[9] ,
                      guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))+
      ggtitle('2018年春节旅游市场',subtitle='旅游人次增长率')
          

p1<-total.income+frame_1
p2<-gr.i+frame_1
p3<-visitor+frame_1
p4<-gr.v+frame_1

p1
  
  ggsave(plot = p1, file = "spring.pdf", device = cairo_pdf, family = "GB1")
multiplot(p1, p2, p3, p4, cols=2)
ggsave('spring-festival.png')
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout = function(x,y)viewport(layout.pos.row = x,layout.pos.col = y)
print(p1,vp = vplayout(1,1))
print(p2,vp = vplayout(1,2))
print(p3,vp = vplayout(2,1))
print(p4,vp = vplayout(2,2))
ggsave("test_layout.png")
dev.off()