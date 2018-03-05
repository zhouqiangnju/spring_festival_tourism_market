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
  library(showtextdb)
}
#set environment.
# 1.load necessary packages
# 2.set textshow environment
# 3.pickup colors to use

setup()



font_add('SimSun','simsun.ttc')
font_add('msyh',regular = 'msyh.ttc',
                bold = 'msyhbd.ttc')


setwd('~/GitHub/spring_festival_tourism_market')

pal.income <- brewer.pal(9,name = 'Reds')
pal.visitor<-brewer.pal(9,name = 'GnBu')

#don't need
display.brewer.all()


#prepare data
spri.data <- read.csv('spring_data.csv',stringsAsFactors = FALSE)[,-1]

#input mapfile
china.map<-st_read('china.geojson',stringsAsFactors = FALSE)
Encoding(china.map$name)<-'UTF-8'

#merge files,sdm stands for spring-data-map
sdm <- merge(spri.data,china.map,by.x = 'province',by.y = 'name',all = TRUE) %>% 
       st_sf(stringsAsFactors = FALSE,crs = 4326,sf_column_name = 'geometry')


sdm$income<-as.numeric(sdm$income)
sdm$gr.i<-as.numeric(sdm$gr.i)
sdm$vistor<-as.numeric(sdm$vistor)
sdm$gr.v<-as.numeric(sdm$gr.v)
# visiualization

  
  frame_1<-
    
           theme(legend.position="right", 
                 
                 axis.title = element_blank(),
                 
                 axis.text = element_blank(),
                 
                 panel.background = element_blank(),
                 
                 panel.grid = element_blank())+
                 
                 theme_economist()
 
total.income <-ggplot(sdm)+
               geom_sf(aes(fill = income))+
               scale_fill_gradient(name='单位：亿元',low = pal.income[1] , high = pal.income[9] ,
                                    guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))+
               ggtitle('2018春节旅游市场')+
               theme(text = element_text(family = 'msyh'))
cairo_pdf('spring2.pdf')
showtext_auto()
total.income+frame_1
dev.off()           
gr.i <-ggplot(sdm)+
       geom_sf(aes(fill=gr.i))+
       scale_fill_gradient(name='%',low = pal.income[1] , high = pal.income[9] ,
                      guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))+
       ggtitle('2018年春节旅游市???',subtitle='旅游收入增长???')

visitor<-ggplot(sdm)+
         geom_sf(aes(fill=as.numeric(sdm$vistor)))+
         scale_fill_gradient(name='万人???',low = pal.visitor[1] , high = pal.visitor[9] ,
                      guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))+
         ggtitle('2018年春节旅游市???',subtitle='旅游人次')
      
gr.v<-ggplot(sdm)+
      geom_sf(aes(fill=as.numeric(sdm$gr.v)))+
      scale_fill_gradient(name='%',low = pal.visitor[1] , high = pal.visitor[9] ,
                      guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))+
      ggtitle('2018年春节旅游市???',subtitle='旅游人次增长???')
          

p1<-total.income+frame_1
p2<-gr.i+frame_1
p3<-visitor+frame_1
p4<-gr.v+frame_1
cairo_pdf('fonttest4.pdf')
showtext_auto()
p+frame_1

dev.off()
  
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