setup<-function(){
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
}
#set environment.
# 1.load necessary packages
# 2.set textshow environment
# 3.pickup colors to use

setup()

showtext_auto()
font_add('SimSun','simsun.ttc',"KaiTi_GB2312")
setwd('~/GitHub/spring_festival_tourism_market')

pal.income <- brewer.pal(9,name = 'Reds')
pal.gr.i<-brewer.pal(9,name = 'Oranges')

#don't need
url<-'http://mp.weixin.qq.com/s?__biz=MzI4MjU2ODM5Mg==&mid=100001413&idx=1&sn=034863c69be1c0b050e46881bd4762b3&chksm=6b96b5225ce13c349ef59854488cd2518a2762d8036fe551c9f79644671ba054a6990e46534b&mpshare=1&scene=23&srcid=0222dIIYThF8wPtsDeD0NnCB#rd'
content <- url %>% getURL(.encoding = 'utf8') %>% readHTMLTable(header = TRUE) %>% '[['(1) 
content <- readHTMLTable(url,header = TRUE,colClasses = c('integer','character','numeric','Percent','numeric','Percent'),.encoding = 'utf8') %>% '[['(1) 
content[is.na(content)]<-c(0)

display.brewer.all()

#prepare data
spri.data <- read.csv('spring_data.csv',stringsAsFactors = FALSE)[,-1]

#input mapfile
china.map<-st_read('china.geojson',stringsAsFactors = FALSE)

capital<-st_read('鐪佷細.shp')
#merge files,sdm stands for spring-data-map
sdm <- merge(spri.data,china.map,by.x = 'province',by.y = 'name',all = TRUE) %>% 
       st_sf(stringsAsFactors = FALSE,crs = 4326,sf_column_name = 'geometry')

Encoding(sdm$province)<-'UTF8'

# visiualization


  theme_clean <- function(base_size=12){
    
    require(grid)
    theme(
      
      axis.title = element_blank(),
      
      axis.text = element_blank(),
      
      panel.background = element_blank(),
      
      panel.grid = element_blank()
    )
    
  }
  
  model<-ggplot(sdm)+ theme(text = element_text(family='SimSun'))
  aftermath<-theme_economist()+theme_clean()+theme(legend.position="right")
  
total.income <- ggplot()+
                geom_sf(data = sdm,aes(fill = as.numeric(sdm$income)))+
                scale_fill_gradient(name='旅游收入(亿元)',low = pal[1] , high = pal[8] ,
                                    guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))+
                ggtitle('2018年春节全国旅游市场')+
                theme(text = element_text(family='SimSun',size = 15,hjust = 0.5))

gr.i <-model+geom_sf(aes(fill=as.numeric(sdm$gr.i)))+
      scale_fill_gradient(name='收入增长率',low = pal.gr.i[2] , high = pal.gr.i[8] ,
                      guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))

visitor<-model+geom_sf(aes(fill=as.numeric(sdm$vistor)))+
         scale_fill_gradient(name='接待游客',low = pal.gr.i[2] , high = pal.gr.i[8] ,
                      guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))
      
gr.v<-model+geom_sf(aes(fill=as.numeric(sdm$gr.v)))+
      scale_fill_gradient(name='游客增长率',low = pal.gr.i[2] , high = pal.gr.i[8] ,
                      guide = 'colorbar',na.value = 'grey50',breaks=pretty_breaks(n=5))
          
total.income<-total.income+theme_economist()+theme_clean()+theme(legend.position="right")

gr.i.map<-gr.i+aftermath
visitor.map<- visitor+aftermath
gr.v.map<-gr.v+aftermath
gr.v.map
visitor.map
