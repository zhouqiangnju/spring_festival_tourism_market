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
setwd('F:/Administrator/Documents/GitHub/spring_festival_tourism_market')
#don't need
url<-'http://mp.weixin.qq.com/s?__biz=MzI4MjU2ODM5Mg==&mid=100001413&idx=1&sn=034863c69be1c0b050e46881bd4762b3&chksm=6b96b5225ce13c349ef59854488cd2518a2762d8036fe551c9f79644671ba054a6990e46534b&mpshare=1&scene=23&srcid=0222dIIYThF8wPtsDeD0NnCB#rd'
content <- url %>% getURL(.encoding = 'utf8') %>% readHTMLTable(header = TRUE) %>% '[['(1) 
content <- readHTMLTable(url,header = TRUE,colClasses = c('integer','character','numeric','Percent','numeric','Percent'),.encoding = 'utf8') %>% '[['(1) 
content[is.na(content)]<-c(0)


province.list<-read.csv('F:/Administrator/Documents/R/Map/BaiduGeoTable/省adcode.csv',stringsAsFactors = FALSE)[,2] %>% 
               substr(1,3) %>% str_replace('省|市|壮|自|回|维','')

#prepare data
spri.data <- read.csv('spring_data.csv',stringsAsFactors = FALSE)[,-1]


#input mapfile
china.map<-st_read('F:/Administrator/Documents/R/Map/china.geojson',stringsAsFactors = FALSE)

capital<-st_read('省会.shp')
#merge files,sdm stands for spring-data-map
sdm <- merge(spri.data,china.map,by.x = 'province',by.y = 'name',all = TRUE) %>% 
       st_sf(stringsAsFactors = FALSE,crs = 4326,sf_column_name = 'geometry')
sdm$income <-as.numeric(sdm$income)
Encoding(sdm$province)<-'UTF8'
# visiualization
pal <- 

  theme_clean <- function(base_size=12){
    
    require(grid)
    theme(
      
      axis.title = element_blank(),
      
      axis.text = element_blank(),
      
      panel.background = element_blank(),
      
      panel.grid = element_blank()
    )
    
}
total.income <- ggplot()+
                geom_sf(data = sdm,aes(fill = income ),show.legend = T)
             
         
               
                

                geom_text(data=sdm,aes(label = province),size=3)
total.income+theme_wsj() + scale_colour_wsj("colors6", "")+theme_clean()
plot(capital)
str(is.na(province.map$NAME))
plot(province.map[is.na(province.map$NAME),])
write.csv(spri.data,'spring_data.csv')
  as.character(content[1,])
names(content) <- as.character(content[1,])
names(content)
