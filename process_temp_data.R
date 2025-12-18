rm(list = ls())
library(prism)
library(lubridate)
library(raster)
library(stringr)



dates_needed<-seq(from=as.Date("1981-01-01"),to=as.Date("2025-10-31"),by="day")

for(i in 9750:length(dates_needed)){
  prism_set_dl_dir("D:/Joe/temp/prism_temp/tmin_raw")
  
  get_prism_dailys(type="tmin",resolution = "4km",minDate = dates_needed[i],maxDate = dates_needed[i],keepZip = F)
  
  curfile_n<-list.files("D:/Joe/temp/prism_temp/tmin_raw",recursive = T,full.names = T,pattern = "*.bil$")
  what<-raster(curfile_n[i])
  what<-as.data.frame(what, xy = TRUE)
  colnames(what)<-c("lon","lat","tmin")
  
  
  prism_set_dl_dir("D:/Joe/temp/prism_temp/tmax_raw")
  
  get_prism_dailys(type="tmax",resolution = "4km",minDate = dates_needed[i],maxDate = dates_needed[i],keepZip = F)
  
  curfile_n<-list.files("D:/Joe/temp/prism_temp/tmax_raw",recursive = T,full.names = T,pattern = "*.bil$")
  what2<-raster(curfile_n[i])
  what2<-as.data.frame(what2, xy = TRUE)
  colnames(what2)<-c("lon","lat","tmax")
  
  what<-merge(what,what2,by=c("lon","lat"),all.x=T)
  
  
  what<-what[what$lon<= -110 & what$lon>= -125 & what$lat>= 30 & what$lat<= 45 & !is.na(what$tmin) & !is.na(what$tmax),]
  what$date<-as.character(dates_needed[i])
  what$geom_point<-paste0("POINT (",round(what$lon,3)," ",round(what$lat,3),")")
  
  write.csv(what,file=paste0("D:/Joe/temp/prism_temp/temp_redivis/",as.character(dates_needed[i]),".csv"),row.names = F)
}

