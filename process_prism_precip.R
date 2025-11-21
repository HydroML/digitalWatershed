rm(list = ls())
library(prism)
library(lubridate)
library(raster)

prism_set_dl_dir("D:/Joe/precip/prism_precip/precip_raw")

dates_needed<-seq(from=as.Date("1981-01-01"),to=as.Date("2025-10-31"),by="day")

for(i in 12080:length(dates_needed)){
  get_prism_dailys(type="ppt",resolution = "4km",minDate = dates_needed[i],maxDate = dates_needed[i],keepZip = F)
  
  curfile_n<-list.files("D:/Joe/precip/prism_precip/precip_raw",recursive = T,full.names = T,pattern = "*.bil$")
  what<-raster(curfile_n[i])
  what<-as.data.frame(what, xy = TRUE)
  colnames(what)<-c("lon","lat","precip")
  
  #whichlon<-which((londata[2,]>= -130 & londata[2,]<= -105) | (londata[1,]>= -130 & londata[1,]<= -105))
  #whichlat<-which((latdata[2,]>= 25 & latdata[2,]<= 50) | (latdata[1,]>= 25 & latdata[1,]<= 50))
  what<-what[what$lon<= -110 & what$lon>= -125 & what$lat>= 30 & what$lat<= 45 & !is.na(what$precip),]
  write.csv(what,file=paste0("D:/Joe/precip/prism_precip/precip_processed/",as.character(dates_needed[i]),".csv"),row.names = F)
}

