###############################################################################################################
##########################################  ERA5 potential evap data   ##############################################
######################################################################################################################
rm(list = ls())

library(ecmwfr)
wf_set_key(user = "?","PUT API KEY HERE")
for(year in 1981:2024){
  for(month in 1:12){
    request <- list(
      variable= "potential_evaporation",
      area = c(45,-125,30,-110), #[what$lon<= -110 & what$lon>= -125 & what$lat>= 30 & what$lat<= 45
      product_type=c("reanalysis"),
      daily_statistic= "daily_sum",
      time_zone="utc-07:00",
      frequency= "1_hourly",
      day=as.character(1:31),
      month= as.character(month),  #c("01", "02", "03","04", "05", "06","07", "08", "09", "10", "11", "12"),
      year = as.character(year),
      #time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
      format = "netcdf",
      dataset_short_name = "derived-era5-single-levels-daily-statistics",
      target = paste0("daily_","potential_evap_",year,"_",month,".nc")
    )
    curfiles<-list.files("D:/Joe/evap/era5_pet/raw_ncs")
    if(!(request$target %in% curfiles)){ 
      ncfile <- wf_request(user="17141",
                           request = request,   
                           transfer = F,  
                           path = "D:/Joe/evap/era5_pet/raw_ncs",
                           verbose = FALSE,
                           time_out = 3600)
      Sys.sleep(222)
      wf_transfer(ncfile$get_url(),user="17141", path = "D:/Joe/evap/era5_pet/raw_ncs"
                  ,filename = paste0("daily_","potential_evap_",year,"_",month,".nc"))
    }
  }
}

library(ncdf4)
library(ncdf4.helpers)

for(year in 1981:2024){
  for(month in 1:12){
    currentWeatherdata<-nc_open(paste0("D:/Joe/evap/era5_pet/raw_ncs/","daily_","potential_evap_",year,"_",month,".nc"))
    
    
    londata<-ncvar_get(currentWeatherdata,varid = "longitude")
    latdata<-ncvar_get(currentWeatherdata,varid = "latitude")
    timedata<-ncvar_get(currentWeatherdata,varid = "valid_time")
    tempdata<-ncvar_get(currentWeatherdata,varid = "pev") #evap is negative and condensation is positive
    
    evap_dat<-expand.grid(londata,latdata,timedata)
    nc_close(currentWeatherdata)
    dimnames(tempdata)<-list(longitude=londata,latitude=latdata,valid_time=timedata)
    
    evap_dat<-as.data.frame.table(tempdata, responseName = "Value")
    
    evap_dat$date<-paste0(year,"-",month,"-",as.numeric(as.character(evap_dat$valid_time))+1)
    evap_dat$date<-as.character(as.Date(evap_dat$date))
    
    evap_dat$geom_point<-paste("POINT(",as.character(evap_dat$longitude),as.character(evap_dat$latitude),")")
    evap_dat$valid_time<-NULL
    evap_dat$longitude<-as.numeric(as.character(evap_dat$longitude))
    evap_dat$latitude<-as.numeric(as.character(evap_dat$latitude))
    write.csv(evap_dat,paste0("D:/Joe/evap/era5_pet/for_redivis/","daily_","potential_evap_",year,"_",month,".csv"),row.names = F)
  }
}



