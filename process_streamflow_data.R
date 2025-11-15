rm(list = ls())
gc()

library(dataRetrieval)
library(lubridate)


HYSETS_watershed_properties <- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/HYSETS_watershed_properties.txt")
HYSETS_watershed_properties<-HYSETS_watershed_properties[HYSETS_watershed_properties$Source=="USGS",]

HYSETS_watershed_properties<-HYSETS_watershed_properties[HYSETS_watershed_properties$Hydrometric_station_longitude>= -130 | HYSETS_watershed_properties$Centroid_Lon_deg_E>= -130,]
HYSETS_watershed_properties<-HYSETS_watershed_properties[HYSETS_watershed_properties$Hydrometric_station_longitude<= -110 | HYSETS_watershed_properties$Centroid_Lon_deg_E<= -110,]

HYSETS_watershed_properties<-HYSETS_watershed_properties[HYSETS_watershed_properties$Hydrometric_station_latitude>= 25 | HYSETS_watershed_properties$Centroid_Lat_deg_N>= 25,]
HYSETS_watershed_properties<-HYSETS_watershed_properties[HYSETS_watershed_properties$Hydrometric_station_latitude<= 45 | HYSETS_watershed_properties$Centroid_Lat_deg_N<= 45,]


#extra site information
HYSETS_watershed_properties$time_zone<-NA
HYSETS_watershed_properties$location_acc<-NA
HYSETS_watershed_properties$state_name<-NA
HYSETS_watershed_properties$altitude_USGS<-NA
HYSETS_watershed_properties$site_type<-NA
HYSETS_watershed_properties$drainage_area_USGS<-NA

curdone<-list.files("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Minute_data/",full.names = T)

for(i in 1:nrow(HYSETS_watershed_properties)){
  
  cur_site<-HYSETS_watershed_properties$Official_ID[i]
  curfile_name<-paste0("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Minute_data/Minute_Streamflow_",paste0("USGS-",cur_site),".csv")
  
  if(!(curfile_name %in% curdone)){
    # ChoptankInfo <- read_waterdata_monitoring_location(paste0("USGS-",cur_site))
    # HYSETS_watershed_properties$time_zone[i]<-ChoptankInfo$time_zone_abbreviation
    # HYSETS_watershed_properties$location_acc[i]<-ChoptankInfo$horizontal_positional_accuracy
    # HYSETS_watershed_properties$state_name[i]<-ChoptankInfo$state_name
    # HYSETS_watershed_properties$altitude_USGS[i]<-ChoptankInfo$altitude
    # HYSETS_watershed_properties$site_type[i]<-ChoptankInfo$site_type
    # HYSETS_watershed_properties$drainage_area_USGS[i]<-ChoptankInfo$drainage_area
    # 
    
    # Raw daily data:
    # rawDailyData <- read_waterdata_daily(monitoring_location_id = paste0("USGS-",cur_site), parameter_code = "00060", time = c("", "2025-10-31"))
    # rawDailyData<-rawDailyData[,c("value","unit_of_measure","approval_status","time","qualifier")]
    # rawDailyData$geometry<-NULL
    # attr(rawDailyData,"request")<-NULL
    # attr(rawDailyData,"queryTime")<-NULL
    # rawDailyData$year<-year(rawDailyData$time)
    # rawDailyData$month<-month(rawDailyData$time)
    # rawDailyData$day<-day(rawDailyData$time)
    # rawDailyData$time<-NULL
    # write.csv(rawDailyData,paste0("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Daily_data/DAILY_Streamflow_",paste0("USGS-",cur_site),".csv"),row.names = F)
    # 
    
    rawMinuteData <- readNWISuv(cur_site, "00060", "", "2025-10-31")
    rawMinuteData<-rawMinuteData[,-c(1,2)]
    attr(rawMinuteData,"url")<-NULL
    attr(rawMinuteData,"siteInfo")<-NULL
    rawMinuteData$year<-year(rawMinuteData$dateTime)
    rawMinuteData$month<-month(rawMinuteData$dateTime)
    rawMinuteData$day<-day(rawMinuteData$dateTime)
    rawMinuteData$hour<-hour(rawMinuteData$dateTime)
    rawMinuteData$minute<-minute(rawMinuteData$dateTime)
    
    
    write.csv(rawMinuteData,curfile_name,row.names = F)
    
    
    print(i)
  }
  
  
  
  if(i %% 100==0) print(paste("at i=",i))
}



###########################################################################################################################################
##################################################################  download daily   #####################################################3
#############################################################################################################################################

rm(list = ls())
gc()

library(dataRetrieval)
library(lubridate)


HYSETS_watershed_properties <- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/HYSETS_watershed_properties.txt")
HYSETS_watershed_properties<-HYSETS_watershed_properties[HYSETS_watershed_properties$Source=="USGS",]

HYSETS_watershed_properties<-HYSETS_watershed_properties[HYSETS_watershed_properties$Hydrometric_station_longitude>= -130 | HYSETS_watershed_properties$Centroid_Lon_deg_E>= -130,]
HYSETS_watershed_properties<-HYSETS_watershed_properties[HYSETS_watershed_properties$Hydrometric_station_longitude<= -110 | HYSETS_watershed_properties$Centroid_Lon_deg_E<= -110,]

HYSETS_watershed_properties<-HYSETS_watershed_properties[HYSETS_watershed_properties$Hydrometric_station_latitude>= 25 | HYSETS_watershed_properties$Centroid_Lat_deg_N>= 25,]
HYSETS_watershed_properties<-HYSETS_watershed_properties[HYSETS_watershed_properties$Hydrometric_station_latitude<= 45 | HYSETS_watershed_properties$Centroid_Lat_deg_N<= 45,]


#extra site information
HYSETS_watershed_properties$time_zone<-NA
HYSETS_watershed_properties$location_acc<-NA
HYSETS_watershed_properties$state_name<-NA
HYSETS_watershed_properties$altitude_USGS<-NA
HYSETS_watershed_properties$site_type<-NA
HYSETS_watershed_properties$drainage_area_USGS<-NA

curdone<-list.files("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Daily_data/",full.names = T)

for(i in 310:nrow(HYSETS_watershed_properties)){
  
  cur_site<-HYSETS_watershed_properties$Official_ID[i]
  curfile_name<-paste0("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Daily_data/DAILY_Streamflow_",paste0("USGS-",cur_site),".csv")
  
  #if(!(curfile_name %in% curdone)){
  if(TRUE){
    # ChoptankInfo <- read_waterdata_monitoring_location(paste0("USGS-",cur_site))
    # HYSETS_watershed_properties$time_zone[i]<-ChoptankInfo$time_zone_abbreviation
    # HYSETS_watershed_properties$location_acc[i]<-ChoptankInfo$horizontal_positional_accuracy
    # HYSETS_watershed_properties$state_name[i]<-ChoptankInfo$state_name
    # HYSETS_watershed_properties$altitude_USGS[i]<-ChoptankInfo$altitude
    # HYSETS_watershed_properties$site_type[i]<-ChoptankInfo$site_type
    # HYSETS_watershed_properties$drainage_area_USGS[i]<-ChoptankInfo$drainage_area
    # 
    
    # Raw daily data:
    
    rawDailyData <- tryCatch({
      #read_waterdata_daily(monitoring_location_id = paste0("USGS-", cur_site),parameter_code = "00060",time = c("", "2025-10-31"))
      readNWISdv(cur_site, "00060", "", "2025-10-31")
    }, error = function(e) {
      message("Error reading water data for site ", cur_site, ": ", e$message)
      return(NULL)
    })
    
    # Only continue if no error occurred
    if (!is.null(rawDailyData) & nrow(rawDailyData)>0) {
      rawDailyData<-rawDailyData[,c(3:5)]
      colnames(rawDailyData)<-c("time","Streamflow","code")
      rawDailyData$year <- year(rawDailyData$time)
      rawDailyData$month <- month(rawDailyData$time)
      rawDailyData$day <- day(rawDailyData$time)
      rawDailyData$time <- NULL
      
      write.csv(rawDailyData,curfile_name,row.names = FALSE)
    }
    
    print(i)
  }
  
  
  
  if(i %% 100==0) print(paste("at i=",i))
}

