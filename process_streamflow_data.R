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

for(i in 1:nrow(HYSETS_watershed_properties)){
  
  cur_site<-HYSETS_watershed_properties$Official_ID[i]
  curfile_name<-paste0("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Daily_data/DAILY_Streamflow_",paste0("USGS-",cur_site),".csv")
  
  if(!(curfile_name %in% curdone)){
  #if(TRUE){
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



###########################################################################################################################################
##################################################################  download catchment attributes  ###########################################
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
HYSETS_watershed_properties$contributing_area_USGS<-NA


for(i in 1:nrow(HYSETS_watershed_properties)){
  
  cur_site<-HYSETS_watershed_properties$Official_ID[i]
  curfile_name<-paste0("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Daily_data/DAILY_Streamflow_",paste0("USGS-",cur_site),".csv")
  
  #if(!(curfile_name %in% curdone)){
  if(is.na(HYSETS_watershed_properties$time_zone[i])){
    
    ChoptankInfo <- tryCatch({
      #read_waterdata_daily(monitoring_location_id = paste0("USGS-", cur_site),parameter_code = "00060",time = c("", "2025-10-31"))
      readNWISsite(cur_site)
    }, error = function(e) {
      message("Error reading water data for site ", cur_site, ": ", e$message)
      return(NULL)
    })
    
    # Only continue if no error occurred
    if (!is.null(ChoptankInfo) & ifelse(is.null(ChoptankInfo), FALSE, nrow(ChoptankInfo) > 0)) {
      HYSETS_watershed_properties$time_zone[i]<-ChoptankInfo$tz_cd
      HYSETS_watershed_properties$location_acc[i]<-ChoptankInfo$coord_acy_cd
      HYSETS_watershed_properties$state_name[i]<-ChoptankInfo$state_cd
      HYSETS_watershed_properties$altitude_USGS[i]<-ChoptankInfo$alt_va
      HYSETS_watershed_properties$site_type[i]<-ChoptankInfo$site_tp_cd
      HYSETS_watershed_properties$drainage_area_USGS[i]<-ChoptankInfo$drain_area_va
      HYSETS_watershed_properties$contributing_area_USGS[i]<-ChoptankInfo$contrib_drain_area_va
    }

    
    # Raw daily data:
    
    # rawDailyData <- tryCatch({
    #   #read_waterdata_daily(monitoring_location_id = paste0("USGS-", cur_site),parameter_code = "00060",time = c("", "2025-10-31"))
    #   readNWISdv(cur_site, "00060", "", "2025-10-31")
    # }, error = function(e) {
    #   message("Error reading water data for site ", cur_site, ": ", e$message)
    #   return(NULL)
    # })
    # 
    # # Only continue if no error occurred
    # if (!is.null(rawDailyData) & nrow(rawDailyData)>0) {
    #   rawDailyData<-rawDailyData[,c(3:5)]
    #   colnames(rawDailyData)<-c("time","Streamflow","code")
    #   rawDailyData$year <- year(rawDailyData$time)
    #   rawDailyData$month <- month(rawDailyData$time)
    #   rawDailyData$day <- day(rawDailyData$time)
    #   rawDailyData$time <- NULL
    #   
    #   write.csv(rawDailyData,curfile_name,row.names = FALSE)
    # }
    
    print(i)
  }
  
  
  
  if(i %% 100==0) print(paste("at i=",i))
}

HYSETS_watershed_properties$Official_ID<-paste0(HYSETS_watershed_properties$Source,"-",HYSETS_watershed_properties$Official_ID)
HYSETS_watershed_properties$Watershed_ID<-NULL
HYSETS_watershed_properties$Source<-NULL


write.csv(HYSETS_watershed_properties,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_properties.csv",row.names = F)





###########################################################################################################################################
##################################################################  download catchment boundaries  ###########################################
#############################################################################################################################################

rm(list = ls())
gc()

library(sf)
library(stringr)

my_shapefile <- st_read("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/HYSETS_watershed_boundaries/HYSETS_watershed_boundaries_20200730.shp")
properties<- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_properties.csv")


my_shapefile<-my_shapefile[my_shapefile$Source=="USGS",]
my_shapefile$OfficialID<-paste0(my_shapefile$Source,"-",my_shapefile$OfficialID)
my_shapefile$Source<-NULL
my_shapefile$features<-NULL
#head(my_shapefile[!(as.numeric(my_shapefile$OfficialID) %in% properties$Official_ID),])


my_shapefile<- my_shapefile[my_shapefile$OfficialID %in% properties$Official_ID,]
my_shapefile <- st_set_crs(my_shapefile, 4326)


st_write(my_shapefile,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_boundaries.shp",delete_dsn = T,driver = "ESRI Shapefile")
st_write(my_shapefile, "C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_boundaries.shz", delete_dsn = TRUE,driver = "ESRI Shapefile")

setwd("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow")
zip(zipfile = "Watershed_boundaries.shp.zip", files = c("Watershed_boundaries.dbf", "Watershed_boundaries.prj","Watershed_boundaries.shp","Watershed_boundaries.shx"))



my_shapefile2 <- st_read("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/basins18/basins18.shp")
my_shapefile2<- st_transform(my_shapefile2, crs = 4326)
my_shapefile2$SITE_NO<-paste0("USGS-",my_shapefile2$SITE_NO)


my_shapefile3 <- st_read("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/basins17/basins17.shp")
my_shapefile3<- st_transform(my_shapefile3, crs = 4326)
my_shapefile3$SITE_NO<-paste0("USGS-",my_shapefile3$SITE_NO)


my_shapefile4 <- st_read("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/basins16/basins16.shp")
my_shapefile4<- st_transform(my_shapefile4, crs = 4326)
my_shapefile4$SITE_NO<-paste0("USGS-",my_shapefile4$SITE_NO)


my_shapefile5 <- st_read("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/basins15/basins15.shp")
my_shapefile5<- st_transform(my_shapefile5, crs = 4326)
my_shapefile5$SITE_NO<-paste0("USGS-",my_shapefile5$SITE_NO)


my_shapefile<-rbind(my_shapefile2,my_shapefile3,my_shapefile4,my_shapefile5)


st_write(my_shapefile,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_boundaries_USGS.shp",delete_dsn = T,driver = "ESRI Shapefile")
st_write(my_shapefile, "C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_boundaries_USGS.shz", delete_dsn = TRUE,driver = "ESRI Shapefile")
setwd("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow")
zip(zipfile = "Watershed_boundaries_USGS.shp.zip", files = c("Watershed_boundaries_USGS.dbf", "Watershed_boundaries_USGS.prj","Watershed_boundaries_USGS.shp","Watershed_boundaries_USGS.shx"))



###########################################################################################################################################
##################################################################  download daily (USGS)   #####################################################3
#############################################################################################################################################

rm(list = ls())
gc()

library(dataRetrieval)
library(lubridate)


my_shapefile <- st_read("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_boundaries_USGS.shp")

curdone<-list.files("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Daily_data/",full.names = T)

for(i in 1:nrow(my_shapefile)){
  
  cur_site<-substr(my_shapefile$SITE_NO[i],start = 6,stop = 999) 
  curfile_name<-paste0("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Daily_data/DAILY_Streamflow_",paste0("USGS-",cur_site),".csv")
  
  if(!(curfile_name %in% curdone)){
    #if(TRUE){
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


#further process for redivis
for(i in 1:length(curdone)){
  curdat<-read.csv(curdone[i])
  curdat$date<-paste0(curdat$year,"-",curdat$month,"-",curdat$day)
  curdat$date<-as.character(as.Date(curdat$date))
  write.csv(curdat,curdone[i],row.names = F)
  if(i %% 100 ==0) print(i)
}





