##################################################################################################################################
###################################################   get 15min data   ################################################################
#######################################################################################################################################

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



library(stringr)
curdone<-list.files("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Daily_data/",full.names = T)
#further process for redivis
for(i in 1:length(curdone)){
  curdat<-read.csv(curdone[i])
  if(!("date" %in% colnames(curdat))){
    curdat$date<-paste0(curdat$year,"-",curdat$month,"-",curdat$day)
    curdat$date<-as.character(as.Date(curdat$date))
  }
  curdat$year<-NULL
  curdat$month<-NULL
  curdat$day<-NULL
  curdat$USGS_code<-substr(curdone[i],start=98,stop= str_length(curdone[i])-4)
  write.csv(curdat,curdone[i],row.names = F)
  if(i %% 100 ==0) print(i)
}



###########################################################################################################################################
##################################################################  download catchment attributes  ###########################################
#############################################################################################################################################

rm(list = ls())
gc()
library(sf)
library(dataRetrieval)
library(lubridate)


my_shapefile <- st_read("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_boundaries_USGS.shz")
my_shapefile2 <- st_read("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_boundaries.shz")

HYSETS_watershed_properties<-data.frame(id=c(my_shapefile$SITE_NO,my_shapefile2$OfficialID),lat=NA,lon=NA)
HYSETS_watershed_properties<-HYSETS_watershed_properties[!duplicated(HYSETS_watershed_properties$id),]
HYSETS_watershed_properties$time_zone<-NA
HYSETS_watershed_properties$location_acc<-NA
HYSETS_watershed_properties$state_name<-NA
HYSETS_watershed_properties$altitude_USGS<-NA
HYSETS_watershed_properties$site_type<-NA
HYSETS_watershed_properties$drainage_area_USGS<-NA
HYSETS_watershed_properties$contributing_area_USGS<-NA
HYSETS_watershed_properties$site_name<-NA
HYSETS_watershed_properties$dec_datum<-NA


for(i in 1:nrow(HYSETS_watershed_properties)){
  
  cur_site<-substr(HYSETS_watershed_properties$id[i],start=6,stop=1000) 
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
      HYSETS_watershed_properties$site_name[i]<-ChoptankInfo$station_nm
      HYSETS_watershed_properties$lat[i]<-ChoptankInfo$dec_lat_va
      HYSETS_watershed_properties$lon[i]<-ChoptankInfo$dec_long_va
      HYSETS_watershed_properties$dec_datum[i]<-ChoptankInfo$dec_coord_datum_cd
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

HYSETS_watershed_properties$geometry<-paste0("POINT(",HYSETS_watershed_properties$lon," ",HYSETS_watershed_properties$lat,")")


write.csv(HYSETS_watershed_properties,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_properties.csv",row.names = F)



###########################################################################################################################################
######################################## combine daily data for redivis    ################################################################
#########################################################################################################################################
rm(list = ls())
library(stringr)

HYSETS_watershed_properties<-read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_properties.csv")
ids_with_data<-list.files("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Daily_data/")
ids_with_data<-substr(ids_with_data,start=18,stop=1000)
ids_with_data<-str_replace(ids_with_data,".csv","")

nfolds<-400
folds<-sample(1:nfolds,size = nrow(HYSETS_watershed_properties),replace = T)

for(i in 1:nfolds){
  curfold_info<-HYSETS_watershed_properties[folds==i,]
  curfold_data_list<-list()
  for(j in 1:nrow(curfold_info)){
    if(curfold_info$id[j] %in% ids_with_data){
      curdat<-read.csv(paste0("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Daily_data/DAILY_Streamflow_",curfold_info$id[j],".csv"))
      curdat$gauge_location<-curfold_info$geometry[j]
      curfold_data_list[[j]]<-curdat
    } 
  }
  curfold_data<-do.call(rbind, curfold_data_list)
  write.csv(curfold_data,file = paste0("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/daily_data_redivis/fold_",i,".csv"),row.names = F)
}


###########################################################################################################################################
##################################################################  get adjacency matrix   #####################################################3
#############################################################################################################################################
rm(list = ls())
library(sf)


my_shapefile <- st_read("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_boundaries_USGS.shz")
my_shapefile2 <- st_read("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_boundaries.shz")
shed_props<-read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Watershed_properties.csv")
shed_props<-shed_props[!is.na(shed_props$lat),]


points_sf <- st_as_sf(shed_props[,c("lon","lat")], coords = c("lon", "lat"), crs = 4269)
points_sf<- st_transform(points_sf, crs = 4326)

adjacency_dat<-data.frame(from=NA,to=NA)

for(i in 1:nrow(shed_props)){
  cursite<-shed_props$id[i]
  curshp<-my_shapefile$geometry[my_shapefile$SITE_NO== cursite]
  is_within <- st_within(points_sf, curshp, sparse = FALSE)
  if(sum(is_within)>0){
    curadj<-data.frame(from=shed_props$id[is_within[,1]],to=cursite)
    adjacency_dat<-rbind(adjacency_dat,curadj)
  }
  if(i %% 100==0) print(paste("at i=",i))
}

adjacency_dat<-adjacency_dat[-1,]
adjacency_dat<-adjacency_dat[adjacency_dat$from!=adjacency_dat$to,]

write.csv(adjacency_dat,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/ancestors.csv",row.names = F)


direct_adjacency<-read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/ancestors.csv")

curloc<-1
while(curloc<= nrow(direct_adjacency)){
  cur_to<-direct_adjacency$to[curloc]
  cur_from<-direct_adjacency$from[curloc]
  
  cur_to_from<-direct_adjacency$from[direct_adjacency$to==cur_to]
  cur_from_to<-direct_adjacency$to[direct_adjacency$from==cur_from]
  
  if(sum(cur_to_from %in% cur_from_to)>0){
    direct_adjacency<-direct_adjacency[-curloc,]
    print(curloc)
  } else{
    curloc<-curloc+1
  }
}

write.csv(direct_adjacency,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/parents.csv",row.names = F)


###########################################################################################################################################
##################################################################  get important ones plus ancestors   #####################################################3
#############################################################################################################################################

primary_basin<-"USGS-11303500"

adjacency_dat<-read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/ancestors.csv")

basins_to_move<-c(primary_basin,adjacency_dat$from[adjacency_dat$to==primary_basin])

for(i in 1:length(basins_to_move)){
  curdat <- tryCatch({
    #read_waterdata_daily(monitoring_location_id = paste0("USGS-", cur_site),parameter_code = "00060",time = c("", "2025-10-31"))
    read.csv(paste0("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/Daily_data/DAILY_Streamflow_",basins_to_move[i],".csv"))
  }, error = function(e) {
    message("Error reading water data for site ", cur_site, ": ", e$message)
    return(NULL)
  })
  
  if(!is.null(curdat)) write.csv(curdat,paste0("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/streamflow/vernalis_plus_daily/DAILY_Streamflow_",basins_to_move[i],".csv"),row.names = F)
}
