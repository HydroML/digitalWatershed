rm(list=ls())

library(lubridate)
library(maps)
library(geosphere)

measurements <- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/casgem/measurements.csv")


measurements<-measurements[,c("site_code","latitude","longitude","classification","msmt_date","wlm_gse","gse_gwe")]
colnames(measurements)[7]<-"WTD"
colnames(measurements)[6]<-"elevation"
measurements<-measurements[complete.cases(measurements),]
measurements$msmt_date<-as.Date(measurements$msmt_date)
measurements<-measurements[month(measurements$msmt_date)<5,]


station_class<-measurements[!duplicated(measurements[,c("site_code","classification")]),] 


ca <- map("state",
          regions = "california",
          plot = FALSE)

# Plot using base plot()
plot(ca$x, ca$y,
     type = "l",
     asp = 1,
     xlab = "Longitude",
     ylab = "Latitude",
     col = "black",
     lwd = 2)

points(station_class$longitude,station_class$latitude,col="red",cex=0.5,pch=20)
rm(ca)

#lets only deal with ones within cc extent for this one
measurements<-measurements[!(measurements$classification %in% c("outside_cc_extent","no_prf_info","within_cc","above_and_below_cc")),]

gc()

measurements<-measurements[year(measurements$msmt_date)>= 1971 & year(measurements$msmt_date)<= 2023 & (month(measurements$msmt_date)<4 | (day(measurements$msmt_date)==1 & month(measurements$msmt_date)==4 )),]
measurements<-measurements[measurements$WTD>0 & measurements$WTD<1000,]

station_class<-measurements[!duplicated(measurements[,c("site_code","classification")]),] 

points(station_class$longitude,station_class$latitude,pch=20,cex=0.6)



classifywell_susmita<-function(sitecode,train_wells){
  cur_meas<-measurements[measurements$site_code==sitecode,]
  
  dist_km <- distHaversine(p1 = cbind(mean(cur_meas$longitude), mean(cur_meas$latitude)),
                           p2 = train_wells[,c("longitude","latitude")])/1000
  
  dist_thresh<-15 #sort(dist_km)[21]
  train_wells$dist<-dist_km
  cur_close_stations<-train_wells[dist_km<= dist_thresh & dist_km>0,]
  
  
  corrs<-rep(NA,nrow(cur_close_stations))
  diffs<-rep(NA,nrow(cur_close_stations))

  for(i in 1:nrow(cur_close_stations)){
    cur_comparison_meas<-measurements[measurements$site_code==cur_close_stations$site_code[i],]
    #print(nrow(cur_comparison_meas))
    if(length(unique(year(cur_comparison_meas$msmt_date)))>=10){
      yearlyave<-data.frame(year=1971:2023, cur_ave=NA, compare_ave=NA)
      for(y in 1:nrow(yearlyave)){
        cur_year<-yearlyave$year[y]
        yearlyave$cur_ave[y]<-mean(cur_meas$WTD[year(cur_meas$msmt_date)==cur_year])
        yearlyave$compare_ave[y]<-mean(cur_comparison_meas$WTD[year(cur_comparison_meas$msmt_date)==cur_year])
      }
      
      yearlyave<-yearlyave[complete.cases(yearlyave),]
      
      if(nrow(yearlyave)>=10){
        corrs[i]<-cor(yearlyave$cur_ave,yearlyave$compare_ave)
        diffs[i]<-abs(mean(diff(yearlyave$cur_ave)) - mean(diff(yearlyave$compare_ave)))
        #print("got 10 years")
      }
    }
  }
  
  corCONFINED<-mean(corrs[cur_close_stations$classification=="below_cc"],na.rm = T)
  corUNCONF<-mean(corrs[cur_close_stations$classification=="above_cc"],na.rm = T)
  
  diffCONFINED<-mean(diffs[cur_close_stations$classification=="below_cc"],na.rm = T)
  diffUNCONF<-mean(diffs[cur_close_stations$classification=="above_cc"],na.rm = T)
  
  if(is.na(corCONFINED) | is.na(diffCONFINED)){
    if(corUNCONF>=0.5 & diffUNCONF<=5 & !is.na(corUNCONF)){
      return("above_cc")
    }
  } 
  
  if(is.na(corUNCONF) | is.na(diffUNCONF)){
    if(corCONFINED>= 0.5 & diffCONFINED<= 5 & !is.na(corCONFINED)){
      return("below_cc")
    }
  }
  
  if(corCONFINED>= 0.5 & diffCONFINED<= 5 & (corCONFINED- corUNCONF)>0.4 & diffCONFINED<diffUNCONF & !is.na(corCONFINED) & !is.na(corUNCONF)){
    return("below_cc")
  } else if(corUNCONF>=0.5 & diffUNCONF<=5 & (corUNCONF- corCONFINED)>0.4 & diffCONFINED>diffUNCONF & !is.na(corUNCONF) & !is.na(corCONFINED)){
    return("above_cc")
  } else{
    return("???")
  }
  
}

nfold<-5
station_class$fold<-sample(1:nfold,nrow(station_class),replace = T)

acc_classified<-rep(NA,nfold)
acc_all<-rep(NA,nfold)
for(f in 1:nfold){
  curtrain<-station_class[station_class$fold!=f,]
  curtest<-station_class[station_class$fold==f,]
  orig_train<-station_class[station_class$fold!=f,]
  gt_class<-curtest$classification
  curtest$classification<-"???"
  contin<-T
  while(contin==T){
    contin<-F
    for(j in 1:nrow(curtest)){
      if(curtest$classification[j]=="???"){
        curtest$classification[j]<-classifywell_susmita(sitecode =  curtest$site_code[j],train_wells = curtrain)
        #print(paste("Done with ",j,". The predicted class is: ",curtest$classification[j]))
        if(curtest$classification[j]!="???") contin<-T
      }
    }
    curtrain<-rbind(orig_train,curtest[curtest$classification!="???",])
    print(paste("Done another iteration... now the training set has",nrow(curtrain),"observations"))
  }
  acc_classified[f]<-sum(gt_class==curtest$classification)/sum(curtest$classification!="???")
  acc_all[f]<-sum(gt_class==curtest$classification)/nrow(curtest)
}






classifywell_baseline<-function(sitecode,train_wells){
  cur_meas<-measurements[measurements$site_code==sitecode,]
  
  dist_km <- distHaversine(p1 = cbind(mean(cur_meas$longitude), mean(cur_meas$latitude)),
                           p2 = train_wells[,c("longitude","latitude")])/1000
  
  dist_thresh<-min(dist_km[dist_km>0]) #sort(dist_km)[21]
  train_wells$dist<-dist_km
  cur_close_stations<-train_wells[dist_km<= dist_thresh & dist_km>0,]
  
  return(cur_close_stations$classification)
}


nfold<-5
station_class$fold<-sample(1:nfold,nrow(station_class),replace = T)

acc_classified<-rep(NA,nfold)
acc_all<-rep(NA,nfold)
for(f in 1:nfold){
  curtrain<-station_class[station_class$fold!=f,]
  curtest<-station_class[station_class$fold==f,]
  orig_train<-station_class[station_class$fold!=f,]
  gt_class<-curtest$classification
  curtest$classification<-"???"
  contin<-T
  while(contin==T){
    contin<-F
    for(j in 1:nrow(curtest)){
      if(curtest$classification[j]=="???"){
        curtest$classification[j]<-classifywell_baseline(sitecode =  curtest$site_code[j],train_wells = curtrain)
        #print(paste("Done with ",j,". The predicted class is: ",curtest$classification[j]))
        if(curtest$classification[j]!="???") contin<-T
      }
    }
    curtrain<-rbind(orig_train,curtest[curtest$classification!="???",])
    print(paste("Done another iteration... now the training set has",nrow(curtrain),"observations"))
  }
  acc_classified[f]<-sum(gt_class==curtest$classification)/sum(curtest$classification!="???")
  acc_all[f]<-sum(gt_class==curtest$classification)/nrow(curtest)
}




#############################################################################################################
# check why Susmita has less classified wells
##############################################################################################################
susmita_gt_classification <- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/aquifer_type_by_prf_susmita_classification.csv")

station_class$susmita_has<-station_class$site_code %in% susmita_gt_classification$site_code

lookhere<-"354527N1195347W001"
cur_meas<-measurements[measurements$site_code==lookhere,]
length(unique(year(cur_meas$msmt_date)))


#################################################################################################################################
###############################################3333   ML-based classifier   #########################################################
#########################################################################################################################################
library(lubridate)
library(maps)
library(geosphere)
library(sf)
library(DescTools)

rm(list = ls())

## clean data so that lat lon all have same datum, and we have smaller sets of vars with a station dataset and a obs dataset
WellCompletion_obs <- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/WellCompletionReports_cleanVwide.csv")
WellCompletion_obs$WellLocation<-NULL
WellCompletion_obs$City<-NULL
WellCompletion_obs$PlannedUseFormerUse<-NULL
WellCompletion_obs$LLAccuracy<-NULL
WellCompletion_obs$GroundSurfaceElevation<-NULL
before_space <- sub(" .*", "", WellCompletion_obs$Location)
WellCompletion_obs$longitude<-as.numeric(substr(before_space,start=7,stop=1000))
after_space <- sub("^[^ ]+ ", "", WellCompletion_obs$Location)
WellCompletion_obs$latitude<-as.numeric(substr(after_space, 1, nchar(after_space) - 1))
WellCompletion_obs$Location<-NULL
WellCompletion_obs$DateWorkEnded<-as.Date(WellCompletion_obs$DateWorkEnded)


sf_obj <- st_as_sf(WellCompletion_obs[WellCompletion_obs$HorizontalDatum=="NAD83", ],
                   coords = c("longitude", "latitude"),crs = 4269,remove = FALSE)


sf_obj <- st_transform(sf_obj, 4326)
xy <- st_coordinates(sf_obj)

WellCompletion_obs$longitude[WellCompletion_obs$HorizontalDatum=="NAD83"]<-xy[,1]
WellCompletion_obs$latitude[WellCompletion_obs$HorizontalDatum=="NAD83"]<-xy[,2]


sf_obj <- st_as_sf(WellCompletion_obs[WellCompletion_obs$HorizontalDatum=="NAD27", ],
                   coords = c("longitude", "latitude"),crs = 4267,remove = FALSE)


sf_obj <- st_transform(sf_obj, 4326)
xy <- st_coordinates(sf_obj)

WellCompletion_obs$longitude[WellCompletion_obs$HorizontalDatum=="NAD27"]<-xy[,1]
WellCompletion_obs$latitude[WellCompletion_obs$HorizontalDatum=="NAD27"]<-xy[,2]
rm(sf_obj,xy,after_space,before_space)

WellCompletion_obs$HorizontalDatum<-NULL
WellCompletion_stations<-WellCompletion_obs
WellCompletion_obs<-WellCompletion_obs[,c("WCRNumber","DateWorkEnded","StaticWaterLevel","location_match")]

WellCompletion_stations<-data.frame(ID=WellCompletion_stations$WCRNumber,longitude=WellCompletion_stations$longitude, latitude=WellCompletion_stations$latitude,
                                    HoleDepth=WellCompletion_stations$TotalDrillDepth, WellDepth= WellCompletion_stations$TotalCompletedDepth,
                                    TopPerf=WellCompletion_stations$TopOfPerforatedInterval, BotPerf=WellCompletion_stations$BottomofPerforatedInterval,source="WCR")

gc()
write.csv(WellCompletion_obs,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/WellCompletion_obs.csv",row.names = F)
write.csv(WellCompletion_stations,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/WellCompletion_stations.csv",row.names = F)



#now clean USGS
USGS_obs <- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/USGS_obs_mancon.csv")
USGS_obs$unit_of_measure<-NULL
USGS_obs$approval_status<-NULL
USGS_obs$hydrologic_unit_code<-NULL
USGS_obs$vertical_datum<-NULL
USGS_obs$horizontal_positional_accuracy_code<-NULL
before_space <- sub("POINT ", "", USGS_obs$geometry)
before_space <- sub(" .*", "", before_space)
USGS_obs$longitude<-as.numeric(substr(before_space,start=2,stop=1000))
after_space <- sub("POINT ", "", USGS_obs$geometry)
after_space <- sub("^[^ ]+ ", "", after_space)
USGS_obs$latitude<-as.numeric(substr(after_space, 1, nchar(after_space) - 1))
USGS_obs$geometry<-NULL
USGS_obs$time<-as.Date(USGS_obs$time)
USGS_obs$value[USGS_obs$parameter_code==62611]<- USGS_obs$altitude[USGS_obs$parameter_code==62611]- USGS_obs$value[USGS_obs$parameter_code==62611]
USGS_obs$value[USGS_obs$parameter_code==65]<- USGS_obs$value[USGS_obs$parameter_code==65]* -1
USGS_obs$parameter_code<-NULL
USGS_obs$altitude<-NULL
USGS_obs$altitude_accuracy<-NULL

sf_obj <- st_as_sf(USGS_obs[USGS_obs$original_horizontal_datum=="NAD83", ],
                   coords = c("longitude", "latitude"),crs = 4269,remove = FALSE)
sf_obj <- st_transform(sf_obj, 4326)
xy <- st_coordinates(sf_obj)
USGS_obs$longitude[USGS_obs$original_horizontal_datum=="NAD83"]<-xy[,1]
USGS_obs$latitude[USGS_obs$original_horizontal_datum=="NAD83"]<-xy[,2]


sf_obj <- st_as_sf(USGS_obs[USGS_obs$original_horizontal_datum=="NAD27", ],
                   coords = c("longitude", "latitude"),crs = 4267,remove = FALSE)
sf_obj <- st_transform(sf_obj, 4326)
xy <- st_coordinates(sf_obj)
USGS_obs$longitude[USGS_obs$original_horizontal_datum=="NAD27"]<-xy[,1]
USGS_obs$latitude[USGS_obs$original_horizontal_datum=="NAD27"]<-xy[,2]
rm(sf_obj,xy,after_space,before_space)

USGS_obs$original_horizontal_datum<-NULL

USGS_stations<-USGS_obs[!duplicated(USGS_obs$monitoring_location_id),]
USGS_stations<-data.frame(ID=USGS_stations$monitoring_location_id, longitude=USGS_stations$longitude, latitude=USGS_stations$latitude,
                          HoleDepth=USGS_stations$hole_constructed_depth, WellDepth= USGS_stations$well_constructed_depth,
                          TopPerf=NA, BotPerf=NA,source="USGS")

USGS_obs$well_constructed_depth<-NULL
USGS_obs$hole_constructed_depth<-NULL
USGS_obs$longitude<-NULL
USGS_obs$latitude<-NULL
gc()

write.csv(USGS_obs,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/USGS_obs.csv",row.names = F)
write.csv(USGS_stations,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/USGS_stations.csv",row.names = F)



# now clean gama
gama_obs <- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/gama_new.csv")
gama_obs$SOURCE<-NULL
gama_obs$geometry<-NULL
gama_obs$GM_DATA_SOURCE<-NULL
gama_obs$GM_DATASET_NAME<-NULL
gama_obs$GM_GIS_HVA<-NULL
gama_obs$GM_GIS_SENATE_DISTRICT<-NULL
gama_obs$GM_WELL_CATEGORY<-NULL
gama_obs$loc_uncertainty<-abs(gama_obs$LATITUDE- gama_obs$GM_LATITUDE) + abs(gama_obs$LONGITUDE- gama_obs$GM_LONGITUDE)
gama_obs$GM_LATITUDE<-NULL
gama_obs$GM_LONGITUDE<-NULL
gama_obs$MEASUREMENT.DATE<-as.Date(gama_obs$MEASUREMENT.DATE,tryFormats="%m/%d/%Y")

gama_stations<-gama_obs[!duplicated(gama_obs$WELL.NUMBER),]
gama_stations<-data.frame(ID=gama_stations$WELL.NUMBER, longitude=gama_stations$LONGITUDE, latitude=gama_stations$LATITUDE,
                          HoleDepth=NA, WellDepth= gama_stations$GM_WELL_DEPTH_FT,
                          TopPerf=gama_stations$GM_TOP_DEPTH_OF_SCREEN_FT, BotPerf=gama_stations$GM_BOTTOM_DEPTH_OF_SCREEN_FT,source="GAMA")

gama_obs$LONGITUDE<-NULL
gama_obs$LATITUDE<-NULL
gama_obs$GM_WELL_DEPTH_FT<-NULL
gama_obs$GM_TOP_DEPTH_OF_SCREEN_FT<-NULL
gama_obs$GM_BOTTOM_DEPTH_OF_SCREEN_FT<-NULL
gama_obs<-gama_obs[!is.na(gama_obs$DEPTH.TO.WATER),]
gc()

write.csv(gama_obs,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/gama_obs.csv",row.names = F)
write.csv(gama_stations,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/gama_stations.csv",row.names = F)



#now clean casgem
CASGEM_obs <- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/casgem/measurements.csv")
casgem_stations <- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/casgem/stations_casgem.csv")
perforations <- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/casgem/perforations.csv")
gc()
CASGEM_obs$msmt_date<-as.Date(CASGEM_obs$msmt_date)

# q for sylvia: how do i match perforations with observations since a single site code can have multiple perforations
casgem_stations<-data.frame(ID=casgem_stations$site_code, longitude=casgem_stations$longitude, latitude=casgem_stations$latitude,
                          HoleDepth=NA, WellDepth= casgem_stations$well_depth,
                          TopPerf=NA, BotPerf=NA,source="CASGEM")

for(i in 1:nrow(casgem_stations)){
  curid<-casgem_stations$ID[i]
  cur_perf<-perforations[perforations$site_code==curid,]
  if(nrow(cur_perf)>0){
    casgem_stations$TopPerf[i]<-min(cur_perf$top_prf_int,cur_perf$bot_prf_int)
    casgem_stations$BotPerf[i]<-max(cur_perf$top_prf_int,cur_perf$bot_prf_int)
    if(i %% 500==0) print(i)
  }
}
rm(i,curid,perforations,cur_perf)

CASGEM_obs<-CASGEM_obs[,c("site_code","msmt_date","gse_gwe")]
CASGEM_obs<-CASGEM_obs[!is.na(CASGEM_obs$gse_gwe),]
gc()

write.csv(CASGEM_obs,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/CASGEM_obs.csv",row.names = F)
write.csv(casgem_stations,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/CASGEM_stations.csv",row.names = F)



all_wells<-rbind(casgem_stations,gama_stations,USGS_stations,WellCompletion_stations)
rm(casgem_stations,gama_stations,USGS_stations,WellCompletion_stations)
gc()

all_obs<-data.frame(ID=CASGEM_obs$site_code,date=as.Date(CASGEM_obs$msmt_date),DWL=CASGEM_obs$gse_gwe)
all_obs<-rbind(all_obs,data.frame(ID=gama_obs$WELL.NUMBER,date=as.Date(gama_obs$MEASUREMENT.DATE),DWL=gama_obs$DEPTH.TO.WATER))
all_obs<-rbind(all_obs,data.frame(ID=USGS_obs$monitoring_location_id,date=as.Date(USGS_obs$time),DWL=USGS_obs$value))
all_obs<-rbind(all_obs,data.frame(ID=WellCompletion_obs$WCRNumber,date=as.Date(WellCompletion_obs$DateWorkEnded),DWL=WellCompletion_obs$StaticWaterLevel))

rm(CASGEM_obs,gama_obs,USGS_obs,WellCompletion_obs)
gc()

write.csv(all_obs,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/all_obs.csv",row.names = F)
write.csv(all_wells,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/all_stations.csv",row.names = F)


################################################################################################################################
#################################################  label wells for which we have data   ############################################
#####################################################################################################################################
rm(list=ls())
library(lubridate)
library(maps)
library(geosphere)
library(sf)
library(DescTools)
gc()

all_stations<-read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/all_stations.csv")
USGS_obs<-read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/USGS_obs.csv")
all_stations$label<-NA

for(i in 1:nrow(all_stations)){
  
  if(all_stations$source[i]=="USGS"){
    curid<-all_stations$ID[i]
    curdat<-USGS_obs[USGS_obs$monitoring_location_id==curid,]
    curdat<-curdat[!is.na(curdat$aquifer_type_code),]
    
    if(nrow(curdat)==1){
      all_stations$label[i]<-curdat$aquifer_type_code
    } else if(nrow(curdat)>1){
      all_stations$label[i]<-Mode(curdat$aquifer_type_code)[1]
      if(!all(curdat$aquifer_type_code ==all_stations$label[i])) print(paste("Check out: ",i))
    } else{
      all_stations$label[i]<-"no data"
    }
    if(i %% 1000 == 0) print(i)
  }
  
}

write.csv(all_stations,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/all_stations.csv",row.names = F)


rm(list = ls())
gc()
library(raster)
library(sf)

all_stations<-read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/all_stations.csv")

cc_thickness<-raster("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/well_classification/Thickness_page.tif")
cc_top<-raster("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/well_classification/Top_surf_page_v2.tif")

stations_sf <- st_as_sf(all_stations, 
                        coords = c("longitude", "latitude"), 
                        crs = 4326) # Assuming WGS84

# 2. Combine rasters into a stack for a single-pass extraction
# This is much faster than extracting from each raster individually
raster_stack <- stack(cc_thickness, cc_top)

# 3. Ensure CRS alignment
# Rasters and points must use the exact same projection
if (st_crs(stations_sf) != st_crs(raster_stack)) {
  stations_sf <- st_transform(stations_sf, crs = crs(raster_stack))
}

# 4. Extract values
# We convert sf to 'Spatial' because the raster package works natively with sp objects
extracted_data <- extract(raster_stack, as(stations_sf, "Spatial"))

all_stations_updated <- cbind(all_stations, extracted_data)

#convert from meters to feet
all_stations_updated$Thickness_page<-all_stations_updated$Thickness_page*3.28084
all_stations_updated$Top_surf_page_v2<-all_stations_updated$Top_surf_page_v2*3.28084

rm(all_stations,cc_thickness,cc_top,raster_stack,stations_sf,extracted_data)
gc()

tozero<-all_stations_updated$Thickness_page==0 & all_stations_updated$Top_surf_page_v2==0
all_stations_updated$Thickness_page[tozero]<-NA
all_stations_updated$Top_surf_page_v2[tozero]<-NA

all_stations_updated$label[all_stations_updated$label=="N" & !is.na(all_stations_updated$label)]<-"U"
all_stations_updated$label[all_stations_updated$label=="M" & !is.na(all_stations_updated$label)]<-"C"



for(i in 1:nrow(all_stations_updated)){
  cur_topperf<-all_stations_updated$TopPerf[i]
  cur_botperf<-all_stations_updated$BotPerf[i]
  if(!is.na(cur_topperf) & !is.na(cur_botperf) & cur_topperf>cur_botperf){
    all_stations_updated$TopPerf[i]<-cur_botperf
    all_stations_updated$BotPerf[i]<-cur_topperf
    cur_topperf<-all_stations_updated$TopPerf[i]
    cur_botperf<-all_stations_updated$BotPerf[i]
  }
  cur_cctop<-all_stations_updated$Top_surf_page_v2[i]
  cur_ccthick<-all_stations_updated$Thickness_page[i]
  if(!is.na(cur_topperf) & !is.na(cur_botperf) & !is.na(cur_ccthick) & !is.na(cur_cctop)){
    if(cur_botperf< cur_cctop){ #good
      if(!is.na(all_stations_updated$label[i])){
        print(paste("We already have a label: ",all_stations_updated$label[i],"and the new label is U. Row= ",i))
      } 
      all_stations_updated$label[i]<-"U"
    }
    if(cur_topperf>(cur_cctop+ cur_ccthick)){ #good
      if(!is.na(all_stations_updated$label[i])){
        print(paste("We already have a label: ",all_stations_updated$label[i],"and the new label is C. Row= ",i))
      }
      all_stations_updated$label[i]<-"C"
    }
    
    if(cur_botperf> (cur_cctop+ cur_ccthick) & cur_topperf< cur_cctop){ #good
      if(!is.na(all_stations_updated$label[i])){
        print(paste("We already have a label: ",all_stations_updated$label[i],"and the new label is X. Row= ",i))
      } 
      all_stations_updated$label[i]<-"X"
    }
    
    if(cur_botperf< (cur_cctop+ cur_ccthick) & cur_topperf< cur_cctop & cur_botperf>cur_cctop){ #good
      if(!is.na(all_stations_updated$label[i])){
        print(paste("We already have a label: ",all_stations_updated$label[i],"and the new label is U. Row= ",i))
      } 
      all_stations_updated$label[i]<-"U"
    }
    
    if(cur_botperf> (cur_cctop+ cur_ccthick) & cur_topperf> cur_cctop & cur_topperf< (cur_cctop+cur_ccthick)){ # good
      if(!is.na(all_stations_updated$label[i])){
        print(paste("We already have a label: ",all_stations_updated$label[i],"and the new label is C. Row= ",i))
      } 
      all_stations_updated$label[i]<-"C"
    }
    
    if(cur_botperf< (cur_cctop+ cur_ccthick) & cur_botperf> cur_cctop & cur_topperf> cur_cctop & cur_topperf< (cur_cctop+cur_ccthick)){ #
      if(!is.na(all_stations_updated$label[i])){
        print(paste("We already have a label: ",all_stations_updated$label[i],"and the new label is I. Row= ",i))
      } 
      all_stations_updated$label[i]<-"I"
    }
    
  }
  if(i %% 1000 ==0) print(i)
}


# fill dataframe with important covariates
library(lubridate)
all_obs<-read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/all_obs.csv")
all_obs<-all_obs[!is.na(all_obs$date),]

all_stations_updated$mean_wtd<-NA
all_stations_updated$median_wtd<-NA
all_stations_updated$n_distinct_months<-NA
all_stations_updated$n_distinct_years<-NA
all_stations_updated$first_year<-NA
all_stations_updated$last_year<-NA
all_stations_updated$median_year<-NA
all_stations_updated$median_month<-NA
all_stations_updated$within_year_variability<-NA
all_stations_updated$across_year_variability<-NA


for(i in 1:nrow(all_stations_updated)){
  curobs<-all_obs[all_obs$ID== all_stations_updated$ID[i],]
  
  if(nrow(curobs)>0){
    all_stations_updated$mean_wtd[i]<-mean(curobs$DWL)
    all_stations_updated$median_wtd[i]<-median(curobs$DWL)
    all_stations_updated$n_distinct_months[i]<-length(unique(month(curobs$date)))
    all_stations_updated$n_distinct_years[i]<-length(unique(year(curobs$date)))
    all_stations_updated$first_year[i]<-min(year(curobs$date))
    all_stations_updated$last_year[i]<-max(year(curobs$date))
    all_stations_updated$median_year[i]<-median(year(curobs$date))
    all_stations_updated$median_month[i]<-median(month(curobs$date))
    
    years_vec<-unique(year(curobs$date))
    years_variability<-rep(NA,length(years_vec))
    years_median_wtd<-rep(NA,length(years_vec))
    for(j in 1:length(years_vec)){
      curyear<-years_vec[j]
      curyear_dat<-curobs[year(curobs$date)==curyear,]
      years_median_wtd[j]<-median(curyear_dat$DWL)
      if(nrow(curyear_dat)==1){
        years_variability[j]<-NA
      } else{
        years_variability[j]<-sd(curyear_dat$DWL)/length(unique(month(curyear_dat$date)))
      }
    }
    all_stations_updated$within_year_variability[i]<-median(years_variability,na.rm = T)
    all_stations_updated$across_year_variability[i]<-sd(years_median_wtd)
  }
  
  
  
  if(i %% 1000==0) print(i)
}

write.csv(all_stations_updated,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/all_stationsV2.csv",row.names = F)


# get elevation
rm(list=setdiff(ls(),"all_stations_updated") )

dem<-raster("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/FABDEM_v1_2.tif")

stations_sf <- st_as_sf(all_stations_updated, 
                        coords = c("longitude", "latitude"), 
                        crs = 4326) # Assuming WGS84

# 4. Extract values
# We convert sf to 'Spatial' because the raster package works natively with sp objects
dem <- extract(dem, as(stations_sf, "Spatial"))

all_stations_updated<-cbind(all_stations_updated,dem)

write.csv(all_stations_updated,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/all_stationsV2.csv",row.names = F)



all_stations_updated$label[all_stations_updated$label=="no data" & !is.na(all_stations_updated$label)]<-NA
library(ranger)
all_stations_updated$label_accuracy<-0
all_stations_updated$label_accuracy[!is.na(all_stations_updated$label)]<-1

plot(all_stations_updated$longitude,all_stations_updated$latitude,pch=20,cex=0.4)
points(all_stations_updated$longitude[!is.na(all_stations_updated$Thickness_page)],
       all_stations_updated$latitude[!is.na(all_stations_updated$Thickness_page)],pch=20,cex=0.4,col="red")

varsTOUSE<-c("longitude","latitude","WellDepth","Thickness_page","Top_surf_page_v2","mean_wtd","median_wtd","n_distinct_months",
             "n_distinct_years","first_year","median_year","median_month","within_year_variability","across_year_variability","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)


varsTOUSE<-c("longitude","latitude","HoleDepth","Thickness_page","Top_surf_page_v2","mean_wtd","median_wtd","n_distinct_months",
             "n_distinct_years","first_year","median_year","median_month","within_year_variability","across_year_variability","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error



varsTOUSE<-c("longitude","latitude","WellDepth","Thickness_page","Top_surf_page_v2","mean_wtd","median_wtd","n_distinct_months",
             "n_distinct_years","first_year","median_year","median_month","across_year_variability","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error



varsTOUSE<-c("longitude","latitude","WellDepth","Thickness_page","Top_surf_page_v2","mean_wtd","median_wtd","n_distinct_months",
             "n_distinct_years","first_year","median_year","median_month","within_year_variability","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error



varsTOUSE<-c("longitude","latitude","WellDepth","Thickness_page","Top_surf_page_v2","mean_wtd","median_wtd","n_distinct_months",
             "n_distinct_years","first_year","median_year","median_month","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error




varsTOUSE<-c("longitude","latitude","Thickness_page","Top_surf_page_v2","mean_wtd","median_wtd","n_distinct_months",
             "n_distinct_years","first_year","median_year","median_month","within_year_variability","across_year_variability","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error



varsTOUSE<-c("longitude","latitude","Thickness_page","Top_surf_page_v2","mean_wtd","median_wtd","n_distinct_months",
             "n_distinct_years","first_year","median_year","median_month","across_year_variability","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error



varsTOUSE<-c("longitude","latitude","Thickness_page","Top_surf_page_v2","mean_wtd","median_wtd","n_distinct_months",
             "n_distinct_years","first_year","median_year","median_month","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error


varsTOUSE<-c("longitude","latitude","Thickness_page","Top_surf_page_v2","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error

points(all_stations_updated$longitude[!is.na(all_stations_updated$label)], all_stations_updated$latitude[!is.na(all_stations_updated$label)],pch=20,cex=0.5,col="lightblue")


table(all_stations_updated$label[is.na(all_stations_updated$Top_surf_page_v2)])

summary(all_stations_updated[is.na(all_stations_updated$Top_surf_page_v2) & all_stations_updated$label=="C",])
summary(all_stations_updated[is.na(all_stations_updated$Top_surf_page_v2) & all_stations_updated$label=="U",])
summary(all_stations_updated[is.na(all_stations_updated$Top_surf_page_v2) & all_stations_updated$label=="X",])


varsTOUSE<-c("longitude","latitude","WellDepth","mean_wtd","median_wtd","n_distinct_months",
             "n_distinct_years","first_year","median_year","median_month","within_year_variability","across_year_variability","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error


varsTOUSE<-c("longitude","latitude","WellDepth","mean_wtd","median_wtd","n_distinct_months",
             "n_distinct_years","first_year","median_year","median_month","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error



varsTOUSE<-c("longitude","latitude","mean_wtd","median_wtd","n_distinct_months",
             "n_distinct_years","first_year","median_year","median_month","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error


varsTOUSE<-c("longitude","latitude","dem")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error


varsTOUSE<-c("longitude","latitude")

train_set<-all_stations_updated[complete.cases(all_stations_updated[,c(varsTOUSE,"label")]),]
testlocs<-complete.cases(all_stations_updated[,c(varsTOUSE)]) & is.na(all_stations_updated$label)
test_set<-all_stations_updated[testlocs,]

mod<-ranger(y=as.factor(train_set$label),x=train_set[,varsTOUSE],importance = "permutation",
            num.trees = 983,replace = F,min.node.size = 1,sample.fraction = 0.703,mtry = round(length(varsTOUSE)*0.257))
mod$confusion.matrix
mod$prediction.error
mod$variable.importance
nrow(train_set)
nrow(test_set)

preds<-predict(mod,test_set)
all_stations_updated$label[testlocs]<-as.character(preds$predictions)
all_stations_updated$label_accuracy[testlocs]<-1- mod$prediction.error

table(all_stations_updated$label)


all_stations_updated$geometry<-paste0("POINT(",all_stations_updated$longitude," ",all_stations_updated$latitude,")")
write.csv(all_stations_updated,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/cleaned_wells_obs/all_stationsVF.csv",row.names = F)


#####################################################################################################################3
###########################   remove duplicates   #######################################################################################3
############################################################################################################################################

obs_list<-list()
dist_thresh<-3.3 # in km
# add wellcompletionreports to list
all_wells$to_add<-T
for(i in 1:nrow(all_wells)){
  if(!all_wells$to_add[i]) next
  
  curid <- all_wells$ID[i]
  cursource <- all_wells$source[i]
  curlon <- all_wells$longitude[i]
  curlat <- all_wells$latitude[i]
  
  curdat <- all_obs[which(all_obs$ID == curid & !is.na(all_obs$date)),]
  if(nrow(curdat)== 0){
    all_wells$to_add[i] <- FALSE
    next
  }
  
  
  # restrict candidate wells first (cheap filters)
  cand_idx <- which(all_wells$to_add & all_wells$source != cursource)
  if(length(cand_idx) == 0){
    obs_list[[i]] <- curdat
    all_wells$to_add[i] <- FALSE
    next
  }
  
  # compute distances ONLY to candidates
  dist_km <- distHaversine(p1 = c(curlon, curlat),p2 = all_wells[cand_idx, c("longitude","latitude")]) / 1000
  
  close_idx <- cand_idx[dist_km < dist_thresh]
  if(length(close_idx) == 0){
    obs_list[[i]] <- curdat
    all_wells$to_add[i] <- FALSE
    next
  }
  
  close_ids <- all_wells$ID[close_idx]
  close_obs_idx <- which(all_obs$ID %in% close_ids & !is.na(all_obs$date))
  if(length(close_obs_idx) == 0){
    obs_list[[i]] <- curdat
    all_wells$to_add[i] <- FALSE
    next
  }
  close_obs <- all_obs[close_obs_idx,]
  
  curdat$possible_match <- NA
  
  for(j in 1:nrow(curdat)){
    cand <- close_obs[close_obs$date == curdat$date[j] & abs(close_obs$DWL - curdat$DWL[j]) < 1,]
    if(nrow(cand) > 0){
      curdat$possible_match[j] <- cand$ID[which.min(abs(cand$DWL - curdat$DWL[j]))]
    }
  }
  
  pm <- curdat$possible_match[!is.na(curdat$possible_match)]
  if(length(pm) == 0){
    obs_list[[i]] <- curdat
    all_wells$to_add[i] <- FALSE
    next
  }
  
  tab <- table(pm)
  keep_ids <- names(tab)[tab / nrow(curdat) >= 0.9]
  
  if(length(keep_ids) > 0){
    print(paste("MATCH FOUND at i =", i))
    obs_list[[i]] <- curdat
    all_wells$to_add[all_wells$ID %in% keep_ids] <- FALSE
  } else{
    obs_list[[i]] <- curdat
  }
  
  all_wells$to_add[i] <- FALSE
  if(i %% 100 == 0) print(i)
}


