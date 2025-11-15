
# get groundwater data US
rm(list = ls())
library(dataRetrieval)
states<-stateCd
states<-states[(states$STATE_NAME %in% c("California","Nevada","Arizona","Oregon")),]

AllSites<-c()
for(i in 1:nrow(states)){
  sites <- whatNWISsites(stateCd = states$STUSAB[i],hasDataTypeCd="gw")
  AllSites<-rbind(AllSites,sites)
  print(i)
}
rm(sites,states,i)

# gwData<-list()
# for(i in 1:nrow(AllSites)){
#   #tryCatch({
#     gw<-readNWISgwl(AllSites$site_no[i])
#     if(sum(!is.na(gw$lev_va))>0){
#       siteinfo<-attr(gw, "siteInfo")
#       dat<-cbind(gw,siteinfo)
#       gwData[[i]]<-dat[,c("lev_dt","lev_va","lev_acy_cd","dec_lat_va","dec_long_va","reliability_cd","aqfr_type_cd","well_depth_va","hole_depth_va")]
#       print(i)
#     }
#   #}, error=function(e){})
# }
library(foreach)
library(doParallel)
registerDoParallel(cores = 6)
getDoParWorkers()
cl <- makeCluster(6)
registerDoParallel(cl)
foreach(i=1:nrow(AllSites),.packages='dataRetrieval') %dopar% {
  gw<-readNWISgwl(AllSites$site_no[i])
  siteinfo<-attr(gw, "siteInfo")[1,,drop=F]
  if(sum(!is.na(gw$lev_va))>0 | (sum(!is.na(siteinfo$aqfr_type_cd))>0 & sum(!is.na(siteinfo$well_depth_va))>0)  ){
    dat<-cbind(gw,siteinfo)
    dat<-dat[,c("lev_dt","lev_va","lev_acy_cd","dec_lat_va","dec_long_va","reliability_cd","aqfr_type_cd","well_depth_va","hole_depth_va","coord_acy_cd","dec_coord_datum_cd","alt_va","alt_acy_va","parameter_cd")]
    saveRDS(dat,file = paste0("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/USGS_Rds/Site",i,".Rds"))
  }
}
stopCluster(cl)

library(data.table)
files<-list.files("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/USGS_Rds/",full.names = T)
datList<-list()
for(i in 1:length(files)){
  what<-readRDS(files[i])
  what$lev_dt<-as.character(what$lev_dt)
  what<-what[!is.na(what$lev_va) | !is.na(what$aqfr_type_cd),]
  datList[[i]]<-what
  if(i%%100==0) print(i)
}
dat<-rbindlist(datList)

dat$point_loc<-paste0("POINT (", dat$dec_long_va," ", dat$dec_lat_va, ")")

# table(str_length(dat$lev_dt)) check this
dat$year<-NA
dat$year[str_length(dat$lev_dt)==4]<-as.numeric(dat$lev_dt[str_length(dat$lev_dt)==4])
dat$year[str_length(dat$lev_dt)==7]<-as.numeric(substr(dat$lev_dt[str_length(dat$lev_dt)==7],start=1,stop=4))
dat$year[str_length(dat$lev_dt)==10]<-year(dat$lev_dt[str_length(dat$lev_dt)==10])

dat$month<-NA
dat$month[str_length(dat$lev_dt)==7]<-as.numeric(substr(dat$lev_dt[str_length(dat$lev_dt)==7],start=6,stop=7))
dat$month[str_length(dat$lev_dt)==10]<-month(dat$lev_dt[str_length(dat$lev_dt)==10])

dat$day<-NA
dat$day[str_length(dat$lev_dt)==10]<-day(dat$lev_dt[str_length(dat$lev_dt)==10])

dat$date<-paste0(dat$year,"-",dat$month,"-",dat$day)
substr(dat$date[substr(dat$date,start=6,stop=7)=="NA"],start = 6,stop=7)<-"06"
substr(dat$date[substr(dat$date,start=9,stop=10)=="NA"],start = 9,stop=10)<-"15"

dat$date<-as.character(as.Date(dat$date))
dat$lev_dt<-NULL

write.csv(dat,"C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/USGS_well_obs.csv",row.names = F)



# check for overlap

library(lubridate)
stations_casgem <- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/stations_casgem.csv")

AllUSData3 <- read.csv("C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/wtd/USGS_well_obs.csv")
AllUSData3<-AllUSData3[AllUSData3$dec_long_va< -114 & AllUSData3$dec_long_va > -124,]
AllUSData3<-AllUSData3[AllUSData3$dec_lat_va< 42 & AllUSData3$dec_lat_va > 33,]

stations_USGS<-AllUSData3[!duplicated(paste(AllUSData3$dec_lat_va, AllUSData3$dec_long_va)),]


dupstations_USGS<-AllUSData3[(duplicated(AllUSData3$dec_lat_va) & duplicated(AllUSData3$dec_long_va)),]
dupstations_USGS<-dupstations_USGS[!(duplicated(dupstations_USGS$dec_lat_va) & duplicated(dupstations_USGS$dec_long_va)),]


plot(stations_USGS$dec_long_va,stations_USGS$dec_lat_va,pch=20,cex=0.5,col="red",xlim=c(-123,-119),ylim=c(35,40.5))
points(stations_casgem$longitude,stations_casgem$latitude,pch=20,cex=0.5)


stations_USGS$in_CASGEM<-0
toR<-5
casgem_locs<-paste(round(stations_casgem$latitude,toR),round(stations_casgem$longitude,toR))
for(i in 1:nrow(stations_USGS)){

  curlatlon<-paste(round(stations_USGS$dec_lat_va[i],toR),round(stations_USGS$dec_long_va[i],toR))
  if(curlatlon %in% casgem_locs){
    stations_USGS$in_CASGEM[i]<-1
    print(paste0("I found one ",i))
  } 
  
  #if(i %% 100==0) print(i)
}




