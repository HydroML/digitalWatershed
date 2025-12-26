rm(list = ls())
library(raster)
library(terra)
tif_folders<-c("N30W120-N40W110_FABDEM_V1-2","N30W130-N40W120_FABDEM_V1-2",
               "N40W120-N50W110_FABDEM_V1-2","N40W130-N50W120_FABDEM_V1-2")


filez<-NULL
for(i in 1:length(tif_folders)){
  filez<-c(filez,list.files(paste0("D:/Joe/DEM/",tif_folders[i]) ,full.names = T))
}
vrt <- vrt(filez)

e <- ext(-125, -110, 30, 45)
vrt<-crop(vrt, e)

writeRaster(vrt, filename = "D:/Joe/DEM/FABDEM_v1_2.tif", overwrite = TRUE)



















