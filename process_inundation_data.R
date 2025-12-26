rm(list = ls())
library(raster)


# Local directory to store files
out_dir <- "D:/Joe/inundation/raw_tiffs"

safe_read_html <- function(base_url) {
  tryCatch(
    {
      html <- readLines(base_url, warn = FALSE)
      list(ok = TRUE,html = html)
    },
    error = function(e) {
      message("Skipping: ", base_url)
      list(ok = FALSE,html = NULL)
    }
  )
}

yearz<-1984:2021
for(y in 1:length(yearz)){
  for(m in 1:12){
    cur_m<-as.character(m)
    if(str_length(cur_m)==1) cur_m<-paste0("0",cur_m)
    base_url <- paste0("https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GSWE/MonthlyHistory/LATEST/tiles/"
                       ,yearz[y],"/",yearz[y],"_",cur_m,"/") # Base URL of the directory you want to download from
    
    
    
    res <- safe_read_html(base_url)
    
    if(res$ok){
      html<-res$html
      
      tif_files <- unique(
        gsub(
          '.*href="([^"]+\\.tif)".*',
          '\\1',
          html[grepl("\\.tif", html)]
        )
      )
      toget<-grepl("0000160000-0000200000", tif_files) | grepl("0000160000-0000240000", tif_files) | grepl("0000120000-0000200000", tif_files) | grepl("0000120000-0000240000", tif_files)
      
      tif_files<-tif_files[toget]
      
      save_az<-tif_files
      save_az[!grepl(paste0(yearz[y],"_",cur_m,"-"),tif_files)]<-paste0(yearz[y],"_",cur_m,"-",
                                                                        save_az[!grepl(paste0(yearz[y],"_",cur_m,"-"),tif_files)])
      
      # Extract .tif filenames using regex
      for (f in 1:length(tif_files)) {
        file_url  <- paste0(base_url, tif_files[f])
        dest_file <- file.path(out_dir, save_az[f])
        
        if (!file.exists(dest_file)) {
          cat("Downloading:", save_az[f], "\n")
          download.file(
            url = file_url,
            destfile = dest_file,
            mode = "wb",
            quiet = TRUE
          )
        } else {
          cat("Skipping (already exists):", save_az[f], "\n")
        }
      }
    }
    
    
  }
}


# download maximum extent as well
base_url <- paste0("https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GSWE/Aggregated/LATEST/extent/tiles/") # Base URL of the directory you want to download from
res <- safe_read_html(base_url)
html<-res$html

tif_files <- unique(
  gsub(
    '.*href="([^"]+\\.tif)".*',
    '\\1',
    html[grepl("\\.tif", html)]
  )
)
toget<-grepl("0000160000-0000200000", tif_files) | grepl("0000160000-0000240000", tif_files) | grepl("0000120000-0000200000", tif_files) | grepl("0000120000-0000240000", tif_files)

tif_files<-tif_files[toget]
save_az<-tif_files

for (f in 1:length(tif_files)) {
  file_url  <- paste0(base_url, tif_files[f])
  dest_file <- file.path(out_dir, save_az[f])
  
  if (!file.exists(dest_file)) {
    cat("Downloading:", save_az[f], "\n")
    download.file(
      url = file_url,
      destfile = dest_file,
      mode = "wb",
      quiet = TRUE
    )
  } else {
    cat("Skipping (already exists):", save_az[f], "\n")
  }
}


library(terra)

setwd("D:/Joe/inundation/raw_tiffs/")
filez<-c("0000160000-0000200000.tif","0000160000-0000240000.tif","0000120000-0000200000.tif","0000120000-0000240000.tif")
cur_filez<-paste0("extent","-",filez)
r <- vrt(cur_filez)
plot(r)

setwd("D:/Joe/inundation/")
outfile <- "max_extent.csv"
writeLines("lon,lat", outfile)

bs <- blocks(r)
readStart(r)

for (i in 1:bs$n) {
  v <- readValues(r, bs$row[i], bs$nrows[i])
  if (is.null(v)) next
  
  rows <- bs$row[i]:(bs$row[i] + bs$nrows[i] - 1)
  cells <- cellFromRowColCombine(
    r,
    rows,
    seq_len(ncol(r))
  )
  
  lat <- yFromCell(r, cells)
  
  idx <- which(v == 1 & lat <= 45)
  if (length(idx) == 0) next
  
  xy <- xyFromCell(r, cells[idx])
  write.table(
    xy,
    outfile,
    sep = ",",
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE
  )
}

readStop(r)




#################################################################################################3333
#####################################  get monthly extent ###########################################
################################################################################################
rm(list = ls())
library(terra)
library(stringr)
library(arrow)


setwd("D:/Joe/inundation/raw_tiffs/")
filez<-c("0000160000-0000200000.tif","0000160000-0000240000.tif","0000120000-0000200000.tif","0000120000-0000240000.tif")
max_extent <- read.csv("D:/Joe/inundation/max_extent.csv")
max_extent <- as.matrix(max_extent)
gc()

yearz<-1984:2021
for(y in 1:length(yearz)){
  for(m in 1:12){
    cur_m<-as.character(m)
    if(str_length(cur_m)==1) cur_m<-paste0("0",cur_m)
    
    cur_filez<-paste0(yearz[y],"_",cur_m,"-",filez)
    
    
    ok <- tryCatch(
      {
        vrt <- vrt(cur_filez)
        TRUE
      },
      error = function(e) {
        message("Skipping: ", yearz[y], "/", m)
        FALSE
      }
    )
    
    #plot(vrt)
    if(ok){
      # Extract values (streamed from VRT)
      vals <- extract(vrt, max_extent)
      gc()
      colnames(vals)<- "val"
      vals$month<-m
      vals$year<-yearz[y]
      
      result <- cbind(max_extent, vals)
      write_parquet(result,paste0("D:/Joe/inundation/for_redivis/vals_",yearz[y],"_",m,".parquet"),compression = "zstd")
    }
  }
}



