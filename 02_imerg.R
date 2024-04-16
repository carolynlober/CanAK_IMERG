library(ncdf4)
library(raster)
library(here)
library(colorRamps)

## ------------------------------------------------------------ ## 
## LOAD ORIGINAL IMERG DATA, AVERAGE OVER MONTHS

# set directory
imerg_loc <- here("precip_data", "IMERG")

# get file names
imerg_fnames <- list.files(imerg_loc)[startsWith(list.files(imerg_loc),'3B-DAY.MS.MRG')]

# get list of unique months to look for
d <- as.character(seq(200006, 202012, 1))
d <- d[which(substring(d,5,6) %in% c("01","02","03","04","05","06","07","08","09","10","11","12"))]

# returns list of all the imerg months averages of HQPrecipitation
imerg_months_full <- sapply(d,FUN = function(x){
  # x is the string representing unique month that should be in chr 22-27 of fname
  month_fnames <- imerg_fnames[which(substring(imerg_fnames,22,27) == x)]
  
  # load all the rasters in the month
  rasters <- stack(paste0(imerg_loc, "/", month_fnames), varname="HQprecipitation")
  
  # return the mean of all the rasters in the month & orient it
  t(flip(flip(calc(rasters, mean, na.rm=T), direction='x'), direction='y')) 
})

## ------------------------------------------------------------ ## 
## REMOVE HIGH SPOTS IN LONG TERM AVERAGE FROM DAILY DATA

# get long term average of all months
imerg_longavg <- calc(brick(imerg_months_full),mean)

# create mask of too-high areas
imerg_longavg_mask <- imerg_longavg
imerg_longavg_mask[imerg_longavg < 0.8] <- 1
imerg_longavg_mask[imerg_longavg >= 0.8] <- NA

# get file names
imerg_fnames <- list.files(imerg_loc, full.names = TRUE)[startsWith(list.files(imerg_loc),'3B-DAY.MS.MRG')]

# mask all the rasters
imerg_masked <- sapply(imerg_fnames, FUN = function(x){
  scene <- t(flip(flip(raster(x, varname="HQprecipitation"), direction='x'), direction='y'))
  scene_mask <- mask(scene, imerg_longavg_mask)
  writeRaster(scene_mask, here("precip_data", "IMERG_mask", x), format='GTiff')
})

# define color palette for plotting precipitation
my_pal <- c('white',colorRamps::matlab.like2(30))

## ------------------------------------------------------------ ## 
## LOAD MASKED IMERG DATA, AVERAGE OVER MONTHS TO CREATE NEW MONTH AVERAGE

# set working directory
imerg_loc <- here("precip_data", "IMERG_mask")

# get file names
imerg_fnames <- list.files(imerg_loc)[startsWith(list.files(imerg_loc),'3B-DAY.MS.MRG')]

# get list of unique months to look for
d <- as.character(seq(200006, 202012, 1))
d <- d[which(substring(d,5,6) %in% c("01","02","03","04","05","06","07","08","09","10","11","12"))]

# returns list of all the imerg months averages of HQPrecipitation
imerg_months_full <- sapply(d,FUN = function(x){
  # x is the string representing unique month that should be in chr 22-27 of fname
  month_fnames <- imerg_fnames[which(substring(imerg_fnames,22,27) == x)]
  
  # load all the rasters in the month
  rasters <- stack(paste0(imerg_loc, "/", month_fnames))
  
  scene <- calc(rasters, mean, na.rm=T) 
  
  writeRaster(scene, here("precip_data", "IMERG_months", x), format='GTiff')
  
  cat(paste0(" done with", x))
})

