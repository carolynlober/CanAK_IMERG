## ------------------------------------------------------------ ## 
## LOAD AND RESAMPLE ABOVE DATA

# set working directory for easier access to filenames
setwd("/Volumes/Extreme\ SSD/precip_data/ABoVE")

# get file names
above_fnames <- list.files()[startsWith(list.files(),'ABoVE_')]

# define mode function ( https://www.tutorialspoint.com/r/r_mean_median_mode.htm )
getmode <- function(v,na.rm=T) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# resample all above files and save to rasters
above_300m <- sapply(above_fnames,FUN = function(x){
  above_scene <- brick(x)[[31]]
  cat(paste("working on ",substr(x,17,23)," "))
  fname <- paste0("/Volumes/Extreme\ SSD/precip_data/ABoVE_resam/",x)
  writeRaster(aggregate(above_scene,fact=10,fun=getmode),fname)
})

## ------------------------------------------------------------ ## 
## EXTRACT ABOVE DATA AT GHCN STATIONS, PUT INTO ghcn_hclust40@data

# extract ABoVE data
above_vec <- lapply(above_300m,FUN = function(x){
  raster::extract(x=x,y=ghcn_hclust40)
})
above_vec <- do.call(cbind,above_vec)

# collapse into vector
above_vec <- apply(above_vec,1,function(x){
  if(sum(is.na(x)) == length(x)){
    return(NA)
  }
  else 
    return(x[which(!is.na(x))])
})

# put into ghcn_hclust40@data
ghcn_hclust40@data$above <- above_vec

# put into ghcn_meta
ghcn_meta$above <- above_vec

## ------------------------------------------------------------ ## 
## LOAD AND RESAMPLE DEM DATA, PUT INTO ghcn_meta

# load DEM (plus slope)
merit_dem <- raster('/Volumes/Extreme\ SSD/precip_data/MERIT\ DEM/merged_merit_projected_2.tif')
#merit_slope <- raster('/Volumes/Extreme\ SSD/precip_data/MERIT\ DEM/merit_dem_slope.tif')
#merit_aspect <- terrain(merit_dem,option="aspect")

# resample DEM (plus slope)
merit_300m <- aggregate(merit_dem,fact=10,fun=mean)
merit_300m_sd <- aggregate(merit_dem,fact=10,fun=sd)
merit_slope_300m <- terrain(merit_300m,opt="slope",unit="degrees")
merit_aspect_300m <- terrain(merit_300m,opt="aspect",unit="degrees")

# save rasters
writeRaster(merit_300m,"/Volumes/Extreme\ SSD/precip_data/MERIT\ DEM/merit_300m.tif")
writeRaster(merit_300m_sd,"/Volumes/Extreme\ SSD/precip_data/MERIT\ DEM/merit_300m_sd.tif")
writeRaster(merit_slope_300m,"/Volumes/Extreme\ SSD/precip_data/MERIT\ DEM/merit_slope_300m.tif", overwrite=T)
writeRaster(merit_aspect_300m,"/Volumes/Extreme\ SSD/precip_data/MERIT\ DEM/merit_aspect_300m.tif", overwrite=T)

# put into ghcn_meta
ghcn_meta$merit <- raster::extract(x=merit_300m,y=ghcn_hclust40)
ghcn_meta$slope <- raster::extract(x=merit_slope_300m,y=ghcn_hclust40)
ghcn_meta$aspect <- raster::extract(x=merit_aspect_300m,y=ghcn_hclust40)

## ------------------------------------------------------------ ## 
## AGGREGATE NEW ghcn_meta COLUMNS TO ghcn_meta_clust

# aggregate merit and landcover 
ghcn_meta_clust$landcover <- as.factor(aggregate(ghcn_meta$above,by=list(ghcn_meta$hclust_40),getmode)$x)
ghcn_meta_clust$merit <- aggregate(ghcn_meta$merit,by=list(ghcn_meta$hclust_40),mean)$x
ghcn_meta_clust$slope <- aggregate(ghcn_meta$slope,by=list(ghcn_meta$hclust_40),mean)$x
ghcn_meta_clust$aspect <- aggregate(ghcn_meta$aspect,by=list(ghcn_meta$hclust_40),mean)$x

# define mode function
# from tutorialspoint
getmode <- function(v,...) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


