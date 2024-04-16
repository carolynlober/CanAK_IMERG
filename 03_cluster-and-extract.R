library(robustHD)
library(cluster)
library(dendextend)
library(Polychrome)
library(rgdal)

## ------------------------------------------------------------ ## 
## CALCULATE PRECIP PERCENTILES, DO THE CLUSTERING

# calculate percentiles (25th is 0 for all, so cannot use) for each station
ghcn_meta$pct_50 <- apply(ghcn_data, 1, function(x){
  quantile(as.numeric(x), 0.5, na.rm=T)
})
ghcn_meta$pct_75 <- apply(ghcn_data, 1, function(x){
  quantile(as.numeric(x), 0.75, na.rm=T)
})

# standardize ghcn_meta & weight lat/lon
ghcn_meta_std <- as.data.frame(apply(ghcn_meta[c("lat", "lon", "elev", "pct_50", "pct_75")], 2, standardize))
ghcn_meta_std$lat <- ghcn_meta_std$lat * 2
ghcn_meta_std$lon <- ghcn_meta_std$lon * 2

# cluster with agnes method (in cluster package)
agnes_stations <- agnes(ghcn_meta_std, method="ward")

# convert to tree object
dend <- as.dendrogram(as.hclust(agnes_stations))

# create color palette
c40 <- createPalette(40,  c("#ff0000", "#00ff00", "#0000ff")) 

# plot the tree
par(pty="m")
dend %>% set("branches_k_color", 
             value = c40, k = 40) %>% 
  plot(main = "GHCN Station Dendrogram", leaflab="none")

# cut the tree
agnes_cut <- cutree(agnes_stations, k=40)
#hclust_cut <- cutree(hclust_stations, k=30)

# put cluster information into 
ghcn_meta$hclust_40 <- as.factor(agnes_cut)

# get crs (WGS84) from big_study 
my_crs <- crs(readOGR(here("gis", "big_study.shp")))

# make spdf
ghcn_hclust40 <- ghcn_meta[,c("file", "lat", "lon", "elev")]
ghcn_hclust40$clust <- as.numeric(as.character(ghcn_meta$hclust_40))
ghcn_hclust40 <- SpatialPointsDataFrame(ghcn_meta[, c(4,3)],
                                        ghcn_hclust40,
                                        proj4string = my_crs)
writeOGR(ghcn_hclust40, here("gis"),
         "ghcn_hclust40", driver="ESRI Shapefile")

## ------------------------------------------------------------ ## 
## AGGREGATE GHCN DATA AND METADATA

# aggregate precip data to ghcn_data_clust 
ghcn_data_clust <- aggregate(ghcn_data, by = list(ghcn_meta$hclust_40), FUN = \(x) mean(x, na.rm=T))
ghcn_data_clust <- ghcn_data_clust[, -1]
ghcn_data_clust[40, ] <- ghcn_data[which(ghcn_meta$hclust_40 == 40), ] # for some reason this was all NaN

# aggregate metadata to ghcn_meta_clust
ghcn_meta_clust <- aggregate(ghcn_meta[, c("lat", "lon", "elev")], by = list(ghcn_meta$hclust_40), mean)

# make count variable
ghcn_meta$count <- rep(1, times = nrow(ghcn_meta))

# aggregate count (num stations in each cluster)
ghcn_meta_clust$count <- aggregate(ghcn_meta$count, by = list(ghcn_meta$hclust_40), sum)$x

## ------------------------------------------------------------ ## 
## CREATE BUFFERED (CONCAVE) HULLS

# get crs for ABoVE land cover 
above_crs <- crs(raster(list.files(here("precip_data", "ABoVE"))[1]))

# reproject ghcn_hclust40 for buffering (using m units in buffer function)
ghcn_hclust40_reproj <- spTransform(ghcn_hclust40, above_crs)

# create buffered (concave) hulls for each cluster
buffered_hulls <- sapply(c(1:40), function(x) {
  cat(x, " ")
  my_clust <- ghcn_hclust40_reproj[which(ghcn_hclust40_reproj$clust==x), ]
  big <- buffer(my_clust, width=100000, dissolve=T)
  buffer(big, width=-90000, dissolve=T)
})

# make spdf
order_ndx <- unique(ghcn_hclust40@data$clust)
data <- ghcn_meta_clust[order_ndx[1], ]
buffered_hulls_spdf <- SpatialPolygonsDataFrame(buffered_hulls[[1]], data = data, match.ID = FALSE)
for(x in 2:40) {
  data <- ghcn_meta_clust[order_ndx[x], ]
  spdf <- SpatialPolygonsDataFrame(buffered_hulls[[x]], data = data, match.ID = FALSE)
  buffered_hulls_spdf <- rbind(buffered_hulls_spdf, spdf)
}

# rename buffered_hulls
buffered_hulls <- buffered_hulls_spdf
rm(buffered_hulls_spdf)

# reproject back to my_crs so that it matches the data
buffered_hulls <- spTransform(buffered_hulls, my_crs)

## ------------------------------------------------------------ ## 
## EXTRACT DAILY IMERG DATA AT BUFFERED HULLS 

# set working directory
imerg_loc <- here("precip_data", "IMERG_mask")

# get file names
imerg_fnames <- list.files(imerg_loc)[startsWith(list.files(imerg_loc),'3B-DAY.MS.MRG')]

# extract data at each file
imerg_data_clust <- sapply(paste0(imerg_loc, "/", imerg_fnames), function(x){
  # open file
  scene <- raster(x)
  # extract data
  return(raster::extract(x=scene,buffered_hulls, fun = mean, na.rm = TRUE))
  # at end of year, print that the year is done
  if (substr(x, 26, 29) == "1231") {
    cat(paste(substr(x, 22, 25)," is done! ")) 
  }
})
write.table(imerg_data_clust, here("results", "imerg_data_clust.csv"))

imerg_data_clust <- as.data.frame(imerg_data_clust)
colnames(imerg_data_clust) <- (paste(substr(imerg_fnames, 22, 25), 
                                     substr(imerg_fnames, 26, 27),
                                     substr(imerg_fnames, 28, 29), sep="-"))

imerg_data_clust <- imerg_data_clust[,1:7520] # match the length of ghcn_data_clust

## ------------------------------------------------------------ ## 
## EXTRACT MONTHLY IMERG DATA AT BUFFERED HULLS
  
# set working directory
imerg_loc <- here("precip_data", "IMERG_months")

# get file names and load rasters
imerg_fnames <- list.files(imerg_loc, full.names = TRUE)
imerg_months_full <- sapply(imerg_fnames, raster)

# extract the data
imerg_months <- lapply(imerg_months_full, function(x){
  raster::extract(x = x, y = buffered_hulls, fun = mean, na.rm = TRUE)
})

# put into matrix
imerg_months <- do.call(cbind, imerg_months)

# make dataframe
imerg_months <- as.data.frame(imerg_months)
colnames(imerg_months) <- names(imerg_months_full)




