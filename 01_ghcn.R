library(DescTools)

## ------------------------------------------------------------ ## 
## LOAD STATION DATA/FILE NAMES, SUBSET TO THE PAD

# set directory 
setwd("/Volumes/Extreme\ SSD/precip_data/GHCN/daily-summaries-latest_12.30")

# set study date range 
my_dates <- as.Date(c("2000-06-01", "2020-12-31"))
all_study_days <- seq(my_dates[1],my_dates[2],by="days")

# ghcn metadata
ghcn_stations_meta <- read.csv("/Volumes/GoogleDrive/My\ Drive/senior/thesis/ghcnd-stations.csv",header=F)
ghcn_stations_meta$V1 <- as.character(ghcn_stations_meta$V1)

# list files
ghcn_fnames <- list.files()

# get files in Alaska
ak_fnames <- ghcn_stations_meta$V1[which(ghcn_stations_meta$V5 == "AK" & startsWith(ghcn_stations_meta$V1,"US"))]
ca_fnames <- ghcn_stations_meta$V1[which(startsWith(ghcn_stations_meta$V1,"CA"))]
to_keep <- c(ak_fnames,ca_fnames)

# get files in Canada and Alaska 
to_remove <- list.files()[which(!(substr(list.files(),1,11) %in% to_keep))]
sapply(to_remove, unlink) 

# loop through and load 
df_all <- sapply(ghcn_fnames,function(x){
  df <- read.csv(x,sep=",",header=T)
  df$DATE <- as.Date(df$DATE)
  station_dates <- c(df$DATE[1],df$DATE[length(df$DATE)])
  if (station_dates %overlaps% my_dates){
    return(df)
  }
  else {
    return(NULL)
  }
})

# remove null items
df_all = df_all[-which(sapply(df_all, is.null))]

# put all coordinates into an array--could extend this to add all the data, too
coords <- sapply(df_all,function(x){
  lat <- x$LATITUDE[1]
  lon <- x$LONGITUDE[1]
  elev <- x$ELEVATION[1]
  fname <- as.character(x$STATION[1])
  sta_name <- as.character(x$NAME[1])
  c(fname, sta_name, lat, lon, elev)
})

# put coordinates into data frame
coords <- data.frame(t(coords))
colnames(coords) <- c("file","name","lat","lon","elev")

# convert lat and lon from factors into numeric 
coords$lat <- as.numeric(as.character(coords$lat))
coords$lon <- as.numeric(as.character(coords$lon))
coords$elev <- as.numeric(as.character(coords$elev))

# define bounds of big study area
xmin <- -120.3
xmax <- -96.74
ymin <- 54.58
ymax <- 62.84

# subset only PAD stations
pad_coords <- subset(coords, lat <= ymax & lat >= ymin & lon <= xmax & lon >= xmin)

## ------------------------------------------------------------ ## 
## LOOP THROUGH FILES AND PUT INTO A DATAFRAME

## put the data into a dataframe
## takes a looong time

# get file names
ghcn_fnames <- paste(pad_coords$file,".csv",sep="")

# load all the files
pad_list <- lapply(ghcn_fnames, function(x){
  read.csv(x,sep=",",header=T)
})

# initialize data frame for all prcp data
pad_ghcn_data <- matrix(nrow=length(pad_list),ncol=length(all_study_days))
pad_ghcn_data <- data.frame(pad_ghcn_data,row.names=pad_coords$file)
colnames(pad_ghcn_data) <- all_study_days

# populate dataframe by looping through all files
for(x in pad_list[1:length(pad_list)]){
  if ("PRCP" %in% colnames(x)) {
    for(d in 1:length(all_study_days)) {
      if (all_study_days[d] %in% as.Date(x$DATE)) {
        pad_ghcn_data[as.character(x$STATION[1]),d] <- x$PRCP[which(as.Date(x$DATE) == all_study_days[d])]
      }
    }
    cat(paste(x$STATION[1]," is done ",sep=""))
  }
  else {
    cat(paste(x$STATION[1]," has no precipitation data",sep=""))
  } 
}

## ------------------------------------------------------------ ## 
## OR, LOAD THE FILE I ALREADY MADE

# I DID THIS
pad_ghcn_data <- read.csv("/Volumes/GoogleDrive/My\ Drive/senior/thesis/pad_stations_data.csv")
rownames(pad_ghcn_data) <- pad_ghcn_data$X
pad_ghcn_data <- pad_ghcn_data[,-1]
colnames(pad_ghcn_data) <- all_study_days

## ------------------------------------------------------------ ## 
## MAKE SURE THE LOADED FILE AGREES WITH PAD_COORDS

# match pad_ghcn_data to pad_coords and vice versa 
pad_ghcn_data <- pad_ghcn_data[which(rownames(pad_ghcn_data) %in% pad_coords$file),]
pad_coords <- pad_coords[which(pad_coords$file %in% rownames(pad_ghcn_data)),]

## ------------------------------------------------------------ ## 
## DEAL W REPEATED STATIONS (IDENTICAL COORDINATES)

# get list of all file names that have at least one matching station with identical coordinates
repeated <- which(duplicated(pad_coords[,c("lat","lon")]))
repeated_pairs <- sapply(repeated,function(x){
  coords <- pad_coords[x,c("lat","lon")]
  pair <- which(pad_coords[,"lat"] == coords$lat & pad_coords[,"lon"] == coords$lon)
  pair_data <- pad_ghcn_data[pair,]
  overlap <- apply(pair_data,2,function(x){
    sum(!is.na(x)) >= 2
  })
  #print(pad_coords[pair,c("file","name")])
  #print(paste("number of overlapping days: ",sum(overlap)))
  #print(paste("proportion of overlapping days: ", sum(overlap)/7520))
  as.character(pad_coords$file[pair])
})
repeated_pairs <- unlist(repeated_pairs)

# list one file name from each group of stations with identical coordinates
repeated_file <- c('CA001182285',
                   'CA002202400',
                   'CA002204100',
                   'CA00220L001',
                   'CA003062693',
                   'CA003063685',
                   'CA003064528',
                   'CA003065995',
                   'CA003070560',
                   'CA003072655',
                   'CA003072915',
                   'CA003073140',
                   'CA003076069',
                   'CA004061629',
                   'CA004063560',
                   'CA004063755',
                   'CA004063753',
                   'CA004064149',
                   'CA004068340',
                   'CA005050919',
                   'CA005061646')

# merge repeats based on prioritizing longest time series
merged_repeats <- sapply(repeated_file,function(x){
  coords <- pad_coords[which(pad_coords$file == x),c("lat","lon")]
  pair <- which(pad_coords[,"lat"] == coords$lat & pad_coords[,"lon"] == coords$lon)
  pair_data <- pad_ghcn_data[pair,]
  num_days <- apply(pair_data,1,function(x){
    sum(!is.na(x))
  })
  num_days <- data.frame(num_days)
  order_files <- rownames(num_days)[order(-num_days)]
  apply(pair_data,2,function(x){
    if (any(!is.na(x))) {
      x[min(which(!is.na(x)))]
    }
    else {
      NA
    }
  })
})
merged_repeats <- as.data.frame(t(merged_repeats))

merged_elev <- sapply(repeated_file,function(x){
  coords <- pad_coords[which(pad_coords$file == x),c("lat","lon")]
  pair <- which(pad_coords[,"lat"] == coords$lat & pad_coords[,"lon"] == coords$lon)
  pad_coords[pair,"elev"]
})

merged_meta <- pad_coords[0,]

for (x in repeated_file) {
  coords <- pad_coords[which(pad_coords$file == x),c("lat","lon")]
  pair <- which(pad_coords[,"lat"] == coords$lat & pad_coords[,"lon"] == coords$lon)
  pair_data <- pad_ghcn_data[pair,]
  num_days <- apply(pair_data,1,function(x){
    sum(!is.na(x))
  })
  num_days <- data.frame(num_days)
  order_files <- rownames(num_days)[order(-num_days)]
  merged_meta <- rbind(merged_meta,pad_coords[which(pad_coords$file == order_files[1]),])
}

## THIS IS MY FINAL FINAL DATA FRAME(S)

# remove the stations with no overlap starting in june 2020
#ghcn_data <- pad_ghcn_data[-which(is.na(pad_coords$pct_50)),]
ghcn_data <- pad_ghcn_data

# remove the stations that have a repeat
ghcn_data <- ghcn_data[-which(rownames(ghcn_data) %in% repeated_pairs),]

# add the merged repeated stations data
ghcn_data <- rbind(ghcn_data,merged_repeats)

# 8/13: scale down by 10 because they are scaled up for some reason
# note that pad_stations_data etc. are unscaled
ghcn_data <- ghcn_data/10

# do the same for the station metadata (info taken from longest time series)
#ghcn_meta <- pad_coords[-which(is.na(pad_coords$pct_50)),]
ghcn_meta <- pad_coords
ghcn_meta <- ghcn_meta[-which(pad_coords$file %in% repeated_pairs),]
ghcn_meta <- rbind(ghcn_meta,merged_meta)
# save only some columns, the others will be recalculated

# take out stations with all NA
all_na <- rownames(ghcn_data)[apply(ghcn_data,1,function(x){sum(is.na(x)) == ncol(ghcn_data)})]
ghcn_meta <- ghcn_meta[-which(ghcn_meta$file %in% all_na),]

# make sure they match 
ghcn_data <- ghcn_data[-which(rownames(ghcn_data) %in% all_na),]

# now we have all the data and metadata for 218 stations in the PAD in ghcn_data and ghcn_meta











