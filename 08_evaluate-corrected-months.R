library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(Metrics)
library(SimDesign)

## ------------------------------------------------------------ ## 
## EXTRACT MONTHLY IMERG DATA AT BUFFERED HULLS

# extract the data
imerg_months_corrected <- lapply(imerg_months_full_corrected,function(x){
  raster::extract(x=x,y=buffered_hulls,fun=mean,na.rm=T)
})

# put into matrix
imerg_months_corrected <- do.call(cbind,imerg_months_corrected)

# make dataframe
imerg_months_corrected <- as.data.frame(imerg_months_corrected)
colnames(imerg_months_corrected) <- names(imerg_months_full_corrected)

## ------------------------------------------------------------ ## 
## PLOT DOUBLE MASS CURVES

# uses double_mass_plot from 06_evaluate-original-months

# get elevation order of clusters
elev_order <- rownames(ghcn_months)[order(ghcn_meta_clust$elev)]

# plot dmc for monthly
p_all <- lapply(elev_order,FUN=function(x){
  ndx1 <- which(rownames(ghcn_months) == x)
  ndx2 <- which(rownames(imerg_months_corrected) == x)
  double_mass_plot(as.numeric(ghcn_months[ndx1,]),as.numeric(imerg_months_corrected[ndx2,])) +
    #labs(subtitle=x) + 
    theme(plot.subtitle=element_text(hjust=0.5)) +
    xlab(NULL) + ylab(NULL) + theme(legend.position = "none")
  #+ scale_colour_manual(values=my_colors)
  #scale_color_gradientn(colours = rainbow(247))
})

# plot all the plots
grid.arrange(grobs=p_all,nrow=5, 
             top= textGrob("Double Mass Curves for Monthly Accumulated Precipitation",
                           y = 0.5,
                           gp = gpar(fontsize = 18)),
             bottom = "Station Cumulative Precipitation",
             left = "IMERG Cumulative Precipitation")

## ------------------------------------------------------------ ## 
## PLOT SCATTER PLOTS

# plot scatter plot for monthly
p_scatter <- lapply(c(1:40),FUN=function(x){
  Station <- as.numeric(ghcn_months[x,])
  IMERG <- as.numeric(imerg_months_corrected[x,])
  
  upper_lim <- max(quantile(Station,0.9,na.rm=T),quantile(IMERG,0.9,na.rm=T))
  temp_df <- data.frame(Station,IMERG)
  colnames(temp_df) <- c("Station","IMERG")
  ggplot(temp_df, aes(x=Station,y=IMERG)) + 
    geom_point(color=c40[x]) + 
    coord_fixed(xlim=c(0,upper_lim),ylim=c(0,upper_lim)) +
    theme(aspect.ratio=1) +
    geom_abline() +
    labs(subtitle=x) + theme(plot.subtitle=element_text(hjust=0.5)) +
    xlab(NULL) + ylab(NULL) 
  #xlab("Station (mm/day)") + ylab("IMERG (mm/day)") 
})
grid.arrange(grobs=p_scatter,ncol=8, 
             top= textGrob("Scatter Plots for Monthly Accumulated Precipitation",
                           y = 0.5, 
                           gp = gpar(fontsize = 18)),
             bottom = "Station Measurement (mm/month)",
             left = "IMERG Estimate (mm/month)")

## ------------------------------------------------------------ ## 
## CALCULATE METRICS

# uses relative_bias from 06_evaluate-original-months

bias_corrected <- sapply(c(1:40),function(x){
  ndx1 <- which(ghcn_data_hclust40$Group.1 == x)
  ndx2 <- which(rownames(imerg_data_hulls) == x)
  tmp_ghcn <- ghcn_data_hclust40[,-1]
  no_na <- which(!is.na(tmp_ghcn[ndx1,]))
  bias(as.numeric(imerg_data_hulls[ndx2,no_na]),as.numeric(tmp_ghcn[ndx1,no_na]))
})

# calculate bias for all of the clusters
bias_total_corrected <- sapply(1:ncol(ghcn_months),function(x){
  imerg_months_corrected[,x] - ghcn_months[,x] 
})
bias_corrected <- apply(bias_total_corrected,1,function(x){mean(x,na.rm=T)})

# calculate relative bias for all clusters
rbias_corrected <- sapply(c(1:40),function(x){
  ndx1 <- which(rownames(ghcn_months) == x)
  ndx2 <- which(rownames(imerg_months_corrected) == x)
  no_na <- which(!is.na(ghcn_months[ndx1,]))
  mean(relative_bias(as.numeric(imerg_months_corrected[ndx2,no_na]),as.numeric(ghcn_months[ndx1,no_na])),na.rm=T)
})

# calculate Pearson's r for all clusters
cor_corrected <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months) == x)
  ndx2 <- which(rownames(imerg_months_corrected) == x)
  cor(as.numeric(ghcn_months[ndx1,]),
      as.numeric(imerg_months_corrected[ndx2,]),use="pairwise.complete.obs")
})

# calculate rmse for all clusters
rmse_corrected <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months) == x)
  ndx2 <- which(rownames(imerg_months_corrected) == x)
  no_na <- which(!is.na(ghcn_months[ndx1,])) # change this so that both have to be non-na
  rmse(as.numeric(ghcn_months[ndx1,no_na]),
       as.numeric(imerg_months_corrected[ndx2,no_na]))
})

# and mae for all clusters
mae_corrected <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months) == x)
  ndx2 <- which(rownames(imerg_months_corrected) == x)
  no_na <- which(!is.na(ghcn_months[ndx1,])) # change this so that both have to be non-na
  mae(as.numeric(ghcn_months[ndx1,no_na]),
      as.numeric(imerg_months_corrected[ndx2,no_na]))
})

# make binary data frames
ghcn_months_bin <- as.data.frame(ghcn_months > 0.5)
imerg_months_bin <- as.data.frame(imerg_months > 0.5)
imerg_months_corr_bin <- as.data.frame(as.data.frame(imerg_months_corrected) > 0.5)


# calculate probability of detection for all clusters
pod <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months_bin) == x)
  ndx2 <- which(rownames(imerg_months_bin) == x)
  calc_pod(ghcn_months_bin[ndx1,],imerg_months_bin[ndx2,])
})

# calculate false alarm ratio for all clusters
far <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months_bin) == x)
  ndx2 <- which(rownames(imerg_months_bin) == x)
  calc_far(ghcn_months_bin[ndx1,],imerg_months_bin[ndx2,])
})

# calculate frequency bias for all clusters
fbi <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months_bin) == x)
  ndx2 <- which(rownames(imerg_months_bin) == x)
  calc_fbi(ghcn_months_bin[ndx1,],imerg_months_bin[ndx2,])
})

# calculate heidke skill score for all clusters
hss <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months_bin) == x)
  ndx2 <- which(rownames(imerg_months_bin) == x)
  calc_hss(ghcn_months_bin[ndx1,],imerg_months_bin[ndx2,])
})

# calculate critical success index for all clusters
csi <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months_bin) == x)
  ndx2 <- which(rownames(imerg_months_bin) == x)
  calc_csi(ghcn_months_bin[ndx1,],imerg_months_bin[ndx2,])
})

# calculate probability of detection for all clusters
pod_corrected <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months_bin) == x)
  ndx2 <- which(rownames(imerg_months_corr_bin) == x)
  calc_pod(ghcn_months_bin[ndx1,],imerg_months_corr_bin[ndx2,])
})

# calculate false alarm ratio for all clusters
far_corrected <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months_bin) == x)
  ndx2 <- which(rownames(imerg_months_corr_bin) == x)
  calc_far(ghcn_months_bin[ndx1,],imerg_months_corr_bin[ndx2,])
})

# calculate frequency bias for all clusters
fbi_corrected <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months_bin) == x)
  ndx2 <- which(rownames(imerg_months_corr_bin) == x)
  calc_fbi(ghcn_months_bin[ndx1,],imerg_months_corr_bin[ndx2,])
})

# calculate heidke skill score for all clusters
hss_corrected <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months_bin) == x)
  ndx2 <- which(rownames(imerg_months_corr_bin) == x)
  calc_hss(ghcn_months_bin[ndx1,],imerg_months_corr_bin[ndx2,])
})

# calculate critical success index for all clusters
csi_corrected <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_months_bin) == x)
  ndx2 <- which(rownames(imerg_months_corr_bin) == x)
  calc_csi(ghcn_months_bin[ndx1,],imerg_months_corr_bin[ndx2,])
})


# put into a table 
table <- data.frame(c(1:40),bias_all_clust,bias_corrected,rbias_all_clust,rbias_corrected,
                    cor_all_clust,cor_corrected,rmse_all_clust,rmse_corrected,
                    mae_all_clust,mae_corrected,pod,far,fbi,hss,csi,pod_corrected,
                    far_corrected,fbi_corrected,hss_corrected,csi_corrected)

table$pod <- pod
table$pod_corrected <- pod_corrected
table$far <- far
table$far_corrected <- far_corrected
table$fbi <- fbi
table$fbi_corrected <- fbi_corrected
table$csi <- csi
table$csi_corrected <- csi_corrected
table$hss <- hss
table$hss_corrected <- hss_corrected

# calculate means
table_means <- apply(table,2,mean)
write.csv(table_means,"/Volumes/GoogleDrive/My\ Drive/senior/thesis/metrics_table.csv")

# plot metric boxplots
par(mfrow=c(2,5))
boxplot(table$bias_all_clust,table$bias_corrected,col=c('#424242','#0B6374'),
        names=c("HQ","corrected"),main="Absolute bias")
abline(h=0, col='red')
boxplot(table$rbias_all_clust,table$rbias_corrected,col=c('#424242','#0B6374'),
        names=c("HQ","corrected"),main="Relative bias")
abline(h=0, col='red')
boxplot(table$cor_all_clust,table$cor_corrected,col=c('#424242','#0B6374'),
        names=c("HQ","corrected"),main="Correlation\ncoefficient",ylim=c(0.25,1))
abline(h=1, col='red')
boxplot(table$rmse_all_clust,table$rmse_corrected,col=c('#424242','#0B6374'),
        names=c("HQ","corrected"),main="RMSE",ylim=c(0,3.5))
abline(h=0, col='red')
boxplot(table$mae_all_clust,table$mae_corrected,col=c('#424242','#0B6374'),
        names=c("HQ","corrected"),main="MAE",ylim=c(0,3.0))
abline(h=0, col='red')

boxplot(table$pod,table$pod_corrected,col=c('#424242','#0B6374'),
        names=c("HQ","corrected"),main="POD",ylim=c(0,1))
abline(h=0, col='red')
boxplot(table$far,table$far_corrected,col=c('#424242','#0B6374'),
        names=c("HQ","corrected"),main="FAR",ylim=c(0,1))
abline(h=0, col='red')
boxplot(table$fbi,table$fbi_corrected,col=c('#424242','#0B6374'),
        names=c("HQ","corrected"),main="FBI",ylim=c(0.25,1.5))
abline(h=1, col='red')
boxplot(table$csi,table$csi_corrected,col=c('#424242','#0B6374'),
        names=c("HQ","corrected"),main="CSI",ylim=c(0,1))
abline(h=0, col='red')
boxplot(table$hss,table$hss_corrected,col=c('#424242','#0B6374'),
        names=c("HQ","corrected"),main="HSS",ylim=c(0,1))
abline(h=0, col='red')



## ------------------------------------------------------------ ## 
## PLOT A COUPLE OF MONTHS/DAYS 

# daily 2000-06-01
fname_corrected <- paste0("/Volumes/Extreme\ SSD/precip_data/IMERG_mask_corrected/",imerg_fnames[1])
plot(raster(fname_corrected),col=my_pal,zlim=c(0,3.5),main="2000-06-01 corrected")

fname_original <- paste0("/Volumes/Extreme\ SSD/precip_data/IMERG_mask/",imerg_fnames[1])
plot(raster(fname_original),col=my_pal,zlim=c(0,3.5),main="2000-06-01 original")

# daily 2010-12-01
fname_corrected <- paste0("/Volumes/Extreme\ SSD/precip_data/IMERG_mask_corrected/",imerg_fnames[3836])
plot(raster(fname_corrected),col=my_pal,zlim=c(0,8.2),main="2010-12-01 corrected")

fname_original <- paste0("/Volumes/Extreme\ SSD/precip_data/IMERG_mask/",imerg_fnames[3836])
plot(raster(fname_original),col=my_pal,zlim=c(0,8.2),main="2010-12-01 original")

# monthly 2000-06
plot(imerg_months_full[[1]],col=my_pal,main="2000-06 original",zlim=c(0,2))
plot(imerg_months_full_corrected[[1]],col=my_pal,main="2000-06 corrected",zlim=c(0,2))

# daily 2010-12-01
which(names(imerg_months_full) == "201012")
plot(imerg_months_full[[127]],col=my_pal,main="2010-12 original",zlim=c(0,2))
plot(imerg_months_full_corrected[[127]],col=my_pal,main="2010-12 corrected",zlim=c(0,2))


     