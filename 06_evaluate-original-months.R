library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(Metrics)
library(reshape2)
library(rlist)

## ------------------------------------------------------------ ## 
## DEFINE DOUBLE MASS PLOT FUNCTION

# function to make double mass curve plot
# Station and IMERG should be numeric vectors, with IMERG already scaled by *24
double_mass_plot <- function(Station, IMERG){
  
  no_na <- !is.na(Station) & !is.na(IMERG)
  Station[!no_na] <- NA
  IMERG[!no_na] <- NA
  # get max bounds
  cumsum_Station <- cumsum(ifelse(is.na(Station), 0, Station)) + Station*0
  cumsum_IMERG <- cumsum(ifelse(is.na(IMERG), 0, IMERG)) + IMERG*0
  upper_lim <- max(max(cumsum_Station,na.rm=T),max(cumsum_IMERG,na.rm=T))
  # create data frame for ggplot
  temp_df <- data.frame(cumsum_Station,cumsum_IMERG,colnames(imerg_months))
  colnames(temp_df) <- c("Station","IMERG","date")
  # plot...
  ggplot(temp_df, aes(x=Station,y=IMERG,col=date)) + # set x and y axes
    geom_point() + # point geometry, color matches presentation theme
    coord_fixed(xlim=c(0,upper_lim),ylim=c(0,upper_lim)) + #set limits to be the same
    theme(aspect.ratio=1) + # fix aspect ratio
    geom_abline() + # plot x=y line 
    scale_colour_discrete(rainbow(247))
}

## ------------------------------------------------------------ ## 
## PLOT DOUBLE MASS CURVES

# get elevation order of clusters
elev_order <- rownames(ghcn_months)[order(ghcn_meta_clust$elev)]

# plot dmc for monthly
p_all <- lapply(elev_order, FUN = function(x){
  ndx1 <- which(rownames(ghcn_months) == x)
  ndx2 <- which(rownames(imerg_months) == x)
  double_mass_plot(as.numeric(ghcn_months[ndx1, ]), as.numeric(imerg_months[ndx2, ])) +
    labs(subtitle = x) + # comment out to remove cluster number
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    xlab(NULL) + ylab(NULL) + theme(legend.position = "none") 
})

# create fake data to get colour bar
data_tmp <- data.frame(200006:202012, rep(1,times=length(200006:202012)))
colnames(data_tmp) <- c("dates", "data")

# make fake plot to get colour bar
p_tmp <- ggplot(data_tmp, aes(x = dates, y = data , col = dates)) +
  geom_point() + 
  scale_colour_gradientn(colours = rainbow(7), limits = c(200006,202012), breaks = c(200006,202012))

# get one legend object (color bar from fake data)
leg <- g_legend(p_tmp)

# make layout 
lay <- rbind(c(rep(1:8, each = 2), 43), c(rep(9:16, each = 2), 41),
             c(rep(17:24, each = 2), 41), c(rep(25:32, each=2), 41),
             c(rep(33:40, each = 2), 42))

tgrob_h <- textGrob("higher\n elevation", hjust = 'center')
tgrob_l <- textGrob("lower\n elevation", hjust = 'center')

# add legend to list
p_all <- list.append(p_all, leg, tgrob_h, tgrob_l)

# plot all the plots
grid.arrange(grobs = p_all, layout_matrix = lay, 
             top = textGrob("Double Mass Curves for Monthly Accumulated Precipitation",
                           y = 0.5,
                           gp = gpar(fontsize = 18)),
             bottom = "Station Cumulative Precipitation (m)",
             left = "IMERG Cumulative Precipitation (m)")

# function to get legend object from plot
g_legend<-function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# code to check available months of a cluster
colnames(ghcn_months)[which(!is.na(ghcn_months[27, ]))]

## ------------------------------------------------------------ ## 
## PLOT SCATTER PLOTS

# plot scatter plot for monthly
p_scatter <- lapply(c(1:40), FUN = function(x) {
  Station <- as.numeric(ghcn_months[x, ])
  IMERG <- as.numeric(imerg_months[x, ])
  
  upper_lim <- max(quantile(Station, 0.9, na.rm = TRUE) , quantile(IMERG, 0.9, na.rm = TRUE))
  temp_df <- data.frame(Station, IMERG)
  colnames(temp_df) <- c("Station", "IMERG")
  ggplot(temp_df, aes(x = Station, y = IMERG)) + 
    geom_point(color = c40[x]) + 
    coord_fixed(xlim = c(0, upper_lim), ylim = c(0, upper_lim)) +
    theme(aspect.ratio = 1) +
    geom_abline() +
    labs(subtitle = x) + theme(plot.subtitle = element_text(hjust = 0.5)) +
    xlab(NULL) + ylab(NULL) 
  #xlab("Station (mm/day)") + ylab("IMERG (mm/day)") 
})
grid.arrange(grobs = p_scatter, ncol = 8, 
             top = textGrob("Scatter Plots for Monthly Accumulated Precipitation",
                           y = 0.5, 
                           gp = gpar(fontsize = 18)),
             bottom = "Station Measurement (mm/month)",
             left = "IMERG Estimate (mm/month)")

## ------------------------------------------------------------ ## 
## CALCULATE METRICS

# define function for calculating relative bias
relative_bias <- function(s, r) { # s = estimate, r = parameter
  e <- 0.5 #mm/day
  if (length(s) != length(r)){
    return("vectors are not the same length")
  }
  else {
    mean(sapply(1:length(s), function(x) {
      (s[x]-r[x])*2/(e+s[x]+r[x])
    }), na.rm = TRUE)
  }
}

# calculate bias for all of the clusters
bias_total <- imerg_months - ghcn_months 
bias_all_clust <- apply(bias_total, 1, \(x) mean(x, na.rm=T))

# calculate relative bias for all clusters
rbias_all_clust <- sapply(c(1:40), function(x) {
  ndx1 <- which(rownames(ghcn_months) == x)
  ndx2 <- which(rownames(imerg_months) == x)
  no_na <- which(!is.na(ghcn_months[ndx1, ]))
  mean(relative_bias(as.numeric(imerg_months[ndx2, no_na]), as.numeric(ghcn_months[ndx1, no_na])), na.rm = TRUE)
})

# calculate Pearson's r for all clusters
cor_all_clust <- sapply(c(1:40), function(x){ 
  ndx1 <- which(rownames(ghcn_months) == x)
  ndx2 <- which(rownames(imerg_months) == x)
  cor(as.numeric(ghcn_months[ndx1, ]),
      as.numeric(imerg_months[ndx2, ]), use = "pairwise.complete.obs")
})

# calculate rmse for all clusters
rmse_all_clust <- sapply(c(1:40), function(x){ 
  ndx1 <- which(rownames(ghcn_months) == x)
  ndx2 <- which(rownames(imerg_months) == x)
  no_na <- which(!is.na(ghcn_months[ndx1, ])) # change this so that both have to be non-na
  rmse(as.numeric(ghcn_months[ndx1, no_na]),
       as.numeric(imerg_months[ndx2, no_na]))
})

# and mae for all clusters
mae_all_clust <- sapply(c(1:40), function(x) { 
  ndx1 <- which(rownames(ghcn_months) == x)
  ndx2 <- which(rownames(imerg_months) == x)
  no_na <- which(!is.na(ghcn_months[ndx1, ])) # change this so that both have to be non-na
  mae(as.numeric(ghcn_months[ndx1, no_na]),
      as.numeric(imerg_months[ndx2, no_na]))
})

# add to ghcn_meta_clust
ghcn_meta_clust$cor <- cor_all_clust
ghcn_meta_clust$bias <- bias_all_clust
ghcn_meta_clust$rmse <- rmse_all_clust
ghcn_meta_clust$mae <- mae_all_clust
ghcn_meta_clust$rbias <- rbias_all_clust

## ------------------------------------------------------------ ## 
## CALCULATE METRICS (DAILY)

# calculate bias for all of the clusters
bias_daily_total <- imerg_data_clust - ghcn_data_clust 
bias_daily_all_clust <- apply(bias_daily_total, 1, \(x) mean(x, na.rm = TRUE))

# calculate relative bias for all clusters
rbias_daily_all_clust <- sapply(c(1:40),function(x){
  ndx1 <- which(rownames(ghcn_data_clust) == x)
  ndx2 <- which(rownames(imerg_data_clust) == x)
  no_na <- which(!is.na(ghcn_data_clust[ndx1, ]))
  mean(relative_bias(as.numeric(imerg_data_clust[ndx2, no_na]),
                     as.numeric(ghcn_data_clust[ndx1, no_na])), na.rm = TRUE)
})

# calculate Pearson's r for all clusters
cor_daily_all_clust <- sapply(c(1:40), function(x) { 
  ndx1 <- which(rownames(ghcn_data_clust) == x)
  ndx2 <- which(rownames(imerg_data_clust) == x)
  cor(as.numeric(ghcn_data_clust[ndx1, ]),
      as.numeric(imerg_data_clust[ndx2, ]), use = "pairwise.complete.obs")
})

# calculate rmse for all clusters
rmse_daily_all_clust <- sapply(c(1:40), function(x) { 
  ndx1 <- which(rownames(ghcn_data_clust) == x)
  ndx2 <- which(rownames(imerg_data_clust) == x)
  no_na <- which(!is.na(ghcn_data_clust[ndx1, ])) # change this so that both have to be non-na
  rmse(as.numeric(ghcn_data_clust[ndx1, no_na]),
       as.numeric(imerg_data_clust[ndx2, no_na]))
})

# and mae for all clusters
mae_daily_all_clust <- sapply(c(1:40),function(x){ 
  ndx1 <- which(rownames(ghcn_data_clust) == x)
  ndx2 <- which(rownames(imerg_data_clust) == x)
  no_na <- which(!is.na(ghcn_data_clust[ndx1,])) # change this so that both have to be non-na
  mae(as.numeric(ghcn_data_clust[ndx1,no_na]),
      as.numeric(imerg_data_clust[ndx2,no_na]))
})

## ------------------------------------------------------------ ## 
## CALCULATE MONTHLY (AVERAGED OVER TIME) BIAS AND RELATIVE BIAS

# calculate monthly bias
bias_total <- imerg_months - ghcn_months 
# get column months
col_months <- as.numeric(substr(colnames(bias_total), 5, 6))

# aggregate bias by month
monthly_bias <- sapply(c(1:12), function(x) {
  apply(bias_total, 1, function(y){
    mean(y[which(col_months == x)], na.rm = TRUE)
  })
})

# add monhtly bias to hclust_summary_m
ghcn_meta_clust$jan <- monthly_bias[,1]
ghcn_meta_clust$feb <- monthly_bias[,2]
ghcn_meta_clust$mar <- monthly_bias[,3]
ghcn_meta_clust$apr <- monthly_bias[,4]
ghcn_meta_clust$may <- monthly_bias[,5]
ghcn_meta_clust$jun <- monthly_bias[,6]
ghcn_meta_clust$jul <- monthly_bias[,7]
ghcn_meta_clust$aug <- monthly_bias[,8]
ghcn_meta_clust$sep <- monthly_bias[,9]
ghcn_meta_clust$oct <- monthly_bias[,10]
ghcn_meta_clust$nov <- monthly_bias[,11]
ghcn_meta_clust$dec <- monthly_bias[,12]

# calculate relative bias
rbias_total <- sapply(1:ncol(ghcn_months), function(x) {
  relative_bias(as.vector(imerg_months[, x]), as.vector(ghcn_months[, x]))
})

# aggregate bias by month
monthly_rbias <- sapply(c(1:12), function(x){
  apply(rbias_total, 1, function(y){
    mean(y[which(col_months == x)], na.rm = TRUE)
  })
})

# add monhtly bias to hclust_summary_m
ghcn_meta_clust$rjan <- monthly_rbias[,1]
ghcn_meta_clust$rfeb <- monthly_rbias[,2]
ghcn_meta_clust$rmar <- monthly_rbias[,3]
ghcn_meta_clust$rapr <- monthly_rbias[,4]
ghcn_meta_clust$rmay <- monthly_rbias[,5]
ghcn_meta_clust$rjun <- monthly_rbias[,6]
ghcn_meta_clust$rjul <- monthly_rbias[,7]
ghcn_meta_clust$raug <- monthly_rbias[,8]
ghcn_meta_clust$rsep <- monthly_rbias[,9]
ghcn_meta_clust$roct <- monthly_rbias[,10]
ghcn_meta_clust$rnov <- monthly_rbias[,11]
ghcn_meta_clust$rdec <- monthly_rbias[,12]

## ------------------------------------------------------------ ## 
## CALCULATE CATEGORICAL METRICS

# make binary data frames
ghcn_data_clust_bin <- as.data.frame(apply(ghcn_data_clust >= 0.5, 2, as.numeric))
colnames(ghcn_data_clust_bin) <- colnames(ghcn_data_clust)
ghcn_data_clust_bin[is.na(ghcn_data_clust)] <- NA

imerg_data_clust_bin <- as.data.frame(apply(imerg_data_clust >= 0.5, 2, as.numeric))
colnames(imerg_data_clust_bin) <- colnames(imerg_data_clust)
imerg_data_clust_bin[is.na(imerg_data_clust)] <- NA

# function to calculate POD
# x and y are equal-length binary vectors; 
calc_pod <- function(ghcn, imerg) { 
  TP <- sum(imerg == 1 & ghcn == 1, na.rm = TRUE)
  FN <- sum(imerg == 0 & ghcn == 1, na.rm = TRUE)
  return(TP/(TP+FN))
}

# function to calculate FAR
# x and y are equal-length binary vectors; 
calc_far <- function(ghcn, imerg) { 
  TP <- sum(imerg == 1 & ghcn == 1, na.rm = TRUE)
  FP <- sum(imerg == 1 & ghcn == 0, na.rm = TRUE)
  return(FP/(FP+TP))
}

# function to calculate FBI
calc_fbi <- function(ghcn, imerg) { 
  TP <- sum(imerg == 1 & ghcn == 1, na.rm = TRUE)
  FP <- sum(imerg == 1 & ghcn == 0, na.rm = TRUE)
  FN <- sum(imerg == 0 & ghcn == 1, na.rm = TRUE)
  return((TP+FP)/(TP+FN))
}

# function to calculate HSS
calc_hss <- function(ghcn, imerg) {
  TP <- sum(imerg == 1 & ghcn == 1, na.rm = TRUE)
  FP <- sum(imerg == 1 & ghcn == 0, na.rm = TRUE)
  FN <- sum(imerg == 0 & ghcn == 1, na.rm = TRUE)
  TN <- sum(imerg == 0 & ghcn == 0, na.rm = TRUET)
  num <- 2*((TP*TN)-(FP*FN))
  denom <- ((TP+FN)*(FN+TN)) + ((TP+FP)*(FP+TN))
  return(num/denom)
}

# function to calculate CSI
calc_csi <- function(ghcn, imerg) {
  TP <- sum(imerg == 1 & ghcn == 1, na.rm = TRUE)
  FP <- sum(imerg == 1 & ghcn == 0, na.rm = TRUE)
  FN <- sum(imerg == 0 & ghcn == 1, na.rm = TRUE)
  return(TP/(TP+FN+FP))
}

# calculate probability of detection for all clusters
pod <- sapply(c(1:40), function(x) { 
  ndx1 <- which(rownames(ghcn_data_clust_bin) == x)
  ndx2 <- which(rownames(imerg_data_clust_bin) == x)
  calc_pod(ghcn_data_clust_bin[ndx1, ], imerg_data_clust_bin[ndx2, ])
})

# calculate false alarm ratio for all clusters
far <- sapply(c(1:40), function(x) { 
  ndx1 <- which(rownames(ghcn_data_clust_bin) == x)
  ndx2 <- which(rownames(imerg_data_clust_bin) == x)
  calc_far(ghcn_data_clust_bin[ndx1, ], imerg_data_clust_bin[ndx2, ])
})

# calculate frequency bias for all clusters
fbi <- sapply(c(1:40), function(x) { 
  ndx1 <- which(rownames(ghcn_data_clust_bin) == x)
  ndx2 <- which(rownames(imerg_data_clust_bin) == x)
  calc_fbi(ghcn_data_clust_bin[ndx1, ], imerg_data_clust_bin[ndx2,])
})

# calculate heidke skill score for all clusters
hss <- sapply(c(1:40), function(x) { 
  ndx1 <- which(rownames(ghcn_data_clust_bin) == x)
  ndx2 <- which(rownames(imerg_data_clust_bin) == x)
  calc_hss(ghcn_data_clust_bin[ndx1, ], imerg_data_clust_bin[ndx2, ])
})

# calculate critical success index for all clusters
csi <- sapply(c(1:40), function(x) { 
  ndx1 <- which(rownames(ghcn_data_clust_bin) == x)
  ndx2 <- which(rownames(imerg_data_clust_bin) == x)
  calc_csi(ghcn_data_clust_bin[ndx1, ], imerg_data_clust_bin[ndx2, ])
})

## ------------------------------------------------------------ ## 
## PLOT BIAS AND OTHER METRICS

# plot bias v elevation 
bias_elev <- ggplot(ghcn_meta_clust, aes(x = merit, y = bias)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_point(alpha = 0.9, size = 3) +
  xlab("Elevation") +
  ylab(NULL) 

ylab <- textGrob("Bias (satellite -\nweather station", hjust = 'right', vjust = 'top')

p_all <- list(ylab, bias_elev)
grid.arrange(grobs = p_all, ncol = 2, widths = c(1, 4))

# plot bias v landcover
bias_lc <- ggplot(ghcn_meta_clust, aes(x = landcover, y = bias)) + 
  geom_abline(slope = 0, intercept = 0) +
  geom_point(alpha = 0.9, size = 3) + 
  xlab("Land Cover Class") +
  ylab("Bias") 

# plot bias v slope
bias_slope <- ggplot(ghcn_meta_clust, aes(x = slope, y = bias)) + 
  geom_abline(slope = 0, intercept = 0) +
  geom_point(alpha = 0.9, size = 3) + 
  xlab("Slope") +
  ylab("Bias") 

# plot bias v aspect monhtly
bias_aspect <- ggplot(ghcn_meta_clust, aes(x = aspect, y = bias)) + 
  geom_abline(slope = 0, intercept = 0) +
  geom_point(alpha = 0.9, size = 3) + 
  xlab("Aspect") +
  ylab("Bias")

all_plots <- list(bias_elev, bias_lc, bias_slope, bias_aspect)
grid.arrange(grobs = all_plots, ncol = 2, 
             top = textGrob("Bias vs. environmental gradients", y = 0.5, gp = gpar(fontsize = 18)),)

# now for relative bias

# plot bias v elevation 
rbias_elev <- ggplot(ghcn_meta_clust, aes(x = merit, y = rbias)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_point(alpha = 0.9, size = 3) +
  xlab("Elevation") +
  ylab("Relative Bias")

# plot bias v landcover
rbias_lc <- ggplot(ghcn_meta_clust, aes(x = landcover, y = rbias)) + 
  geom_abline(slope = 0, intercept = 0) +
  geom_point(alpha = 0.9, size = 3) + 
  xlab("Land Cover Class") +
  ylab("Relative Bias") 

# plot bias v slope
rbias_slope <- ggplot(ghcn_meta_clust, aes(x = slope, y = rbias)) + 
  geom_abline(slope = 0, intercept = 0) +
  geom_point(alpha = 0.9, size = 3) + 
  xlab("Slope") +
  ylab("Relative Bias") 

# plot bias v aspect monhtly
rbias_aspect <- ggplot(ghcn_meta_clust, aes(x = aspect, y = rbias)) + 
  geom_abline(slope = 0, intercept = 0) +
  geom_point(alpha = 0.9, size = 3) + 
  xlab("Aspect") +
  ylab("Relative Bias")

all_plots <- list(rbias_elev, rbias_lc, rbias_slope, rbias_aspect)
grid.arrange(grobs = all_plots, ncol = 2, 
             top = textGrob("Bias vs. environmental gradients", y = 0.5, gp = gpar(fontsize = 18)),)

## ------------------------------------------------------------ ## 
## PLOT BIAS AND OTHER METRICS BY MONTH

small_df_m <- ghcn_meta_clust[, c("Group.1", "lat", "lon", "elev", "merit", "landcover", "slope", "aspect",
                                     "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")]
small_df_m <- melt(data = small_df_m, id.vars = c("Group.1", "lat", "lon", "elev", "merit", "landcover", "slope", "aspect"),
                   variable.name = "month", value.name = "bias")

# elevation
m_e <- ggplot(data = small_df_m, aes(x = merit, y = bias, col = month)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_smooth(method = "loess", se = TRUE, level = 0.5) +
  xlab("Elevation") +
  ylab("Bias") +
  geom_segment(data = lm_tidy_cont, aes(col = month, x = min_elev, xend = max_elev, y = `(Intercept)` + elev*min_elev, yend = `(Intercept)` + elev*max_elev))

#scale_colour_manual(values = c25[1:12])

# slope
m_s <- ggplot(data = small_df_m, aes(x = slope, y = bias, col = month)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_smooth(method = "loess", se = TRUE, level = 0.5) +
  xlab("Slope") +
  ylab("Bias")

# aspect
m_a <- ggplot(data = small_df_m, aes(x = aspect, y = bias, col = month)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_smooth(method = "loess", se = TRUE, level = 0.5) +
  xlab("Aspect") +
  ylab("Bias")

# landcover
m_l <- ggplot(data = small_df_m, aes(x = landcover, y = bias, col = month)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_point() +
  xlab("Landcover") +
  ylab("Bias")

all_plots <- list(m_e, m_l, m_s, m_a)
grid.arrange(grobs = all_plots, ncol = 2, 
             top = textGrob("Bias vs. various environmental gradients", y = 0.5, gp = gpar(fontsize = 18)),)




