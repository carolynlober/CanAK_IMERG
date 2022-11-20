library(tidyr)
library(purrr)
library(reshape2)
library(ggplot2)
library(dplyr)

## ------------------------------------------------------------ ## 
## CONTINUOUS LINEAR REGRESSION ON BIAS

months = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
lm_cont <- sapply(months,function(x){
  lm(as.formula(paste(x,"~","elev",sep="")),data=ghcn_meta_clust)
})
lm_tidy_cont <- map(lm_cont, broom::tidy) %>% 
  bind_rows(.id = "month") %>% 
  select(month, term, estimate) %>% 
  pivot_wider(names_from = "term", values_from = "estimate")

small_df_m <- ghcn_meta_clust[,c("Group.1","lat","lon","elev","merit","landcover","slope","aspect",
                                     "jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")]
small_df_m <- melt(data=small_df_m, id.vars=c("Group.1","lat","lon","elev","merit","landcover","slope","aspect"),
                   variable.name = "month", value.name = "bias")
min_elev <- min(small_df_m$elev)
max_elev <- max(small_df_m$elev)

ggplot() + 
  geom_abline(slope=0,intercept=0) +
  geom_point(data=small_df_m, aes(x=elev,y=bias,col=month),size=1) + 
  geom_segment(data=lm_tidy_cont, aes(col=month, x = min_elev, xend = max_elev, y = `(Intercept)` + elev*min_elev, yend = `(Intercept)` + elev*max_elev)) +
  xlab("Elevation (m)") + ylab("Bias (mm/day)") + theme_bw() + labs(color="Month") +
  ylim(c(-10,0))

## ------------------------------------------------------------ ## 
## CONTINUOUS LINEAR REGRESSION ON RELATIVE BIAS

lm_cont <- sapply(months,function(x){
  lm(as.formula(paste("r",x,"~","elev",sep="")),data=ghcn_meta_clust)
})
lm_tidy_cont <- map(lm_cont, broom::tidy) %>% 
  bind_rows(.id = "month") %>% 
  select(month, term, estimate) %>% 
  pivot_wider(names_from = "term", values_from = "estimate")

small_df_r <- ghcn_meta_clust[,c("Group.1","lat","lon","elev","merit","landcover","slope","aspect",
                                 "rjan","rfeb","rmar","rapr","rmay","rjun","rjul","raug","rsep","roct","rnov","rdec")]
small_df_r <- melt(data=small_df_r, id.vars=c("Group.1","lat","lon","elev","merit","landcover","slope","aspect"),
                   variable.name = "month", value.name = "bias")
min_elev <- min(small_df_r$elev)
max_elev <- max(small_df_r$elev)

ggplot() + 
  geom_abline(slope=0,intercept=0) +
  geom_point(data=small_df_m, aes(x=elev,y=bias,col=month),size=1) + 
  geom_segment(data=lm_tidy_cont, aes(col=month, x = min_elev, xend = max_elev, y = `(Intercept)` + elev*min_elev, yend = `(Intercept)` + elev*max_elev)) +
  xlab("Elevation (mm)") + ylab("Bias (mm/day)") + theme_bw() + ylim(-5,0) #+ 
  #geom_smooth(data=small_df_r, aes(x=elev,y=bias,col=month),formula=y~x,method="lm",se=F)

## ------------------------------------------------------------ ## 
## APPLY TO IMERG MONTHLY DATA

crs(imerg_months_full[[1]]) <- my_crs
merit_reproj <- projectRaster(merit_300m,imerg_months_full[[1]])
merit_resam <- resample(merit_reproj,imerg_months_full[[1]],method='bilinear')

col_months <- as.numeric(substr(names(imerg_months_full),5,6))

imerg_months_full_corrected <- lapply(c(1:length(imerg_months_full)),function(x){
  orig_month <- imerg_months_full[[x]]
  my_month <- col_months[x]
  correction <- (merit_resam * as.numeric(lm_tidy_cont[my_month,"elev"])) + as.numeric(lm_tidy_cont[my_month,"(Intercept)"])
  corrected <- sum(orig_month,-correction)
  
  # set values below 0 to 0
  corrected[corrected < 0] <- 0
  
  return(corrected)
})

plot(imerg_months_full[[3]],main="Uncorrected IMERG for August 2000",xlab="º Longitude",
     ylab="º Latitude",col=my_pal)
plot(imerg_months_full_corrected[[3]],main="Corrected IMERG for August 2000",xlab="º Longitude",
     ylab="º Latitude",col=my_pal)





