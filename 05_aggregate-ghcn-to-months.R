library(lubridate)

## ------------------------------------------------------------ ## 
## AGGREGATE ghcn_data_clust TO MONTHS (ghcn_months)

# define function that returns NA if there isn't enough data in a month
my_monthaverage <- function(x) {
  total_days <- days_in_month(as.Date(colnames(x)[1],format="%Y-%m-%d"))
  min_days <- ceiling(total_days/4)
  apply(x,1,function(dat){
    # if the row does not have more than 25% coverage for a month, return NA
    if (sum(!is.na(dat)) < min_days) { 
      NA
    }
    else {
      mean(dat,na.rm=T)#*total_days # 8/13: now averaging, not summing months
    }
  })
}

d_ <- paste(substring(d,1,4),substring(d,5,6),sep="-")

# aggregate months from station data (clustered)
ghcn_months <- sapply(d_,FUN = function(x){
  # get columns where the month string is within the column (date) name
  my_month <- ghcn_data_clust[,which(grepl(x,colnames(ghcn_data_clust),fixed=TRUE))]
  my_monthaverage(my_month)
})
ghcn_months <- data.frame(ghcn_months,row.names=rownames(ghcn_data_clust))
colnames(ghcn_months) <- d

