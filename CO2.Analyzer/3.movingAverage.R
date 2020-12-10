require(reshape2)
require(ggplot2)
require(plyr)
require(zoo)
require(caTools)

# Plotting Parameters
plotsize = 1200 # pixels
golden = 1.618 # ratio
res = 150

# Check machine and set path appropriately
if(Sys.info()["nodename"] == 'Turing.local'){
  setwd('/Users/rhy/Dropbox/Work/./Analysis/Rich Code')
} else if (Sys.info()["nodename"] == 'Richs-MacBook-Air.local'){
  setwd('/Users/Rich_Yaxley/Dropbox (Personal)/Work/./Analysis/Rich Code')
}

df <- read.csv(file='co2.csv', header=T)

'''2015-03-26
We need to smooth the data to numerically differentiate. Average a small number of neighboring points and walk up. 
Try taking several 1-min chunks rather than 0-3000 epoch
Calculate the mean trough to peak values. Drop the peak to trough values.
'''

# Smooth data test
x <- df$red[200000:200530]
plot(x, col='black', cex=1.4)
lines(runmean(x,5), col='red', lwd = 5)
lines(runquantile(x, 9, probs=0.5), col='blue', lwd=3)




# Smooth data with running mean
df$green.s <- NA
df$blue.s <- NA
df$red.s <- NA
df$clear.s <- NA

for(run in unique(df$run)){
  df[which(df$run==run), ]$green.s <- runmean(df[which(df$run==run), ]$green, 5)
  df[which(df$run==run), ]$red.s <- runmean(df[which(df$run==run), ]$red, 5)
  df[which(df$run==run), ]$blue.s <- runmean(df[which(df$run==run), ]$blue, 5)
  df[which(df$run==run), ]$clear.s <- runmean(df[which(df$run==run), ]$clear, 5)
}



# Generate a moving window average for every run
timepoints <- 1200  # 240 * 5 = 1200 timepoints (1200/60 samples/sec) = 20 s
df.m <- data.frame() #NULL
system.time(
     for(i in unique(df$run)){
          print(i)
          dfsub <- subset(df, run==i)
          dfsub <- subset(dfsub, runtime >= 0 & runtime < 3000) # subset of samples
          ts <- zoo(dfsub)
          
          tsmean <- rollapply(ts[ ,c('green.s','blue.s','red.s','clear.s')], width=timepoints,
                              by=1, align = 'center', by.column = TRUE, fill = NA,
                              FUN=function(x){ as.numeric(x)[timepoints] - median(as.numeric(x),na.rm=TRUE)})

          z <- as.data.frame(tsmean)
          dfsub$green.avg <- z$green
          dfsub$blue.avg <- z$blue
          dfsub$red.avg <- z$red
          dfsub$clear.avg <- z$clear

          df.m <- rbind(df.m, dfsub)

          # Save each moving average to a var
          assign(paste('m.r', i, sep=''), dfsub)
     }
)
write.csv(df.m, file="co2-moving-window-average-0-3000s-median.csv", row.names=F)




