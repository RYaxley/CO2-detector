require(reshape2)
require(ggplot2)
require(plyr)
require(zoo)

# Check machine and set path appropriately
if(Sys.info()["nodename"] == 'Turing.local'){
  setwd('/Users/rhy/Dropbox/Work/./Analysis/Rich Code')
} else if (Sys.info()["nodename"] == 'Richs-MacBook-Air.local'){
  setwd('/Users/Rich_Yaxley/Dropbox (Personal)/Work/./Analysis/Rich Code')
}

# Plotting Parameters
plotsize = 1200 # pixels
golden = 1.618 # ratio
res = 150


df <- read.csv(file='co2-moving-window-average-0-3000s-median.csv', header=T, sep = ',')

# Subset data.
dfsub <- subset(df, select = -c(6:9) ) # Drop original green, blue, red, and clear columns
dfsub <- dfsub[complete.cases(dfsub), ] # Drop incomplete rows with NAs

# Melt data to collapse channels into a single column
dfsub <- melt(dfsub,
              id.vars=c('run','conc','resp','time','runtime'),
              measure.vars = c('green.avg','blue.avg','red.avg','clear.avg'),
              variable.name = 'channel', value.name='intensity')

#---------------------------------------------------------------------------#
# Jim's peak-to-valley code from analysis.R
#---------------------------------------------------------------------------#
# Function for checking whether the central element in a vector is an extremum,
# and determining which kind.
# Returns +1 for a maximum, -1 for a minimum, and zero for any other point.
findlocalextremum <- function(v){
     nv <- length(v)
     if (nv%%2==0 ){ mid <- (nv+2)/2 } else{  mid <- (nv+1)/2 }
     ret <- 0
     if(!is.unsorted( v[1:mid]) && !is.unsorted(-v[mid:nv])){ ret <-  1 }
     if(!is.unsorted(-v[1:mid]) && !is.unsorted( v[mid:nv])){ ret <- -1 }
     return(ret)
}

#Function to find all extrema in a long-ish vector
findextrema <- function(dat, width=5){
     ld <- length(dat)
     buf <- (width-1)/2
     xx <- sapply((1+buf):(ld-buf), function(q) findlocalextremum( dat[(q-buf):(q+buf)] ) )
     return(c(rep(NA,buf), xx, rep(NA,buf)))
}

# Size of window around each point with which to determine whether the
# central point is an extremum
width <- 11
# Create new column for extreme values
dfsub$extrema <- NA

# Create new dataframe to fill
df.features <- data.frame(run = character(0),
                          channel = character(0),
                          conc = character(0),
                          resp = character(0),
                          p2t.height.m = numeric(0),
                          p2t.height.sd = numeric(0),
                          p2t.time.m = numeric(0),
                          p2t.time.sd = numeric(0),
                          t2p.time.m = numeric(0),
                          t2p.time.sd = numeric(0) )

t.start <- 0 # start time
t.stop  <- 3000 # stop time

# dfsub <- subset(dfsub, run!=00 & run!=24) # 24 is causing problems & 00 is a blind test with varied concentrations
dfsub <- subset(dfsub, run!=0) # Maybe we can keep run 24 now??? 0 is a blind test with varied concentrations

for(i in unique(dfsub$run) ){
     for(j in c('green.avg','blue.avg','red.avg','clear.avg')){
          print(paste(i,j))

          d <- subset(dfsub, run==i & channel==j & runtime >= t.start & runtime < t.stop)

          d$extrema <- findextrema(d$intensity, width = width )

          # removes non-extrema observations
          d.e <- d[d$extrema!=0 & !is.na(d$extrema), ]

          peaks   <- d.e$extrema ==  1
          troughs <- d.e$extrema == -1

          # Peak to Trough
          PT <- which( peaks & c(troughs[-1],FALSE) )
          PT <- sort( c(PT,PT+1) )
          d.p2t <- d.e[PT, ]

          # Trough to Peak
          TP <- which( troughs & c(peaks[-1],FALSE) )
          TP <- sort( c(TP,TP+1) )
          d.t2p <- d.e[TP, ]

          np2t <- nrow(d.p2t)
          nt2p <- nrow(d.t2p)

          #height differences
          peak.to.trough.heights <- d.p2t$intensity[seq(2,np2t,by=2)] - d.p2t$intensity[seq(2,np2t,by=2)-1]
          trough.to.peak.heights <- d.t2p$intensity[seq(2,nt2p,by=2)] - d.t2p$intensity[seq(2,nt2p,by=2)-1]

          #time differences
          peak.to.trough.timeunits <- d.p2t$time[seq(2,np2t,by=2)] - d.p2t$time[seq(2,np2t,by=2)-1]
          trough.to.peak.timeunits <- d.t2p$time[seq(2,nt2p,by=2)] - d.t2p$time[seq(2,nt2p,by=2)-1]

          # Append feature data
          features <- c(i, j, d$conc[1], d$resp[1],
                      mean(peak.to.trough.heights), sd(peak.to.trough.heights),
                      mean(peak.to.trough.timeunits), sd(peak.to.trough.timeunits),
                      mean(trough.to.peak.timeunits), sd(trough.to.peak.timeunits))
          df.features <- rbind(df.features, t(features))
     }
}

# Format, name, and write feature table
names <- c('run','channel','conc', 'resp', 'p2t.height.m', 'p2t.height.sd',
           'p2t.time.m', 'p2t.time.sd', 't2p.time.m', 't2p.time.sd')
names(df.features) <- names
write.csv(df.features,
          file=paste('co2.features-',t.start,'-',t.stop,'-median.csv', sep=''),
          row.names=F)

#---------------------------------------------------------------------------#

png("ExtremaIntervals.png", height=plotsize/golden, width=plotsize, res = res)
par(mfrow=c(2,2))
plot(peak.to.trough.heights,  type="l")
plot(trough.to.peak.heights,  type="l")
plot(peak.to.trough.timeunits,type="l")
plot(trough.to.peak.timeunits,type="l")
dev.off()


# Aggregate data
aggregate(intensity~channel+run+conc+resp, data=dfsub, mean)


require(gridExtra)
tab <- read.csv(file = 'co2.features-0-3000.csv')
grid.table(head(tab,10), h.odd.alpha=.4)
