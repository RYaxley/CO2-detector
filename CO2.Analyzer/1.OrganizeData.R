# CO2 Concentration dataset

# Check machine and set path appropriately
if(Sys.info()["nodename"] == 'Turing.local'){
  setwd('/Users/rhy/Dropbox/Work/./Analysis/Rich Code')
} else if (Sys.info()["nodename"] == 'Richs-MacBook-Air.local'){
  setwd('/Users/Rich_Yaxley/Dropbox (Personal)/Work/./Analysis/Rich Code')
}
  
data <- NULL
datapath <- file.path('../../Data')
files <- list.files(datapath, pattern='*.TXT')

start.time <- Sys.time()
for(file in files){
                
  df <- read.csv(file.path(datapath, file), sep=',', header=F, stringsAsFactors=F)  

  # Name columns
  names(df) <- c('green','blue','red','clear', 'p2v', 'est.Conc', 'est.Resp')
  
  # For run 0 the green channel reads in as 'character' type.  Converting to
  # numeric sends the few "Light Blocked" values to NAs.
  df$green <- as.numeric(df$green)
  df$blue <- as.numeric(df$blue)
  df$red <- as.numeric(df$red)
  df$clear <- as.numeric(df$clear)
  
  # Replacing the bizarrely large and small values with NAs
  df$green[df$green > 1000000 | df$green < 100] <- NA
  df$blue[df$blue   > 1000000 | df$blue  < 100] <- NA
  df$red[df$red     > 1000000 | df$red   < 100] <- NA
  df$clear[df$clear > 1000000 | df$clear < 100] <- NA
  
  # Extract experiment #, respiration rate, CO2 concentration from filename
  df$run  <- substr(file,2,3)
  df$resp <- substr(file,6,7)
  df$conc <- substr(file,9,10)
  df$time <- 1:nrow(df) # timepoint
  df$runtime <- round(df$time * 1/60, 3) # clock time
  
  # Reorder columns & factors
  df <- df[c('run','conc','resp','time','runtime','p2v','est.Conc', 'est.Resp',
             'green','blue','red','clear')]
  df$run  <- factor(df$run,c('00','01','02','03','04','05','06','07','08','09','10',
                             '11','12','13','14','15','16','17','18','19','20',
                             '21','22','23','24','25','26','27','28','29','30',
                             '31','32','33','34','35','36','37','38','39','40',
                             '90','91','92','93','94','95'))
  df$resp <- factor(df$resp,c('00','06','12','15','18','24','30'))
  df$conc <- factor(df$conc,c('00','10','15','25','35','45','55','65'))
  data <- rbind(data, df)
  
  print(file)
  print(dim(df))
  print(table(complete.cases(df)))
  
}
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)


# Write clean data for future use
write.csv(data, file='co2.csv', row.names=F)
# Downsample dataset for quicker testing
write.csv(data[seq(1,nrow(data),by=5), ], file='co2-downsampled.csv', row.names=F)

