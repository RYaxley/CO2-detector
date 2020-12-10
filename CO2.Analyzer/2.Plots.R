require(reshape2)
require(ggplot2)
require(plyr)
# require(zoo)
require(grid)

setwd('/Users/rhy/Dropbox/Work/./Analysis/Rich Code/')

# Plotting Parameters
plotsize = 1200 # pixels
golden = 1.618 # ratio
res = 150

data <- read.csv(file='co2.csv', header=T)

# Thin data further for quicker plotting
df <- data[seq(1,nrow(data),by=10), ]


# Convert data to long format for plotting
df.long <- melt(df, id.vars = c('run','conc','resp','time','runtime', 'p2v', 'est.Conc', 'est.Resp'),
                variable.name = 'channel', value.name = 'intensity')

# Boxplots of channel intensity
fname <- "Plots/Box-Intensity-x-Channel.png"
ggplot(data=df.long, aes(x=channel, y=intensity, color=channel)) + geom_boxplot() +
     geom_jitter(position=position_jitter(w=.38), size=1, alpha=.01) +
     theme_light()
dev.copy(png, fname, height=plotsize/golden, width=plotsize, res=res)
dev.off()

fname <- "Plots/Box-Intensity-x-Respiration.png"
ggplot(data=df.long, aes(x=channel, y=intensity, color=channel)) + geom_boxplot() +
     geom_jitter(position=position_jitter(w=.38), size=.1, alpha=.01)  +
     facet_grid(.~resp) + theme_light()
dev.copy(png, fname, height=plotsize/golden, width=plotsize, res=res)
dev.off()

fname <- "Plots/Box-Intensity-x-Concentration.png"
ggplot(data=df.long, aes(x=channel, y=intensity, color=channel)) + geom_boxplot() +
     geom_jitter(position=position_jitter(w=.38), size=.5, alpha=.01)  +
     facet_grid(.~conc) + theme_light()
dev.copy(png, fname, height=plotsize/golden, width=plotsize, res=res)
dev.off()


fname <- "Plots/Line-Red-x-Time.png"
ggplot(data=df.long, aes(x=runtime, y=intensity, color=run)) +
     geom_line(subset = .(channel %in% c("red"))) + facet_grid(conc~.) +
     ggtitle("red channel")
dev.copy(png, fname, height=plotsize/golden, width=plotsize, res=res)
dev.off()

# Zoom in
epoch <- subset(df.long, runtime > 0 & runtime < 400)
fname <- "Plots/Line-Red-x-Time-zoom.png"
ggplot(data=epoch, aes(x=runtime, y=intensity, color=run)) +
     geom_line(subset = .(channel %in% c("red"))) + facet_grid(conc~.) +
     ggtitle("red channel")
dev.copy(png, fname, height=plotsize/golden, width=plotsize, res=res)
dev.off()

# Zoom in more
epoch <- subset(df.long, runtime > 0 & runtime < 100)
fname <- "Plots/Line-Red-x-Time-zoomzoom.png"
ggplot(data=epoch, aes(x=runtime, y=intensity, color=run)) +
     geom_line(subset = .(channel %in% c("red"))) + facet_grid(conc~.) +
     ggtitle("red channel")
dev.copy(png, fname, height=plotsize/golden, width=plotsize, res=res)
dev.off()



fname <- "Plots/Line-Green-x-Time.png"
ggplot(data=df.long,
       aes(x=runtime, y=intensity, color=run)) +
       geom_line(subset = .(channel %in% c("green"))) +
       facet_grid(conc~.) +
       ggtitle("green channel")
dev.copy(png, fname, height=plotsize/golden, width=plotsize, res=res)
dev.off()

fname <- "Plots/Line-Blue-x-Time.png"
ggplot(data=df.long,
       aes(x=runtime, y=intensity, color=run)) +
       geom_line(subset = .(channel %in% c("blue"))) +
       facet_grid(conc~.) +
       ggtitle("blue channel")
dev.copy(png, fname, height=plotsize/golden, width=plotsize, res=res)
dev.off()

fname <- "Plots/Line-Clear-x-Time.png"
ggplot(data=df.long, aes(x=time, y=intensity, color=run)) +
       geom_line(subset = .(channel %in% c("clear"))) +
       facet_grid(conc~.) +
       ggtitle("clear channel")
dev.copy(png, fname, height=plotsize/golden, width=plotsize, res=res)
dev.off()


z <- subset(df.long, run==10 & runtime > 0 & runtime < 320 & channel=='green')
plot(z$intensity)
