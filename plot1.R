########################################################################
#  1. Have total emissions from PM2.5 decreased in the United States   #
#     from 1999 to 2008? Using the base plotting system, make a plot   #
#     showing the total PM2.5 emission from all sources for each of    #
#     the years 1999, 2002, 2005, and 2008.                            #
########################################################################

library(dplyr)

# read the data if necessary
if (!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")

# aggregate the data
  data <- NEI %>% group_by(year) %>%  summarise(sum(Emissions)/10^6) 

# set columns names for convenience
  colnames(data) <- c("year", "emissions")

# a function that does the plot
  plot1 <- function(){
  
    pp<-par(mfrow=c(1,1), mar=c(5,5,4,2))
    
    plot(data,
         type = "o",
         lwd  = 3,
         col  = "steelblue",
         main = expression("Total PM"[2.5]*" emissions in USA"),
         ylab = expression("Emissions, tons * 10"^{6}),
         xlab = "Year"
    )
    
    grid(nx=25,ny=30)
    
    axis(1, at=c(1999:2008))
    
    par(pp)
    
  }

# draw the plot on the screen
  plot1()

# and to a PNG file
  png(filename="plot1.png")
  plot1()
  dev.off()
