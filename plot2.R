###########################################################################
#  2. Have total emissions from PM2.5 decreased in the Baltimore City,    #
#     Maryland (fips == "24510") from 1999 to 2008? Use the base plotting #
#     system to make a plot answering this question.                      #
###########################################################################

library(dplyr)

# read the data if necessary
  if (!any(ls()== "NEI")) NEI <- readRDS("summarySCC_PM25.rds")

# filter and aggregate the data
  data <- NEI %>% filter(fips=="24510") %>% group_by(year) %>%  summarise(sum(Emissions)) 

# set columns names for convenience
  colnames(data) <- c("year", "emissions")

# a function that does the plot
  plot1 <- function(){
  
    pp<-par(mfrow=c(1,2))
    
    plot(data,
         type = "o",
         lwd  = 3,
         col  = "darkgreen",
         main = "Total emissions (1999-2008)",
         ylab = "Emissions, tons",
         xlab = "Year"
    )
    
    barplot(
      height = data$emissions,
      width  = 1,
      space  = .2,
      names.arg = data$year,
      col =  rgb(red=220,green=255,blue=230,alpha=220,NULL,255),
      main = "Total emissions (1999-2008)",
      xlab = "Year",
      ylab = "Emissions, tons"
    )
    par(pp)
    
  }

# draw the plot on the screen
  plot1()

# and to a PNG file
  png(filename="plot1.png")
  plot1()
  dev.off()
