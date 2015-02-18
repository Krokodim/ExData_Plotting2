###########################################################################
#  3.Of the four types of sources indicated by the type (point, nonpoint, #
#    onroad, nonroad) variable, which of these four sources have seen     #
#    decreases in emissions from 1999–2008 for Baltimore City? Which have #
#    seen increases in emissions from 1999–2008? Use the ggplot2 plotting #
#    system to make a plot answer this question.                          #
###########################################################################

library(dplyr)

# read the data if necessary
  if (!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")
  if (!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

# filter and aggregate the data
  data1 <- NEI %>% filter(fips=="24510") %>% group_by(year, type) %>%  summarise(sum(Emissions)) 

# set columns names for convenience
  colnames(data1) <- c("year", "type", "emissions")

# a function that does the plot
  plot3 <- function(){
  
    library(ggplot2)
    
#     g <- ggplot(data, aes(year, emissions, color=type)) +
#     
#          geom_line() + 
#          xlab("year") + 
#          ylab(expression('Total PM'[2.5]*" Emissions")) + 
#          ggtitle('Total Emissions in Baltimore City, Maryland from 1999 to 2008') 
#          print(g)
    
    qplot(emissions, year, data = data1, geom = "smooth", method = "loess", facets =  ~ type)

    
    
  }

# draw the plot on the screen
  plot3()

# and to a PNG file
  png(filename="plot1.png")
  plot3()
  dev.off()
