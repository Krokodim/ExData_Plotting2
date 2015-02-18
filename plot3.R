###########################################################################
#  3.Of the four types of sources indicated by the type (point, nonpoint, #
#    onroad, nonroad) variable, which of these four sources have seen     #
#    decreases in emissions from 1999?2008 for Baltimore City? Which have #
#    seen increases in emissions from 1999?2008? Use the ggplot2 plotting #
#    system to make a plot answer this question.                          #
###########################################################################

library(dplyr)
library(ggplot2)

# read the data if necessary
  if (!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")


# filter and aggregate the data
  data <- NEI %>% filter(fips=="24510") %>% group_by(year, type) %>%  summarise(sum(Emissions))  %>% group_by(year, type)

# set columns names for convenience
  colnames(data) <- c("year", "type", "emissions")

# a function that does the plot
  plot3 <- function(){
    # construct the plot
      g <- ggplot(data, aes(year, emissions))
  
    # split into horizontal facets by types
      g <- g + facet_grid(type ~ ., scales='free_y') 
  
    # draw a green line fo emissions
      g <- g + geom_line (color="darkgreen", lwd=1) 
      
    # draw thick points
      g <- g + geom_point(color="darkgreen", size=2) 
  
    # label the plot & axes  
      g <- g +  labs(
                  list(
                    title = "Total emissions in Baltimore City, Maryland", 
                    x = "Year", 
                    y = "Total emissions, tons"
                  )
                )

    # return the constructed plot
      return(g)
  }
 


# construct the plot 
  g <- plot3()

# draw the plot on the screen
  print(g)

# and to a PNG file
  ggsave("plot3.png", plot=g, width=6, height=5, dpi=96, units="in")
