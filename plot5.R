###########################################################################
#  4.How have emissions from motor vehicle sources changed from 1999–2008 #
#    in Baltimore City?                                                   #
###########################################################################


library(dplyr)
library(ggplot2)

# read the data if necessary
  if (!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")

# filter and aggregate the data
  scc  <- SCC[grepl('Onroad', SCC$Data.Category, ignore.case=TRUE),"SCC"]
  data <- NEI %>%  filter(SCC %in% scc & fips=="24510") %>% group_by(year) %>%  summarise(sum(Emissions))  %>% group_by(year)

# set columns names for convenience
  colnames(data) <- c("year", "emissions")

# a function that does the plot
  plot5 <- function(){
    # construct the plot
      g <- ggplot(data, aes(year, emissions))
      
    # format the X axis
      g <- g + scale_x_continuous(breaks=c(1999:2008))
  
    # draw a green line fo emissions
      g <- g + geom_line (color="darkgreen",  lwd=2) 
      
    # draw thick points
      g <- g + geom_point(color="darkgreen", size=3) 
  
    # label the plot & axes  
      g <- g +  labs(
                  list(
                    title = "Total motor vehicle emissions in Baltimore City, Maryland", 
                    x = "Year", 
                    y = expression("Total emissions, tons"),
                    sub = ""
                  )
                )
    # return the constructed plot
      return(g)
  }

# construct the plot 
  g <- plot5()

# draw the plot on the screen
  print(g)

# and to a PNG file
  ggsave("plot5.png", plot=g, width=7, height=5, dpi=96, units="in")
