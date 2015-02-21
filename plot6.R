###########################################################################
#  4.How have emissions from motor vehicle sources changed from 1999–2008 #
#    in Baltimore City?                                                   #
###########################################################################

# According to wikipedia :)
#  - Baltimore area is 239 square km
#  - LA area is 1301,97 square km, i.e. ~5.4 times wider
#  so in this exercise we will work with emissions per 1 square km
#  such an approach makes them comparable

library(dplyr)
library(ggplot2)

# read the data if necessary
  if (!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")
  if (!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

# filter and aggregate the data
  scc  <- SCC[grepl('Onroad', SCC$Data.Category, ignore.case=TRUE),"SCC"]
  data <- NEI %>%  filter(SCC %in% scc & fips %in% c("24510","06037")) %>% 
  group_by(year,fips) %>%  summarise(sum(Emissions))  %>% group_by(year,fips)


# set the county area 
  data$area <- ifelse(data$fips=="24510", 239, ifelse(data$fips=="06037",1301.97,0))

# set columns names for convenience
  colnames(data) <- c("year", "county", "emissions", "area")

# adjust the emissions amount per sqare km
  data$em.per.km2 <- data$emissions / data$area 

# county names should be readable
  data$county <- factor(
    data$county,
    levels=c("24510","06037"),
    labels=c("Baltimore City, Maryland", "Los Angeles County, California")
    )

# a function that does the plot
  plot6 <- function(){
    # construct the plot
      g <- ggplot(data, aes(year, em.per.km2)) 
      
    # facets
      g <- g + facet_grid(. ~ county)
      
    # format the X axis
      g <- g + scale_x_continuous(breaks=c(1999,2002,2005,2008))
  
    # bars are also good for dynamic analysis...
      g <- g + geom_area (stat="identity", fill="steelblue", alpha=1/5) 
      
    # label the plot & axes  
      g <- g +  labs(
                  list(
                    title = "Total motor vehicle emissions comparison between two US counties", 
                    x = "Year", 
                    y = expression("Total emissions, tons / km"^{2}),
                    sub = ""
                  )
                )
    # return the constructed plot
      return(g)
  }

# construct the plot 
  g <- plot6()

# draw the plot on the screen
  print(g)

# and to a PNG file
  ggsave("plot6.png", plot=g, width=7, height=5, dpi=96, units="in")
