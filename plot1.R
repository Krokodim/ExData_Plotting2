## This first line will likely take a few seconds. Be patient!
if (!any(ls()== "NEI")) NEI <- readRDS("summarySCC_PM25.rds")
if (!any(ls()== "SCC")) SCC <- readRDS("Source_Classification_Code.rds")

########################################################################
#  1. Have total emissions from PM2.5 decreased in the United States   #
#     from 1999 to 2008? Using the base plotting system, make a plot   #
#     showing the total PM2.5 emission from all sources for each of    #
#     the years 1999, 2002, 2005, and 2008.                            #
########################################################################

library(dplyr)

data <- NEI %>% group_by(year) %>%  summarise(sum(Emissions)/1000) 
colnames(data) <- c("year", "emissions")

plot(data,
     type = "l",
     lwd  = 5,
     col  = "darkgreen",
     main = "Total emissions (1999-2008)",
     ylab = "Emissions, kilotonns",
     xlab = "Year"
     )
