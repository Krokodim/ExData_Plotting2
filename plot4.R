###########################################################################
#  4.Across the United States, how have emissions from coal               #
#    combustion-related sources changed from 1999?2008?                   #
###########################################################################


library(dplyr)
library(ggplot2)

# read the data if necessary
  if (!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")
  if (!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

# filter and aggregate the data
  scc  <- SCC[grepl('coal', SCC$Short.Name, ignore.case=TRUE),"SCC"]
  data <- NEI %>%  filter(SCC %in% scc) %>% group_by(year) %>%  summarise(sum(Emissions)/1000)  %>% group_by(year)

# set columns names for convenience
  colnames(data) <- c("year", "emissions")

# a function that does the plot
  plot4 <- function(){
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
                    title = "Total coal combustion-related emissions in USA", 
                    x = "Year", 
                    y = expression("Total emissions, tons ? 10"^{3}),
                    sub = ""
                  )
                )
    # return the constructed plot
      return(g)
  }

# construct the plot 
  g <- plot4()

# draw the plot on the screen
  print(g)

# and to a PNG file
  ggsave("plot4.png", plot=g, width=6, height=5, dpi=96, units="in")
