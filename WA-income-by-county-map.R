#WA-income-by-county-map.R
#5/14/15
#get income data by county from census fact checker
#get WA carto. boundary file from TIGER DB
#followed outline from http://www.kevjohnson.org/making-maps-in-r/
#income and poverty 1999:2000 http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=CF
#2013 cartographic boundary shapefiles http://www.census.gov/geo/maps-data/data/cbf/cbf_tracts.html


require(ggplot2)
require(rgdal)
require(scales)
require(ggmap)
require(dplyr)
require(Cairo)
require(maptools)


#read shapefile to describe county boundaries
#give it directory and name of file.
#transform into something ggplot can use
tract <- readOGR(dsn = "cb_2013_53_tract_500k", layer = "cb_2013_53_tract_500k")
tract <- fortify(tract, region = "GEOID")

county <- readOGR(dsn = "cb_2013_53_tract_500k", layer = "cb_2013_53_tract_500k")
county <- fortify(county, region = "COUNTYFP")

#read and clean data
#tract$id has a bunch of trailing values that don't mean anything. trim after 5
tract$id <- substring(tract$id, 0, 5)

data <- read.csv("WA-income-by-county/DEC_00_SF3_GCTP14.ST05_with_ann.csv",
                 stringsAsFactors=FALSE)
data <- data[,c("GCT_STUB.target.geo.id2", "HC02")]
colnames(data) <- c("id","med.household.income")
data <- data[-c(1:2),]

#join data and tract DFs using ID column
plotData <- left_join(tract, data)
plotData$med.household.income <- as.numeric(plotData$med.household.income)

#time to plot!
p <- ggplot() + geom_polygon(data = plotData,
                             aes(x = long,
                                 y = lat,
                                 group = group,
                                 fill = med.household.income)) +
  geom_polygon(data = county, aes(x=long,
                                  y=lat,
                                  group=group),
                                  fill=NA,
                                  color="black",
                                  size=0.25) +
  coord_map() + 
  scale_fill_distiller(palette = "Greens") + 
  guides(fill = guide_legend(reverse=TRUE)) + 
  theme_nothing(legend=TRUE) + 
  labs(title = "Median Household Income", fill="")
  
ggsave(p, file = "WA-income-by-county-map.png",
       width=5, height=4,type="cairo-png")

