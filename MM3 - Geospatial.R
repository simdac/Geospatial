# Geospatial Analysis with R

install.packages("ggmap")
install.packages("rgdal")
install.packages("rgeos")
install.packages("maptools")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tmap")

library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
library(tmap)



# readOGR function is used to load a shapefile and assign it to a new spatialobject called “lnd”
# readOGR accepts two arguments: dsn which stands for “data source name” and specifies the directory in which the file is stored, and layer which specifies the file name
# no need to include the file extention .shp
# lnd object contains the population of London Boroughs in 2001 and the percentage of the population participating in sporting activities
lnd <- readOGR(dsn = "data", layer = "london_sport")

nrow(lnd)
ncol(lnd)

# How to load different types of spatial data
?readOGR


# lnd is made up of a number of slots
# data --> non attribute data
head(lnd@data, n = 2)

mean(lnd$Partic_Per) # short for mean(lnd@data$Partic_Per)

sapply(lnd@data, class)


# Pop_2001 is a factor
# coerce the variable into the correct, numeric, format
lnd$Pop_2001 <- as.numeric(as.character(lnd$Pop_2001))

lnd@proj4string


# Example of plot
x <- 1 : 400
y <- sin(x / 10) * exp(x * -0.01)
plot(x, y)


plot(lnd)

# select rows of lnd@data where sports participation is less than 1
lnd@data[lnd$Partic_Per < 13, 1:3]

# Select zones where sports participation is between 20 and 25%
sel <- lnd$Partic_Per > 20 & lnd$Partic_Per < 25
plot(lnd[sel, ], add = TRUE)
head(sel)

plot(lnd, col = "lightgrey") # plot the london_sport object
sel <- lnd$Partic_Per > 25
plot(lnd[ sel, ], col = "turquoise", add = TRUE) # add selected zones to map



### -----------------------------------
# Creating & Manipulating Spatial Data
### -----------------------------------

vec <- vector(mode = "numeric", length = 3)
df <- data.frame(x = 1:3, y = c(1/2, 2/3, 3/4))

class(vec)
class(df)

sp1 <- SpatialPoints(coords = df)
class(sp1)

spdf <- SpatialPointsDataFrame(sp1, data = df)
class(spdf)


proj4string(lnd) <- NA_character_ # remove CRS information from lnd
proj4string(lnd) <- CRS("+init=epsg:27700") # assign a new CRS

EPSG <- make_EPSG() # create data frame of available EPSG codes
EPSG[grepl("WGS 84$", EPSG$note), ] # search for WGS 84 code

lnd84 <- spTransform(lnd, CRS("+init=epsg:4326")) # reproject
saveRDS(object = lnd84, file = "data/lnd84.Rds")  # TO BE USED AT THE END OF THE SESSION


# Create and look at new crime_data object
crime_data <- read.csv("data/mps-recordedcrime-borough.csv",
                       stringsAsFactors = FALSE)

head(crime_data$CrimeType) # information about crime type

# Extract "Theft & Handling" crimes and save
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ]
head(crime_theft, 2)

# Calculate the sum of the crime count for each district, save result
crime_ag <- aggregate(CrimeCount ~ Borough, FUN = sum, data = crime_theft)
head(crime_ag, 2)

# Compare the name column in lnd to Borough column in crime_ag to see which rows match.
lnd$name %in% crime_ag$Borough #%in% command to identify which values in lnd$name are also contained in the Borough names of the aggregated crime data

# Return rows which do not match
lnd$name[!lnd$name %in% crime_ag$Borough]




library(dplyr)

head(lnd$name) # dataset to add to
head(crime_ag$Borough) # the variables to join

lnd@data <- left_join(lnd@data, crime_ag, by = c('name' = 'Borough'))

library(tmap)
qtm(lnd, "CrimeCount")



# Spatial Joins
library(rgdal)

stations <- readOGR(dsn = "data", layer = "lnd-stns")
proj4string(stations) # full geographical detail
proj4string(lnd) # coordinate reference system (CRS)


bbox(stations) # the extent, 'bounding box' of stations
bbox(lnd) # return the bounding box of the lnd object


# Create reprojected stations object
stations <- spTransform(stations, CRSobj = CRS(proj4string(lnd)))
plot(lnd) 
points(stations) # overlay the station points

# Clip the stations so that only those falling within London boroughs are retained
stations <- stations[lnd, ] 
plot(stations)


###------------------------
# Making Maps
###------------------------
# Using 'tmap' package
library(tmap)
vignette("tmap-nutshell")

qtm(shp = lnd, fill = "Partic_Per", fill.palette = "-Blues")
qtm(shp = lnd, fill = c("Partic_Per", "Pop_2001"), fill.palette = "Blues", ncol = 2)

tm_shape(lnd) +
  tm_fill("Pop_2001", thres.poly = 0) +
  tm_facets("name", free.coords = TRUE, drop.units = TRUE)

# Transform the coordinate reference system
install.packages("OpenStreetMap")
library("OpenStreetMap")

lnd_wgs = spTransform(lnd, CRS("+init=epsg:4326"))
if(curl::has_internet()) {
  osm_tiles = tmaptools::read_osm(bbox(lnd_wgs)) # download images from OSM
  tm_shape(osm_tiles) + tm_raster() +
    tm_shape(lnd_wgs) +
    tm_fill("Pop_2001", fill.title = "Population, 2001", scale = 0.8, alpha = 0.5) +
    tm_layout(legend.position = c(0.89, 0.02)) 
} else {
  tm_shape(lnd_wgs) +
    tm_fill("Pop_2001", fill.title = "Population, 2001", scale = 0.8, alpha = 0.5) +
    tm_layout(legend.position = c(0.89, 0.02))
}



# Using 'ggmap' package
library(ggplot2)
p <- ggplot(lnd@data, aes(Partic_Per, Pop_2001))

# To add text to the plot
p + geom_point(aes(colour = Partic_Per, size = Pop_2001)) +
  geom_text(size = 2, aes(label = name))

# ggmap requires spatial data to be supplied as data.frame
# fortify() used to extract as a data frame
library(rgeos)
lnd_f <- fortify(lnd) 

# The step above has lost the attribute information associated with the lnd object
# Add it back using the left_join function from the dplyr package
head(lnd_f, n = 2) # peak at the fortified data
lnd$id <- row.names(lnd) # allocate an id variable to the sp data
head(lnd@data, n = 2) # final check before join (requires shared variable name)
lnd_f <- left_join(lnd_f, lnd@data) # join the data


map <- ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) +
  geom_polygon() + coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)",
       fill = "% Sports\nParticipation") +
  ggtitle("London Sports Participation")

map + scale_fill_gradient(low = "white", high = "black")


# Creating interactive maps with leaflet
install.packages("leaflet")
library(leaflet)

lnd84 <- readRDS('data/lnd84.Rds')

leaflet() %>%
  addTiles() %>%
  addPolygons(data = lnd84)

library("tidyr")
library("dplyr")

london_data <- read.csv("data/census-historic-population-borough.csv")
ltidy <- gather(london_data, date, pop, -Area.Code, -Area.Name)
head(ltidy, 2)

head(lnd_f, 2) # identify shared variables with ltidy

ltidy <- rename(ltidy, ons_label = Area.Code) # rename Area.code variable
lnd_f <- left_join(lnd_f, ltidy)

# Rename the date variable
lnd_f$date <- gsub(pattern = "Pop_", replacement = "", lnd_f$date)


# Use facet to produce one map per year
library("ggplot2")
ggplot(data = lnd_f, # the input data
       aes(x = long, y = lat, fill = pop/1000, group = group)) + # define variables
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ date) + # one plot per time slice
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red", # colors
                       midpoint = 150, name = "Population\n(thousands)") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks
