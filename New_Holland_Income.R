# install.packages("ggmap")
# install.packages("rgdal")
# install.packages("sp")
# install.packages("acs")
# install.packages("scales")
# install.packages("leaflet")
# install.packages("dplyr")

library(magrittr)
library(raster)
library(scales)
library(ggplot2)

library(ggmap)
library(rgeos)

library(leaflet)  # for interactive maps (NOT leafletR here)
library(ggplot2)  # for plotting
library(dplyr)    # for working with data frames

#############################################################################

##    tigris downloads the TIGER shapefile from the Census Bureau and reads it
##    into R with readOGR from the rgdal package to return a spatial data frame.

library(tigris)
library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects

# states() %>% plot()

#############################################################################

library(acs)
##    Census Data
##    "http://www.census.gov/newsroom/press-releases/2015/cb15-206.html

##    api.key.install("my_key_here")
##    You can get your own API key from
##    the Census Bureau

##    Get Raw Data from Census Bureau
##    http://censusreporter.org/tables/B19013/
##    Median household income in the past 12 months (year 2014)
##    (2014 Inflation-adjusted dollars)

##    Table Number Definitions
##    https://www.socialexplorer.com/data/ACS2012_5yr/documentation/333af309-eac4-4a1c-861f-eca812e26152

setwd("~/Edu/Data Science/maps")
censusapikey <- "c9d114c9b96164efbd7e739d9cb76c034782b4a7"

##    shapefiles
##    http://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2015&layergroup=Census+Tracts

##    census catography
##    http://www2.census.gov/geo/tiger/GENZ2010/ReadMe.pdf

##    http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=CF

##########################################################################

library(tigris)
library(sp)
library(acs)      # American Community Survey

# install.packages("devtools")
# devtools::install_github('walkerke/tigris')

gis_tracts <- tracts(state = 'PA', county = c('Lancaster '))

exprvec <- gis_tracts$GEOID
pattern <- "^.+([[:digit:]]{7})"
temp1 <- sapply(regmatches(exprvec, gregexpr(pattern, exprvec)),
                function(e) regmatches(e, regexec(pattern, e))) 
gis_tracts$GEOID <- do.call(cbind, temp1)[2,]

###############################################################################

income_data <- acs.fetch(endyear = 2014, geography = geo.make(state = "PA",
                         county = "Lancaster", tract = "*"), 
                         variable = "B19013_001", key=censusapikey)
income_df <- as.data.frame(income_data@estimate)
income_df$GEOID <- paste0(as.character(income_df@geography$state),
                          as.character(income_df@geography$county),
                          income_df@geography$tract)
colnames(income_df) <- c("hhincome","GEOID")

exprvec <- income_df$GEOID
temp1   <- sapply(regmatches(exprvec, gregexpr(pattern, exprvec)),
                             function(e) regmatches(e, regexec(pattern, e))) 
income_df$GEOID <- do.call(cbind, temp1)[2,]

###############################################################################

data_merged <- geo_join(chi, income_chi, "GEOID", "GEOID")

###############################################################################

# markers and paths are easy to access  #  x=-95.36, y=29.76
# 40°6'8???N 76°5'16???W / 40.10222°N 76.08778°W / 40.10222; -76.08778 (40.102095, -76.087646)
xlong <- -(76.08778 + 76.0877)/2
ylat <-  (40.10222 + 40.102095)/2

d <- function(x=xlong, y=ylat, n,r,a){
    round(data.frame(
        lon = jitter(rep(x,n), amount = a),
        lat = jitter(rep(y,n), amount = a)
    ), digits = r)
}
df <- d(n=50,r=3,a=.3)
mapg <- get_googlemap(center = c(lon = xlong, lat = ylat), #markers = df,
                     scale = 1) # path = df, markers = df
ggmap(mapg)
ggmap(mapg) +
    geom_point(aes(x = xlong, y = ylat), size = 3, colour = 'black') # data=df
    #geom_path(aes(x = lon, y = lat), data = df)


###############################################################################

library(leaflet)

#mp <- get_googlemap(mapg, markers)
map <- get_map(mapg, zoom=9)
pal <- colorQuantile("Greens", NULL, n = 9)
popup <- paste0("Median household income: ", as.character(chi_merged$hhincome))

p <- leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(data = data_merged,
      fillColor = pal(data_merged$hhincome),
      fillOpacity = 0.7,
      weight = 0.2,
      smoothFactor = 0.2,
      popup = popup) %>%
     # 
     setView(xlong, ylat, zoom = 9) %>%
     addLegend(pal = pal, values = data_merged$hhincome,
               position = "topright",
               title = "Income in Lancaster County")
p  

#############################################################################
#############################################################################

##    Save map with leaflet layers, an html file type, as as image file:
##    devtools::install_github('wch/webshot')

##    webshot::install_phantomjs()
library(htmlwidgets)
library(webshot)

## save html to png
saveWidget(p, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "Lancaster_Income.png",
        cliprect = "viewport")

#############################################################################


leaflet(data=phWI.1) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(~ dec_long_va, ~dec_lat_va, color = "red",
                        radius = 3, stroke = FALSE, fillOpacity = 0.8,
                        opacity = 0.8, popup =~station_nm)

