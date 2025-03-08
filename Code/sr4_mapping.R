## -----------------------------------------------------------------------------
##
## Project: Summer Research Project
##
## Script purpose: Loading Shapefile(s), mapping data and correcting 
##
## Date: 10/06/2024
## Author: BOD
##
## -----------------------------------------------------------------------------



##-----------------------------------------------------------------------------
## Loading Packages 
##-----------------------------------------------------------------------------


library(sf)
library(spdep) # for neighbour list 
library(dplyr)
library(ggplot2)
library(viridis)
library(tidygeocoder)
library(sp)
library(cowplot)


##-----------------------------------------------------------------------------
## Loading Shapefiles
##-----------------------------------------------------------------------------


# Load in county shapefile 
counties_sf <-st_read("./Data/Map Data/counties/counties.shp")
counties_sf <- dplyr::select(counties_sf, c(NAME_TAG, geometry))
counties_sf <- rename(counties_sf, County = NAME_TAG)
counties_sp <- as(counties_sf, "Spatial")


# Republic Of Ireland counties 
ni_counties <- c('Antrim', 'Down', 'Armagh', 'Londonderry', 'Tyrone', 'Fermanagh')
roi_counties_sf <- counties_sf[!counties_sf$County %in% ni_counties, ]
roi_counties_sp <- as(roi_counties_sf, "Spatial")


# Load in Republic Of Ireland provinces 
provinces_sf <-st_read("./Data/Map Data/provinces/provinces.shp")
provinces_sf <- dplyr::select(provinces_sf, c(NAME, geometry))
provinces_sf <- rename(provinces_sf, Province = NAME)
provinces_sp <- as(provinces_sf, "Spatial")


# Load in Eircode shapefile 
eircodes_sf <-st_read("./Data/Map Data/eircodes/eircodes.shp")
eircodes_sf <- rename(eircodes_sf, Eircode = RoutingKey)
eircodes_sf <- st_zm(eircodes_sf) # remove the z coordinate from sf
eircodes_sp <- as(eircodes_sf, "Spatial") 



# Load in data
  # hd <- readr::read_csv("./Data/hd_textmined_temp.csv")

# # Plot data and shapefile =
  # ggplot(counties_sf) + geom_sf() +
  #   scale_fill_viridis() + theme_bw()
  # g <- ggplot(counties_sf) + geom_sf() +
  #   scale_fill_viridis() 
  # g <- g + geom_point(data = hd, 
  #                     aes(x = Longitude, y = Latitude, color = Price))
  # g


##-----------------------------------------------------------------------------
## Removing Outliers  
##-----------------------------------------------------------------------------


# create a shapefile from hd 
hd_spdf <- SpatialPointsDataFrame(coords = hd[, c("Longitude", "Latitude")], data = hd,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

# hd points within counties_sf border
hd_spdf_within <- hd_spdf[!is.na(over(hd_spdf, as(counties_sp, "SpatialPolygons"))), ]

# investigate the points outside 
hd_outside <- hd[!(hd$Id %in% hd_spdf_within$Id), ]


new_lat <- c(53.44667435543448, 53.31295852749137, 52.38455899070449, 53.31313800365343,
             52.2582051766416, 53.282955944139765, 52.60252365239631, 53.32477159805044, 
             52.23843584208007, 52.26306868098251, 52.665281100226835, 53.61369034209258,
             52.13069688164601, 52.51482704708016)
new_long <- c(-6.2058299118456715, -6.272995615472377, -6.578823062349466, -6.272738123404951, 
              -7.102180924362127, -6.146134777398823, -6.213086846630266, -6.25733428184365, 
              -7.096114931306214, -7.117861362979499, -8.651876123994517, -7.088512153818952,
              - 10.37642556270303, -6.284910937868937)

# update the hd
hd$Latitude[hd$Id %in% hd_outside$Id] <- new_lat
hd$Longitude[hd$Id %in% hd_outside$Id] <- new_long

# remove value wth missing coordinates 
hd <- hd[!is.na(hd$Longitude),]


## Last merge, they should all be within now 
# create a shapefile from hd 
hd_spdf <- SpatialPointsDataFrame(coords = hd[, c("Longitude", "Latitude")], data = hd,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

# hd points within counties_sf border
hd_spdf_within <- hd_spdf[!is.na(over(hd_spdf, as(counties_sp, "SpatialPolygons"))), ]

# all inside border.
dim(hd_spdf_within) == dim(hd)




##-----------------------------------------------------------------------------
## Plot Data 
##-----------------------------------------------------------------------------


# Investigate points in norther Ireland 
# create a shapefile from hd 
hd_spdf <- SpatialPointsDataFrame(coords = hd[, c("Longitude", "Latitude")], data = hd,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

# hd points within counties_sf border
hd_spdf_within <- hd_spdf[!is.na(over(hd_spdf, as(roi_counties_sp, "SpatialPolygons"))), ]

# investigate the points outside 
hd_outside <- hd[!(hd$Id %in% hd_spdf_within$Id), ]

# goecoding doesnt work, manually enter these 
new_lat <- c(
53.72880382736809, 
53.273113427930674,
51.909662908899925,
53.64315935580801)

new_long <- c(
-8.997919490692208,
-9.076679996352485,
-8.198785374427981,
-6.6916980848992695)

hd$Latitude[hd$Id %in% hd_outside$Id] <- new_lat
hd$Longitude[hd$Id %in% hd_outside$Id] <- new_long




### Correct now 
# create a shapefile from hd 
hd_spdf <- SpatialPointsDataFrame(coords = hd[, c("Longitude", "Latitude")], data = hd,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))


##-----------------------------------------------------------------------------
## Assign provinces, counties, eircodes 
##-----------------------------------------------------------------------------


## Assign Correct Counties 
# update shapefile of data 
hd_sf <- st_as_sf(hd, coords = c("Longitude", "Latitude"), crs = st_crs(roi_counties_sf))

# join shapefile and counties ROI
hd_join_county <- st_join(hd_sf, roi_counties_sf)
# table(hd_join_county$County == hd_join_county$County) #310 incorrect counties. 

# set the county to the County value from shapefile 
hd$County = hd_sf$County

# Check County Assinment 
# g <- ggplot(roi_counties_sf) + geom_sf() +
#   scale_fill_viridis() + theme_bw()
# g <- g + geom_point(data = hd, 
#                     aes(x = Longitude, y = Latitude, color = County)) + ggtitle('Map of Observations by County')
# g


## Assignn Province

# join shapefile and eircodes ROI
hd_join_province <- st_join(hd_sf, provinces_sf)
hd$Province <- hd_join_province$Province

# Check Eircodes
# g <- ggplot(provinces_sf) + geom_sf() +
#   scale_fill_viridis() + theme_bw()
# g <- g + geom_point(data = hd, 
#                     aes(x = Longitude, y = Latitude, color = Province)) + ggtitle('Map of Observations by Province')
# g


## Assignn Correct Eircodes
# set sphiercal as false so that the files join correctly 
sf_use_s2(F)

# join shapefile and eircodes ROI
hd_join_eircodes <- st_join(hd_sf, eircodes_sf)
hd$Eircode <- hd_join_eircodes$Eircode

## missign values for eircodes 
no_eircode <- hd[is.na(hd$Eircode), ]

# input the missing values 
no_eircode$PostalCode[no_eircode$Id %in% c(505607)] <- 
  c('P85 XT27')

no_eircode$Eircode <- substr(no_eircode$PostalCode, 1, 3)
hd$Eircode[hd$Id %in% no_eircode$Id] <- no_eircode$Eircode


# Check Eircodes

# g <- ggplot(ecodes) + geom_sf(aes(fill = Eircode)) + theme(legend.position="none") + scale_fill_discrete()
# g <- g + geom_point(data = hd, 
#                     aes(x = Longitude, y = Latitude), size = 0.2) + ggtitle('Map of Observations by Eircode')
# g
# 
# tm_shape(eircodes_sf) + tm_fill("MAP_COLORS") + tm_format("World", title="A green World")
# 



#### the data hd now contains correct county, provence and eircode assignment ####


dim(hd)


##-----------------------------------------------------------------------------
## Create Base Map 
##-----------------------------------------------------------------------------

# map of Ire counties
map_counties <- ggplot() + theme_map() +
  geom_sf(data = counties_sf, fill = NA, color = "lightgrey", alpha = 0.2) 
map_counties

# map of roi counties
map_roi_counties <- ggplot() + theme_map() +
  geom_sf(data = roi_counties_sf, fill = "cornsilk2", color = "darkgrey", alpha = 1) 
map_roi_counties

# map of eircodes 
map_roi_eircodes <- ggplot() + theme_map() +
  geom_sf(data = eircodes_sf, fill = "cornsilk2", color = "darkgrey", alpha = 1) 
map_roi_eircodes

map_roi_eircodes <- ggplot() + theme_map() +
  geom_sf(data = eircodes_sf, fill = "cornsilk2", color = "darkgrey", alpha = 1) +
map_roi_eircodes







