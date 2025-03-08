## -----------------------------------------------------------------------------
##
## Project: Summer Research Project
##
## Script purpose: Creating sub areas, defining shapefiles and assigning subareas
##
## Date: 02/07/2024
## Author: BOD
##
## -----------------------------------------------------------------------------


##-----------------------------------------------------------------------------
## Create Regions 
##-----------------------------------------------------------------------------

# City Centres
city_centres <- readr::read_csv(here("Data/Sub Areas/city_centres.csv"))


# define a bounding box of each city 
limerick_box <- data.frame(
  lat = c(52.68876358220959, 52.61972287850951, 52.68876358220959, 52.61972287850951),
  lon = c(-8.524260976788852, -8.524260976788852, -8.67995784689225, -8.67995784689225),
  label = c("ne", "se", "nw", "sw")
)

dublin_box <- data.frame(
  lat = c(53.47726004287839, 53.22153969554782, 53.47726004287839, 53.22153969554782),
  lon = c(-6.00517849202134, -6.00517849202134, -6.473235936021361, -6.473235936021361),
  label = c("ne", "se", "nw", "sw")
)

cork_box <- data.frame(
  lat = c(51.94910939321306, 51.85476491613192, 51.94910939321306, 51.85476491613192),
  lon = c(-8.334525826180034, -8.334525826180034, -8.637547910320375, -8.637547910320375),
  label = c("ne", "se", "nw", "sw")
)

galway_box <- data.frame(
  lat = c(53.30451673258911, 53.254270611474595, 53.30451673258911, 53.254270611474595),
  lon = c(-8.960220231705378, -8.960220231705378, -9.133220595148106, -9.133220595148106),
  label = c("ne", "se", "nw", "sw")
)


##-----------------------------------------------------------------------------
## Create Shapefiles for cities 
##-----------------------------------------------------------------------------

# Convert limerick_box to an sf object
limerick_box_sf <- st_as_sf(limerick_box, coords = c("lon", "lat"), crs = 4326)
dublin_box_sf <- st_as_sf(dublin_box, coords = c("lon", "lat"), crs = 4326)
galway_box_sf <- st_as_sf(galway_box, coords = c("lon", "lat"), crs = 4326)
cork_box_sf <- st_as_sf(cork_box, coords = c("lon", "lat"), crs = 4326)

# Create Bounding _boxes and intersect with roi_counties_sf
limerickB_box <- st_bbox(limerick_box_sf)
limerick_box_poly <- st_as_sfc(limerickB_box)
limerick_sf <- st_intersection(roi_counties_sf, limerick_box_poly)

corkB_box <- st_bbox(cork_box_sf)
cork_box_poly <- st_as_sfc(corkB_box)
cork_sf <- st_intersection(roi_counties_sf, cork_box_poly)

galwayB_box <- st_bbox(galway_box_sf)
galway_box_poly <- st_as_sfc(galwayB_box)
galway_sf <- st_intersection(roi_counties_sf, galway_box_poly)

dublinB_box <- st_bbox(dublin_box_sf)
dublin_box_poly <- dublinB_box %>% st_as_sfc()
dublin_sf <- st_intersection(roi_counties_sf, dublin_box_poly)


##-----------------------------------------------------------------------------
## Create Shapefiles for the observations in cities 
##-----------------------------------------------------------------------------

# find the houses within each subregion and create sf and sp for each one 
limerick_sp <- as(limerick_sf, "Spatial")
hd_limerick_sp <- hd_spdf[!is.na(over(hd_spdf, as(limerick_sp, "SpatialPolygons"))), ]
hd_limerick <- hd[(hd$Id %in% hd_limerick_sp$Id), ]

cork_sp <- as(cork_sf, "Spatial")
hd_cork_sp <- hd_spdf[!is.na(over(hd_spdf, as(cork_sp, "SpatialPolygons"))), ]
hd_cork <- hd[(hd$Id %in% hd_cork_sp$Id), ]

dublin_sp <- as(dublin_sf, "Spatial")
hd_dublin_sp <- hd_spdf[!is.na(over(hd_spdf, as(dublin_sp, "SpatialPolygons"))), ]
hd_dublin <- hd[(hd$Id %in% hd_dublin_sp$Id), ]

galway_sp <- as(galway_sf, "Spatial")
hd_galway_sp <- hd_spdf[!is.na(over(hd_spdf, as(galway_sp, "SpatialPolygons"))), ]
hd_galway <- hd[(hd$Id %in% hd_galway_sp$Id), ]



##-----------------------------------------------------------------------------
## Assign observations to towns 
##-----------------------------------------------------------------------------

# Import the urban areas 
town_centres <- readr::read_csv(here("Data/Sub Areas/town_centres.csv"))


# distance from each obervation to each town in km
dist_hd_towns <- data.frame(geodist(hd, town_centres)/1000)
hd_towns <- hd[apply(dist_hd_towns, 1, function(row) any(row < 10)), ]



# The remainder are rural 
hd_rural <- hd[!(hd$Id %in% c(hd_galway$Id, hd_cork$Id, hd_limerick$Id, hd_dublin$Id, hd_towns$Id)), ]



  
# Assign Id's to the hd representing the region 
hd$SubArea <- NA
hd$SubArea[hd$Id %in% hd_towns$Id] <- 'Towns'
hd$SubArea[hd$Id %in% hd_galway$Id] <- 'Galway'
hd$SubArea[hd$Id %in% hd_dublin$Id] <- 'Dublin'
hd$SubArea[hd$Id %in% hd_limerick$Id] <- 'Limerick'
hd$SubArea[hd$Id %in% hd_cork$Id] <- 'Cork'
hd$SubArea[hd$Id %in% hd_rural$Id] <- 'Rural'






