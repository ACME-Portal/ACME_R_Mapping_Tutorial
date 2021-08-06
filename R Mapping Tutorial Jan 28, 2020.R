# R Mapping Tutorial - Jan 28, 2020
# Wylie Fuller 

install.packages("curl")
install.packages("xts")

# Libraries ----
library(tidyverse)
library(lme4)
library(lattice)
library(MuMIn)
library(AICcmodavg)
library(faraway)
library(knitr)
library(AICcmodavg)
library(DT)
library(boot)
library(dotwhisker)
library(stargazer)
library(cowplot)
library(broom)
library(sf)
library(tmap)
library(raster)
library(blockCV)
library(shinyjs)
library(tmaptools)
library(precrec)
library(mapview)
library(visreg)
library(MASS)
library(stars)
library(lwgeom)
library(rgeos)
library(leaflet)
library(mapedit)


##### SECTION 0: How to adapt this script for your computer ----------

# 1) Change x11() to quartz() on Mac's
x11()  # PC
quartz()  # Mac

## 2) Filenames:
# All raster() format rasters aren't actually stored as data in your R workspace. Data is pulled from their file location every time you use them.

## Set you working directory, and keep all the input data in the "Input Data" folder, *within* your working directory.
# This should let you access the rasters without changing anything else in the file names!

## 3) Pipes:
# Pipes help write concise lines of code with fewer intermediate variables. Spatial data often needs a lot of intermediate steps and conversions.
# source: https://www.datacamp.com/community/tutorials/pipe-r-tutorial -- skip through into to "how to use"

#  function(data)   ==   data %>% function()
sf_1 <- make_valid(sf_1)
sf_1 <- sf_1 %>% make_valid()   # Makes sf_1 have valid geometry, and saves the output back as sf_1. 

#  function(data, y)   ==   data %>% f(y)    # y can be more data or argument (e.g. col = "blue")
filter(data, YEAR > 1993) 
data %>% filter(YEAR > 1993)  

data %>% filter(YEAR > 1993) %>% st_intersection(data_2)   # filters "data" then intersects it with other data

#  function_2(funtion_1(data)))  ==  function_1(data) %>% function_2()  ==  data %>% function_1() %>% function_2()
nrow(filter(data, YEAR > 1993))
data %>% filter(YEAR > 1993) %>% nrow()    # Select data with YEAR > 1993, then count the number of rows of this data

# Pipes use the input thing as the first argument for a function
# if you need the thing from the pipe to go somewhere else (second argument) put a "." symbol where you want the piped thing to go
function(y, x)  =  x %>% function(y, .)

## Example pipe workflow:
# Take a spatial object, turn it into a table for a function that doesn't work on spatial objects, apply the function,
# then turn the table back into a spatial object, and SAVE the result as a new spatial object. 
simple_feature_newdata <- simple_feature %>% data.frame() %>% .[!duplicated(.[, "Camera_Site"]),] %>% st_as_sf()  # Note the "." where I want the piped thing to be used



##### SECTION 1: What is spatial data?  ------------

## Part 1 - Simple Features - sf -----
# Simple features "sf" are tables with a fancy header, and spatial data stored separately in a geometry column. 
# Class "sfc" has geometry type GEOMETRY. These are less useful.   
# Class "sf" has geometry type POINT, MULTIPOINT, MULTIPOLYGON, etc. These are your go-to class for analysis.

# Load data from a .csv with Lat/Long coordinates - use projection WGS 84 (crs = 4326) for all Lat/Long coordinates.
wildlife_cameras <- st_read("Input Data/wildlife_cameras_within3km_MLP_stations.csv", options=c("X_POSSIBLE_NAMES=lon","Y_POSSIBLE_NAMES=lat"), 
                          crs = 4326)
wildlife_cameras <- st_transform(wildlife_cameras, crs = 3400)  # Transform to the crs you want for your datasets.
mapview(wildlife_cameras) 

# Load data from a shapefile or geopackage 
bc_parks <- st_read("Input Data/Parks.shp")  # No crs specified in the .shp file. Have to know what it is, or make a good guess.
bc_regional_parks <- st_read("Input Data/Parks.shp", crs = 3005)
mapview(bc_regional_parks)

wilmore_wilderness <- st_read("Input Data/Wilmore_Wilderness.gpkg")
mapview(wilmore_wilderness)

# Lat/Long data is ALWAYS in crs = 4326. UTM Data (mE/mN) is loaded in whatever projection the data came in. 

# Simple features can be converted to tables, worked with, and converted back to simple features. 
test <- waterbodies %>% data.frame() %>% .[order(-waterbodies$Shape_Area), ] %>% st_as_sf()
mapview(test)


### Part 2 - Export sf Data -----
## Export sf *points* data to a csv and retain location information - 
# Say you've created an awesome new sf points feature (camera locations) with a geometry column but no Eastings/Northings data. 
# You need to retain either lat/long or Easting/Northing columns to be able to plot the .csv after you export it. 

wildlife_cameras <- wildlife_cameras %>% dplyr::select(-Easting, -Northing, -Easting_mlp, -Northing_mlp, -usethis_crs)  
# Remove all loc data for this demo. Remember: crs = 3400 for the data in the geometry column  
mapview(wildlife_cameras)  # Still plots, because the geometry column is present - this gives sf objects their spatial attributes. 

## Extract UTM coordinates from the geometry column --
cameras_utm <- do.call(rbind, st_geometry(wildlife_cameras)) %>% 
  as_tibble() %>% setNames(c("Easting","Northing"))

# Compare UTM table to the geometry list in the original dataset
cameras_utm
wildlife_cameras$geometry

# Add Easting and Northing columns on the original dataset, with the UTM data extracted from the geometry column
wildlife_cameras$Easting <- cameras_utm$Easting
wildlife_cameras$Northing <- cameras_utm$Northing

# Add a column stating what projection to plot the UTM cooridnates in. This is how you make friends with the next person using the dataset.
wildlife_cameras$usethis_crs = "3400"

# Convert the sf object to a table, remove the geometry column, and save as a .csv
## Tables/Tibbles/Dataframes do not store the geometry column properly - it's a *list* of coordinates inside a column.
wildlife_cameras_table <- wildlife_cameras %>% as_tibble() %>% dplyr::select(-geometry)  

# Write table to csv
write_csv(wildlife_cameras_table, "wildlife_cameras_table.csv")


## Save sf objects to a shapefile (.shp) or geopackage (.gpkg). --
st_write(wildlife_cameras, dsn = "wildlife_cameras_within3km_MLP_stations.shp", 
         layer = "wildlife_cameras_within3km_MLP_stations")
st_write(wildlife_cameras, dsn = "wildlife_cameras_within3km_MLP_stations.gpkg", 
         layer = "wildlife_cameras_within3km_MLP_stations")



### Part 3 - Rasters -----
# Rasters use package "raster". They are not stored in a tidy format so you have to use diffferent indexing. See below.
# *Unlike* sf objects, "raster" stores only the header info for raster objects in your R Workspace/Environment.  
# The true data stays stored on your computer file-path every time you call a raster object. Make sure you have the correct file path for the raster in your code!

## Load a raster --
# Single-band raster:
nrcan_file <- "Input Data/canada_landcover_25class_clip_resize.tif"
nrcan_raster <- raster(nrcan_file)   # Remote-sensed classification from: https://webservices.maps.canada.ca/arcgis/rest/services/NRCan/canada_landcover_2000_2011_25class_en/ImageServer
# Note: this data came in an Arc .lyr (layer) file for all of Canada. In ArcGIS I zoomed in to the area of interest and exported it as a .tif.
# There are tools for converting .lyr files in R. QGIS can  also use most ArcGIS data formats. 

# Indexing a raster
nrcan_raster$canada_landcover_25class_clip_resize  # extracts the *layer*. Find a multi-band image and a raster stack.
nrcan_raster@crs  # Gives the "proj4string" code. Websites that give info on projections will include this code somewhere. Each projection has a different code. 

# Viewing a raster
x11(10,10)
tm_shape(nrcan_raster) + tm_raster()   # Static map. I find these work for a whole study area, but zooming and panning requires coordinates.
tmap_mode("view")  # Set this, and then enter the code above - sets the same map to interactive viewing
tmap_mode("plot")

mapview(nrcan_raster) # Plots warped, because mapview reprojects data into Web Mercator for the basemap tiles.
# For interactive viewing, I prefer mapview over tmap view, but tmap view is handy for quick exploration of maps you'd like to plot static.

## Reproject a raster:
crs_4326 = "+proj=longlat +datum=WGS84 +no_defs"   # Set crs
projectRaster(nrcan_raster, crs = crs_4326)        # Project raster

# Load a topography raster
# Later - create a raster brick
raster_stack

viewRGB()  # Test this with a raster stack or raster brick


#### Interactively zoom on a raster plot in R --- 
plot(nrcan_raster)
s <- raster::select(nrcan_raster)
plot(s)
t <- raster::select(s)
plot(t)
## Heck ya!




##### SECTION 2: Tidying your data --------

### Part 1 - Projections -----

# Load Lat/Long data in WGS 84 - crs 4326 - proj4string "+proj=longlat +datum=WGS84 +no_defs"
used_locations <- st_read("Input Data/Real and Random Locations/real_deer_with_UTM.csv", options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude"), 
                          crs = 4326)
used_locations <- st_transform(deer_locations, crs = 3400)  # Transform to the crs I want for my project

# Load UTM coordinates Easting/Northing in *whatever crs they have been specified in*. 
espp <- st_read("Input Data/ESPP_clean.csv", options = c("X_POSSIBLE_NAMES=Easting", "Y_POSSIBLE_NAMES=Northing"), crs = 26911)  
mapview(t_espp)

# Demo: Load UTM cooridnates in the *wrong* projection:
t_espp <- st_read("Input Data/ESPP_clean.csv", options = c("X_POSSIBLE_NAMES=Easting", "Y_POSSIBLE_NAMES=Northing"), crs = 3400) 


### Part 2 - Making sf valid  ------
# If you're loading sf from other datasets, invalid geometries can be a pain. Lots of errors come up during processing if geometries are invalid.

# Load an sf object from a shapefile:
seismic_grids_clip <- st_read("Input Data/seismic_grids_lesser_slave_lake.shp", crs = 3400) 
mapview(seismic_grids_clip)
## Note - sometimes shp files load with a crs already in place, sometimes they don't. Try once without the "crs =" argument, and see if it has a crs existing. 

# Get the number of invalid geometries for an sf object
count_seismic_grids_clip_notvalid <- seismic_grids_clip %>% st_is_valid %>% sum(na.rm = TRUE) %>% sum(sum() - nrow(seismic_grids_clip)) %>% abs()
## This gets the number of valid features for an object, then subtracts from the total number of features to find the number of invalid ones. 

seismic_grids_clip <- st_make_valid(seismic_grids_clip)
# Run the code to count invalid features again - this time all features are valid!

## Note - sometimes st_make_valid converts sf objects to "sfc" objects with "geometry type = GEOMETRY" instead of POLYGON, MULTIPOLYGON, etc
class(seismic_grids_clip)  # "sf" - good!

# sfc objects are rarely useful - make a feature valid and convert it back to class "sf" with the code below: 
seismic_grids_clip <- st_make_valid(seismic_grids_clip) %>% st_cast("MULTIPOLYGON")  

## Note - you can add the "make valid" code directly when you add a shp file.
seismic_grids_clip <- st_read("Input Data/seismic_grids_lesser_slave_lake.shp", crs = 3400) %>% st_make_valid()



##### SECTION 3: Viewing Spatial Data ---------

## Static Maps --   *** plotting demonstations are shown in the Deer Analysis section, "Section 7 - Making Maps"
# ggplot  - works for features, not as well for rasters. 
# ggmap   - lots of examples, but I can't get it to work. 
# tmap    - I like it!
# plot()  - basic. Works!

## Interactive Maps -- 
# tmap - too simple 
# leaflet - too complex 
# mapview  - totally great

## Mapview Maps
mapview(deer_locations)
mapview(nrcan_raster) # Plots warped, because mapview reprojects data into Web Mercator for the basemap tiles.


### Leaflet Maps ------
# Leaflet demos are available here: https://rstudio.github.io/leaflet/

# 1) Options: leaflet(options = leafletOptions(option_1 = __, option_2 = __, option_3 = __)) - https://leafletjs.com/reference-1.0.0.html#map-option
leaflet(options = leafletOptions(boxZoom = TRUE))

# 2) Add data: 
leaflet(options = leafletOptions(boxZoom = TRUE)) %>% addCircles(data = st_transform(deer_locations, 4326)) %>% 
  addCircles(data = st_transform(wildlife_cameras, 4326))
# Note: Must always call st_transform for sf in leaflet. It's a pain! 

# 3) Add tiles: 
leaflet(options = leafletOptions(boxZoom = TRUE)) %>% addCircles(data = st_transform(deer_locations, 4326)) %>% 
  addCircles(data = st_transform(wildlife_cameras, 4326)) %>% addProviderTiles("Esri.WorldTopoMap", "CartoDB.Positron")

# 4) Add other map options:
leaflet(options = leafletOptions(boxZoom = TRUE)) %>% addCircles(data = st_transform(deer_locations, 4326)) %>% 
  addCircles(data = st_transform(wildlife_cameras, 4326)) %>% addProviderTiles("Esri.WorldTopoMap", "CartoDB.Positron") %>% 
  addLayersControl(overlayGroups = c("OSM Deer Locations", "Wildlife Cameras"))

## Leaflet is customizable, but you have to specify *everything*. Use mapview unless you want to have all of Leaflet's options.



### Mapedit -- demonstration -------
# Select or modify points on a map. Pretty great.

# Create features
circle <- mapview(deer_locations_ALL_SITES) %>% editMap()
circle$finished  # A sf object
mapview(circle$finished)

# Edit features
selectMap(
  leaflet() %>% addTiles() %>% addCircles(data = st_transform(deer_locations, 4326))
)  # Not working

selectFeatures(deer_locations_ALL_SITES)  # Lets you click on individual points

# Intsersect deer locations with our new study region
test_area <- circle$finished %>% st_transform(3400)   # reproject data and save this as a specific object

test <- st_intersection(deer_locations_ALL_SITES, test_area)   # clip/intersect features to within the study area
mapview(test)  # it works!


                                        
### SECTION 4: How do I interact with spatial data? ---------

## Basics -- 
# Query sf, rename, select, filter, create new objects
# sf to table, do something, back to sf
# Merging sf - by deleting excess columns, and by converting to table with full_join
## Note - this only works for points, where there are only two coordinates to extract for every feature
# Other things?


### Example 1 - Reclassify features based on Zoning ID ----------

### Vic Zoning - Identify Zones ----
Vic_Zoning <- st_read("Input Data/Vic_Zoning.shp")
# Figure out data values to classify Residential zoning 

# Convert sf to table and remove geometry
vic_table <- Vic_Zoning %>% as_tibble() %>% dplyr::select(-geometry)
names(vic_table)
barplot(table(vic_table$Title))

# Get a table of the sorted frequencies for Zoning Titles
vic_table_count <- plyr::count(vic_table, "Title")
vic_table_count <- vic_table_count[order(-vic_table_count$freq), ]

# Attach zoning codes to the titles table
names(vic_table)
vic_zone_codes <- dplyr::select(vic_table, "Zoning", "Title")

# Select only the non-duplicated zone codes
zones_no_duplicates <- vic_zone_codes[duplicated(vic_zone_codes$Zoning) == FALSE, ]

## Find a join that attaches zone code to title but keeps the number of rows of the vic_table_count --
vic_zones_and_frequency <- left_join(vic_table_count, zones_no_duplicates, by = "Title")
vic_zones_and_frequency %>% print(n = 90)
## Anything with 10 or less, call "Urban". Maybe 5 or less. 


### Vic Zoning - Reclassify  ----
# Add column to reclassify
Vic_Zoning$Zone <- Vic_Zoning$Title

# Add NA as a level so it doesn't get selected with the first query and reclassification
levels(Vic_Zoning$Zone)
Vic_Zoning$Zone <- addNA(Vic_Zoning$Zone)
length(Vic_Zoning$Zone[Vic_Zoning$Zone == "Single Family Dwelling District.pdf"])  # Now matches frequency table

levels(Vic_Zoning$Zone)[596] <- "Urban"
# Everything NA gets called Urban

### Reclassification Scheme ---
# Add "Residential" as a level to the factor to avoid generating NAs - This is important!
levels(Vic_Zoning$Zone) <- c(levels(Vic_Zoning$Zone), "Residential")
levels(Vic_Zoning$Zone)[597]  # The last level is "Residential"

# Rename some zones as residential
Vic_Zoning$Zone[Vic_Zoning$Zone == "Single Family Dwelling District.pdf"] = "Residential"
Vic_Zoning$Zone[Vic_Zoning$Zone == "Two Family Dwelling District.pdf"] <- "Residential"
Vic_Zoning$Zone[Vic_Zoning$Zone == "Multiple Dwelling District.pdf"] <- "Residential"
Vic_Zoning$Zone[Vic_Zoning$Zone == "Central Business District-2"] <- "Urban"
Vic_Zoning$Zone[Vic_Zoning$Zone == "Central Business District-1"] <- "Urban"
Vic_Zoning$Zone[Vic_Zoning$Zone == "Heavy Industrial District.pdf"] <- "Urban"

# Now, relabel the rest as "Urban" ---
Vic_Zoning$Zone[!Vic_Zoning$Zone == "Residential"] <- "Urban"

# Check new levels 
unique(Vic_Zoning$Zone)  # Whoop whoop!

# Map outputs
mapview(Vic_Zoning, zcol = "Zone")   # This is awesome!

# # Save backup ---
# Backup_Vic_Zoning_Reclassified <- Vic_Zoning
# st_write(Vic_Zoning, dsn = "Input Data/Vic_Zoning_Reclassified.shp")







##### SECTION 5: Spatial Analysis - Deer Data -------

# This section demonstrates a typical analysis process using species observations and land-cover variables. 

###  Spatial analysis steps:
# 1) Prepare landscape datasets
# 2) Extract landscape variables to your camera/species observations and tidy your data for analysis
# 3) Investigate your data, run your models, check your models   -- not the focus of mapping analysis here --
# 4) Build layers predictor variables and run your model to estimate responses
# 5) Map your results


### Section 1 -- Load Data --------

### Part 1 - Load Deer Data and Merge -------
# Load csv of deer locations and random locations  --
used_locations <- st_read("Input Data/Real and Random Locations/real_deer_with_UTM.csv", options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude"), 
                          crs = 4326)
# Always load Lat/Long data in WGS 84, "crs=4326". Transform to your projection afterwards.
used_locations <- st_transform(deer_locations, crs = 3400)

random_locations <- st_read("Input Data/Real and Random Locations/random_deer_with_UTM.csv", options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude"), 
                            crs = 4326)
random_locations <- st_transform(random_locations, crs = 3400)
str(random_locations)

## Investigate sf attributes
# Note - Easting and Northing and the values in the "geometry" column are the same for these datasets. 
# If Easting and Northing were generated in a different UTM zone or projection, and st_transform was used, then
# the geometry column coordinates would be different.

# Test if features are valid.
count_deer_notvalid <- used_locations %>% st_is_valid %>% sum(na.rm = TRUE) %>% sum(sum() - nrow(used_locations)) %>% abs()
count_random_notvalid <- used_locations %>% st_is_valid %>% sum(na.rm = TRUE) %>% sum(sum() - nrow(random_locations)) %>% abs()

# View real and random locations
mapview(deer_locations, col.regions = "dodgerblue3", viewer.suppress = FALSE) +
  mapview(random_locations, col.regions = "firebrick2") 
# Adding viewer.suppress = TRUE in the first mapview() prints the map in your internet browser!
# Sources: Mapview - https://r-spatial.github.io/mapview/reference/mapView.html
#          Advanced Controlls - https://r-spatial.github.io/mapview/articles/articles/mapview_02-advanced.html
#          Set Global Options - https://r-spatial.github.io/mapview/articles/articles/mapview_03-options.html


## Add "Use" column to deer data and merge layers into a single sf object
used_locations$Use = "1"
random_locations$Use = "0"

# Use rbind **because the column names are the same for both datasets**. If they aren't, you have to delete and rename non-shared columns.
deer_locations <- rbind(used_locations, random_locations)

# Test if they're joined 
nrow(deer_locations) # 14632
filter(deer_locations, Use == "1") %>% nrow() # 2470
filter(deer_locations, Use == "0") %>% nrow() # 12162
12162 + 2470 # 14632 -- check!

# Map the joined dataset
mapview(deer_locations,  zcol = "Use", viewer.suppress = TRUE)

## Add an MCP and 10km buffer around all deer points to set the study region --
study_mcp <- st_convex_hull(st_union(deer_locations))
study_area <- st_buffer(study_mcp, 10000, nQuadSegs = 30, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1)  # Args nQuadSegs on are defaults
mapview(study_mcp) + mapview(study_area) # Check!
## Note - this comes up in later analysis - study_area is class sfc_polygon -- need class "sf" often.
study_area <- st_sf(study_area)
summary(study_area)



### Part 2 - Load linear and block features  ---- 

## Load one full dataset to demonstrate clipping
cutblocks_all <- st_read("Input Data/cutblocks_OSM.shp", crs = 3400)  %>% st_make_valid()

# Count invalid features
count_cutblocks_notvalid <- cutblocks_all %>% st_is_valid %>% sum(na.rm = TRUE) %>% sum(sum() - nrow(cutblocks_all)) %>% abs()

# Clip features to study area - st_intersection performs a clip in R 
# Intersect keeps the attribute info of the clip feature you use - i.e keeps the feature area of the study_area polygon, and extra stuff. Delete this extra stuff if you want.
t_cutblock_clip <- st_intersection(cutblocks, study_area)

# Plot clipped features
x11(10,10)
tmap_mode("plot")
tm_shape(t_cutblock_clip) + tm_polygons()


## Load the remaining features I have already clipped in ArcGIS to a smaller study area
seismic_lines <- st_read("Input Data/seismic_line_mcp_clip.shp", crs = 3400)  %>% st_make_valid()
seismic_grids <- st_read("Input Data/seismic_grid_mcp_clip.shp", crs = 3400)  %>% st_make_valid()
roads <- st_read("Input Data/roads_mcp_clip.shp", crs = 3400)  %>% st_make_valid()
waterbodies <- st_read("Input Data/waterbodies_mcp_clip.shp", crs = 3400)  %>% st_make_valid()
cutblocks <- st_read("Input Data/cutblocks_mcp_clip.shp", crs = 3400)  %>% st_make_valid()
study_area <- st_read("Input Data/study_area_southeast.shp", crs = 3400) %>% st_make_valid()

# Mapview:
mapview()


### Part 3 - Load a raster land-classification dataset  -----
#  Load "raster" format raster
nrcan_file <- "Input Data/canada_landcover_25class_clip_resize.tif"
nrcan_raster <- raster(nrcan_file)

#  Load "stars" format raster for turning this raster into a shapefile  
nrcan_stars <- read_stars("Input Data/canada_landcover_25class_clip_resize.tif")
## Note: tmap, ggplot, and mapview don't plot "stars" objects. "Stars" objects let some cool analyses be done, but they can only be mapped with plot()
# Stars: https://cran.r-project.org/web/packages/stars/index.html - look for the vignettes.

# Plot "raster" raster and "stars" raster -- they should look identical
tm_shape(nrcan_raster) + 
  tm_raster() 

tm_shape(nrcan_stars) + 
  tm_raster() + 
 



### Section 2 -- Modify Raster Datasets for Analysis -----

## Part 1 - Reclassify Raster Data  --------
# Reclassify values based on grouping scheme for the land-classes - do this before conversion to features. Use raster::reclassify()

# Get unique values in the nrcan_raster. We will reclass these to new values
values(nrcan_raster) # Recreates the raster cell-by-cell in a list format. LONG.
freq_nrcan_raster <- freq(nrcan_raster) # Generates a frequency table. Works with *classes* (1-25) but would take forever if you
#  had a raster with numerical values. For a raster this big, this still takes a long time.
# Test this: values(r) <- runif(ncell(r)) 

# Go to the metadata to find what each value represents. I have a table showing me what I want to reclass 
# the *word* class names into, but I don't know what class names correspond with what values 
# source: https://webservices.maps.canada.ca/arcgis/rest/services/NRCan/canada_landcover_2000_2011_25class_en/ImageServer
# The metadata for this raster classification scheme is here: https://webservices.maps.canada.ca/arcgis/rest/services/NRCan/canada_landcover_2000_2011_25class_en/ImageServer/rasterAttributeTable
# Or, load this raster into QGIS/ArcGIS so you can see what numerical values correspond with its class names.

### Reclassification Scheme:
# 1, TO: 16 - Temperate or subpolar needle-leaved coniferous forest, high density",    
# 2, TO: 16 - Temperate or subpolar needle-leaved coniferous forest, medium density",  
# 3, TO: 15 - Temperate or subpolar needle-leaved coniferous forest, low density",     
# 4, TO: 12 - Cold deciduous broad-leaved forest, high density",                       
# 5, TO: 12 - Cold deciduous broad-leaved forest, medium density",                     
# 6, TO: 11 -"Cold deciduous broad-leaved forest, low to medium density, young regenerating",    
# 7, TO: 14 - Mixed needle-leaved cold deciduous broad-leaved forest, high density",   
# 8, TO: 14 - Mixed needle-leaved cold deciduous forest, medium density",
# 9, TO: 13 - Mixed needle-leaved cold deciduous forest, low to medium density, young regeneration",   
# 10,  TO: 9 - "Shrubland",                                                              
# 11,  TO: 4 - "Grassland",
# 12,  TO: 8 - "Herb-shrub, low vegetation cover",                                       
# 13,  TO: 16 - "Taiga, sparse conifer",                                                   
# 14,  TO: 3 - "Shrub-herb-lichen cover",
# 15,  TO: 3 - "Herb-shrub-lichen cover",
# 16,  TO: 3 - "Lichens-Shrub-Herb Cover",
# 17,  TO: 10 - "Wetland, shrub-herb",
# 18,  TO: 10 - "Wetland, treed",
# 19,  TO: 7 - "Cultivated and managed areas, high biomass",
# 20,  TO: 6 - "Cultivated and managed areas, medium biomass",
# 21,  TO: 5 - "Cultivated and managed areas, low biomass",
# 22,  TO: 1 - "Barren land",
# 23,  TO: 2 - "Urban and built-up",
# 24,  TO: 17 - "Water bodies natural and artificial",
# 25,  TO: 1 - "Snow and ice",

# Build a reclassification list and turn it into a matrix 
m <- c(1, 16, 2, 16, 3, 15, 4, 12, 5, 12, 6, 11, 7, 14, 8, 14, 9, 13, 10, 9, 11, 4, 12, 8, 13, 16,
       14, 3, 15, 3, 16, 3, 17, 10, 18, 10, 19, 7, 20, 6, 21, 5, 22, 1, 23, 2, 24, 17, 25, 1)  
# format: "is", "becomes" -- first number is the old value, second is the new value
# If you're reclassifying a *range*, the format is:  "from", "to", "becomes" -- i.e.  c(0, 5, 1, 6, 10, 2, 11, 15, 3 ...) 
rclmat <- matrix(m, ncol=2, byrow=TRUE)  # double check the reclassification matrix against table above
# for reclassifying a range: rclmat <- matrix(m, ncol= 3, byrow=TRUE) 
nrcan_rc <- reclassify(nrcan_raster, rclmat, filename = "nrcan_rc_Jan17.tif")

# Plot original raster and reclassified raster
x11(8, 8)
tm_shape(nrcan_raster) + 
  tm_raster()

x11(8, 8)
tm_shape(nrcan_rc) + 
  tm_raster()


### Part 2 - Crop the nrcan raster -------- 
## Crop the nrcan_rc raster to a smaller study area to speed up the next analysis --
study_area  # double check by plotting again if you want

# 1) Convert sf to sp
study_area_sp <- study_area %>% as("Spatial")

# 2) raster::crop() -- to the rectangular extent of the "sp" study_area object
t_nrcan_crop <- crop(nrcan_rc, study_area_sp)
extent(t_nrcan_crop)   # Extents are the same 
extent(study_area_sp)  # The same!

plot(t_nrcan_crop)  
plot(study_area_sp, add = TRUE)  # adds this plot to the preceeding plot 

# 3) raster::mask() -- mask as NA all values insie the rectangle but outside the oblong study_area bounds
t_nrcan_mask <- mask(t_nrcan_crop, study_area_sp)
plot(t_nrcan_mask)  
plot(study_area_sp, add = TRUE)  

## Let's keep both - I like the rectangular crop better.
nrcan_crop <- t_nrcan_crop
# nrcan_mask <- t_nrcan_mask


### Part 3 - Convert the cropped nrcan raster to a "stars" raster and convert to an sf object ---------
## Convert the cropped raster into a "stars" object for raster to feature conversion -- 
nrcan_stars <- st_as_stars(nrcan_crop)
plot(nrcan_stars)
plot(study_area_sp, add = TRUE)
# Note how the presentation of the "stars" object is slightly different. If there were multiple layers - i.e. image bands, or different 
# raster layers in a raster brick or raster stack, plot(stars_object) would plot all the layers!  

## Convert nrcan_lc_stars to a vector/feature layer --
nrcan_sf <- st_as_sf(nrcan_stars, as_points = FALSE, merge = TRUE)
# source: https://r-spatial.github.io/stars/articles/stars5.html
# Note - *this* step is why we cropped the larger raster. Plotting an sf object this complex for most of Northern Alberta was *slow*.  
plot(nrcan_sf)
class(nrcan_sf)



### Section 3 -- Prep  data for analysis  -----

### Part 1 - Select age-classes of cutblocks for analysis ------ 
x11(10,10)
options(sf_max.plot = 1)
tm_shape(cutblocks) + tm_polygons() +
  tm_shape(study_area) + tm_polygons(alpha = 0, border.col = "dodgerblue4")

# We'd like to test if deer select for old, medium, or young age cutblocks - probably some age preference for regenerating vegetation!
hist(cutblocks$YEAR, breaks = 50)
## Because simple features are tables with geometry, we can index any column like a normal dataframe. 

# Create new sf objects for cutblock age classes
cutblocks_post11 <- cutblocks %>% filter(YEAR >= "2011" & YEAR <= 2016)  # Most deer locations were observed before 2016, so that's our stop date.   
cutblocks_97to11 <- cutblocks %>% filter(YEAR < "2011" & YEAR >= 1997)
cutblocks_pre97 <- cutblocks %>% filter(YEAR < 1997)

# Plot them - 
x11()
tm_shape(cutblocks_pre97) + tm_polygons(col = "forestgreen", border.alpha = 0) +
  tm_shape(cutblocks_97to11) + tm_polygons(col = "dodgerblue2", border.alpha = 0) +
  tm_shape(cutblocks_post11) + tm_polygons(col = "hotpink", border.alpha = 0)
  

### Part 2 - Buffer Deer Locations --------
filter(deer_locations, Use == "1")
filter(deer_locations, Use == "0")

deer_loc_500m <- st_buffer(deer_locations, 500, nQuadSegs = 30, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1) 
# Arguments nQuadSegs onwards are defaults. Units of buffer distance are in the units of the projection - meters for UTM zones.

mapview(deer_loc_500m, zcol = "Use")
# This looks messy - it's easy to filter and buffer use and available locations separately so you can have more access to them when mapping

# Buffer Use and Available locations separately 
deer_use_buffer <- st_buffer(filter(deer_locations, Use == "1"), 500, nQuadSegs = 30, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1) 
deer_avail_buffer <- st_buffer(filter(deer_locations, Use == "0"), 500, nQuadSegs = 30, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1) 

# Map used and available location buffers separately 
mapview(deer_use_buffer, col.regions = "firebrick4") + mapview(deer_avail_buffer, col.regions = "dodgerblue4")
# The first layer you call in mapview gets plotted above, then others get plotted below 

# OR use a filter on the combined layer to plot attributes separately
mapview(filter(deer_loc_500m, Use == "1"), col.regions = "firebrick4") + mapview(filter(deer_loc_500m, Use == "0"), col.regions = "dodgerblue4")




### Section 4 -- Extract data from land-cover maps to deer locations, in R and ArcGIS ----------

### Part 1 - "Tabulate Intersection" on NRCAN land-cover and deer points -----
# Note - you could use this code for counting the amount of habitat around camera locations.
# Examples from: https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r. Second example, using sf.


# Field polygons - Deer buffers  ---
# Backup data:
deer_loc_500m_ALL_SITES <- deer_loc_500m  # Save as a backup so we can modify (clip) the existing file
deer_locations_ALL_SITES <- deer_locations

# Clip data: Intersect deer buffers and deer locations with the study area and only keep those.
deer_loc_500m <- st_intersection(deer_loc_500m, study_area)
deer_locations <- st_intersection(deer_locations, study_area)
## Note -- columns OBJECTID.1, BUFF_DIST, ORIG_FID, Shape_Length, Shape_Area are all from the intersected study_area polygon. You can delete these.
# Intersect keeps the attribute information of both features, unlike clip which just keeps the attributes of the first feature.

### Add new column: Point_ID for unique values for all deer used and available locations.
nrow(deer_loc_500m)  # 14632 unique features
deer_loc_500m$Point_ID = seq(1:14632)  # OBJECTID repeats itself for used and available locations. Add "Point_ID" for 14632 unique values.
range(deer_loc_500m$Point_ID)

nrow(deer_locations)
deer_locations$Point_ID = seq(1:14632)

# Class polygons - we want areas of land-cover features inside the deer buffers
nrcan_sf
# Are these valid? I got an error the first time I ran intersect
nrcan_sf_notvalid <- nrcan_sf %>% st_is_valid %>% sum(na.rm = TRUE) %>% sum(sum() - nrow(nrcan_sf)) %>% abs()
# Make valid
nrcan_sf <- st_make_valid(nrcan_sf)  # Check above -- now 0 invalid features

## Note: tool below took too long with the subset of 12732 deer locations. 250 locations took over 15 minutes and I cancled it.
deer_buffers_subset <- deer_loc_500m[1:35, ]
mapview(deer_buffers_subset)

## Run "Tabulate Intersection" process in R --
# intersect - note that sf is intelligent with attribute data!
pi <- st_intersection(nrcan_sf, deer_buffers_subset)   # Data = nrcan_sf, field = deer_buffers_subset  *EXTREMELY SLOW STEP even on a desktop

mapview(pi, zcol = "nrcan_rc_Jan17")  # It works!

# add in areas in m2
attArea <- pi %>% 
  mutate(area = st_area(.) %>% as.numeric())

# for each field (buffer), get area per NRCAN land class
deer_nrcan_area <- attArea %>% 
  as_tibble() %>% 
  group_by(Point_ID, nrcan_rc_Jan17) %>% 
  summarize(area = sum(area))
# Looks good!

# Rename columns, row values, and pivot the table for one row per deer instead of multiple rows per deer
deer_nrcan_area <- rename(deer_nrcan_area, land_class = nrcan_rc_Jan17)

# Rename land-class numbers with words
deer_nrcan_area[deer_nrcan_area$land_class == 4, 2] <- "Grassland"
deer_nrcan_area[deer_nrcan_area$land_class == 5, 2] <- "Cultivated Areas - Low Biomass"
deer_nrcan_area[deer_nrcan_area$land_class == 6, 2] <- "Cultivated Areas - Medium Biomass"
# etc.. see full naming scheme below

# Pivot table
deer_nrcan_area_table <- pivot_wider(deer_nrcan_area, id_cols = Point_ID, names_from = land_class, values_from = area)
# Switch NAs to Zero
deer_nrcan_area_table <- deer_nrcan_area_table %>% mutate_if(is.numeric , replace_na, replace = 0)

# This table is now in a format we can use for modelling in R.
### This code is performed again on the more complete NRCAN data below


### Part 2 - Get distance from deer_locations to the nearest landcover features --------
# source: https://stackoverflow.com/questions/49200458/find-nearest-features-using-sf-in-r

#   closest <- list()
#   for(i in seq_len(nrow(deer_locations))){
#     closest[[i]] <- seismic_grids[which.min(
#       st_distance(seismic_grids, deer_locations[i,])),]
#   }
### This took over 30 minutes on the desktop computer. Do not run on your laptop!
length(closest)
nrow(deer_locations)  # It looks like closest ran on the deer locations, not the seismic lines. Good. Now, what to do with it?
head(closest)
## This didn't work. Too slow. This is easy to do in a GIS.


### Export data from R to ArcGIS -----
# nrcan layers
st_write(deer_loc_500m, dsn = "GIS Bridge/deer_buffers500_studyarea.shp")
st_write(deer_locations, dsn = "GIS Bridge/deer_locations_studyarea.shp")
st_write(seismic_grids, dsn = "GIS Bridge/seismic_grids.shp")
st_write(seismic_lines, dsn = "GIS Bridge/seismic_lines.shp")
st_write(roads, dsn = "GIS Bridge/roads.shp")
st_write(waterbodies, dsn = "GIS Bridge/waterbodies.shp")
st_write(cutblocks, dsn = "GIS Bridge/cutblocks.shp")
st_write(cutblocks_pre97, dsn = "GIS Bridge/cutblocks_pre97.shp")
st_write(cutblocks_97to11, dsn = "GIS Bridge/cutblocks_97to11.shp")
st_write(cutblocks_post11, dsn = "GIS Bridge/cutblocks_post11.shp")
st_write(nrcan_sf, dsn = "GIS Bridge/nrcan_vector.shp")
st_write(study_area, dsn= "GIS Bridge/study_area.shp")
# This is a quick step. R exports these layers fast, but maps them slowly.
writeRaster(nrcan_rc, "GIS Bridge/nrcan_reclassified.tif")
writeRaster(nrcan_crop, "GIS Bridge/nrcan_crop_reclassified.tif")

### GIS Notes --
# ArcGIS is set up to delete your tool processing history automatically after 3 weeks. You should change this if you have a project longer than 3 weeks!
# Go to "Geoprocessing Menu > Geoprocessing Options > Results Management section > keep results younger than: "Never Delete"
# Now Arc will log everything you did for the project, so you can see what your smarter self did before going AWOL for a Christmas or the summer. 
# You have to set this individually for every new project you start. 
# Image: "GIS Analysis/Arc_results_history_save.PNG"


### Part 3 - Distance Analysis - "Generate Near Table"   ----

# Tool: "Generate near table"
# Tool paramaters image: "GIS Analysis/generate_near_table.PNG"
# Python snippet: 
arcpy.GenerateNearTable_analysis(in_features="From R\deer_locations_studyarea", 
                      near_features="'From R\seismic_lines'", out_table="E:/Wylie 2019/OSM Deer.gdb/t_deer_near_seism_lines", search_radius="", 
                      location="NO_LOCATION", angle="NO_ANGLE", closest="CLOSEST", closest_count="0", method="PLANAR")
# "Near table" repeated for each of seismic_lines, seismic_grids, roads, and waterbodies. Tables joined with deer data file.
# Out: "deer_locations_withdistances.shp"
# How to join data: "GIS Analysis/join_table_to_spatial_data.PNG"

# Import data back to R:
deer_distances <- st_read("Input Data/deer_locations_withdistances.shp", crs = 3400)
# Rename columns:
deer_distances <- rename(deer_distances, dist_seis_lines = NEAR_DIST)
deer_distances <- rename(deer_distances, dist_roads = NEAR_DIS_1)
deer_distances <- rename(deer_distances, dist_waterbodies = NEAR_DIS_2)


## Note: when you're running the same tool on multiple datasets (deer + seism_lines, seism_grids, roads, waterbodies) you can export the python snippet from 
# a tool you just ran, paste the code into a Python notebook in R, edit the inputs or parameters, and repeat it over all your remaining datasets
# E.g: 
arcpy.GenerateNearTable_analysis(in_features="From R\deer_locations_studyarea", 
                                near_features="'From R\seismic_grids'", out_table="E:/Wylie 2019/OSM Deer.gdb/t_deer_near_seism_grids", search_radius="", 
                                 location="NO_LOCATION", angle="NO_ANGLE", closest="CLOSEST", closest_count="0", method="PLANAR")
arcpy.GenerateNearTable_analysis(in_features="From R\deer_locations_studyarea", 
                                 near_features="'From R\waterbodies'", out_table="E:/Wylie 2019/OSM Deer.gdb/t_deer_near_waterbodies", search_radius="", 
                                 location="NO_LOCATION", angle="NO_ANGLE", closest="CLOSEST", closest_count="0", method="PLANAR")
# Paste this into the Python window in Arc and run it all at once!
# How to access Results and python snippets: "GIS Analysis/Access_results_copy_python_snippet.PNG"


### Part 4 - Area Analysis - "Tabulate Intersection" ----
## Tabulate Intersection on NRCAN layer
# Tool - "tabulate intersection"
# Image - "GIS Analysis/tabulate_intersect_deer_nrcan.PNG"
# Python snippet - 
arcpy.TabulateIntersection_analysis(in_zone_features="From R\deer_buffers500_studyarea", zone_fields="Point_ID", in_class_features="From R\nrcan_vector", 
                                    out_table="E:/Wylie 2019/OSM Deer.gdb/t_deer_nrcan_tabulate_intersect", 
                                    class_fields="nr__J17", sum_fields="", xy_tolerance="-1 Unknown", out_units="SQUARE_METERS")
# Out: "Input Data/deer_nrcan_tab_intersect.csv"

## Tabulate intersection on other layers:
# Cutblocks pre_97, 97to11, post11
# Same steps as nrcan layer
deer_cutblocks_pre97_tab_int <- read_csv("Input Data/deer_cutblocks_pre97_tab_intersect.csv")



### Part 5 - Organize GIS Data into something we can use for analysis -------

### NRCAN Data ---
# 1) Rename numerical classes with words 
# 2) Pivot table wider

# Import data
deer_nrcan_intersect <- read_csv("Input Data/deer_nrcan_tab_intersect.csv")
deer_nrcan_intersect <- rename(deer_nrcan_intersect, Land_Class = nr__J17)
  
## Rename numerical land-classes to words --
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 1, 3] <- "Barren/Snow-Ice"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 2, 3] <- "Urban"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 3, 3] <- "Lichen, Herb, Shrub"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 4, 3] <- "Grassland"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 5, 3] <- "Cultivated Areas - Low Biomass"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 6, 3] <- "Cultivated Areas - Medium Biomass"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 7, 3] <- "Cultivated Areas - High Biomass"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 8, 3] <- "Herb-Shrub, Low Vegetetaion Cover"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 9, 3] <- "Shrubland"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 10, 3] <- "Wetland - Herb-Shrub, Treed"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 11, 3] <- "Broadleaf - low to medium density, young regeneration"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 12, 3] <- "Broadleaf - medium to high density"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 13, 3] <-  "Mixedwood - low to medium density, young regeneration"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 14, 3] <-  "Mixedwood - medium to high density"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 15, 3] <- "Conifer - low density"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 16, 3] <-  "Conifer - medium to high density"
deer_nrcan_intersect[deer_nrcan_intersect$Land_Class == 17, 3] <- "Water"

unique(deer_nrcan_intersect$Land_Class)  # new names!

## Pivot the tables wider so each land-class has its own column and area value --
# The initial table is garbage to work with in R - each deer location "Point_ID" has a new row for every feature it intersects with -- nr_17 4, 5, 6, 9 etc..
#  <Row> <Point_ID> <Land_Class>   <AREA>       <PERCENTAGE>
#   1       1         4             11408.       1.45
#   2       1         5             629692.     80.2 
#   3       1         6             52342.       6.67
#   4       1         9             91597.      11.7 

# We want a table where each Point_ID only occurs once, and every land-cover variable is its own new colum
#  Point_ID   Broadlf_regen   Conifer    Water      etc..                                      
#       1           80          2           3               
#       2           40          50          10               
#       3           10          45          30                  

# Pivot wider
pivot_deer_NRCAN_table <- pivot_wider(deer_nrcan_intersect, id_cols = Point_ID, names_from = Land_Class, values_from = AREA)
# Switch NAs to Zero
pivot_deer_NRCAN_table <- pivot_deer_NRCAN_table %>% mutate_if(is.numeric , replace_na, replace = 0)


## Merge Distances and Area values into one table based on Point_ID ---
# Point_ID is not present for deer_distances because I forgot to add to before exporting the data to Arc. If you had Point_ID, you 
#  could merge distances and area and have a completed dataset, ready for analysis. 

## End of data preparation section of the tutorial.

# From here: 
# Have a good dataframe
# Build and test models
# Come up with a preferred model to plot results


### Section 5 -- Build dataframes, models and predictor dataset maps ----------

### Part 1 - Import Data and Build a Model  ------
# Import existing dataset
deer_data <- read_csv("Input Data/deer_data_Dec02_main_backup.csv")
which(is.na(deer_data))

# Fix roads distance NAs
deer_data$roads_dist
which(is.na(deer_data$roads_dist))
deer_data$roads_dist[is.na(deer_data$roads_dist)] <- 15000

# Model:
model_1 <- glm(formula = Use ~ cultivated_high + broadleaf_regen +
                 conifer_low + lakes_dist + roads_dist + seismic_grid_dist + 
                 seismic_linetrail_dist - 1, family = "binomial", data = deer_data, na.action = "na.fail")

## Steps:
# Create "Distance to" rasters for distance predictor variables
# Create focal statistics rasters for area predictor variables

# Area variables: 
c(cultivated_med, cultivated_med, broadleaf_regen, broadleaf_medhigh, conifer_low)

# Distance variables:
c(lakes_dist, roads_dist, seismic_grid_dist, seismic_linetrail_dist)


### Part 2 - Extract rasters of specific land-classes from the NRCAN layer ------
#  Perform Focal Statistics on them, and make them a Raster Brick/Stack

### Reclassify NRCAN again to extract specific features --
mapview(nrcan_crop)

# Classification scheme:
#  17 - "Water bodies natural and artificial",
#  16 - "Taiga, sparse conifer",                                                   
#  16 - Temperate or subpolar needle-leaved coniferous forest, high density",    
#  16 - Temperate or subpolar needle-leaved coniferous forest, medium density",  
#  15 - Temperate or subpolar needle-leaved coniferous forest, low density",
#  14 - Mixed needle-leaved cold deciduous broad-leaved forest, high density",   
#  14 - Mixed needle-leaved cold deciduous forest, medium density",
#  13 - Mixed needle-leaved cold deciduous forest, low to medium density, young regeneration",   
#  12 - Cold deciduous broad-leaved forest, high density",                       
#  12 - Cold deciduous broad-leaved forest, medium density",                     
#  11 - "Cold deciduous broad-leaved forest, low to medium density, young regenerating",  
#  10 - "Wetland, shrub-herb",
#  10 - "Wetland, treed",
#  9 - "Shrubland",                                                              
#  8 - "Herb-shrub, low vegetation cover",    
#  7 - "Cultivated and managed areas, high biomass",
#  6 - "Cultivated and managed areas, medium biomass",
#  5 - "Cultivated and managed areas, low biomass",
#  4 - "Grassland",
#  3 - "Shrub-herb-lichen cover",
#  3 - "Herb-shrub-lichen cover",
#  3 - "Lichens-Shrub-Herb Cover",
#  2 - "Urban and built-up",
#  1 - "Barren land",
#  1 - "Snow and ice",

# Select broadleaf regenerating
m <- c(1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0, 8, 0, 9, 0, 10, 0, 11, 1, 12, 0, 13, 0, 14, 0, 15, 0, 16, 0, 17, 0)  
rclmat <- matrix(m, ncol=2, byrow=TRUE)  # 11 to 1 (broadleaf_regen), all else to 0
nrcan_broadleaf_regen <- reclassify(nrcan_crop, rclmat, filename = "nrcan_crop_broadleaf_regen.tif")
freq(nrcan_broadleaf)  # Only values of 0 or 1 -- good.

# Conifer
m <- c(1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0, 8, 0, 9, 0, 10, 0, 11, 0, 12, 0, 13, 0, 14, 0, 15, 1, 16, 0, 17, 0)  
rclmat <- matrix(m, ncol=2, byrow=TRUE)  
nrcan_conifer <- reclassify(nrcan_crop, rclmat, filename = "nrcan_crop_conifer_low.tif")
freq(nrcan_conifer)  # Only values of 0 or 1 -- good.

# Cultivated high
m <- c(1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 1, 8, 0, 9, 0, 10, 0, 11, 0, 12, 0, 13, 0, 14, 0, 15, 0, 16, 0, 17, 0)  
rclmat <- matrix(m, ncol=2, byrow=TRUE)  
nrcan_cultivated_high <- reclassify(nrcan_crop, rclmat, filename = "nrcan_crop_cultivated_high.tif")


### Part 3 - Run focal stats on land-cover layers with a 500m circular window, calculating the mean -----
# raster:: focalWeight(), then raster::focal()

# Calculate a focal weight matrix for use in the focal() function
fwmatrix <- focalWeight(nrcan_broadleaf_regen, 500, type = "circle")  # units of distance are those of the crs

# fwmatrix has values of 0.076923 for all values in the circle. Assign these to "1"
fwmatrix[fwmatrix > 0] <- 1

# Focal statistics
broadleaf_regen_focal500 <- focal(nrcan_broadleaf_regen, fwmatrix, fun=mean, filename = "nrcan_broadleaf_focal500m.tif")
conifer_focal500 <- focal(nrcan_conifer, fwmatrix, fun=mean, filename = "nrcan_conifer_focal500m.tif")
cultivated_focal500 <- focal(nrcan_cultivated_high, fwmatrix, fun=mean, filename = "nrcan_cultivated_high_focal500m.tif")

mapview(cultivated_focal500) # This looks good!

# Note -- will add these to a raster stack later for input into model predictions


### Part 4 - Create distance rasters -----
# Test - convert a subset feature layer to a raster, then run raster::distance()

# Create a subset window 
subset <- mapview(roads) %>% mapedit::editMap()
subset <- subset$finished %>% st_transform(crs = 3400)

# Intsersect subset with roads
roads_subset <- st_intersection(roads, subset)
mapview(roads_subset) + mapview(subset)

# Convert sf to sp
roads_sp <- as(roads_subset, "Spatial") 

# Create a constant raster with the right extent and projection to add the "roads" values to
# Empty raster with total size of study area, cell size of 50m, and crs=3400. 
extent <- extent(subset)
constant_raster <- raster(ext = extent, crs = "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m", 
                          res = 25, vals = 1)
# Do the constant raster and the nrcan_broadleaf have the same extent? Do they overlap?  --- Do this *if* making a raster for your whole study region
# mapview(constant_raster) + mapview(nrcan_broadleaf_regen)  # Yes, same extent and projection 

# Create a raster from the roads feature layer
roads_raster <- rasterize(roads_sp, constant_raster, field = 1)
mapview(roads_raster) + mapview(roads_subset)   # 25 meter resolution not fine enough to capture all roads, but good enough for now.

# Slowish step - do not run
# roads_dist_raster <- distance(roads_raster, filename = "roads_distance.tif")
mapview(roads_dist_raster)  # Pretty  : )

### Distance rasters in ArcGIS ---
# Tool - Euclidean distance
# Tool image - "GIS Analysis/euclidean_distance_tool.PNG"
# Python snippet - 
arcpy.gp.EucDistance_sa("From R\roads", "E:/Wylie 2019/OSM Deer.gdb/t_roads_distance", "", "50", "", "PLANAR")
# Rasters produced with a rectangle equal to the nrcan_cropped extent.
# Rasters clipped to the exact study area boundaries for new files
# Out: "Input Data/t_roads_distance.tif" -- etc for all rectangular files
# Out: "Input Data/t_roads_distance_clipped.tif" -- etc for all clipped files



##### Section 6 -- Load Datasets and predict habitat selection   -----------

## Distance Rasters --
r_seis_line_dist <- raster("Input Data/t_seis_line_crop_distance.tif")
r_seis_grid_dist <- raster("Input Data/t_seis_grid_crop_distance.tif")
r_roads_dist <- raster("Input Data/t_roads_crop_distance.tif")
r_lakes_dist <- raster("Input Data/t_waterbodies_crop_distance.tif")

## Area Rasters --
broadleaf_regen_focal500 
conifer_focal500 
cultivated_focal500


### Part 1 - Load all rasters into a single raster stack ------

# Raster stacks are compilations of multiple raster layers with the same spatial extent and resolution.
# A raster stack is required as an input into the predict() function to map model predictions.
# The most annoying thing about raster stacks is they have to have *identical* extents. My rasters differ a very very very little bit.

stack(r_seis_grid_dist, r_seis_line_dist)  # error - different extent

# Map all raster extents to make sure they mostly overlap, before resampling so they exactly overlap
a <- extent(r_seis_grid_dist) %>% as("SpatialPolygons")
b <- extent(r_seis_line_dist) %>% as("SpatialPolygons")
c <- extent(r_roads_dist) %>% as("SpatialPolygons")
d <- extent(r_lakes_dist) %>% as("SpatialPolygons")
e <- extent(broadleaf_regen_focal500) %>% as("SpatialPolygons") %>% st_as_sf()
f <- extent(conifer_focal500) %>% as("SpatialPolygons")
g <- extent(cultivated_focal500) %>% as("SpatialPolygons")

# Or just put these directly into mapview --
mapview(extent(r_seis_grid_dist) %>% as("SpatialPolygons")) + 
  mapview(extent(r_seis_line_dist) %>% as("SpatialPolygons")) + 
  mapview(extent(r_roads_dist) %>% as("SpatialPolygons")) + 
  mapview(extent(r_lakes_dist) %>% as("SpatialPolygons")) + 
  mapview(extent(broadleaf_regen_focal500) %>% as("SpatialPolygons")) + 
  mapview(extent(conifer_focal500) %>% as("SpatialPolygons")) +
  mapview(extent(cultivated_focal500) %>% as("SpatialPolygons"))

## Seismic grid distance is narrower even though the data was cropped to the same extent. ArcGIS does this. Resample can fix this.

r_seis_grid_dist_resample <- resample(r_seis_grid_dist, r_seis_line_dist)  # resample() is the brute force method and risks changing your data substantially
# from tool description - "Before using resample, you may want to consider using these other functions instead: aggregate, disaggregate, crop, extend, merge."
## Note -- resample also changes the cell size! Cell size (resolution) needs to be identical for all stacked rasters

mapview(r_seis_grid_dist) + mapview(r_seis_grid_dist_resample)  # they look the same, so rename the main file as the resampled file
r_seis_grid_dist <- r_seis_grid_dist_resample   

# Resample the rest of the input distance and focal statistics datasets 
r_roads_dist <- resample(r_roads_dist, r_seis_line_dist)
r_lakes_dist <- resample(r_lakes_dist, r_seis_line_dist)
broadleaf_regen_focal500 <- resample(broadleaf_regen_focal500, r_seis_line_dist)
conifer_focal500 <- resample(conifer_focal500, r_seis_line_dist)
cultivated_focal500 <- resample(cultivated_focal500, r_seis_line_dist)

# Test stack again
stack <- raster::stack(r_seis_grid_dist, r_seis_line_dist, r_roads_dist, r_lakes_dist, 
                       broadleaf_regen_focal500, conifer_focal500, cultivated_focal500)

# use addLayer and dropLayer to modify the contents of an existing stack  e.g.  stack <- addLayer(stack, newlayer)

## Rename stacked rasters so they are identical to the variables in our model - these are our new "dataframe" input in predict()
names(coefficients(model_1))
names(stack)
names(stack) <- c("seismic_grid_dist", "seismic_linetrail_dist", "roads_dist", "lakes_dist", "broadleaf_regen", 
                  "conifer_low", "cultivated_high")

names(coefficients(model_1)) %in% names(stack)  # TRUE!


### Working with Raster Stacks ---
stack@layers  
# Select different "slots" of the raster

stack$seismic_linetrail_dist
# Select different layers of the raster stack. Bands of a multi-band satellite image would show up here.

# View all layers in a stack
mapview(stack$roads_dist) + mapview(stack$seismic_linetrail_dist)  + mapview(stack$lakes_dist)   # etc



### Part 2 - Apply the predict() function ------
# raster::predict() source -  https://rdrr.io/cran/raster/man/predict.html
t_predict <- predict(stack, model_1, filename = "predict_model_1.tif", scale = "response", progress = "window", overwrite = TRUE)

mapview(t_predict)  # Blamo!





### Section 7 -- Making maps --------

### Part 1 - Colour Palettes -----
# source: https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# source: http://www.sthda.com/english/wiki/colors-in-r

install.packages("wesanderson")
library(wesanderson)

View(wes_palettes)
wes_palette("Darjeeling1")
pal_Darjeeling <- wesanderson::wes_palette("Darjeeling1", 21, type = "continuous")
pal_Zissou <- wesanderson::wes_palette("Zissou1", 7, type = "continuous")


# More palettes --
library("RColorBrewer")
display.brewer.all()  # Static
tmaptools::palette_explorer()   # Interactive

pal_Spectral <- get_brewer_pal("Spectral", n = 8, contrast = c(0.25, 0.75))   # Adjusting contrast adjusts the colour range - "zoom"

### How to reverse a colour palette -- 
pal_Spectral_reverse <- rev(pal_Spectral)

pal_Red_Blue <- get_brewer_pal("RdYlBu", n = 8)
pal_Red_Blue_reverse <- rev(pal_Red_Blue)



### Part 2 - Making static maps using tmap() ------

## Set tmap options ahead of time --
# https://rdrr.io/cran/tmap/man/tmap_options.html

tmap_options(bg.color = "blue", legend.text.color = "white")

tmap_style("natural")  # Select a predefined style 
# other available styles are: "white", "gray", "natural", "cobalt", "col_blind", "albatross", "beaver", "bw", "watercolor"

tmap_options_diff()  # See what options are different
tmap_options_reset()  # Reset options


### Build a map -- -- --
# tmap elements - https://www.rdocumentation.org/packages/tmap/versions/2.3-2/topics/tmap-element  *** Shows all functions and additions ***
# tm_raster() - https://www.rdocumentation.org/packages/tmap/versions/2.3-2/topics/tm_raster
# tm_layout() - https://www.rdocumentation.org/packages/tmap/versions/2.2/topics/tm_layout

tm_shape(t_predict) +                                   
  tm_raster(title = "Estimated Habitat Selection", palette = pal_Spectral_reverse) + 
tm_shape(waterbodies) + 
  tm_polygons()
  
# Always call this first. Rasters and vectors are all plotted using tm_shape()
# Add base layers - tm_polygons(), tm_lines(), tm_text(), tm_symbols(), etc
# You can add successive layers with more calls to tm_shape() + tm_(layer-type) 
# -- make sure you call the right layer-type for the data you loaded in tm_shape()! 

tm_shape(t_predict) +                                   
  tm_raster(title = "Estimated Habitat Selection", palette = pal_Spectral_reverse)

# Colour palette needs more breaks to show variability between 0 and -5
hist(t_predict, breaks = 20)  # show the distribution of pixel values 

# Test adding more breaks to the raster visualization
tm_shape(t_predict) +                                   
  tm_raster(title = "Estimated Habitat Selection", palette = pal_Spectral_reverse, breaks = c(-20, -10, -7.5, -5, -3, -1, -0.5, -0.25, 0))
# pal_Spectral_reverse has 8 breaks, so we want 8 breaks here to use all the colours. If you need more breaks, you can add them when you create the palette 

tm_shape(t_predict) +                                   
  tm_raster(title = "Estimated Habitat Selection", palette = pal_Zissou, breaks = c(-20, -7.5, -5, -2.5, -1, -0.5, -0.25, 0))

# Red-Blue palettes are nice for showing hot and cold areas, but if your map has lots of "medium" values, they will show up as yellow-green. 
## Lesson - If you choose a palette with nice end colours, intermediate values in your data will make your maps look like puke.

# Good enough!
tm_shape(t_predict) +                                   
  tm_raster(title = "Estimated Habitat Selection", palette = pal_Red_Blue_reverse, breaks = c(-20, -10, -7.5, -5, -2, -1, -0.5, -0.25, 0))

# Compare discrete palette above to continuous palette below. The message is VERY different.
tm_shape(t_predict) +                                   
  tm_raster(title = "Estimated Habitat Selection", palette = pal_Red_Blue_reverse, style = "cont")


### Add map elements ---
lakes <- st_read("Input Data/lakes_crop.shp", crs = 3400) %>% make_valid()
cities <- st_read("Input Data/cities.shp", crs = 3400)

map <- tm_shape(t_predict) +                                   
  tm_raster(title = "Habitat Selection", palette = pal_Red_Blue_reverse, style = "cont") + 
tm_shape(lakes) + tm_polygons(col = "royalblue") +
  tm_shape(cities) + tm_dots(col = "grey25", size = 0.25, legend.show = FALSE) + tm_text("NAME", ymod = 0.25, xmod = 1) +
tm_compass(north = 0, type = "arrow", position = c("RIGHT", "TOP")) + tm_scale_bar(position = c("RIGHT","BOTTOM"))

tmap_save(map_1, file = "test_save.jpeg", width = 15, height = 35)
### The insanely frustrating thing about save_tmap is that changing *either* the height or the width changes your map output. 
tmap_save(map, file = "test_save_2.png", width = 5)
tmap_save(map, file = "test_save_1.png", width = 20)
tmap_save(map, file = "test_save_3.png", asp = 0)

### How to save maps with a constant aspect ratio between R-Stuio viewer, x11/quartz, and output files?
# I don't know!


### Conclusions -
# R is good for making static maps to quickly view data.
# R is capable *but takes a lot of work* for making presentation-quality static maps for papers and reports.    
# Colour pallets are difficult - rasters come out mediocre without a lot of work.
# Basemaps are difficult to access - there may be alternatives to ggmap but I haven't found them.
# Map size changes based on the size of the windows you view and save maps in. This *screws* with all your formatting. 
#    Anyone have an application that lets you view and plot maps with a consistent aspect ratio?  





### Part 2 - Making interactive maps using mapview() ------

# Create a map with multiple layers
map <- mapview(t_predict)

# Save your map as an html file! Easy!
mapshot(map, url = "habitat_selection_predictions.html")



### Section 8: Other cool things - Remote Sensing ---------

# sen2R - sentinel images
# MODIS
# NDVI and remote sensing layers 
# Image classification

# Plotting multi-band images
# 3D viewer

library(MODIStsp)
library(sen2r)
# sen2r comes up with errors. Andrew, help!

### MODIS -------
# source - https://www.earthdatascience.org/courses/earth-analytics/multispectral-remote-sensing-modis/modis-data-in-R/

MODIStsp()   # Open image downloading GUI

# Open modis bands (layers with "SC" in the name that end in .tif)   
modis_Jan <- list.files("Snow_Cov_Daily_500m_v6/SC", 
                        pattern = glob2rx("*SC*.tif$"),
                        full.names = TRUE)
                                    
# create a raster stack and then a raster brick
modis_snow_stack <- stack(modis_Jan)
modis_snow <- brick(modis_snow_stack)   # Input the raster stack into the raster brick

# A raster stack is a collection of multiple raster layer files with the same extent and resolution. 
# A raster brick is formed from a single file - e.g. the stack we just created above.
# source - https://rspatial.org/raster/spatial/4-rasterdata.html#rasterstack-and-rasterbrick

plot(modis_snow$MOD10_A1_SC_2019_003)  # plot a single band in a raster stack or raster brick 

mapview(modis_snow$MOD10_A1_SC_2019_003)    # mapview automatically re-projects to web-mercator

# Transform coordinate system -- NOT THE RIGHT WAY
crs(modis_snow)
crs(modis_snow) <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
crs(modis_snow) <- CRS('+init=EPSG:3005')  # Another simpler way to do it using the EPSG number
# This just tells the raster that the existing coordinate system is actually a different one. Changes nothing.

plot(modis_snow$MOD10_A1_SC_2019_003, 
     box = FALSE, 
     axes = FALSE)

## Transform the modis_snow coordinate system --
modis_snow <- projectRaster(modis_snow, crs = CRS('+init=EPSG:3005'))
plot(modis_snow$MOD10_A1_SC_2019_003)

# Look at values for the raster
hist(modis_snow$MOD10_A1_SC_2019_003)
hist(modis_snow)  # This shows us the images with snow in them. values 0 to 100 are NDSI (normalized-difference snow index) snow cover
#  https://nsidc.org/support/faq/what-ndsi-snow-cover-and-how-does-it-compare-fsc

# Everything above 100 is a band designed to show some other image characteristic.  source - https://nsidc.org/data/MYD10_L2/versions/6 
# 0–100: NDSI snow cover
# 200: missing data
# 201: no decision
# 211: night
# 237: inland water
# 239: ocean
# 250: cloud
# 254: detector saturated
# 255: fill


### Landsat ----
# Landsat data available from - https://earthexplorer.usgs.gov/ - have to create an account - fairly easy

# Using package "rLandsat8" - source http://terradue.github.io/rLandsat8/index.html
install.packages("RCurl")
library(devtools)
install_url("https://github.com/Terradue/rLandsat8/releases/download/v0.1-SNAPSHOT/rLandsat8_0.1.0.tar.gz")
library(rLandsat8)

l <- ReadLandsat8("Landsat-8")
?ReadLandsat8   # Not working


## Try the steps used in MODIS above --
landsat <- list.files("Landsat-8/LC080440242019072201T1-SC20191024000537", 
                        pattern = glob2rx("*.tif$"),
                        full.names = TRUE)
                        
landsat_stack <- stack(landsat)
landsat_brick <- brick(landsat_stack)                        
            
# Transform crs - SLOW step 
# landsat_brick <- projectRaster(landsat_brick, crs = CRS('+init=EPSG:3005'))
# crs(landsat_brick)

x11(10,10)
plot(landsat_brick$LC08_L1TP_044024_20190722_20190801_01_T1_sr_band3)

## Plot an image --
# Landsat-8 image bands - excellent description! https://landsat.gsfc.nasa.gov/landsat-8/landsat-8-bands/
# https://landsat.gsfc.nasa.gov/landsat-data-continuity-mission/
  
# https://www.rdocumentation.org/packages/mapview/versions/2.7.0/topics/viewRGB
mapview::viewRGB(landsat_brick, r = 4, g = 3, b = 2) # Interactive
mapview::viewRGB(landsat_brick, r = 7, g = 6, b = 5)
# Can't use the band names, have to refer to the "slots" in the image
landsat_brick@data

plotRGB(landsat_brick, r = 7, g = 6, b = 5)  # Static
# I dodn't know what this error message means



# Excellent walkthrough - https://www.earthdatascience.org/courses/earth-analytics/multispectral-remote-sensing-data/landsat-data-in-r-geotiff/
## Retry loading data with simpler bands 
options(stringsAsFactors = FALSE)  # Turn off factors

## Try the steps used in MODIS above --
landsat <- list.files("Landsat-8/LC080440242019072201T1-SC20191024000537", 
                      pattern = glob2rx("*band*.tif$"),
                      full.names = TRUE)
### Selecting only the .tif files with *band* in them. All other tif files for data quality but not image plotting

landsat_stack <- stack(landsat)
landsat_brick <- brick(landsat_stack)                
landsat_brick@data   # NOW Data slot 1 = Band 1, Slot 2 = Band 2, etc

# Retry plotting with new band names
mapview::viewRGB(landsat_brick, r = 4, g = 3, b = 2)
plotRGB(landsat_brick, r = 4, g = 3, b = 2)  # Error again. Resolve later


## NDVI ---
# source - https://www.earthdatascience.org/courses/earth-analytics/multispectral-remote-sensing-data/vegetation-indices-NDVI-in-R/

# NDVI is an index computed by: (NIR - Red) / (NIR + Red) 
landsat_ndvi <- (landsat_brick[[5]] - landsat_brick[[4]]) / (landsat_brick[[5]] + landsat_brick[[4]])

# Overlay function is faster - provide a function and a list of layers 

# Create a function for NDVI
ndvi_function <- function(b5, b4){
  diff <- (b5 - b4) / (b5 + b4)
  return(diff)
}

# Apply the function to the raster layers with overlay()
landsat_ndvi_ov <- overlay(landsat_brick[[5]],
                           landsat_brick[[4]],
                        fun = ndvi_function)
x11(10,10)
plot(landsat_ndvi_ov)


### Add a second landsat image and merge ---

landsat_2 <- list.files("Landsat-8/LC080450242019051001T1-SC20191024000602.tar", 
                      pattern = glob2rx("*band*.tif$"),
                      full.names = TRUE)

landsat_stack <- stack(landsat_2)
landsat_brick_2 <- brick(landsat_stack)                
landsat_brick_2@data

plot(landsat_brick_2$LC08_L1TP_045024_20190510_20190521_01_T1_sr_band3)

landsat_merge <- merge(landsat_brick, landsat_brick_2, filename = "landsat_merge.tif")
plot(landsat_merge)

