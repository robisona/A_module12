## Module 12 Assignment: Spatial Data
## NR 995 (1BB)
## Fall 2017
## Group A: Andrew Robison, Connor Breton, Korik Vargas

###############################################################################################################
## Create and save an R script in RStudio with the file name GroupLetter_module12.R.  You will submit
## this file as this week’s assignment, so be sure to follow good coding practices throughout.  Include
## your answers to the following questions as labeled comments in the script.

## The data for this week are derived from the National Land Cover Database (NLCD) and the Early 
## Detection and Distribution Mapping System (EDDMapS) for glossy buckthorn (Frangula alnus).  These 
## two datasets are created and curated by two distinct organizations, one focused on mapping land cover
## changes over time and the other on documenting invasive species distributions in North America 
## (primarily the United States).  The land cover map is thematic, with numeric values corresponding to
## particular land cover types (see lulc.csv).  The buckthorn data contains occurrence points and other
## attribute information as available.


#Set working directory:
# setwd("C:/Users/Connor/Documents/word_files/graduate_courses/r/module_12/assignment")
setwd("C:\\Users\\Drew\\Documents\\UNH\\Courses\\NR 995 - R\\Modules\\12")


###############################################################################################################
## (1)	Read in the land cover raster data (lulc_05.tif).  What map units are used for the given CRS?  
## What geographic region does the dataset cover? Optional: Can you put a name to the CRS?

# Load raster package into sessions
library(raster)

# Read in data using the raster function
# LULC <- raster("lulc_05.tif")
LULC <- raster("./assignment/lulc_05.tif")

# Check geographic region covered
plot(LULC)  
# LULC covers the geographic region of New England

# Put a name to the CRS
crs_LULC <- proj4string(LULC)  

# Check that name is assigned to CRS and examine output
crs_LULC  
# Datum = NAD83
# Units = meters
# Projection = Albert Equal Area Projection (found in r-bloggers link)

# Examine map extent 
extent(LULC)
# X extent = 1739505 to 2286435
# Y extent = 2193885 to 3032985


###############################################################################################################
## (2)	Read in the shapefile of glossy buckthorn points (buckthorn_pts).  What is the CRS for the 
## buckthorn dataset?  Is it the same as the land use data? What geographic region does the dataset cover? 

# Load rgdal package into session
library(rgdal)

# Read in buckthorn_pts shapefile
# Buck = readOGR(dsn = getwd(), layer="buckthorn_pts")
Buck = readOGR(dsn = "assignment", layer="buckthorn_pts")

# Look at CRS
Buck@proj4string
# Same as the LULC - NAD83, Albers Equal Area Projection

# Look at extent of cover
Buck@bbox
# X extent = -735848.2 to 2180214
# Y extent = 1603059.2 to 3005056

# Check geographic region covered
plot(Buck)
# Buck appears to cover the geographic region of the US


###############################################################################################################
## (3)	Crop (aka clip) the buckthorn points to the geographic area covered by the land cover dataset.
## How many buckthorn occurrence points are in your new dataset? 

# Clip the Buck by the LULC
NE.Buck <- crop(Buck, LULC)

# Check new clipped data
plot(NE.Buck)

# Look at structure and nrows of the new data frame
str(NE.Buck@data)
nrow(NE.Buck@data)
# 4010 rows?


###############################################################################################################
## (4)	Create a map that displays the land cover data and the buckthorn occurrence locations in the 
## object you created in the previous question. 

# Basic/default plot
plot(LULC)
plot(NE.Buck, pch = 19, add = T)

### OR ###

# Prettier version
plot_pts = list("sp.points", as(NE.Buck, "SpatialPoints"), col = "black", pch = 19)
spplot(LULC, sp.layout=list(plot_pts))


###############################################################################################################
## (5)	 How many unique reporters (REPORTER variable) are there for buckthorn in New England? (HINT:
## remember that when factors are subsetted, R still remembers all of the factor levels in the original 
## dataset.  Considering using as.character() in your analysis.) 

# Find the length of the unique entires in the REPORTER column of the NE.Buck data
length(unique(NE.Buck$REPORTER))
# Returns 244 unique reporters


###############################################################################################################
## (6)	Extract the land cover class to the buckthorn point dataset that you created in question 3 and 
## create a non-spatial data frame that includes the following for each observation: the coordinates, 
## the scientific name (“SCIEN”), the reporter (“REPORTER”), and the extracted land cover value for the 
## point.  What land cover class has the most glossy buckthorn observations?
  

# Create specified data frame and extract the LULC values for the NE.Buck points
pts.LULC <- data.frame(coordinates(NE.Buck), NE.Buck$SCIEN, NE.Buck$REPORTER, extract(LULC, NE.Buck))
head(pts.LULC)
str(pts.LULC)

# Create a function for mode 
# (source: https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode)
mode.fxn <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Apply mode.fxn to extracted LULC data points
mode.fxn(pts.LULC$extract.LULC..NE.Buck.)
# 2 is the land cover class with the most glossy buckthorn observations

###############################################################################################################
## In 1-2 sentences, identify the contribution of each group member to the assignment.  Upload your .R file to
## submit your assignment in myCourses, one per group.

