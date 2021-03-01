#-----------------
# Internal Data
#-----------------

#-----------------
# Example data files to test rmap
#-----------------

#' Example GCAM .proj file
#'
#' @source example GCAM output
#' @format R table or .csv
#' @examples
#' \dontrun{
#'  library(rmap);
#'  rmap::exampleGCAMproj
#' }
"exampleGCAMproj"

#' Example data by param to plot in Maps
#'
#' @source example data by Param
#' @format R table or .csv
#' @examples
#' \dontrun{
#'  library(rmap);
#'  mapData <- rmap::exampleMapDataParam
#' }
"exampleMapDataParam"

#' Example data by class to plot in Maps
#'
#' @source example data by class
#' @format R table or .csv
#' @examples
#' \dontrun{
#'  library(rmap);
#'  mapData <- rmap::exampleMapDataClass
#' }
"exampleMapDataClass"


#-----------------
# World Maps (Countries, States)
#-----------------

#' World Map of Countries
#'
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapCountries)
#'  head(mapCountries@data)
#' }
"mapCountries"

#' World Map of States
#'
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapStates)
#'  head(mapStates@data)
#' }
"mapStates"

#-----------------
# GCAM Maps (Regions, Basins, Land)
#-----------------

#' GCAM 32 Regions
#'
#' @source From JGCRI confluence page. \url{https://confluence.pnnl.gov/confluence/display/JGCRI/GCAM+Shape+Files}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapGCAMReg32)
#'  head(mapGCAMReg32@data)
#' }
"mapGCAMReg32"


#' GCAM Basins
#'
#' @source From JGCRI confluence page. \url{https://confluence.pnnl.gov/confluence/display/JGCRI/GCAM+Shape+Files}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapGCAMBasins)
#'  head(mapGCAMBasins@data)
#' }
"mapGCAMBasins"

#' GCAM Land
#'
#' @source From the moirai project.
#' Shapefile received directly from Alan V. Di Vittorio.
#' Component files available at:
#' \url{https://github.com/JGCRI/moirai/blob/master/ancillary/moirai_valid_boundaries.zip}
#' \url{https://github.com/JGCRI/moirai/blob/build-r-package/tests/testthat/test_compare_raster_w_outputs.R}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapGCAMLand)
#'  head(mapGCAMLand@data)
#' }
"mapGCAMLand"

#-----------------
# Grids (0.5 and 0.25)
#-----------------


#-----------------
# Hydrology Maps (HydroShed, HUC)
#-----------------

# Hydro sheds
# https://www.hydrosheds.org/page/hydrobasins
# Lehner, B., Grill G. (2013): Global river hydrography and network routing:
# baseline data and new approaches to study the world’s large river systems.
# Hydrological Processes, 27(15): 2171–2186. Data is available at www.hydrosheds.org

#' HydroSHEDS level 1
#' @source Lehner, B., Grill G. (2013): Global river hydrography and network routing:
#' baseline data and new approaches to study the world’s large river systems.
#' Hydrological Processes, 27(15): 2171–2186. \url{https://www.hydrosheds.org/page/hydrobasins}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapHydroShed1)
#'  head(mapHydroShed1@data)
#' }
"mapHydroShed1"

#' HydroSHEDS level 2
#' @source Lehner, B., Grill G. (2013): Global river hydrography and network routing:
#' baseline data and new approaches to study the world’s large river systems.
#' Hydrological Processes, 27(15): 2171–2186. \url{https://www.hydrosheds.org/page/hydrobasins}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapHydroShed2)
#'  head(mapHydroShed2@data)
#' }
"mapHydroShed2"

#' HydroSHEDS level 3
#' @source Lehner, B., Grill G. (2013): Global river hydrography and network routing:
#' baseline data and new approaches to study the world’s large river systems.
#' Hydrological Processes, 27(15): 2171–2186. \url{https://www.hydrosheds.org/page/hydrobasins}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapHydroShed3)
#'  head(mapHydroShed3@data)
#' }
"mapHydroShed3"


# USGS HUC 2 (52 States)
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapUS52HUC2)
#'  head(mapUS52HUC2@data)
#' }
"mapUS52HUC2"

# USGS HUC 2 (49 States)
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapUS49HUC2)
#'  head(mapUS49HUC2@data)
#' }
"mapUS49HUC2"

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapUS52HUC4)
#'  head(mapUS52HUC4@data)
#' }
"mapUS52HUC4"

# USGS HUC 2 (49 States)
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapUS49HUC4)
#'  head(mapUS49HUC4@data)
#' }
"mapUS49HUC4"


#-----------------
# US Maps ( 52 State, 49 State, Counties, Regions, Grid Regions)
#-----------------

#' US 52 States
#' Includes Alaska, Hawaii and Puerto Rico as well as DC.
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapUS52)
#'  head(mapUS52@data)
#' }
"mapUS52"

#' US 52 States Compact
#' Includes Alaska, Hawaii and Puerto Rico as well as DC (Re-positioned)
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapUS52Compact)
#'  head(mapUS52Compact@data)
#' }
"mapUS52Compact"

#' US 49 States
#' Excludes Alaska, Hawaii and Puerto Rico. Includes DC.
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapUS49)
#'  head(mapUS49@data)
#' }
"mapUS49"

#' US 52 Counties
#' Includes Alaska, Hawaii and Puerto Rico as well as DC.
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapUS52County)
#'  head(mapUS52County@data)
#' }
"mapUS52County"

#' US 52 Counties Compact
#' Includes Alaska, Hawaii and Puerto Rico as well as DC (Repositioned)
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapUS52CountyCompact)
#'  head(mapUS52CountyCompact@data)
#' }
"mapUS52CountyCompact"

#' US 49 States
#' Excludes Alaska, Hawaii and Puerto Rico. Includes DC.
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapUS49County)
#'  head(mapUS49County@data)
#' }
"mapUS49County"

#' Empty global grid 0.25 degrees
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#' library(rmap)
#' head(grid025)
#' }
"grid025"

#' US 49 States
#' Excludes Alaska, Hawaii and Puerto Rico. Includes DC.
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#' library(rmap)
#' head(grid050)
#' }
"grid050"


#' Gridded Population of the World, Version 4 (GPWv4)
#' https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11/data-download#
#' @source SEDAC, https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11/data-download#
#' @format R tibble
#' @examples
#' \dontrun{
#' library(rmap)
#' head(data_pop_GPWv4To2015)
#' }
"grid_pop_GPWv4To2015"


#--------------------
# Cropped
#--------------------

#' Cropped of GCAM Basins and US52
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#' library(rmap)
#' head(mapGCAMBasinsUS52)
#' }
"mapGCAMBasinsUS52"

#' Cropped of GCAM Basins and US49
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#' library(rmap)
#' head(mapGCAMBasinsUS49)
#' }
"mapGCAMBasinsUS49"

#' GCAM Land cropped to US52
#'
#' @source Created from rmap maps
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapGCAMLandUS52)
#'  head(mapGCAMLandUS52@data)
#' }
"mapGCAMLandUS52"

#' GCAM Land cropped to US49
#'
#' @source Created from rmap maps.
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapGCAMLandUS49)
#'  head(mapGCAMLandUS49@data)
#' }
"mapGCAMLandUS49"


#--------------------
# Mergers
#--------------------

#' Merge of GCAM 32 and US52
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#' library(rmap)
#' head(mapGCAMReg32US52)
#' }
"mapGCAMReg32US52"


#' Merge of Countries and US52
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#' library(rmap)
#' head(mapCountriesUS52)
#' }
"mapCountriesUS52"

#--------------------
# Intersections
#--------------------

#' Intersection of GCAM Basins and countries.
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#' library(rmap)
#' head(mapIntersectGCAMBasinCountry)
#' }
"mapIntersectGCAMBasinCountry"

#' Intersection of GCAM Basins and GCAM 32 Regions.
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#' library(rmap)
#' head(mapIntersectGCAMBasin32Reg)
#' }
"mapIntersectGCAMBasin32Reg"

#' Intersection of GCAM Basins and US 52 Regions.
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#' library(rmap)
#' head(mapIntersectGCAMBasinUS52)
#' }
"mapIntersectGCAMBasinUS52"


#' Intersection of GCAM Basins and US 52 County Regions.
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#' library(rmap)
#' head(mapIntersectGCAMBasinUS52County)
#' }
"mapIntersectGCAMBasinUS52County"
