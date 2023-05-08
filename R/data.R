#-----------------
# Internal Data
#-----------------

#-----------------
# Example data files to test rmap
#-----------------

#' mapping_tethys_grid_basin_region_country
#'
#' @source tethys, gcam
#' @format R table
#' @examples
#' \dontrun{
#'  library(rmap);
#'  mapping_tethys_grid_basin_region_country <- rmap::mapping_tethys_grid_basin_region_country
#' }
"mapping_tethys_grid_basin_region_country"

#' mapping_US49
#'
#' @source rmap
#' @format R table
#' @examples
#' \dontrun{
#'  library(rmap);
#'  mapping_US49 <- rmap::mapping_US49
#' }
"mapping_US49"

#' mapping_US52
#'
#' @source rmap
#' @format R table
#' @examples
#' \dontrun{
#'  library(rmap);
#'  mapping_US52 <- rmap::mapping_US52
#' }
"mapping_US52"

#' mapping_country_gcam32
#'
#' @source rmap
#' @format R table
#' @examples
#' \dontrun{
#'  library(rmap);
#'  mapping_country_gcam32 <- rmap::mapping_country_gcam32
#' }
"mapping_country_gcam32"


#' mapping_gcambasins
#'
#' @source rmap
#' @format R table
#' @examples
#' \dontrun{
#'  library(rmap);
#'  mapping_gcambasins <- rmap::mapping_gcambasins
#' }
"mapping_gcambasins"

#-----------------
# Example data files to test rmap
#-----------------

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

#' Example grid data
#'
#' @source example grid population data https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11/data-download#
#' @format tibble
#' @examples
#' \dontrun{
#'  library(rmap);
#'  gridData <- rmap::example_gridData_GWPv4To2015
#' }
"example_gridData_GWPv4To2015"


#-----------------
# World Maps (Countries, States)
#-----------------

#' World Map of Countries
#'
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapCountries[,"subRegion"])
#'  head(mapCountries)
#' }
"mapCountries"

#' World Map of States
#'
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapStates[,"subRegion"])
#'  head(mapStates)
#' }
"mapStates"

#-----------------
# US HUC levels
#-----------------

# USGS HUC 2 (52 States)
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapUS52HUC2[,"subRegion"])
#'  head(mapUS52HUC2)
#' }
"mapUS52HUC2"

# USGS HUC 2 (49 States)
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapUS49HUC2[,"subRegion"])
#'  head(mapUS49HUC2)
#' }
"mapUS49HUC2"

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapUS52HUC4[,"subRegion"])
#'  head(mapUS52HUC4)
#' }
"mapUS52HUC4"

# USGS HUC 2 (49 States)
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapUS49HUC4[,"subRegion"])
#'  head(mapUS49HUC4)
#' }
"mapUS49HUC4"

#-----------------
# GCAM Maps (Regions, Basins)
#-----------------

#' GCAM 32 Regions
#'
#' @source Modified versions of shapefiles from \url{https://zenodo.org/record/4688451#.YdMNTmjMJPY}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapGCAMReg32[,"subRegion"])
#'  head(mapGCAMReg32)
#' }
"mapGCAMReg32"

#' GCAM 32 Regions with Uruguay
#'
#' @source Modified versions of shapefiles from \url{https://zenodo.org/record/4688451#.YdMNTmjMJPY}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapGCAMReg32Uruguay[,"subRegion"])
#'  head(mapGCAMReg32Uruguay)
#' }
"mapGCAMReg32Uruguay"

#' GCAM 32 Regions with EU countries
#'
#' @source Modified versions of shapefiles from \url{https://zenodo.org/record/4688451#.YdMNTmjMJPY}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapGCAMReg32EU[,"subRegion"])
#'  head(mapGCAMReg32EU)
#' }
"mapGCAMReg32EU"


#' GCAM Basins
#'
#' @source Modified versions of shapefiles from \url{https://zenodo.org/record/4688451#.YdMNTmjMJPY}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapGCAMBasins[,"subRegion"])
#'  head(mapGCAMBasins)
#' }
"mapGCAMBasins"

#' GCAM Land
#'
#' Intersection of mapGCAMBasins and mapGCAMReg32
#'
#' @source Modified versions of shapefiles from \url{https://zenodo.org/record/4688451#.YdMNTmjMJPY}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapGCAMLand[,"subRegion"])
#'  head(mapGCAMLand)
#' }
"mapGCAMLand"


#-----------------
# Hydrology Maps (HydroShed)
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
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapHydroShed1[,"subRegion"])
#'  head(mapHydroShed1)
#' }
"mapHydroShed1"

#' HydroSHEDS level 2
#' @source Lehner, B., Grill G. (2013): Global river hydrography and network routing:
#' baseline data and new approaches to study the world’s large river systems.
#' Hydrological Processes, 27(15): 2171–2186. \url{https://www.hydrosheds.org/page/hydrobasins}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapHydroShed2[,"subRegion"])
#'  head(mapHydroShed2)
#' }
"mapHydroShed2"

#' HydroSHEDS level 3
#' @source Lehner, B., Grill G. (2013): Global river hydrography and network routing:
#' baseline data and new approaches to study the world’s large river systems.
#' Hydrological Processes, 27(15): 2171–2186. \url{https://www.hydrosheds.org/page/hydrobasins}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapHydroShed3[,"subRegion"])
#'  head(mapHydroShed3)
#' }
"mapHydroShed3"

#-----------------
# US Maps ( 52 State, 49 State, Counties, Regions, Grid Regions)
#-----------------

#' US 52 States
#' Includes Alaska, Hawaii and Puerto Rico as well as DC.
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapUS52[,"subRegion"])
#'  head(mapUS52)
#' }
"mapUS52"

#' US 52 States Compact
#' Includes Alaska, Hawaii and Puerto Rico as well as DC (Re-positioned)
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapUS52Compact[,"subRegion"])
#'  head(mapUS52Compact)
#' }
"mapUS52Compact"

#' US 49 States
#' Excludes Alaska, Hawaii and Puerto Rico. Includes DC.
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapUS49[,"subRegion"])
#'  head(mapUS49)
#' }
"mapUS49"

#' US 52 Counties
#' Includes Alaska, Hawaii and Puerto Rico as well as DC.
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapUS52County[,"subRegion"])
#'  head(mapUS52County)
#' }
"mapUS52County"

#' US 52 Counties Compact
#' Includes Alaska, Hawaii and Puerto Rico as well as DC (Repositioned)
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapUS52CountyCompact[,"subRegion"])
#'  head(mapUS52CountyCompact)
#' }
"mapUS52CountyCompact"

#' US 49 States
#' Excludes Alaska, Hawaii and Puerto Rico. Includes DC.
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format sf
#' @examples
#' \dontrun{
#'  library(rmap)
#'  plot(mapUS49County[,"subRegion"])
#'  head(mapUS49County)
#' }
"mapUS49County"

#--------------------
# Cropped
#--------------------

#' Cropped map of GCAM Basins and US52
#' @source JGCRI
#' @format sf
#' @examples
#' \dontrun{
#' library(rmap)
#' plot(mapGCAMBasinsUS52[,"subRegion"])
#' head(mapGCAMBasinsUS52)
#' }
"mapGCAMBasinsUS52"

#' Cropped map of GCAM Basins and US49
#' @source JGCRI
#' @format sf
#' @examples
#' \dontrun{
#' library(rmap)
#' plot(mapGCAMBasinsUS49[,"subRegion"])
#' head(mapGCAMBasinsUS49)
#' }
"mapGCAMBasinsUS49"


#--------------------
# Mergers
#--------------------

#' Merge of GCAM 32 and US52
#' @source JGCRI
#' @format sf
#' @examples
#' \dontrun{
#' library(rmap)
#' plot(mapGCAMRef32US52[,"subRegion"])
#' head(mapGCAMReg32US52)
#' }
"mapGCAMReg32US52"


#' Merge of Countries and US52
#' @source JGCRI
#' @format sf
#' @examples
#' \dontrun{
#' library(rmap)
#' plot(mapCountriesUS52[,"subRegion"])
#' head(mapCountriesUS52)
#' }
"mapCountriesUS52"

#--------------------
# Intersections
#--------------------

#' Intersection of GCAM Basins and countries.
#' @source JGCRI
#' @format sf
#' @examples
#' \dontrun{
#' library(rmap)
#' plot(mapIntersectGCAMBasinCountry[,"subRegion"])
#' head(mapIntersectGCAMBasinCountry)
#' }
"mapIntersectGCAMBasinCountry"

#' Intersection of GCAM Basins and GCAM 32 Regions + Uruguay.
#' @source JGCRI
#' @format sf
#' @examples
#' \dontrun{
#' library(rmap)
#' plot(mapIntersectGCAMBasin32RegUruguay[,"subRegion"])
#' head(mapIntersectGCAMBasin32RegUruguay)
#' }
"mapIntersectGCAMBasin32RegUruguay"

#' Intersection of GCAM Basins and US 52 Regions.
#' @source JGCRI
#' @format sf
#' @examples
#' \dontrun{
#' library(rmap)
#' plot(mapIntersectGCAMBasinUS52[,"subRegion"])
#' head(mapIntersectGCAMBasinUS52)
#' }
"mapIntersectGCAMBasinUS52"


#' Intersection of GCAM Basins and US 52 County Regions.
#' @source JGCRI
#' @format sf
#' @examples
#' \dontrun{
#' library(rmap)
#' plot(mapInteresectGCAMBasinUS52County[,"subRegion"])
#' head(mapIntersectGCAMBasinUS52County)
#' }
"mapIntersectGCAMBasinUS52County"


#-----------------
# SubRegions list in pre-loaded maps
#-----------------

#' List of subRegions in Pre-loaded maps
#'
#' @source Compiled from each map
#' @format List
#' @examples
#' \dontrun{
#'  library(rmap);
#'  names(allSubRegions)
#' }
"allSubRegions"
