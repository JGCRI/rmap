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

#' mapping_param_query
#'
#' @source rmap
#' @format R table
#' @examples
#' \dontrun{
#'  library(rmap);
#'  mapping_param_query <- rmap::mapping_param_query
#' }
"mapping_param_query"

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
# US HUC levels
#-----------------

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



# USGS HUC 2 (52 States)
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS52HUC2df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS52HUC2df)
#' }
"mapUS52HUC2df"

# USGS HUC 2 (49 States)
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS49HUC2df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS49HUC2df)
#' }
"mapUS49HUC2df"

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS52HUC4df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS52HUC4df)
#' }
"mapUS52HUC4df"

# USGS HUC 2 (49 States)
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

#' USGS Hydrological Unit Code (HUC)
#' @source \url{https://water.usgs.gov/GIS/huc.html} \url{https://nrcs.app.box.com/v/huc}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS49HUC4df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS49HUC4df)
#' }
"mapUS49HUC4df"



#-----------------
# GCAM Maps (Regions, Basins, Land)
#-----------------

#' GCAM 32 Regions
#'
#' @source Modified versions of shapefiles from \url{https://zenodo.org/record/4688451#.YdMNTmjMJPY}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapGCAMReg32)
#'  head(mapGCAMReg32@data)
#' }
"mapGCAMReg32"

#' GCAM 32 Regions with Uruguay
#'
#' @source Modified versions of shapefiles from \url{https://zenodo.org/record/4688451#.YdMNTmjMJPY}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapGCAMReg32Uruguay)
#'  head(mapGCAMReg32Uruguay@data)
#' }
"mapGCAMReg32Uruguay"


#' GCAM Basins
#'
#' @source Modified versions of shapefiles from \url{https://zenodo.org/record/4688451#.YdMNTmjMJPY}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap)
#'  sp::plot(mapGCAMBasins)
#'  head(mapGCAMBasins@data)
#' }
"mapGCAMBasins"


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

#' Intersection of GCAM Basins and GCAM 32 Regions + Uruguay.
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#' library(rmap)
#' head(mapIntersectGCAMBasin32RegUruguay)
#' }
"mapIntersectGCAMBasin32RegUruguay"

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
#'  names(mapsSubRegions)
#' }
"mapsSubRegions"

#-----------------
# World Maps (Countries, States)
#-----------------

#' World Map of Countries
#'
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapCountriesdf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapCountriesdf)
#' }
"mapCountriesdf"

#' World Map of States
#'
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapStatesdf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapStatesdf)
#' }
"mapStatesdf"

#-----------------
# GCAM Maps (Regions, Basins, Land)
#-----------------

#' GCAM 32 Regions
#'
#' @source Modified versions of shapefiles from \url{https://zenodo.org/record/4688451#.YdMNTmjMJPY}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMReg32df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMReg32df)
#' }
"mapGCAMReg32df"

#' GCAM 32 Regions with Uruguay
#'
#' @source Modified versions of shapefiles from \url{https://zenodo.org/record/4688451#.YdMNTmjMJPY}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMReg32Uruguaydf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMReg32Uruguaydf)
#' }
"mapGCAMReg32Uruguaydf"


#' GCAM Basins
#'
#' @source Modified versions of shapefiles from \url{https://zenodo.org/record/4688451#.YdMNTmjMJPY}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMBasinsdf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMBasinsdf)
#' }
"mapGCAMBasinsdf"

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
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapHydroShed1df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapHydroShed1df)
#' }
"mapHydroShed1df"

#' HydroSHEDS level 2
#' @source Lehner, B., Grill G. (2013): Global river hydrography and network routing:
#' baseline data and new approaches to study the world’s large river systems.
#' Hydrological Processes, 27(15): 2171–2186. \url{https://www.hydrosheds.org/page/hydrobasins}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapHydroShed2df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapHydroShed2df)
#' }
"mapHydroShed2df"

#' HydroSHEDS level 3
#' @source Lehner, B., Grill G. (2013): Global river hydrography and network routing:
#' baseline data and new approaches to study the world’s large river systems.
#' Hydrological Processes, 27(15): 2171–2186. \url{https://www.hydrosheds.org/page/hydrobasins}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapHydroShed3df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapHydroShed3df)
#' }
"mapHydroShed3df"


#-----------------
# US Maps ( 52 State, 49 State, Counties, Regions, Grid Regions)
#-----------------

#' US 52 States
#' Includes Alaska, Hawaii and Puerto Rico as well as DC.
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS52df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS52df)
#' }
"mapUS52df"

#' US 52 States Compact
#' Includes Alaska, Hawaii and Puerto Rico as well as DC (Re-positioned)
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS52Compactdf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS52Compactdf)
#' }
"mapUS52Compactdf"

#' US 49 States
#' Excludes Alaska, Hawaii and Puerto Rico. Includes DC.
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS49df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS49df)
#' }
"mapUS49df"

#' US 52 Counties
#' Includes Alaska, Hawaii and Puerto Rico as well as DC.
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS52Countydf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS52Countydf)
#' }
"mapUS52Countydf"

#' US 52 Counties Compact
#' Includes Alaska, Hawaii and Puerto Rico as well as DC (Repositioned)
#' @source US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS52CountyCompactdf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS52CountyCompactdf)
#' }
"mapUS52CountyCompactdf"

#' US 49 States
#' Excludes Alaska, Hawaii and Puerto Rico. Includes DC.
#' @source Made with Natural Earth. \url{http://www5.statcan.gc.ca/cansim/}
#' @format A SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapUS49Countydf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapUS49Countydf)
#' }
"mapUS49Countydf"


#--------------------
# Cropped
#--------------------

#' Cropped of GCAM Basins and US52
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMBasinsUS52df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMBasinsUS52df)
#' }
"mapGCAMBasinsUS52df"

#' Cropped of GCAM Basins and US49
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMBasinsUS49df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMBasinsUS49df)
#' }
"mapGCAMBasinsUS49df"

#--------------------
# Mergers
#--------------------

#' Merge of GCAM 32 and US52
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapGCAMReg32US52df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapGCAMReg32US52df)
#' }
"mapGCAMReg32US52df"


#' Merge of Countries and US52
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapCountriesUS52df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapCountriesUS52df)
#' }
"mapCountriesUS52df"

#--------------------
# Intersections
#--------------------

#' Intersection of GCAM Basins and countries.
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapIntersectGCAMBasinCountrydf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapIntersectGCAMBasinCountrydf)
#' }
"mapIntersectGCAMBasinCountrydf"

#' Intersection of GCAM Basins and GCAM 32 Regions.
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapIntersectGCAMBasin32Regdf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapIntersectGCAMBasin32Regdf)
#' }
"mapIntersectGCAMBasin32Regdf"

#' Intersection of GCAM Basins and GCAM 32 Regions + Uruguay.
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapIntersectGCAMBasin32RegUruguaydf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapIntersectGCAMBasin32RegUruguaydf)
#' }
"mapIntersectGCAMBasin32RegUruguaydf"

#' Intersection of GCAM Basins and US 52 Regions.
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapIntersectGCAMBasinUS52df, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapIntersectGCAMBasinUS52df)
#' }
"mapIntersectGCAMBasinUS52df"


#' Intersection of GCAM Basins and US 52 County Regions.
#' @source JGCRI
#' @format R tibble
#' @examples
#' \dontrun{
#'  library(sp); library(rmap); library(ggplot2)
#'  ggplot() +
#'  geom_polygon(data = mapIntersectGCAMBasinUS52Countydf, aes(x = long, y = lat, group = group),
#'              colour = "black", fill = NA)
#'  head(mapIntersectGCAMBasinUS52Countydf)
#' }
"mapIntersectGCAMBasinUS52Countydf"

