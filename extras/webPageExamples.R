library(metis)

# Maps Available
#------------------------------------------
head(mapUS49@data) # To View data in shapefile
metis::metis.map(mapUS49, labels=T)


# US49
#------------------------------------------
data = data.frame(subRegion=c("CA","FL","ID","MO","TX","WY"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
data%>%head()
metis.mapsProcess(polygonTable=data,
                  folderName = "vignetteMaps", mapTitleOn = F)

# US52
#------------------------------------------
data = data.frame(subRegion=c("AK","FL","ID","MO","TX","WY"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7));
data%>%head()
metis.mapsProcess(polygonTable=data,
                  folderName = "vignetteMaps", mapTitleOn = F)

# Counties
#------------------------------------------
unique(mapUS49County@data$subRegion) # Check subRegion Names
unique(mapUS49County@data$subRegionAlt) # Check Alternate names
data = data.frame(subRegion=c("Pender_NC","Larue_KY","Jim Wells_TX","Orange_IN","Putnam_FL","Ellis_KS"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
data%>%head()
metis.mapsProcess(polygonTable=data,
                  folderName = "vignetteMaps",
                  nameAppend = "_Alt",
                  mapTitleOn = F)


# GCAM 32
#------------------------------------------
unique(mapGCAMBasins@data$subRegion) # Check Available Regions
data = data.frame(subRegion=c("Colombia","China","EU-12","Pakistan","Middle East","Japan"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
metis.mapsProcess(polygonTable=data,
                  folderName = "vignetteMaps",
                  mapTitleOn = F)


# GCAM Basins
#------------------------------------------
unique(mapGCAMBasins@data$subRegion) # Check Available Regions
data = data.frame(subRegion=c("Negro","La_plata","Great","New_England","Indus","Zambezi"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
metis.mapsProcess(polygonTable=data,
                  folderName = "vignetteMaps",
                  mapTitleOn = F)

# World Countries
#------------------------------------------
unique(mapCountries@data$subRegion) # Check Available Regions
data = data.frame(subRegion=c("Colombia","China","India","Spain","Ghana","Iran"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
metis.mapsProcess(polygonTable=data,
                  folderName = "vignetteMaps",
                  mapTitleOn = F)


# World States
#------------------------------------------
unique(mapStates@data$subRegion) # Check Available Regions
data = data.frame(subRegion=c("Punjab","FL","TX","Faryab","Assam","Lac"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
metis.mapsProcess(polygonTable=data,
                  folderName = "vignetteMaps",
                  mapTitleOn = F)


# Select Maps
#------------------------------------------
data = data.frame(subRegion=c("Colombia","China","India"),
                  x=c(2050,2050,2050),
                  value=c(5,10,15))

# Auto selection by metis will choose metis::mapCountries
metis.mapsProcess(polygonTable=data,
                  folderName = "vignetteChooseMap",
                  mapTitleOn = F)

# User can specify that they want to plot this data on metis::mapGCAMReg32
metis.mapsProcess(polygonTable=data,
                  subRegShape = metis::mapGCAMReg32,
                  folderName = "vignetteChooseMap",
                  nameAppend = "Chosen",
                  mapTitleOn = F)


# Select US Compact
#------------------------------------------
data = data.frame(subRegion=c("AK","HI","PR","MO","TX","WY"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
metis.mapsProcess(polygonTable=data,
                  subRegShape=metis::mapUS52Compact,
                  folderName = "vignetteMaps", mapTitleOn = F)


# Select US Compact Counties
#------------------------------------------
unique(mapUS52CountyCompact@data$subRegion) # Check subRegion Names
unique(mapUS52CountyCompact@data$subRegionAlt) # Check Alternate names
data = data.frame(subRegion=c("Aleutians West_AK","Sabana Grande_PR","Kalawao_HI","Orange_IN","Putnam_FL","Ellis_KS"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
metis.mapsProcess(polygonTable=data,
                  subRegShape=metis::mapUS52CountyCompact,
                  folderName = "vignetteMaps",
                  nameAppend = "_Alt",
                  mapTitleOn = F)


# Custom Subset Existing
#------------------------------------------
shapeSubset <- metis::mapStates # Read in World States shape file
shapeSubset <- shapeSubset[shapeSubset@data$region %in% c("Colombia"),] # Subset the shapefile to Colombia
shapeSubset@data <- droplevels(shapeSubset@data)
shapeSubset@data <- shapeSubset@data %>% dplyr::rename(states=subRegion) # Lets assume the subRegion column was called "states"
metis.map(shapeSubset,fillCol="states") # View custom shape
head(shapeSubset@data) # review data
unique(shapeSubset@data$states) # Get a list of the unique subRegions

# Plot data on subset
data = data.frame(states=c("Cauca","Valle del Cauca","Antioquia","Córdoba","Bolívar","Atlántico"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
metis.mapsProcess(polygonTable=data,
                  subRegShape = shapeSubset,
                  subRegCol = "states",
                  subRegType = "ColombiaStates",
                  folderName = "vignetteMaps_shapeSubset",
                  mapTitleOn = F)


# Subset Crop to another
#------------------------------------------
shapeSubRegions <- metis::mapUS49County
shapeCropTo <- metis::mapUS49
shapeCropTo <- shapeCropTo[shapeCropTo@data$subRegion %in% c("TX"),]
shapeCropTo@data <- droplevels(shapeCropTo@data)
shapeCrop<- sp::spTransform(shapeCropTo,raster::crs(shapeSubRegions))
shapeCrop <-raster::crop(shapeSubRegions,shapeCropTo)
shapeCrop@data <- shapeCrop@data%>%dplyr::select(subRegion)
shapeCrop$subRegion%>%unique() # Check subRegion names
metis.map(shapeCrop)

# Plot data on subset
data = data.frame(county=c("Wise_TX","Scurry_TX","Kendall_TX","Frio_TX","Hunt_TX","Austin_TX"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
metis.mapsProcess(polygonTable=data,
                  subRegShape = shapeCrop,
                  subRegCol = "county",
                  subRegType = "TexasCounties",
                  folderName = "vignetteMaps_shapeCrop",
                  mapTitleOn = F)


# Crop to Boundary
#------------------------------------------
data = data.frame(subRegion = c("India","China"), year=c(2010,2010),value = c(32,54))
metis.mapsProcess(polygonTable = data, mapTitleOn = F, folderName = "vignetteMaps", cropToBoundary=F, )
metis.mapsProcess(polygonTable = data, mapTitleOn = F, folderName = "vignetteMaps", cropToBoundary=T,
                  nameAppend="Cropped")


# Extend
#------------------------------------------
data = data.frame(
  subRegion = c("India","China"), year=c(2010,2010), value = c(32,54))
metis.mapsProcess(polygonTable = data, mapTitleOn=F, folderName = "vignetteMaps",
                  #cropToBoundary =T,
                  extension = T, nameAppend="Extended")

# Can increase the extnded boundaries by using expandPercent
metis.mapsProcess(polygonTable = data, mapTitleOn=F, folderName = "vignetteMaps",
                  cropToBoundary =T,
                  extension = T, nameAppend="Extended10", expandPercent = 50)


# Multi-Year
#------------------------------------------
data = data.frame(subRegion = c("Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece"),
                  year = c(rep(2025,5),
                           rep(2050,5),
                           rep(2075,5),
                           rep(2100,5)),
                  value = c(32, 38, 54, 63, 24,
                            37, 53, 23, 12, 45,
                            23, 99, 102, 85, 75,
                            12, 76, 150, 64, 90))
metis.mapsProcess(polygonTable = data, folderName ="multiYear",
                  cropToBoundary=T, extension = T )


# Multi-Class
#------------------------------------------
data = data.frame(subRegion = c("Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece"),
                  class = c(rep("municipal",5),
                            rep("industry",5),
                            rep("agriculture",5),
                            rep("transport",5)),
                  year = rep(2010,20),
                  value = c(32, 38, 54, 63, 24,
                            37, 53, 23, 12, 45,
                            23, 99, 102, 85, 75,
                            12, 76, 150, 64, 90))
metis.mapsProcess(polygonTable = data, folderName ="multiClass",
                  cropToBoundary=T, extension = T )


# Multi-Scenario Diff
#------------------------------------------
data = data.frame(subRegion = c("Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece"),
                  scenario = c("scen1","scen1","scen1","scen1","scen1",
                               "scen2","scen2","scen2","scen2","scen2",
                               "scen3","scen3","scen3","scen3","scen3"),
                  year = rep(2010,15),
                  value = c(32, 38, 54, 63, 24,
                            37, 53, 23, 12, 45,
                            40, 44, 12, 30, 99))
metis.mapsProcess(polygonTable = data, folderName ="multiScenario",
                  cropToBoundary=T, extension = T, scenRef="scen1", scenDiff=c("scen3"))


# Multi-Year DIff
#------------------------------------------
data = data.frame(subRegion = c("Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece"),
                  year = c(rep(2010,5),rep(2020,5),rep(2030,5)),
                  value = c(32, 38, 54, 63, 24,
                            37, 53, 23, 12, 45,
                            40, 45, 12, 50, 63))
metis.mapsProcess(polygonTable = data, folderName ="multiYear",
                  cropToBoundary=T, extension = T, xRef=2010, xDiff = c(2020))


# Scale Range
#------------------------------------------
data = data.frame(subRegion = c("Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece"),
                  scenario = c("scen1","scen1","scen1","scen1","scen1",
                               "scen2","scen2","scen2","scen2","scen2"),
                  year = rep(2010,10),
                  value = c(32, 38, 54, 63, 24,
                            37, 50, 23, 12, 45))
metis.mapsProcess(polygonTable = data, folderName ="scaleRange",
                  cropToBoundary=T, extension = T, scenRef="scen1",
                  scaleRange = c(0,50), scaleRangeDiffAbs = c(-100,100), scaleRangeDiffPrcnt = c(-60,60))


# Color Palettes
#------------------------------------------
data = data.frame(subRegion = c("Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece"),
                  scenario = c("scen1","scen1","scen1","scen1","scen1",
                               "scen2","scen2","scen2","scen2","scen2"),
                  year = rep(2010,10),
                  value = c(32, 38, 54, 63, 24,
                            37, 53, 23, 12, 45))
metis.mapsProcess(polygonTable = data, folderName ="colorPalettes",
                  cropToBoundary=T, extension = T, scenRef="scen1",
                  classPalette = "pal_wet", classPaletteDiff = "pal_div_BrGn")

data <- metis.readgcam (
  #gcamdatabase = "Path_to_GCAMdatabase",
  dataProjFile = metis::exampleGCAMproj,
  scenOrigNames = c("GCAM_SSP5","GCAM_SSP3"),
  scenNewNames = c("SSP5","SSP3"),
  paramsSelect = c("landAlloc", "elecByTechTWh","watSupRunoffBasin","pop"),  # From Param list ?metis.readgcam
  regionsSelect = c("India","China","Pakistan"))

