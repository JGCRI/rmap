library(rmap)

# Maps Available
#------------------------------------------
head(mapUS49@data) # To View data in shapefile
rmap::map(mapUS49, labels=T)


# US49
#------------------------------------------
data = data.frame(subRegion=c("CA","FL","ID","MO","TX","WY"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
data%>%head()
rmap::map(data=data,
                  folder = "vignetteMaps", mapTitleOn = F)

# US52
#------------------------------------------
data = data.frame(subRegion=c("AK","FL","ID","MO","TX","WY"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7));
data%>%head()
rmap::map(data=data,
                  folder = "vignetteMaps", mapTitleOn = F)

# Counties
#------------------------------------------
unique(mapUS49County@data$subRegion) # Check subRegion Names
unique(mapUS49County@data$subRegionAlt) # Check Alternate names
data = data.frame(subRegion=c("Pender_NC","Larue_KY","Jim Wells_TX","Orange_IN","Putnam_FL","Ellis_KS"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
data%>%head()
rmap::map(data=data,
                  folder = "vignetteMaps",
                  nameAppend = "_Alt",
                  mapTitleOn = F)


# GCAM 32
#------------------------------------------
unique(mapGCAMBasins@data$subRegion) # Check Available Regions
data = data.frame(subRegion=c("Colombia","China","EU-12","Pakistan","Middle East","Japan"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
rmap::map(data=data,
                  folder = "vignetteMaps",
                  mapTitleOn = F)


# GCAM Basins
#------------------------------------------
unique(mapGCAMBasins@data$subRegion) # Check Available Regions
data = data.frame(subRegion=c("Negro","La_plata","Great","New_England","Indus","Zambezi"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
rmap::map(data=data,
                  folder = "vignetteMaps",
                  mapTitleOn = F)

# World Countries
#------------------------------------------
unique(mapCountries@data$subRegion) # Check Available Regions
data = data.frame(subRegion=c("Colombia","China","India","Spain","Ghana","Iran"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
rmap::map(data=data,
                  folder = "vignetteMaps",
                  mapTitleOn = F)


# World States
#------------------------------------------
unique(mapStates@data$subRegion) # Check Available Regions
data = data.frame(subRegion=c("Punjab","FL","TX","Faryab","Assam","Lac"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
rmap::map(data=data,
                  folder = "vignetteMaps",
                  mapTitleOn = F)


# Select Maps
#------------------------------------------
data = data.frame(subRegion=c("Colombia","China","India"),
                  x=c(2050,2050,2050),
                  value=c(5,10,15))

# Auto selection by rmap will choose rmap::mapCountries
rmap::map(data=data,
                  folder = "vignetteChooseMap",
                  mapTitleOn = F)

# User can specify that they want to plot this data on rmap::mapGCAMReg32
rmap::map(data=data,
                  shape = rmap::mapGCAMReg32,
                  folder = "vignetteChooseMap",
                  nameAppend = "Chosen",
                  mapTitleOn = F)


# Select US Compact
#------------------------------------------
data = data.frame(subRegion=c("AK","HI","PR","MO","TX","WY"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
rmap::map(data=data,
                  shape = rmap::mapUS52Compact,
                  folder = "vignetteMaps", mapTitleOn = F)


# Select US Compact Counties
#------------------------------------------
unique(mapUS52CountyCompact@data$subRegion) # Check subRegion Names
unique(mapUS52CountyCompact@data$subRegionAlt) # Check Alternate names
data = data.frame(subRegion=c("Aleutians West_AK","Sabana Grande_PR","Kalawao_HI","Orange_IN","Putnam_FL","Ellis_KS"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
rmap::map(data=data,
                  shape=rmap::mapUS52CountyCompact,
                  folder = "vignetteMaps",
                  nameAppend = "_Alt",
                  mapTitleOn = F)


# Custom Subset Existing
#------------------------------------------
shapeSubset <- rmap::mapStates # Read in World States shape file
shapeSubset <- shapeSubset[shapeSubset@data$region %in% c("Colombia"),] # Subset the shapefile to Colombia
shapeSubset@data <- droplevels(shapeSubset@data)
shapeSubset@data <- shapeSubset@data %>% dplyr::rename(states=subRegion) # Lets assume the subRegion column was called "states"
rmap::map(shapeSubset,fillCol="states") # View custom shape
head(shapeSubset@data) # review data
unique(shapeSubset@data$states) # Get a list of the unique subRegions

# Plot data on subset
data = data.frame(states=c("Cauca","Valle del Cauca","Antioquia","C?rdoba","Bol?var","Atl?ntico"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
rmap::map(data = data,
                  shape = shapeSubset,
                  subRegCol = "states",
                  subRegType = "ColombiaStates",
                  folder = "vignetteMaps_shapeSubset",
                  mapTitleOn = F)


# Subset Crop to another
#------------------------------------------
shapeSubRegions <- rmap::mapUS49County
shapeCropTo <- rmap::mapUS49
shapeCropTo <- shapeCropTo[shapeCropTo@data$subRegion %in% c("TX"),]
shapeCropTo@data <- droplevels(shapeCropTo@data)
shapeCrop<- sp::spTransform(shapeCropTo,raster::crs(shapeSubRegions))
shapeCrop <-raster::crop(shapeSubRegions,shapeCropTo)
shapeCrop@data <- shapeCrop@data%>%dplyr::select(subRegion)
shapeCrop$subRegion%>%unique() # Check subRegion names
rmap::map(shapeCrop)

# Plot data on subset
data = data.frame(county=c("Wise_TX","Scurry_TX","Kendall_TX","Frio_TX","Hunt_TX","Austin_TX"),
                  x=c(2050,2050,2050,2050,2050,2050),
                  value=c(5,10,15,34,2,7))
rmap::map(data = data,
                  shape = shapeCrop,
                  subRegCol = "county",
                  subRegType = "TexasCounties",
                  folder = "vignetteMaps_shapeCrop",
                  mapTitleOn = F)


# Crop to Boundary
#------------------------------------------
data = data.frame(subRegion = c("India","China"), year=c(2010,2010),value = c(32,54))
rmap::map(data = data, mapTitleOn = F, folder = "vignetteMaps", cropToBoundary=F, )
rmap::map(data = data, mapTitleOn = F, folder = "vignetteMaps", cropToBoundary=T,
                  nameAppend="Cropped")


# Extend
#------------------------------------------
data = data.frame(subRegion = c("India","China"), year=c(2010,2010), value = c(32,54))
rmap::map(data = data, mapTitleOn = F, folder = "vignetteMaps",
          #cropToBoundary =T,
          background = T, nameAppend = "Extended")

# Can increase the extnded boundaries by using expandPercent
rmap::map(data = data, mapTitleOn = F, folder = "vignetteMaps",
          cropToBoundary = T,
          background = T, nameAppend = "Extended10", expandPercent = 50)


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
rmap::map(data = data,
                  folder = "multiYear",
                  cropToBoundary = T,
                  background = T)


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
rmap::map(data = data,
                  folder = "multiClass",
                  cropToBoundary = T,
                  background = T)


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
rmap::map(data = data,
                  folder = "multiScenario",
                  cropToBoundary = T,
                  background = T,
                  scenRef = "scen1",
                  scenDiff = c("scen3"))

data = data
folder = "multiScenario"
cropToBoundary = T
background = T
scenRef = "scen1"
scenDiff = c("scen3")

# Multi-Scenario Class Diff
#------------------------------------------
data = data.frame(subRegion = rep(c("Austria","Spain", "Italy", "Germany","Greece"),18),
                  scenario = rep(c("scen1","scen1","scen1","scen1","scen1",
                               "scen2","scen2","scen2","scen2","scen2",
                               "scen3","scen3","scen3","scen3","scen3"),6),
                  year = c(rep(2010,30),rep(2050,30),rep(2100,30)),
                  class = rep(c(rep("coal",15),rep("solar",15)),3),
                  value = c(32, 38, 54, 63, 24,
                            37, 53, 23, 12, 45,
                            40, 44, 12, 30, 99,
                            37, 53, 23, 12, 45,
                            40, 44, 12, 30, 99,
                            32, 38, 54, 63, 24,
                            c(32, 38, 54, 63, 24)*1.5,
                            c(37, 53, 23, 12, 45)*1.7,
                            c(40, 44, 12, 30, 99)*1.9,
                            c(37, 53, 23, 12, 45)*2.5,
                            c(40, 44, 12, 30, 99)*3,
                            c(32, 38, 54, 63, 24)*3.5,
                            c(32, 38, 54, 63, 24)*4,
                            c(37, 53, 23, 12, 45)*6,
                            c(40, 44, 12, 30, 99)*8,
                            c(37, 53, 23, 12, 45)*10,
                            c(40, 44, 12, 30, 99)*12,
                            c(32, 38, 54, 63, 24)*14))
rmap::map(data = data,
          folder = "multiClassScenYear",
          cropToBoundary = T,
          background = T,
          scenRef = "scen1",
          xRef = 2010)


# Multi-Year DIff
#------------------------------------------
data = data.frame(subRegion = c("Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece"),
                  year = c(rep(2010,5),rep(2020,5),rep(2030,5)),
                  value = c(32, 38, 54, 63, 24,
                            37, 53, 23, 12, 45,
                            40, 45, 12, 50, 63))
rmap::map(data = data,
                  folder = "multiYear",
                  cropToBoundary = T,
                  background = T,
                  xRef = 2010,
                  xDiff = c(2020))


# Scale Range
#------------------------------------------
data = data.frame(subRegion = c("Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece"),
                  scenario = c("scen1","scen1","scen1","scen1","scen1",
                               "scen2","scen2","scen2","scen2","scen2"),
                  year = rep(2010,10),
                  value = c(32, 38, 54, 63, 24,
                            37, 50, 23, 12, 45))
rmap::map(data = data, folder = "scaleRange",
                  cropToBoundary = T, background = T, scenRef = "scen1",
                  scaleRange = c(0, 50), scaleRangeDiffAbs = c(-100, 100),
                  scaleRangeDiffPrcnt = c(-60, 60))


# Color Palettes
#------------------------------------------
data = data.frame(subRegion = c("Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece"),
                  scenario = c("scen1","scen1","scen1","scen1","scen1",
                               "scen2","scen2","scen2","scen2","scen2"),
                  year = rep(2010,10),
                  value = c(32, 38, 54, 63, 24,
                            37, 53, 23, 12, 45))
rmap::map(data = data, folder = "colorPalettes",
                  cropToBoundary = T, background = T, scenRef = "scen1",
                  classPalette = "pal_wet", classPaletteDiff = "pal_div_BrGn")


# Numeric2Cat
#------------------------------------------

# Create a list of ranges and categorical color scales for each parameter
numeric2Cat_param <- list("param1",
                          "param2")
numeric2Cat_breaks <- list(c(-Inf, 0.1,1.1,2.1,3.1,4.1,5.1,10.1,Inf),
                           c(-Inf, 0.1, 0.2, 0.4,Inf))
numeric2Cat_labels <- list(c("0","1","2","3","4","5","10",">10"),
                           c("None (0<WSI<0.1)","Low (0.1<WSI<0.2)","Moderate (0.2<WSI<0.4)","Severe (WSI>0.4)"))
numeric2Cat_palette <- list(c("0"="green","1"="#fee5d9","2"="#fcbba1",
                              "3"="#fc9272","4"="#fb6a4a","5"="#de2d26",
                              "10"="#a50f15",">10"="black"),
                            c("pal_ScarcityCat")) # Can be a custom scale or an R brewer paletter or an rmap palette
numeric2Cat_legendTextSize <- list(c(0.7),
                                   c(0.7))
numeric2Cat_list <-list(numeric2Cat_param=numeric2Cat_param,
                        numeric2Cat_breaks=numeric2Cat_breaks,
                        numeric2Cat_labels=numeric2Cat_labels,
                        numeric2Cat_palette=numeric2Cat_palette,
                        numeric2Cat_legendTextSize=numeric2Cat_legendTextSize); numeric2Cat_list

data = data.frame(subRegion=c("CA","AZ","TX","NH","ID","OH",
                              "CA","AZ","TX","NH","ID","OH"),
                  x=c(2050,2050,2050,2050,2050,2050,
                      2050,2050,2050,2050,2050,2050),
                  value=c(0,1,3,20,2,1,
                          0,0.1,0.3,0.2,0.25,0.5),
                  param = c(rep("param1",6),rep("param2",6)))
rmap::map(data = data,
                  folder = "numeric2cat",
                  numeric2Cat_list = numeric2Cat_list)



# Comprehensive GCAM Example
#------------------------------------------

library(rmap)
library(dplyr)

dfParam <- rmap::exampleMapDataParam %>%
  dplyr::filter(region %in% c("India","China","Pakistan"),
                param %in% c("landAlloc", "elecByTechTWh","watWithdrawBySec","pop")); dfParam
dfClass <- rmap::exampleMapDataClass%>%
  dplyr::filter(region %in% c("India","China","Pakistan"),
                param %in% c("landAlloc", "elecByTechTWh","watWithdrawBySec","pop")); dfClass

# Plot data aggregated by param
map(data = dfParam, folder = "GCAMbyParam",
    cropToBoundary = T, background = T, xRange = c(2020,2030,2040,2050),
    scenRef = "SSP3", xRef = 2020, scaleRange = data.frame(param = c("landAlloc", "elecByTechTWh","watWithdrawBySec","pop"),
                                              min = c(0,0,0,0),
                                              max = c(10000,15000,2000,2000)))

# Plot data aggregated by Class1
map(data = dfClass, folder = "GCAMbyClass",
    cropToBoundary = T, background = T, xRange = c(2020,2030,2040,2050),
    scenRef = "SSP3", xRef = 2020)

data = dfClass
folder = "GCAMbyClass"
cropToBoundary = T
background = T
xRange = c(2020,2030,2040,2050)
scenRef = "SSP3"
xRef = 2020


# Colors
library(ggplot2)
library(rmap)
library(dplyr)

outputdir = paste0(getwd(),"/colorPalettes")
if(!dir.exists(outputdir)){dir.create(outputdir)}

for(pal_i in names(rmap::colors())){

a<- rmap::colors()[[pal_i]];a
if(is.null(names(a))){names(a)<-a};a
a1 <-data.frame(color=as.vector(a),label=names(a)) %>% distinct();a1
a2 <- expand.grid(X=1:ceiling(sqrt(nrow(a1))),Y=1:ceiling(sqrt(nrow(a1)))) %>% as.data.frame();a2
a3 <- a2[1:nrow(a1),] %>% bind_cols(a1); a3

ggplot(a3,aes(x=X,y=Y, fill=label))+
  scale_fill_manual(values=a)+
  geom_tile()+
  geom_label(aes(label=label),fill="white",alpha=0.8) +
  coord_fixed(ratio=1)+
  theme_void()+ theme(legend.position = "none")+
  ggtitle(paste0(pal_i))->p; p

ggsave(plot=p, file=paste0(outputdir,"/",pal_i,".png"),width=max(7,min(0.5*nrow(a1),30)),height=max(7,min(0.5*nrow(a1),30)),units="in")
}

