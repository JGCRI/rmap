
library(rmap)
map <- rmap::map
colors <- rmap::colors

#----------------------------
# Example rmap.mapsProcess
#----------------------------
data = data.frame(
  subRegion = c("TX","AZ"),
  value = c(32,54))
map(data)


# Countries crop to Boundary
data = data.frame(subRegion = c("India","China"), value = c(32,54))
map(data, cropToBoundary=T)

# Countries crop to Boundary
data = data.frame(subRegion = c("La_plata","Amazon"),value = c(32,54))
map(data , cropToBoundary=T)

# Multi-Scenario Diff plots
data = data.frame(subRegion = c("TX","TX", "AZ", "AZ"),
                  scenario = c("scen1","scen2","scen1","scen2"),
                  value = c(32, 38, 54, 63))
map(data, scenRef="scen1")

# Multi-Year
data = data.frame(subRegion = c("TX","TX", "AZ", "AZ"),
                  year = c("2050","2100","2050","2100"), value = c(32, 38, 54, 63))
map(data, folder="multiyear")

# Multi-Class
data = data.frame(subRegion = c("TX","TX", "AZ", "AZ"),
                  class = c("class1","class2","class1","class2"),
                  value = c(32, 38, 54, 63))
map(data)


# Scale Range
data = data.frame(subRegion = c("TX","TX", "AZ", "AZ"),
                  scenario = c("scen1","scen2","scen1","scen2"),
                  value = c(32, 38, 54, 63))
map(data,
                  scaleRange = c(30,50), scaleRangeDiffPrcnt = c(10,30))

# Colors
data = data.frame(subRegion = c("TX","TX", "AZ", "AZ"),
                  scenario = c("scen1","scen2","scen1","scen2"),
                  value = c(32, 38, 54, 63))
map(data, scenRef="scen1",
                  classPalette = "pal_wet", classPaletteDiff = "pal_green")

# Extended Boundary
data = data.frame(
  subRegion = c("India","China"), value = c(32,54))
map(data,
background = T)

# Color Palette
colors("pal_hot")  # to test color palette
colors("pal_div_BrGn")  # to test color palette

# Map
library(rmap);  head(mapGCAMReg32@data)
rmap::mapplot(mapUS49, labels=T)



