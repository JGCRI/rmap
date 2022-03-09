library(rmap); library(dplyr); library(ggplot2)


# Grid Example
# a1 <- readRDS("C:/Z/metarepos/khan-etal_2021_NatureScientificData/figures/df_annual.RDS"); a1
# dataGridTest <- a1 %>% rename(scenario = rcp_gcm) %>%
#  filter (year == 2010)

# Load example data set from NASA SEDAC world population and reshape
# https://sedac.ciesin.columbia.edu/data/collection/gpw-v4
dataGridTest = rmap::example_gridData_GWPv4To2015 %>%
  dplyr::mutate(units="person",
                value=value/1,
                class="class1",
                scenario="scenario1")

dataGridTest = dataGridTest %>% bind_rows(dataGridTest%>%mutate(scenario="scenario2",value=value*10))
dataGridTest = dataGridTest %>% bind_rows(dataGridTest%>%mutate(class="class2",value=value*5))
dataGridTest$scenario%>%unique()
dataGridTest$x%>%unique()

fillColumn = "value"

theme_custom = theme(panel.background = element_rect(size =1, fill="red"),
      strip.text = element_text(size=15, color="blue"))

theme_ggplot = ggplot2::theme_dark()

# Test Grids
if(T){# Tests
# Need to assign the grid data to the "grid" argument
rmap::map(data=dataGridTest %>%
            filter(x %in% c(2015),
                   scenario %in% c("scenario1"),
                   class %in% c("class1")),
    folder = "rmapTEST_grid_Single"
    )

  # data=dataGridTest %>%
  #   filter(x %in% c(2015),
  #          scenario %in% c("scenario1"),
  #          class %in% c("class1"))
  # folder = "rmapTEST_grid_Single"

rmap::map(data=dataGridTest %>%
            filter(x %in% c(2015),
                   #scenario %in% c("scenario1"),
                   class %in% c("class1")),
          folder = "rmapTEST_grid_MultiScenario",
          scenRef = "scenario1"
)

rmap::map(data=dataGridTest %>%
            filter(#x %in% c(2015),
                   scenario %in% c("scenario1"),
                   class %in% c("class1")),
          folder = "rmapTEST_grid_MultiX",
          xRef = "2015",
          scenRef = "scenario1"
)

rmap::map(data=dataGridTest %>%
            filter(x %in% c(2015),
                   scenario %in% c("scenario1"),
                   #class %in% c("class1")
                   ),
          folder = "rmapTEST_grid_MultiClass",
          scenRef = "scenario1"
)

rmap::map(data=dataGridTest,
          folder = "rmapTEST_grid_MultiAll",
          scenRef = "scenario1",
          xRef = "1990"
)

rmap::map(data=dataGridTest %>%
            filter(x %in% c(2015)) %>%
            rename(multiFacet1=class,
                   multiFacet2=scenario),
          folder = "rmapTEST_grid_MultiFacet",
          col = "multiFacet1",
          row = "multiFacet2"
)

rmap::map(data=dataGridTest %>%
            filter(x %in% c(2015),
                   scenario %in% c("scenario1"),
                   class %in% c("class1")) %>%
            head(10000),
          folder = "rmapTEST_grid_background",
          background =T,
          alpha = 1,
          #zoom = -4,
          theme =theme_dark(),
          underLayer = rmap::mapCountries,
          overLayer = rmap::mapUS52,
          overLayerFill = "red",
          overLayerAlpha = 0.3
          #printFig = F
          )

}

# Test Polygons
if(T){

  # Single
  dataPoly_test = data.frame(subRegion = c("CA","ID","AK","MA","NV","NM"),
                    x = c(2050,2050,2050,2050,2050,2050),
                    value = c(5,10,15,34,2,7))
  a<-rmap::map(data=dataPoly_test,
            shape = rmap::mapUS52Compact,
            folder = "rmapTEST_poly_Single",
            underLayer = rmap::mapUS52Compact,
            labels=T,
            underLayerLabels = T,
            crop = F,
            legendSingleValue = T,
            legendSingleColor = "green"); a

  # Test Background colors
  a<-rmap::map(data=dataPoly_test,
               shape = rmap::mapUS52Compact,
               folder = "rmapTEST_poly_Single",
               underLayer = rmap::mapUS52Compact,
               labels=T,
               underLayerLabels = T,
               crop = F,
               background = "white"); a

  # Single
  dataPoly_test = data.frame(subRegion = c(1:6),
                             x = c(2050,2050,2050,2050,2050,2050),
                             value = c(5,10,15,34,2,7))
  a<-rmap::map(data=dataPoly_test,
               shape = rmap::mapGCAMBasins,
               folder = "rmapTEST_subRegionAlt",
               labels=T); a

  # data=dataPoly_test
  # folder = "rmapTEST_subRegionAlt"
  # labels=T


  # Single Custom Shape
  dataPoly_test = data.frame(subRegion = c("Punjab","Baluchistan","K.P.","Sind","Yazd","Tehran"),
                             x = c(2050,2050,2050,2050,2050,2050),
                             value = c(5,10,15,34,2,7))
  a<-rmap::map(data=dataPoly_test,
               shape=rmap::mapStates,
               folder = "rmapTEST_poly_SingleCustom",
               underLayer = rmap::mapCountries,
               labels=T,
               underLayerLabels = T,
               crop = T,
               legendSingleValue = T,
               legendSingleColor = "green",
               background = T); a

  # Single
  dataPoly_test = data.frame(subRegion = c("USA", "Argentina","Chile","Brazil"),
                             x = c(2050,2050,2050,2050),
                             value = c(5,10,15,34))
  a<-rmap::map(data=dataPoly_test,
               folder = "rmapTEST_poly_Single",
               crop = T); a

}

# Test Simple Map Direct
if(T){
  rmap::map(data=rmap::mapUS49,
            folder="rmapTEST_shape",
            labels = T, labelSize =3, labelFill="white",labelRepel=0,labelAlpha = 0.4)

  rmap::map(data=rmap::mapStatesdf%>%dplyr::filter(region=="Pakistan")%>%droplevels(),
            folder="rmapTEST_shape",
            labels = T, labelSize =3, labelFill="white",labelRepel=0,labelAlpha = 0.4)
}


# Test Xanthos gird
if(T){
a <- data.table::fread("C:/Z/models/im3components/output/resampled_wrf_to_xanthos_monthly_RAINC_mm_2009_01_to_2009_01.csv") %>%
  tibble::as_tibble() %>% dplyr::select(lat,lon,value=`2009_01`); a
a1<-rmap::map(data=a,
              alpha=0.8,
              legendType = "kmean",
              crop = T,
              background=T,
              #legendTitle = "XSD",
              underLayer = rmap::mapCountries,
              overLayer = rmap::mapUS49,
              overLayerLabels = T,
              labelSize = 2,
              labelFill = "white",
              labelAlpha = 0.7,
              #zoom=-1,
              labelRepel = 0)

#
a <- data.table::fread("C:/Z/models/argus/inst/extdata/exampleDataUSA_template.csv") %>%
  tibble::as_tibble(); a
a1<-rmap::map(data=a,
              alpha=0.8,
              legendType = "kmean",
              crop = T,
              background=T,
              legendTitle = "XSD",
              underLayer = rmap::mapUS49,
              #underLayerLabels = F,
              overLayer = rmap::mapUS49,
              #overLayerLabels = T,
              labelSize = 3,
              labels=T,
              #labelFill = "white",
              labelAlpha = 0.7,
              zoom=-1,
              labelRepel = 0
              )
}

# Test Shapefile
if(T){
library(rgdal)
examplePolyFolder<-paste("C:/Z/data/mapFiles/gis/shapefiles_Argentina",sep="")
examplePolyFile<-paste("ArgentinaLocalBasin",sep="")
x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
names(x@data); head(x@data);
sp::plot(x)
# Rename subRegion column
x@data <- x@data %>% dplyr::mutate(subRegion=cuenca);
# Generate some random data for each subRegion
data <- data.frame(subRegion=unique(x@data$subRegion),
                   value = 100*runif(length(unique(x@data$subRegion))));
head(data)

rmap::map(data,
          shape=x,
          underLayer=rmap::mapStates,
          underLayerLabels = T,
          zoom=-6)
}

# Map with rivers
if(T){
library(rmap)
library(rgdal)
library(dplyr)
examplePolyFolder<-paste("C:/Users/khan404/Downloads/Lakes_and_Rivers_Shapefile",sep="")
examplePolyFile<-paste("hydrography_l_rivers_v2",sep="")
x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
names(x@data); head(x@data);
# sp::plot(x)
# Rename subRegion column
x@data <- x@data %>% dplyr::mutate(subRegion=UIDENT,
                                   subRegion=as.character(subRegion));
x1 <- rmap::shape_to_df(x); head(x1)
# Generate some random data for each subRegion

head(data)
}


# CombinedOnly
if(T){
library(rmap);

dataa = data.frame(subRegion = c("Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece",
                                "Austria","Spain", "Italy", "Germany","Greece"),
                  scenario = c("scen1","scen1","scen1","scen1","scen1",
                               "scen2","scen2","scen2","scen2","scen2",
                               "scen3","scen3","scen3","scen3","scen3"),
                  value = c(32, 38, 54, 63, 24,
                            37, 53, 23, 12, 45,
                            40, 44, 12, 30, 99))

head(dataa)

mapx <- rmap::map(data = dataa,
                  underLayer = rmap::mapCountries,
                  diffOnly = F,
                  save=T,
                  show=F,
                  scenRef = "scen1",
                  background = T)

names(mapx)
mapx[[1]]
mapx[[2]]

#
# data = dataa
# underLayer = rmap::mapCountries
# combinedOnly = T
# diffOnly = T
# save=F
# show=F
# scenRef = "GCAM_SSP2"
# #scenDiff = c("scen2"),
# background = T
}



# covr
if(T){
library(covr)
cov <- package_coverage()
as.data.frame(cov)
head(cov)
zero_coverage(cov)
}

# Intersected maps
if(T){
data <- data.frame(subRegion=c("New_England_X_Canada","New_England_X_USA"), value = c(5,10))
map_find_df(data)
rmap::map(data)
}


# JOSS review maczokni test

library(dplyr)
library(geodaData)
library(devtools)
devtools::install_github("JGCRI/rmap") # Update rmap
library(rmap)

ncovr <- geodaData::ncovr

# Subset first 10 rows to avoid repeated subRegions
mydata <- ncovr %>%
  dplyr::select(NAME,HR60) %>%
  head(10); mydata

# Will give you the relevant plot but multiple counties
# See how you can define your own columns as arguments
rmap::map(mydata,
          subRegCol = "NAME",
          valueCol = "HR60")

# Here you can see all the multiple counties labelled.
# rmap appends the State for you as it recognizes this.
# You can also see some of the other features of rmap here
# such as underLayer, zoom, labels etc.
rmap::map(mydata,
          subRegCol="NAME",
          valueCol="HR60",
          labels = T,
          labelSize = 3,
          labelRepel = T,
          underLayer = rmap::mapUS49,
          zoom=-2)



# Test for Brinda
library(rmap); library(dplyr); library(data.table); library(tibble)
total_basin_production_value_clean <- data.table::fread("C:/Z/models/tests/total_basin_production_value_clean.csv") %>% tibble::as_tibble()
plot_regions = c("South America_Southern",
                 "South America_Southern",
                 "Argentina", "Colombia", "Brazil")
plot_basin_production_value <- total_basin_production_value_clean %>%
  filter(year %in% c(2015, 2050), region %in% plot_regions, scenario == "ARM_Reference") %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  mutate(param = "param1") %>%
  select(scenario, subRegion, param, value, year)

plot_map <- rmap::map(data=plot_basin_production_value,
                      #shape = rmap::mapIntersectGCAMBasin32Reg,
                      #folder = paste(getwd(), "/", PLOT_FOLDER, "/maps", sep = ""),
                      #nameAppend = paste("_LAC_production_v2", sep = ""),
                      xRef = 2015,
                      xDiff = 2050
                      )


plot_basin_production_value <- total_basin_production_value_clean %>%
  filter(year %in% c(2015, 2050), region %in% plot_regions, scenario == "ARM_Reference") %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  mutate(param = "param1") %>%
  mutate(scenario = paste0(scenario,".",year)) %>%
  select(scenario, subRegion, param, value)

plot_map <- rmap::map(plot_basin_production_value,
                      #folder = paste(getwd(), "/", PLOT_FOLDER, "/maps", sep = ""),
                      nameAppend = "_check",
                      scenRef = "ARM_Reference.2015",
                      scenDiff = c("ARM_Reference.2050"))


# Brinda DiffAbs

library(rmap); library(dplyr); library(data.table); library(tibble)
crop_bio_share_2050 <- data.table::fread("C:/Z/models/tests/crop_bio_share_2050.csv") %>% tibble::as_tibble()

plot_map <- rmap::map(crop_bio_share_2050,
                      folder = paste(getwd(),"/brinda", sep = ""),
                      title = paste("Basin crop/biomass land in 2050"),
                      scenRef = "ARM_Reference",
                      scenDiff = c("SW_high_CL_Reference", "ARM_Policy_CO2only",
                                   "ARM_Policy_ls_MAC_global", "SW_high_CL_Policy_ls_MAC_global"),
                      nameAppend = paste("_crop_bio_share_", "2050", sep = ""))

diff_crop_bio_share_2050 <- data.table::fread("C:/Z/models/tests/diff_crop_bio_share_2050.csv") %>% tibble::as_tibble()
plot_map <- rmap::map(diff_crop_bio_share_2050,
                      folder = paste(getwd(), "/brinda", sep = ""),
                      title = paste("Basin crop/biomass land"),
                      nameAppend = paste("_crop_bio_alloc_basin_policy_v3", sep = ""))


# Reviewer Reka Comments 28 Feb 2022
library(rmap)
library(geodaData)
library(dplyr)
library(sf)

# get data for testing
data("ncovr")
mydata <- ncovr %>% select(NAME, STATE_NAME, FIPS, HR60) %>% st_drop_geometry() %>%
  dplyr::left_join(rmap::mapUS52County@data, by="FIPS") %>%
  dplyr::mutate(value=HR60)
rmap::map(data=mydata)

# Counter Example
download.file("https://www2.census.gov/geo/tiger/GENZ2018/kml/cb_2018_us_county_20m.zip","counties.zip")
unzip("counties.zip")
counties_geojson <- sf::st_read("cb_2018_us_county_20m.kml")
mapthis <- dplyr::left_join(counties_geojson, mydata, by = c("GEOID" = "FIPS"))
tmap::qtm(mapthis, fill = "HR60")

# Tigris
tg_counties <- tigris::counties()
mapthis <- dplyr::left_join(tg_counties, mydata, by = c("GEOID" = "FIPS"))
tmap::qtm(mapthis, fill = "HR60")

# Example
library(rmap)
library(tidyverse)
library(readxl)
library(janitor)
library(sf); library(dplyr)

# read in new test data
download.file("https://data.london.gov.uk/download/gcse-results-by-borough/a8a71d73-cc48-4b30-9eb5-c5f605bc845c/gcse-results.xlsx",
              destfile = "gcse-results.xlsx")
gcse_results <- readxl::read_xlsx("gcse-results.xlsx", sheet = "2020-21")

# clean up test data
colnames <- paste0(gcse_results[1,], gcse_results[2,])
colnames <- gsub("NA", "", colnames)
names(gcse_results) <- colnames
gcse_results <- gcse_results %>%
  clean_names() %>%
  slice(4:36) %>%
  mutate(number_of_pupils_at_the_end_of_key_stage_4 = as.numeric(number_of_pupils_at_the_end_of_key_stage_4))

# try to map using Local Authority name
my_map <- rmap::map(gcse_results,
                    subRegCol = "area",
                    valueCol = "number_of_pupils_at_the_end_of_key_stage_4")

my_map <- rmap::map(gcse_results %>%
                      dplyr::mutate(
                    subRegion = area,
                    value = number_of_pupils_at_the_end_of_key_stage_4))


# Try with others
london_boroughs <- ne_states(country = "United Kingdom", returnclass = "sf") %>% filter(region == "Greater London")
gcse_results_sf <- left_join(london_boroughs, gcse_results,by = c("name" = "area"))
qtm(gcse_results_sf, fill = "number_of_pupils_at_the_end_of_key_stage_4")
