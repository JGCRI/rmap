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
          subRegion = "NAME",
          value = "HR60")

# Here you can see all the multiple counties labelled.
# rmap appends the State for you as it recognizes this.
# You can also see some of the other features of rmap here
# such as underLayer, zoom, labels etc.
rmap::map(mydata,
          subRegion="NAME",
          value="HR60",
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
library(rmap); library(geodaData); library(dplyr); library(sf); library(tidyverse)
library(readxl); library(janitor)

# Example 1
data("ncovr")
mydata <- ncovr %>%
  dplyr::select(NAME, STATE_NAME, FIPS, HR60) %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(rmap::mapUS52County, by="FIPS")
rmap::map(data=mydata, value = "HR60", legendTitle = "HR60")


# Download file: https://data.london.gov.uk/download/gcse-results-by-borough/a8a71d73-cc48-4b30-9eb5-c5f605bc845c/gcse-results.xlsx
# read in new test data
#download.file("https://data.london.gov.uk/download/gcse-results-by-borough/a8a71d73-cc48-4b30-9eb5-c5f605bc845c/gcse-results.xlsx",
#             destfile = "gcse-results.xlsx")
gcse_results <- readxl::read_xlsx("C:/Users/khan404/Downloads/gcse-results.xlsx", sheet = "2020-21")

# clean up test data
colnames <- paste0(gcse_results[1,], gcse_results[2,])
colnames <- gsub("NA", "", colnames)
names(gcse_results) <- colnames
gcse_results <- gcse_results %>%
  janitor::clean_names() %>%
  slice(4:36) %>%
  mutate(number_of_pupils_at_the_end_of_key_stage_4 = as.numeric(number_of_pupils_at_the_end_of_key_stage_4))

# try to map using Local Authority name
my_map <- rmap::map(gcse_results,
                    subRegion = "area",
                    value = "number_of_pupils_at_the_end_of_key_stage_4",
                    legendTitle = "Pupils at KS4", save=F)


# Issue: https://github.com/openjournals/joss-reviews/issues/4015#issuecomment-1152889162
library(rmap); library(geodaData); library(dplyr); library(sf); library(tidyverse)
library(readxl); library(janitor)

gcse_results <- read_xlsx("C:/Users/khan404/Downloads/gcse-results.xlsx", sheet = "2020-21")
gcse_results_2 <- read_xlsx("C:/Users/khan404/Downloads/gcse-results.xlsx", sheet = "2019-20")

# clean up test data
colnames <- paste0(gcse_results[1,], gcse_results[2,])
colnames <- gsub("NA", "", colnames)
names(gcse_results) <- colnames
gcse_results <- gcse_results %>%
  clean_names() %>%
  slice(4:36) %>%
  mutate(number_of_pupils_at_the_end_of_key_stage_4 = as.numeric(number_of_pupils_at_the_end_of_key_stage_4))

colnames <- paste0(gcse_results_2[1,], gcse_results_2[2,])
colnames <- gsub("NA", "", colnames)
names(gcse_results_2) <- colnames
gcse_results_2 <- gcse_results_2 %>%
  clean_names() %>%
  slice(4:36) %>%
  mutate(number_of_pupils_at_the_end_of_key_stage_4 = as.numeric(number_of_pupils_at_the_end_of_key_stage_4))

gcse_results_joined <- rbind(gcse_results %>% mutate(year = "2020"),
                             gcse_results_2 %>% mutate(year = "2019"))

# Multiple ggplots saved as a list which can be edited later
rmap::map(data = gcse_results_joined,
          subRegion = "area",
          value = "number_of_pupils_at_the_end_of_key_stage_4")->mapx

mapx$map_param_KMEANS
mapx$map_param_MEAN_KMEANS
mapx$map_param_MEAN_KMEANS + ggplot2::theme_dark()
ggsave("map_param_MEAN_KMEANS_dark.png",width=8,height=8)

# Comapre across time
rmap::map(data = gcse_results_joined,
          subRegion = "area",
          value = "number_of_pupils_at_the_end_of_key_stage_4",
          xRef = 2019,
          save =T, show =F) -> mapx

mapx$map_param_KMEANS_xDiffPrcnt
ggsave("map_param_MEAN_KMEANS.png",width=8,height=8)

gcse_results_joined_classes <- gcse_results_joined %>%
  dplyr::mutate(gender = "girls") %>%
  dplyr::bind_rows(gcse_results_joined %>%
                     dplyr::mutate(gender="boys",
                                   number_of_pupils_at_the_end_of_key_stage_4 =
                                     number_of_pupils_at_the_end_of_key_stage_4*runif(nrow(gcse_results_joined))))

rmap::map(data = gcse_results_joined_classes,
               class = "gender",
               subRegion = "area",
               xRef = 2019,
               value = "number_of_pupils_at_the_end_of_key_stage_4") -> mapx


mapx$map_param_KMEANS
mapx$map_param_KMEANS_xDiffPrcnt

gcse_results_joined_classes_scenario <- gcse_results_joined_classes %>%
  dplyr::mutate(income_level = "high_income") %>%
  dplyr::bind_rows(gcse_results_joined_classes %>%
                     dplyr::mutate(income_level="low_income",
                                   number_of_pupils_at_the_end_of_key_stage_4 =
                                     number_of_pupils_at_the_end_of_key_stage_4*runif(nrow(gcse_results_joined_classes))))

rmap::map(data = gcse_results_joined_classes_scenario,
          class = "gender",
          scenario = "income_level",
          subRegion = "area",
          xRef = 2019,
          scenRef = "low_income",
          value = "number_of_pupils_at_the_end_of_key_stage_4",
          save=T,show=F) -> mapx


mapx$map_param_KMEANS
mapx$map_param_KMEANS_DiffPrcnt

# Test Covid data
# Our World in Data JHU https://github.com/owid/covid-19-data/tree/master/public/data
# State vaccination data: https://github.com/owid/covid-19-data/raw/master/public/data/vaccinations/us_state_vaccinations.csv
# Prep Data and keep only country names
covid_data <- read.csv(url("https://github.com/owid/covid-19-data/raw/master/public/data/vaccinations/us_state_vaccinations.csv"))%>%
  tibble::as_tibble() %>%
  dplyr::select(subRegion=location,date,value=people_vaccinated_per_hundred) %>%
  dplyr::mutate(subRegion = dplyr::if_else(subRegion=="New York State","New York",subRegion)) %>%
  dplyr::filter(date == max(date),
                subRegion %in% rmap::mapUS49$subRegionAlt); covid_data

rmap::map(covid_data,
          title=paste0("People Vaccinated per hundered ",max(covid_data$date)),
          legendTitle = "People per 100")


# Brinda Diff test
library(rmap)
plot_map <- rmap::map(data="ls_irr_water_wd_basin_2050.csv",
                      title = paste("Basin livestock + irrigation water withdrawal in 2050"),
                      scenRef = "ARM_Reference",
                      scenDiff = c( "SW_high_CL_Reference","ARM_Policy_ls_MAC_global"),
                      nameAppend = paste("_water_wd_basin_", "2050", sep = ""),
                      pdfpng = 'pdf')

data="ls_irr_water_wd_basin_2050.csv"
title = paste("Basin livestock + irrigation water withdrawal in 2050")
scenRef = "ARM_Reference"
scenDiff = c( "ARM_Policy_ls_MAC_global")
nameAppend = paste("_water_wd_basin_", "2050", sep = "")
pdfpng = 'pdf'


library(rmap); library(dplyr)
shapeSubset <- rmap::mapStates # Read in World States shape file
shapeSubset <- shapeSubset %>% dplyr::filter(region == "Canada")
m1<-rmap::map(data=shapeSubset,
          labels=T,
          labelSize = 3,
          labelFill = "white",
          labelAlpha = 0.6,
          labelRepel = 2,
          underLayer=rmap::mapCountriesUS52,
          background=T,
          zoomx = -1)

library(rmap); library(dplyr)
shapeSubset <- rmap::mapStates # Read in World States shape file
shapeSubset <- shapeSubset %>% dplyr::filter(region == "Canada")
m1<-rmap::map(data=shapeSubset,
              labels=T,
              labelSize = 3,
              labelFill = "white",
              labelAlpha = 0.6,
              labelRepel = 2,
              underLayer=rmap::mapCountriesUS52,
              background=T,
              crs="+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")



# Issue #90
# https://github.com/JGCRI/rmap/issues/90
library(rmap)
data = data.frame("subRegion"=c("EU-12","EU-15"));
rmap::map(data, legendShowForce=T)
rmap::mapGCAMReg32$subRegion%>%unique()

# Issue #89
# https://github.com/JGCRI/rmap/issues/89
library(rmap)
rmap::map(mapGCAMReg32, nameAppend="_reg32")
rmap::map(mapGCAMReg32Uruguay, nameAppend="_reg32Uruguay")


# Paper Maps
library(rmap)
data = data.frame(subRegion = c("China","India","Pakistan","Iran","Afghanistan"),
                  value = c(5,10,15,34,2))
map(data)

library(rmap)
data = data.frame(subRegion = c("CA","FL","ID","MO","TX","WY"),
                  value = c(5,10,15,34,2,7))
map(data,
    underLayer = mapUS52Compact,
    crop_to_underLayer = T,
    labels = T)

library(rmap)
data = data.frame(subRegion = c("Spain","Germany","Austria","Greece","Italy",
                                "Spain","Germany","Austria","Greece","Italy",
                                "Spain","Germany","Austria","Greece","Italy"),
                  value = c(5,10,15,34,2,
                            15,50,34,50,20,
                            1,2,7,13,5),
                  scenario = c("scen1","scen1","scen1","scen1","scen1",
                               "scen2","scen2","scen2","scen2","scen2",
                               "scen3","scen3","scen3","scen3","scen3"))
map(data,
    underLayer = mapCountries,
    scenRef = "scen1",
    background = T)


library(rmap); library(dplyr);
a1<-readRDS("a1.RDS")
rmap::map(a1)


#...........
# Abby County maps
#-----------

library(rmap); library(dplyr); library(stats)
data1 = rmap::mapUS49County %>% as.data.frame() %>% dplyr::mutate(value=as.numeric(FIPS)*sample(runif(10000), n(), replace = TRUE)); data
rmap::map(data1, palette="Reds", show=F, nameAppend = "_reds")
data2 = rmap::mapUS49County %>% as.data.frame() %>% dplyr::mutate(value=log2(as.numeric(FIPS))*sample(runif(10000), n(), replace = TRUE)); data
rmap::map(data2, palette="Greens", show=F, nameAppend = "_greens")
data3 = rmap::mapUS49County %>% as.data.frame() %>% dplyr::mutate(value=as.numeric(FIPS)*sample(runif(10000), n(), replace = TRUE)); data
rmap::map(data3, palette="Blues", show=F, nameAppend = "_blues")

#...........
# Review https://github.com/openjournals/joss-reviews/issues/4015#issuecomment-1216784122
#-----------

library(stringr)
library(dplyr)
library(rmap)

# Get infectious disease data
download.file("https://data.chhs.ca.gov/dataset/03e61434-7db8-4a53-a3e2-1d4d36d6848d/resource/75019f89-b349-4d5e-825d-8b5960fc028c/download/odp_idb_2020_ddg_compliant.csv",
              destfile = "infect_disease_us_county.csv", mode ="wb")

i_disease = read.csv("infect_disease_us_county.csv") ; colnames(i_disease)

# Clean data : keep years 2018-2020 and cases of Malaria, Typhus, Dengue and Lyme diseases
i_4_disease <- i_disease %>%
  subset(grepl("Malaria|Typhus|Dengue|Lyme", Disease) & grepl("Total", Sex) & grepl("2018|2019|2020", Year)) %>%
  filter(County != "California")  %>% # csv contains a row with sum of cases for all counties in California
  mutate(County = paste0(County, "_CA")) # add proper state abbreviation to match built-in map counties

malaria_map_diff_2018 = map(i_4_disease[i_4_disease == "Malaria", ],
                            subRegion = "County",
                            value = "Cases",
                            save = T, show=F,
                            xRef = "2018",
                            underLayer = mapUS49County,
                            labels=T, legendSingleValue = 0, showNA = T)

malaria_map_diff_2018$map_param_KMEANS
malaria_map_diff_2018$map_param_KMEANS_xDiffAbs
malaria_map_diff_2018$map_param_KMEANS_xDiffPrcnt


# EU Test

library(rmap); library(dplyr);

colors_eu <- c("#68affc", "#a2e84f", "#9d0d6c", "#4aeeb6", "#fe2b1c", "#36e515",
               "#8115b4", "#63a122", "#da73f8", "#1c5f1e", "#fa1bfc", "#bcdeae",
               "#7a3003", "#20d8fd", "#fa718e", "#347383", "#fab5b5", "#5a396e",
               "#ed9845", "#1945c5", "#e8d746", "#b69cfd", "#544516", "#ad7484",
               "#aee39a", "#b1475c", "#42f18f", "#ff98cc", "#13a64f", rep("gray90",40)); colors_eu
names(colors_eu) <- c(eu,mapGCAMReg32$region); colors_eu

rmap::map(mapGCAMReg32EU,save=F, labels = T, palette = colors_eu)
