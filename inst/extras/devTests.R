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
