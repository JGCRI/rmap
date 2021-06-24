library(rmap); library(dplyr); library(ggplot2)


# Grid Example
# a1 <- readRDS("C:/Z/metarepos/khan-etal_2021_NatureScientificData/figures/df_annual.RDS"); a1
# dataGridTest <- a1 %>% rename(scenario = rcp_gcm) %>%
#  filter (year == 2010)

# Load example data set from NASA SEDAC world population and reshape
# https://sedac.ciesin.columbia.edu/data/collection/gpw-v4
dataGridTest = rmap::grid_pop_GPWv4To2015 %>%
  tidyr::gather(key="x", value="value",-lat,-lon) %>%
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
          facetCols = "multiFacet1",
          facetRows = "multiFacet2"
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
  dataPoly_test = data.frame(subRegion = c("CA","FL","ID","MO","TX","WY"),
                    x = c(2050,2050,2050,2050,2050,2050),
                    value = c(5,10,15,34,2,7))
  a<-rmap::map(data=dataPoly_test,
            folder = "rmapTEST_poly_Single",
            underLayer = rmap::mapUS52Compact,
            labels=T,
            underLayerLabels = T,
            cropToBoundary = F); a

  # Single
  dataPoly_test = data.frame(subRegion = c("USA", "Argentina","Chile","Brazil"),
                             x = c(2050,2050,2050,2050),
                             value = c(5,10,15,34))
  a<-rmap::map(data=dataPoly_test,
               folder = "rmapTEST_poly_Single",
               cropToBoundary = T); a

}

# Test Simple Map Direct
if(T){
  rmap::map(data=rmap::mapUS49,
            folder="rmapTEST_shape",
            labels = T, labelSize =3, labelFill="white",labelForce=0,labelAlpha = 0.4)

  rmap::map(data=rmap::mapStatesdf%>%dplyr::filter(region=="Pakistan")%>%droplevels(),
            folder="rmapTEST_shape",
            labels = T, labelSize =3, labelFill="white",labelForce=0,labelAlpha = 0.4)
}


# Test Xanthos gird
a <- data.table::fread("C:/Z/models/im3components/output/resampled_wrf_to_xanthos_monthly_RAINC_mm_2009_01_to_2009_01.csv") %>%
  tibble::as_tibble() %>% dplyr::select(lat,lon,value=`2009_01`); a
a1<-rmap::map(data=a,
              alpha=0.8,
              legendType = "kmean",
              cropToBoundary = T,
              background=T,
              legendTitle = "XSD",
              underLayer = rmap::mapCountries,
              overLayer = rmap::mapCountriesUS52,
              overLayerLabels = T,
              labelSize = 3,
              labelFill = "white",
              labelAlpha = 0.7,
              zoom=-1,
              labelForce = 0)

