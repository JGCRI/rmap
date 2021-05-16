library(rmap); library(dplyr); library(ggplot2)


# Grid Example
# a1 <- readRDS("C:/Z/metarepos/khan-etal_2021_NatureScientificData/figures/df_annual.RDS"); a1
# dataGrid <- a1 %>% rename(scenario = rcp_gcm) %>%
#  filter (year == 2010)

# Load example data set from NASA SEDAC world population and reshape
# https://sedac.ciesin.columbia.edu/data/collection/gpw-v4
dataGrid = rmap::grid_pop_GPWv4To2015 %>%
  tidyr::gather(key="x", value="value",-lat,-lon) %>%
  dplyr::mutate(units="person",
                value=value/1,
                class="class1",
                scenario="scenario1")

dataGrid = dataGrid %>% bind_rows(dataGrid%>%mutate(scenario="scenario2",value=value*10))
dataGrid = dataGrid %>% bind_rows(dataGrid%>%mutate(class="class2",value=value*5))
dataGrid$scenario%>%unique()
dataGrid$x%>%unique()

fillColumn = "value"

theme_custom = theme(strip.background = element_rect(size =1, fill="lightblue1"),
      strip.text = element_text(size=15, color="blue"))

theme_ggplot = ggplot2::theme_dark()

if(T){# Tests
# Need to assign the grid data to the "grid" argument
rmap::map(grid=dataGrid %>%
            filter(x %in% c(2015),
                   scenario %in% c("scenario1"),
                   class %in% c("class1")),
    folder = "rmapTEST_Single",
    #theme_rmap = F,
    #theme_custom=theme_custom,
    #theme_ggplot = theme_ggplot,
    #scenRef = "scenario1"
    )

rmap::map(grid=dataGrid %>%
            filter(x %in% c(2015),
                   #scenario %in% c("scenario1"),
                   class %in% c("class1")),
          folder = "rmapTEST_MultiScenario",
          theme_rmap = F,
          #theme_custom=theme_custom,
          #theme_ggplot = theme_ggplot,
          scenRef = "scenario1"
)

rmap::map(grid=dataGrid %>%
            filter(#x %in% c(2015),
                   scenario %in% c("scenario1"),
                   class %in% c("class1")),
          folder = "rmapTEST_MultiX",
          theme_rmap = F,
          #theme_custom=theme_custom,
          #theme_ggplot = theme_ggplot,
          xRef = "2015",
          scenRef = "scenario1"
)

# grid=dataGrid %>%
#   filter(#x %in% c(2015),
#     scenario %in% c("scenario1"),
#     class %in% c("class1"))
# folder = "rmapTEST_MultiX"
# theme_rmap = F
# xRef = "2015"
# scenRef = "scenario1"

rmap::map(grid=dataGrid %>%
            filter(x %in% c(2015),
                   scenario %in% c("scenario1"),
                   #class %in% c("class1")
                   ),
          folder = "rmapTEST_MultiClass",
          theme_rmap = F,
          #theme_custom=theme_custom,
          #theme_ggplot = theme_ggplot,
          scenRef = "scenario1"
)

rmap::map(grid=dataGrid,
          folder = "rmapTEST_MultiAll",
          theme_rmap = F,
          #theme_custom=theme_custom,
          #theme_ggplot = theme_ggplot,
          scenRef = "scenario1",
          xRef = "1990"
)
}

# grid=dataGrid %>% filter(x %in% c(1990,2015))
# folder = "rmapTESTOUTPUTS"
# theme_rmap = F
# #theme_custom=theme_custom,
# #theme_ggplot = theme_ggplot,
# scenRef = "scenario1"


# Example
data = data.frame(subRegion = c("CA","FL","ID","MO","TX","WY"),
x = c(2050,2050,2050,2050,2050,2050),
value = c(5,10,15,34,2,7))
map(data,folder = "vignetteMaps", mapTitleOn = F)
