context("map Tests")
library(rmap); library(testthat); library(dplyr); library(sf)

test_that("map plots shape correctly", {

  mapx <- rmap::map(rmap::mapUS49, show = F, save=F)
  tVal1 <- length(mapx)
  testthat::expect_gt(tVal1,0)
})

test_that("map plots polygonData correctly", {

  data = data.frame(subRegion=c("FL","ID"),
                    value=c(1,3))
  mapx <- rmap::map(data, show = F, save=F)
  tVal1 <- length(mapx)
  testthat::expect_gt(tVal1,0)
})

test_that("map saves and outputs correctly", {


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
  mapx <- rmap::map(data,
                    xRef=2025, show=F, save=F)
  tVal1 <- length(mapx)
  testthat::expect_gt(tVal1,0)
})

test_that("map plots grid data correctly", {

  data = example_gridData_GWPv4To2015 %>%
    dplyr::filter(x == 2015);
  mapx <- rmap::map(data, show = F, save=F)
  tVal1 <- length(mapx)
  testthat::expect_gt(tVal1,0)
})

test_that("map plots multi-class, multi-x, multi-scen data correctly", {

  data = rmap::exampleMapDataClass %>%
    dplyr::filter(region %in% c("India","China","Pakistan"),
                  param %in% c("watWithdrawBySec","pop"),
                  x %in% c(2010,2015),
                  scenario %in% c("GCAM_SSP3","GCAM_SSP5"))
  mapx <- rmap::map(data, show = F, save=F,
                    xRef = "2010",
                    scenRef = "GCAM_SSP3",
                    background = T,
                    crop = F,
                    scaleRange = c(30,40),
                    scaleRangeDiffAbs = c(-100,100),
                    scaleRangeDiffPrcnt = c(-60,60),
                    zoom = 1)
  tVal1 <- length(mapx)
  testthat::expect_gt(tVal1,0)
})

test_that("map saves pdf", {
  mapx <- rmap::map(rmap::mapUS49, show = F, pdfpng="pdf")
  tVal1 <- length(mapx)
  testthat::expect_gt(tVal1,0)
})

test_that("map saves both pdf and png", {
  mapx <- rmap::map(rmap::mapUS49, show = F, pdfpng="both")
  tVal1 <- length(mapx)
  testthat::expect_gt(tVal1,0)
})


test_that("numeric2cat works", {
  # Create a list of ranges and categorical color scales for each parameter
  numeric2Cat_param <- list("param1",
                            "param2")
  numeric2Cat_breaks <- list(c(-Inf, 0.1,1.1,2.1,3.1,4.1,5.1,10.1,Inf),
                             c(-Inf, 0.1, 0.2, 0.4,Inf))
  numeric2Cat_labels <- list(c("0","1","2","3","4","5","10",">10"),
                             c(names(jgcricolors::jgcricol()$pal_scarcityCat)))
  numeric2Cat_palette <- list(c("0"="green","1"="#fee5d9","2"="#fcbba1",
                                "3"="#fc9272","4"="#fb6a4a","5"="#de2d26",
                                "10"="#a50f15",">10"="black"),
                              c("pal_scarcityCat")) # Can be a custom scale or an R brewer palette or an rmap palette
  numeric2Cat_legendTextSize <- list(c(0.7),
                                     c(0.7))
  numeric2Cat_list <-list(numeric2Cat_param = numeric2Cat_param,
                          numeric2Cat_breaks = numeric2Cat_breaks,
                          numeric2Cat_labels = numeric2Cat_labels,
                          numeric2Cat_palette = numeric2Cat_palette,
                          numeric2Cat_legendTextSize = numeric2Cat_legendTextSize); numeric2Cat_list

  data = data.frame(subRegion = c("CA","AZ","TX","NH","ID","OH",
                                  "CA","AZ","TX","NH","ID","OH"),
                    x = c(2050,2050,2050,2050,2050,2050,
                          2050,2050,2050,2050,2050,2050),
                    value = c(0,1,3,20,2,1,
                              0,0.1,0.3,0.2,0.25,0.5),
                    param = c(rep("param1",6),rep("param2",6)))

  mapx <- rmap::map(data = data,
            background = T,
            underLayer = rmap::mapCountries,
            numeric2Cat_list = numeric2Cat_list,
            show=F, save=F)
  tVal1 <- length(mapx)
  testthat::expect_gt(tVal1,0)
})

test_that("test projections", {
  # ESRI:54032  World Azimuthal Equidistant
  mapx <- rmap::map(rmap::mapUS49, show =F, save = F, crs="+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")
  tVal1 <- length(mapx)
  testthat::expect_gt(tVal1,0)
})


test_that("test layers and labels", {
  mapx <- rmap::map(data = rmap::mapUS49, show =F, save = F,
                    underLayer = rmap::mapCountries,
                    labels = T,
                    underLayerLabels = T,
                    underLayerColor = "red",
                    underLayerLwd = 0.5,
                    underLayerFill = "blue",
                    overLayer = rmap::mapGCAMBasins,
                    overLayerLabels = T,
                    overLayerColor = "green",
                    overLayerLwd = 2,
                    overLayerFill = "yellow",
                    crop = T,
                    labelSize = 3,
                    labelColor = "red",
                    labelFill = "white",
                    labelAlpha = 0.8,
                    labelBorderSize = 0.1,
                    background="black")
  tVal1 <- length(mapx)
  testthat::expect_gt(tVal1,0)
})

test_that("test multi-row multi-col", {
  data = data.frame(subRegion = c("Austria","Spain", "Italy", "Germany","Greece",
                                  "Austria","Spain", "Italy", "Germany","Greece",
                                  "Austria","Spain", "Italy", "Germany","Greece",
                                  "Austria","Spain", "Italy", "Germany","Greece"),
                    rcp = c(rep("RCP1",5),
                            rep("RCP2",5),
                            rep("RCP1",5),
                            rep("RCP2",5)),
                    gcm = c(rep("GCM1",5),
                            rep("GCM1",5),
                            rep("GCM2",5),
                            rep("GCM2",5)),
                    value = c(32, 38, 54, 63, 24,
                              37, 53, 23, 12, 45,
                              23, 99, 102, 85, 75,
                              12, 76, 150, 64, 90))
  mapx <- rmap::map(data = data, show=F, save=F,
            underLayer = rmap::mapCountries,
            row = "rcp",
            col = "gcm",
            background = T )
  tVal1 <- length(mapx)
  testthat::expect_gt(tVal1,0)
})




