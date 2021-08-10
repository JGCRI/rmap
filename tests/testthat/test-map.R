context("map Tests")
library(rmap); library(testthat); library(dplyr)

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
                    xRef=2025)
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

