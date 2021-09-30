#' load_data
#'
#' Load required data from the rmapdata package
#'
#' @export

load_data <- function() {

  print("Starting to load data from rmapdata...")

  if(dir.exists("temp")){
    unlink("temp",recursive=T)
    dir.create("temp")
  } else {
    dir.create("temp")
  }

  # Load rmapdata files from github repo:
  rmapdata_data <- c(
    "exampleMapDataParam",
    "exampleMapDataClass",
    "example_gridData_GWPv4To2015",
    "mapCountries",
    "mapStates",
    "mapGCAMReg32",
    "mapGCAMBasins",
    "mapHydroShed1",
    "mapHydroShed2",
    "mapHydroShed3",
    "mapUS52",
    "mapUS52Compact",
    "mapUS49",
    "mapUS52County",
    "mapUS52CountyCompact",
    "mapUS49County",
    "mapGCAMBasinsUS52",
    "mapGCAMBasinsUS49",
    "mapGCAMReg32US52",
    "mapCountriesUS52",
    "mapIntersectGCAMBasinCountry",
    "mapIntersectGCAMBasin32Reg",
    "mapIntersectGCAMBasinUS52",
    "mapIntersectGCAMBasinUS52County",
    "mapsSubRegions",
    "mapCountriesdf",
    "mapStatesdf",
    "mapGCAMReg32df",
    "mapGCAMBasinsdf",
    "mapHydroShed1df",
    "mapHydroShed2df",
    "mapHydroShed3df",
    "mapUS52df",
    "mapUS52Compactdf",
    "mapUS49df",
    "mapUS52Countydf",
    "mapUS52CountyCompactdf",
    "mapUS49Countydf",
    "mapGCAMBasinsUS52df",
    "mapGCAMBasinsUS49df",
    "mapGCAMReg32US52df",
    "mapCountriesUS52df","mapIntersectGCAMBasinCountrydf",
    "mapIntersectGCAMBasin32Regdf",
    "mapIntersectGCAMBasinUS52df",
    "mapIntersectGCAMBasinUS52Countydf")

  for(data_i in rmapdata_data){
    url_i <- paste0("https://github.com/JGCRI/rmapdata/blob/main/data/",data_i,".rda?raw=true")
    download.file(url_i, destfile= "temp/tempfile.rda", mode = "wb")
    load("temp/tempfile.rda")
    unlink("temp/tempfile.rda")
  }

  unlink("temp")

  print("Finished loading data from rmapdata...")

}
