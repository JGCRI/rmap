
library(tibble);library(dplyr);library(rgdal);library(devtools);library(rmap); library(tmaptools)
library(rgeos); library(rgcam); library(maptools); library(rnaturalearth);library(rmapshaper)
library(sf)

redoMaps = T

dataFileFolder <- "C:/Z/data/mapFiles"

# Current Data
#data(package="rmap")

#-----------------
# World Maps (Countries, States)
#-----------------

# Worldmap countries
#-------------------
if(redoMaps){
  library(rnaturalearth)
  x=rnaturalearth::ne_countries(returnclass="sf", scale='medium')
  #x <- sf::st_read("C:/Z/data/mapFiles/gis/metis/naturalEarth/ne_10m_admin_0_countries.shp")
  mapx <- x
  mapx <- mapx %>%
    dplyr::select(subRegion=admin,subRegionAlt=adm0_a3, pop_est, gdp_md_est, pop_year, lastcensus, gdp_year) %>%
    dplyr::mutate(region=subRegion,subRegionType="country", source="rnaturalearth::ne_countries()",
                  subRegion = as.character(subRegion),
                  subRegion=if_else(subRegion=="United States of America","USA",subRegion))
  mapx <- mapx[!grepl("Antarctica",mapx$subRegion),]
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area,
                  region = dplyr::if_else(region=="United States of America","USA",region),
                  subRegion = dplyr::if_else(subRegion=="United States of America","USA",subRegion))
  mapCountries <- mapx
  mapCountries <- mapCountries %>%
    dplyr::mutate(name="mapCountries") %>%
    dplyr::select(subRegion,region,subRegionAlt,source, geometry, area, name,
                  pop_est, gdp_md_est, pop_year, lastcensus, gdp_year)
  sf::st_crs(mapCountries) <- 4326
  use_data(mapCountries, version=3, overwrite=T)
}

# Worldmap states
#-------------------
if(redoMaps){
  # From rnaturalearth
  library(rnaturalearth)
  x <- rnaturalearth::ne_states(returnclass="sf")
  mapx <- x
  mapx <- mapx %>%
    dplyr::select(region=admin,subRegion=name,subRegionAlt=postal) %>%
    dplyr::mutate(subRegionType="states", source="rnaturalearth::ne_states()",
                  region = as.character(region),
                  region=if_else(region=="United States of America","USA",region))
  mapx <- mapx[!grepl("Antarctica",mapx$region),]
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  mapx <- mapx[!is.na(mapx$subRegion),]
  mapx %>% filter(is.na(subRegion))
  mapx <- mapx %>%
    dplyr::mutate(subRegion = if_else(is.na(subRegion),subRegionAlt,subRegion)) %>%
    dplyr::filter(!is.na(subRegion));
  # Renaming subregions in mapStates so that states with USPS can be plotted with states with full names in other countries
  mapStates <- mapx
  mapStates <- mapStates %>%
    dplyr::mutate(
      subRegionAlt = as.character(subRegionAlt),
      subRegion = as.character(subRegion),
      subRegion1 = subRegionAlt,
      subRegionAlt = subRegion,
      subRegion = subRegion1,
      subRegion = dplyr::case_when(region != "USA" ~ subRegionAlt,
                                   TRUE ~ subRegion)
    ) %>%
    dplyr::select(-subRegion1)

  mapStates <- mapStates %>%
    dplyr::mutate(name="mapStates") %>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry)
  sf::st_crs(mapStates) <- 4326
  use_data(mapStates, version=3, overwrite=T)

  library(rmapshaper); library(tmap)
  format(object.size(mapUS49), units="Mb")
  format(object.size(mapStates), units="Mb")
  mapStates1 <- sf::st_simplify(mapStates, preserveTopology = TRUE, dTolerance = 10)
  mapStates1 <- rmapshaper::ms_simplify(as(mapStates,"Spatial"), keep = 0.1, keep_shapes = T) %>%
    sf::st_as_sf()
  mapStates1 <- tmaptools::simplify_shape(shp = mapStates, fact = 0.1, keep.units = FALSE, keep.subunits = FALSE)
  format(object.size(mapStates1), units="Mb")
  plot(mapStates1[1])
  rmap::map(mapGCAMReg32)
  rmap::map(mapStates1 %>% dplyr::filter(region=="United Kingdom"))
  rmap::map(mapStates1 %>% dplyr::filter(region=="Pakistan"))
  rmap::map(mapStates %>% dplyr::filter(region=="United Kingdom"))
  plot(mapStates[1])
  format(object.size(mapStates1), units="Mb")
  mapStates2<-rgeos::gSimplify(as(mapStates,"Spatial"), byid=TRUE, width=1) %>%
    sf::st_as_sf()
  format(object.size(mapStates2), units="Mb")
  rmap::map(mapStates2 %>% dplyr::filter(region=="United Kingdom"))
  rmap::map(mapStates %>% dplyr::filter(region=="United Kingdom"))
  plot(mapStates1[1])
  plot(mapStates[1])

  # Check
  shapeSubset <- mapStates # Read in World States shape file
  shapeSubset <- shapeSubset %>% dplyr::filter(region=="United Kingdom") # Subset the shapefile to Colombia
  plot(shapeSubset[,"subRegion"]) # View custom shape

  # Check
  shapeSubset <- mapStates # Read in World States shape file
  shapeSubset <- shapeSubset %>% dplyr::filter(region=="Colombia") # Subset the shapefile to Colombia
  plot(shapeSubset[,"subRegion"]) # View custom shape

  # Check
  shapeSubset <- mapStates # Read in World States shape file
  shapeSubset <- shapeSubset %>% dplyr::filter(region=="Pakistan") # Subset the shapefile to Colombia
  plot(shapeSubset[,"subRegion"]) # View custom shape

  # Check
  shapeSubset <- mapStates # Read in World States shape file
  shapeSubset <- shapeSubset %>% dplyr::filter(subRegion=="Punjab") # Subset the shapefile to Colombia
  plot(shapeSubset[,"region"]) # View custom shape

}

#-----------------
# GCAM Maps (Regions, Basins, Land)
#-----------------

# GCAM 32 Regions
#------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gcam_boundary_zenodo4688451/main_outputs",sep="")
  examplePolyFile<-paste("region_boundaries_moirai_combined_3p1_0p5arcmin",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  idMapping <- tibble::as_tibble(data.table::fread(paste(dataFileFolder,"/gis/metis/gcam/GCAM_region_names.csv",sep=""),skip=4,header=T))
  mapx@data <- mapx@data %>%
    dplyr::mutate(reg_id=as.character(reg_id))%>%
    left_join(idMapping%>%dplyr::mutate(reg_id=as.character(GCAM_region_ID)),by="reg_id")%>%
    dplyr::mutate(subRegionType="GCAMReg32",
                  source="https://doi.org/10.5281/zenodo.4688451",
                  subRegion=region,
                  region=subRegion,
                  subRegionAlt=reg_id,
                  region=gsub("-","_",region),
                  subRegion=gsub("-","_",subRegion))%>%
    dplyr::select(region,subRegion,subRegionAlt,subRegionType,source)
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  mapx <- mapx[!is.na(mapx$subRegion),]
  mapx %>% filter(is.na(subRegion))
  mapGCAMReg32 <- mapx
  mapGCAMReg32 <- mapGCAMReg32 %>%
    dplyr::mutate(name="mapGCAMReg32")%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry)%>%
    st_transform(st_crs(4326))
  sf::st_crs(mapGCAMReg32) <- 4326
  use_data(mapGCAMReg32, version=3, overwrite=T)

}


# GCAM Basins
#------------------
if(redoMaps){

  examplePolyFolder<-paste(dataFileFolder,"/gcam_boundary_zenodo4688451/main_outputs",sep="")
  examplePolyFile<-paste("glu_boundaries_moirai_combined_3p1_0p5arcmin",sep="")
  y = sf::st_read(paste0(examplePolyFolder,"/",examplePolyFile,".shp"))
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)

  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=glu_nm, subRegionAlt=glu_id) %>%
    dplyr::mutate(source="https://doi.org/10.5281/zenodo.4688451",
                  region = subRegion,
                  region=gsub("-","_",region),
                  subRegion=gsub("-","_",subRegion),
                  subRegionType="GCAMBasin",
                  subRegion=gsub("_Basin","",subRegion),
                  subRegion=gsub("Rfo","Rio",subRegion),
                  subRegion=gsub("-","_",subRegion),
                  subRegion=case_when(subRegion=="HamuniMashkel"~"Hamun_i_Mashkel",
                                      subRegion=="Yucat_µ„ön_Peninsula"~"Yucatan_Peninsula",
                                      subRegion=="Rh(ne"~"Rhone",
                                      subRegion=="Hong_(Red_River)"~"Hong_Red_River",
                                      TRUE~subRegion))%>%
    dplyr::select(region,subRegion,subRegionAlt,subRegionType,source)
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  mapx <- mapx[!is.na(mapx$subRegion),]

  # GCAM Basins
  mapGCAMBasins <- mapx
  mapGCAMBasins <- mapGCAMBasins %>%
    dplyr::mutate(name="mapGCAMBasins")%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry)%>%
    sf::st_transform(st_crs(4326))
  sf::st_crs(mapGCAMBasins) <- 4326
  use_data(mapGCAMBasins, version=3, overwrite=T)
}

# GCAM Land
#------------------
if(redoMaps){

  examplePolyFolder<-paste(dataFileFolder,"/gcam_boundary_zenodo4688451/main_outputs",sep="")
  examplePolyFile<-paste("reg_glu_boundaries_moirai_combined_3p1_0p5arcmin",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)

  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::mutate(subRegion=paste0(glu_nm,"_X_",reg_nm), subRegionAlt=key,
                  subRegion_GCAMBasin=glu_nm, subRegionAlt_GCAMBasin=glu_id,
                  subRegion_GCAMReg32=reg_nm, subRegionAlt_GCAMReg32=reg_id) %>%
    dplyr::mutate(source="https://doi.org/10.5281/zenodo.4688451",
                  region=subRegion,
                  region=gsub("-","_",region),
                  subRegion=gsub("-","_",subRegion),
                  subRegion=gsub("_Basin","",subRegion),
                  subRegion=gsub("Rfo","Rio",subRegion),
                  subRegion=gsub("-","_",subRegion),
                  subRegion=case_when(subRegion=="HamuniMashkel"~"Hamun_i_Mashkel",
                                      subRegion=="Yucat_µ„ön_Peninsula"~"Yucatan_Peninsula",
                                      subRegion=="Rh(ne"~"Rhone",
                                      subRegion=="Hong_(Red_River)"~"Hong_Red_River",
                                      TRUE~subRegion),
                  subRegion_GCAMBasin=gsub("-","_",subRegion_GCAMBasin),
                  subRegion_GCAMBasin=gsub("_Basin","",subRegion_GCAMBasin),
                  subRegion_GCAMBasin=gsub("Rfo","Rio",subRegion_GCAMBasin),
                  subRegion_GCAMBasin=gsub("-","_",subRegion_GCAMBasin),
                  subRegion_GCAMBasin=case_when(subRegion_GCAMBasin=="HamuniMashkel"~"Hamun_i_Mashkel",
                                      subRegion_GCAMBasin=="Yucat_µ„ön_Peninsula"~"Yucatan_Peninsula",
                                      subRegion_GCAMBasin=="Rh(ne"~"Rhone",
                                      subRegion_GCAMBasin=="Hong_(Red_River)"~"Hong_Red_River",
                                      TRUE~subRegion_GCAMBasin))%>%
    dplyr::select(region,subRegion,subRegionAlt,source,
                  subRegion_GCAMBasin, subRegionAlt_GCAMBasin,
                  subRegion_GCAMReg32, subRegionAlt_GCAMReg32)
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)

  # GCAM Basins
  mapGCAMLand <- mapx
  mapGCAMLand <- mapGCAMLand %>%
    dplyr::mutate(name="mapGCAMLand")%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry,
                  subRegion_GCAMBasin, subRegionAlt_GCAMBasin,
                  subRegion_GCAMReg32, subRegionAlt_GCAMReg32)%>%
    st_transform(st_crs(4326))
  sf::st_crs(mapGCAMLand) <- 4326
  use_data(mapGCAMLand, version=3, overwrite=T)
}


#-----------------
# Hydrology Maps (HydroShed, HUC)
#-----------------


# Hydro sheds
# https://www.hydrosheds.org/page/hydrobasins
# Lehner, B., Grill G. (2013): Global river hydrography and network routing:
# baseline data and new approaches to study the world’s large river systems.
# Hydrological Processes, 27(15): 2171–2186. Data is available at www.hydrosheds.org

# HydroSheds Level 1
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/other/subbasin_hydrobasin",sep="")
  examplePolyFile<-paste("hydrobasins_level_1",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HYBAS_ID) %>%
    dplyr::mutate(region=subRegion,subRegionType="hydroshed1", subRegionAlt=subRegion,source="https://www.hydrosheds.org/page/hydrobasins")%>%
    dplyr::select(region,subRegion,subRegionAlt,subRegionType,source)
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  mapx <- mapx[!is.na(mapx$subRegion),]
  mapHydroShed1 <- mapx
  mapHydroShed1 <- mapHydroShed1 %>%
    dplyr::mutate(name="mapHydroShed1")%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry)
  sf::st_crs(mapHydroShed1) <- 4326
  use_data(mapHydroShed1, version=3, overwrite=T)
}

# HydroSheds Level 2#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/other/subbasin_hydrobasin",sep="")
  examplePolyFile<-paste("hydrobasins_level_2",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HYBAS_ID, SUB_AREA) %>%
    dplyr::mutate(region=subRegion,subRegionType="hydroshed2", subRegionAlt=subRegion,source="https://www.hydrosheds.org/page/hydrobasins")%>%
    dplyr::select(region,subRegion,subRegionAlt,subRegionType,source)
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  mapx <- mapx[!is.na(mapx$subRegion),]
  mapHydroShed2 <- mapx
  mapHydroShed2 <- mapHydroShed2 %>%
    dplyr::mutate(name="mapHydroShed2")%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry)
  sf::st_crs(mapHydroShed2) <- 4326
  use_data(mapHydroShed2, version=3, overwrite=T)
}

# HydroSheds Level 3
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/other/subbasin_hydrobasin",sep="")
  examplePolyFile<-paste("hydrobasins_level_3",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HYBAS_ID, SUB_AREA) %>%
    dplyr::mutate(region=subRegion,subRegionType="hydroshed3", subRegionAlt=subRegion,source="https://www.hydrosheds.org/page/hydrobasins")%>%
    dplyr::select(region,subRegion,subRegionAlt,subRegionType,source)
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  mapx <- mapx[!is.na(mapx$subRegion),]
  mapHydroShed3 <- mapx
  mapHydroShed3 <- mapHydroShed3 %>%
    dplyr::mutate(name="mapHydroShed3")%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry)
  sf::st_crs(mapHydroShed3) <- 4326
  use_data(mapHydroShed3, version=3, overwrite=T)
}

# HUC USGS
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

rgdal::writeOGR(obj=rmap::mapUS52HUC2,dsn=paste0("C:/Z/data/mapFiles/gis/usgs"),
                layer="mapUS52HUC2",driver="ESRI Shapefile", overwrite_layer=TRUE)
rgdal::writeOGR(obj=rmap::mapUS49HUC2,dsn=paste0("C:/Z/data/mapFiles/gis/usgs"),
                layer="mapUS49HUC2",driver="ESRI Shapefile", overwrite_layer=TRUE)
rgdal::writeOGR(obj=rmap::mapUS52HUC4,dsn=paste0("C:/Z/data/mapFiles/gis/usgs"),
                layer="mapUS52HUC4",driver="ESRI Shapefile", overwrite_layer=TRUE)
rgdal::writeOGR(obj=rmap::mapUS49HUC4,dsn=paste0("C:/Z/data/mapFiles/gis/usgs"),
                layer="mapUS49HUC4",driver="ESRI Shapefile", overwrite_layer=TRUE)

# USGS HUC Levels
# US52 HUC 2
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/usgs",sep="")
  examplePolyFile<-paste("mapUS52HUC2",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(region,subRegion=subRegn,subRegionAlt=sbRgnAl,subRegionType=sbRgnTy,source, STATES, name)
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  mapx <- mapx[!is.na(mapx$subRegion),]
  #---------------------
  mapUS52HUC2 <- mapx
  mapUS52HUC2 <- mapUS52HUC2 %>%
    dplyr::mutate(name="mapUS52HUC2")%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry) %>%
    st_set_crs(st_crs(4326))
  sf::st_crs(mapUS52HUC2) <- 4326
  use_data(mapUS52HUC2, version=3, overwrite=T)
}

# US49 HUC 2
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/usgs",sep="")
  examplePolyFile<-paste("mapUS49HUC2",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(region,subRegion=subRegn,subRegionAlt=sbRgnAl,subRegionType=sbRgnTy,source, STATES, name)
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  mapx <- mapx[!is.na(mapx$subRegion),]
  #---------------------
  mapUS49HUC2 <- mapx
  mapUS49HUC2 <- mapUS49HUC2 %>%
    dplyr::mutate(name="mapUS49HUC2")%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry)%>%
    st_set_crs(st_crs(4326))
  sf::st_crs(mapUS49HUC2) <- 4326
  use_data(mapUS49HUC2, version=3, overwrite=T)
}

# US52 HUC 4
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/usgs",sep="")
  examplePolyFile<-paste("mapUS52HUC4",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(region,subRegion=subRegn,subRegionAlt=sbRgnAl,subRegionType=sbRgnTy,source, STATES, name)
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  mapx <- mapx[!is.na(mapx$subRegion),]
  #---------------------
  mapUS52HUC4 <- mapx
  mapUS52HUC4 <- mapUS52HUC4 %>%
    dplyr::mutate(name="mapUS52HUC4")%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry)%>%
    st_set_crs(st_crs(4326))
  sf::st_crs(mapUS52HUC4) <- 4326
  use_data(mapUS52HUC4, version=3, overwrite=T)
}

# US49 HUC 4
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/usgs",sep="")
  examplePolyFile<-paste("mapUS49HUC4",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(region,subRegion=subRegn,subRegionAlt=sbRgnAl,subRegionType=sbRgnTy,source, STATES, name)
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  mapx <- mapx[!is.na(mapx$subRegion),]
  #---------------------
  mapUS49HUC4 <- mapx
  mapUS49HUC4 <- mapUS49HUC4 %>%
    dplyr::mutate(name="mapUS49HUC4")%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry)%>%
    st_set_crs(st_crs(4326))
  sf::st_crs(mapUS49HUC4) <- 4326
  use_data(mapUS49HUC4, version=3, overwrite=T)
}


#-----------------
# US Maps ( 52 State, 49 State, Counties, Regions, Grid Regions)
#-----------------


rgdal::writeOGR(obj=rmap::mapUS49,dsn=paste0("C:/Z/data/mapFiles/gis/usa"),
                layer="mapUS49", driver="ESRI Shapefile", overwrite_layer=TRUE)

rgdal::writeOGR(obj=rmap::mapUS52,dsn=paste0("C:/Z/data/mapFiles/gis/usa"),
                layer="mapUS52", driver="ESRI Shapefile", overwrite_layer=TRUE)

rgdal::writeOGR(obj=rmap::mapUS52Compact,dsn=paste0("C:/Z/data/mapFiles/gis/usa"),
                layer="mapUS52Compact", driver="ESRI Shapefile", overwrite_layer=TRUE)

rgdal::writeOGR(obj=rmap::mapUS49County,dsn=paste0("C:/Z/data/mapFiles/gis/usa"),
                layer="mapUS49County", driver="ESRI Shapefile", overwrite_layer=TRUE)

rgdal::writeOGR(obj=rmap::mapUS52County,dsn=paste0("C:/Z/data/mapFiles/gis/usa"),
                layer="mapUS52County", driver="ESRI Shapefile", overwrite_layer=TRUE)

rgdal::writeOGR(obj=rmap::mapUS52CountyCompact,dsn=paste0("C:/Z/data/mapFiles/gis/usa"),
                layer="mapUS52CountyCompact", driver="ESRI Shapefile", overwrite_layer=TRUE)


# US 52 (including Alaska, Hawaii and Puerto Rico)
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/usa",sep="")
  examplePolyFile<-paste("mapUS52",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  mapx <- x
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  #mapx <- mapx[!is.na(mapx$subRegion),]
  #---------------------
  mapUS52 <- mapx
  mapUS52 <- mapUS52 %>%
    dplyr::mutate(name="mapUS52")%>%
    dplyr::select(subRegion = subRegn,region,subRegionAlt=sbRgnAl,source, area, name, geometry)%>%
    st_set_crs(st_crs(4326))
  sf::st_crs(mapUS52) <- 4326
  use_data(mapUS52, version=3, overwrite=T)
}


# US 52 with Alaska (AK), Hawaii (HI) and Puerto Rico (PR) shrunken and shifted
#-------------------
  if(redoMaps){
    examplePolyFolder<-paste(dataFileFolder,"/gis/usa",sep="")
    examplePolyFile<-paste("mapUS52Compact",sep="")
    x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
    mapx <- x
    # convert to sf
    mapx <- sf::st_as_sf(mapx); mapx
    area_thresh <- units::set_units(50, km^2)
    sf::sf_use_s2(FALSE)
    mapx_area<-sf::st_area(mapx)
    mapx <- mapx%>%
      dplyr::mutate(area=mapx_area)
    #---------------------
    mapUS52Compact <- mapx
    mapUS52Compact <- mapUS52Compact %>%
      dplyr::mutate(name="mapUS52Compact")%>%
      dplyr::select(subRegion = subRegn,region,subRegionAlt=sbRgnAl,source, area, name, geometry)%>%
      st_set_crs(st_crs(4326))
    sf::st_crs(mapUS52Compact) <- 4326
    use_data(mapUS52Compact, version=3, overwrite=T)
  }

# US 49 (Excluding Alsaka, Hawaii and Puerto Rico)
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/usa",sep="")
  examplePolyFile<-paste("mapUS49",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  mapx <- x
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  #---------------------
  mapUS49 <- mapx
  mapUS49 <- mapUS49 %>%
    dplyr::mutate(name="mapUS49")%>%
    dplyr::select(subRegion = subRegn,region,subRegionAlt=sbRgnAl,source, area, name, geometry)%>%
    st_set_crs(st_crs(4326))
  sf::st_crs(mapUS49) <- 4326
  use_data(mapUS49, version=3, overwrite=T)
}

# US 52 Counties
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/usa",sep="")
  examplePolyFile<-paste("mapUS52County",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  mapx <- x
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  #---------------------
  mapUS52County <- mapx
  mapUS52County <- mapUS52County %>%
    dplyr::mutate(name="mapUS52County")%>%
    dplyr::select(subRegion = subRegn,region,subRegionAlt=sbRgnAl,source, area, name, geometry, STATEFP, FIPS)%>%
    st_set_crs(st_crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  sf::st_crs(mapUS52County) <- 4269
  sf::st_transform(mapUS52County, 4326)
  sf::st_crs(mapUS52County) <- 4326
  use_data(mapUS52County, version=3, overwrite=T)
}

# US 52 Counties with Alaska (AK), Hawaii (HI) and Puerto Rico (PR) shrunken and shifted
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/usa",sep="")
  examplePolyFile<-paste("mapUS52CountyCompact",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  mapx <- x
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  #---------------------
  mapUS52CountyCompact <- mapx
  mapUS52CountyCompact <- mapUS52CountyCompact %>%
    dplyr::mutate(name="mapUS52CountyCompact")%>%
    dplyr::select(subRegion = subRegn,region,subRegionAlt=sbRgnAl,source, area, name, geometry, STATEFP, FIPS)%>%
    st_set_crs(st_crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  sf::st_crs(mapUS52CountyCompact) <- 4269
  sf::st_transform(mapUS52CountyCompact, 4326)
  sf::st_crs(mapUS52CountyCompact) <- 4326
  use_data(mapUS52CountyCompact, version=3, overwrite=T)
}

# US 49 Counties
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/usa",sep="")
  examplePolyFile<-paste("mapUS49County",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  mapx <- x
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  #---------------------
  mapUS49County <- mapx
  mapUS49County <- mapUS49County %>%
    dplyr::mutate(name="mapUS49County")%>%
    dplyr::select(subRegion = subRegn,region,subRegionAlt=sbRgnAl,source, area, name, geometry, STATEFP, FIPS)%>%
    st_set_crs(st_crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  sf::st_crs(mapUS49County) <- 4269
  sf::st_transform(mapUS49County, 4326)
  sf::st_crs(mapUS49County) <- 4326
  use_data(mapUS49County, version=3, overwrite=T)
}

# Merge
#-------------------

# Merge US52 with GCAM Regs
if(redoMaps){
  m1 <- rmap::mapUS52
  m2 <- rmap::mapGCAMReg32 %>% filter(region != "USA")
  mapx<-m1 %>% dplyr::bind_rows(m2)

  mapGCAMReg32US52 <- mapx
  mapGCAMReg32US52 <- mapGCAMReg32US52 %>%
    dplyr::mutate(name="mapGCAMReg32US52")%>%
    st_set_crs(st_crs(4326))

  mapGCAMReg32US52 <- mapGCAMReg32US52 %>%
    dplyr::mutate(region=subRegion)
  sf::st_crs(mapGCAMReg32US52) <- 4326
  use_data(mapGCAMReg32US52, version=3, overwrite=T)
}

# Merge US52 with Countries file
if(redoMaps){
  m1 <- rmap::mapUS52
  m2 <- rmap::mapCountries %>% filter(region != "USA")
  mapx<-m1 %>% dplyr::bind_rows(m2)

  mapCountriesUS52 <- mapx
  mapCountriesUS52 <- mapCountriesUS52 %>%
    dplyr::mutate(name="mapCountriesUS52")%>%
    st_set_crs(st_crs(4326))
  mapCountriesUS52 <- mapCountriesUS52 %>%
    dplyr::mutate(region=subRegion)
  sf::st_crs(mapCountriesUS52) <- 4326
  use_data(mapCountriesUS52, version=3, overwrite=T)
}

# Merge Uruguay with GCAM 32
if(redoMaps){

  # Break out Uruguay in QGIS
  # First save as shapefile
  # library(sf); library(rmap)
  # sf::st_write(rmap::mapGCAMReg32, "mapGCAMReg32.shp")
  # Modify in QGIS and save as  "C:/Z/data/mapFiles/gcam_modified/mapGCAMReg32Uruguay.shp"

  examplePolyFolder<-paste(dataFileFolder,"/gcam_modified",sep="")
  examplePolyFile<-paste("mapGCAMReg32Uruguay",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  mapx <- x
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)
  #mapx <- mapx[!is.na(mapx$subRegion),]
  #---------------------
  mapGCAMReg32Uruguay <- mapx
  mapGCAMReg32Uruguay <- mapGCAMReg32Uruguay %>%
    dplyr::mutate(name="mapGCAMReg32Uruguay",
                  source = "https://doi.org/10.5281/zenodo.4688451")%>%
    dplyr::select(subRegion = subRegn,region,subRegionAlt=sbRgnAl,source, area, name, geometry)%>%
    st_set_crs(st_crs(4326))
  mapGCAMReg32Uruguay <- mapGCAMReg32Uruguay %>%
    dplyr::mutate(region=subRegion)
  sf::st_crs(mapGCAMReg32Uruguay) <- 4326
  use_data(mapGCAMReg32Uruguay, version=3, overwrite=T)
}


# Intersections
#-------------------

# Intersection of GCAM Basins and Countries
if(redoMaps){
  m1 <- mapGCAMBasins
  m2 <- mapCountries
  mapx<-sf::st_intersection(m1,m2)

  mapx <- mapx %>%
    dplyr::mutate(subRegion_GCAMBasin=subRegion,
                  subRegionAlt_GCAMBasin=subRegionAlt,
                  subRegion=paste(subRegion,subRegion.1,sep="_X_"),
                  region=region.1,
                  subRegionAlt=paste(subRegionAlt,subRegionAlt.1,sep="_X_")) %>%
    dplyr::rename(source_GCAMBasin = source,
                  source_Country = source.1,
                  subRegion_Country=subRegion.1,
                  subRegionAlt_Country=subRegionAlt.1) %>%
    dplyr::select(-region.1, -pop_est, -area, -area.1, -name.1)

  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)

  mapIntersectGCAMBasinCountry <- mapx
  mapIntersectGCAMBasinCountry <- mapIntersectGCAMBasinCountry %>%
    dplyr::mutate(name="mapIntersectGCAMBasinCountry",
                  source = paste0(source_GCAMBasin,"_X_", source_Country))%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry,
                  subRegion_GCAMBasin,subRegionAlt_GCAMBasin,source_GCAMBasin,
                  subRegion_Country, subRegionAlt_Country, source_Country)%>%
    st_set_crs(st_crs(4326))
  mapIntersectGCAMBasinCountry <- mapIntersectGCAMBasinCountry %>%
    dplyr::mutate(region=subRegion)
  sf::st_crs(mapIntersectGCAMBasinCountry) <- 4326
  use_data(mapIntersectGCAMBasinCountry, version=3, overwrite=T)
}

# Intersection of GCAM Basins and US 52 States, mapIntersectGCAMBasinUS52
if(redoMaps){
  m1 <- mapGCAMBasins
  m2 <- mapUS52
  mapx<-sf::st_intersection(m1,m2)

  mapx <- mapx %>%
    dplyr::mutate(subRegion_GCAMBasin=subRegion,
                  subRegionAlt_GCAMBasin=subRegionAlt,
                  subRegion=paste(subRegion,subRegion.1,sep="_X_"),
                  region=region.1,
                  subRegionAlt=paste(subRegionAlt,subRegionAlt.1,sep="_X_")) %>%
    dplyr::rename(source_GCAMBasin = source,
                  source_US52 = source.1,
                  subRegion_US52=subRegion.1,
                  subRegionAlt_US52=subRegionAlt.1) %>%
    dplyr::select(-region.1, -area, -area.1, -name.1)

  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)

  mapIntersectGCAMBasinUS52 <- mapx
  mapIntersectGCAMBasinUS52 <- mapIntersectGCAMBasinUS52 %>%
    dplyr::mutate(name="mapIntersectGCAMBasinUS52",
                  source = paste0(source_GCAMBasin,"_X_", source_US52))%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry,
                  subRegion_GCAMBasin,subRegionAlt_GCAMBasin,source_GCAMBasin,
                  subRegion_US52, subRegionAlt_US52, source_US52)%>%
    st_set_crs(st_crs(4326))
  mapIntersectGCAMBasinUS52 <- mapIntersectGCAMBasinUS52 %>%
    dplyr::mutate(region=subRegion)
  sf::st_crs(mapIntersectGCAMBasinUS52) <- 4326
  use_data(mapIntersectGCAMBasinUS52, version=3, overwrite=T)
}

# Intersection of GCAM Basins and US 52 County, mapIntersectGCAMBasinUS52County
if(redoMaps){
  m1 <- mapGCAMBasins
  m2 <- mapUS52County
  m2 <- m2 %>% sf::st_transform(sf::st_crs(m1))
  mapx<-sf::st_intersection(m1,m2)

  mapx <- mapx %>%
    dplyr::mutate(subRegion_GCAMBasin=subRegion,
                  subRegionAlt_GCAMBasin=subRegionAlt,
                  subRegion=paste(subRegion,subRegion.1,sep="_X_"),
                  region=region.1,
                  subRegionAlt=paste(subRegionAlt,subRegionAlt.1,sep="_X_")) %>%
    dplyr::rename(source_GCAMBasin = source,
                  source_US52County = source.1,
                  subRegion_US52County=subRegion.1,
                  subRegionAlt_US52County=subRegionAlt.1) %>%
    dplyr::select(-region.1, -area, -area.1, -name.1)

  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)

  mapIntersectGCAMBasinUS52County <- mapx
  mapIntersectGCAMBasinUS52County <- mapIntersectGCAMBasinUS52County %>%
    dplyr::mutate(name="mapIntersectGCAMBasinUS52County",
                  source = paste0(source_GCAMBasin,"_X_", source_US52County))%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry,
                  subRegion_GCAMBasin,subRegionAlt_GCAMBasin,source_GCAMBasin,
                  subRegion_US52County, subRegionAlt_US52County, source_US52County, FIPS)%>%
    st_set_crs(st_crs(4326))
  mapIntersectGCAMBasinUS52County <- mapIntersectGCAMBasinUS52County %>%
    dplyr::mutate(region=subRegion)
  sf::st_crs(mapIntersectGCAMBasinUS52County) <- 4326
  use_data(mapIntersectGCAMBasinUS52County, version=3, overwrite=T)
}

# Intersection of GCAM Basins and GCAM32Uruguay, mapIntersectGCAMBasin32RegUruguay
if(redoMaps){
  m1 <- mapGCAMBasins
  m2 <- mapGCAMReg32Uruguay
  mapx<-sf::st_intersection(m1,m2)

  mapx <- mapx %>%
    dplyr::mutate(subRegion_GCAMBasin=subRegion,
                  subRegionAlt_GCAMBasin=subRegionAlt,
                  subRegion=paste(subRegion,subRegion.1,sep="_X_"),
                  region=region.1,
                  subRegionAlt=paste(subRegionAlt,subRegionAlt.1,sep="_X_")) %>%
    dplyr::rename(source_GCAMBasin = source,
                  source_GCAMReg32Uruguay = source.1,
                  subRegion_GCAMReg32Uruguay=subRegion.1,
                  subRegionAlt_GCAMReg32Uruguay=subRegionAlt.1) %>%
    dplyr::select(-region.1, -area, -area.1, -name.1)

  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area)

  mapIntersectGCAMBasin32RegUruguay <- mapx
  mapIntersectGCAMBasin32RegUruguay <- mapIntersectGCAMBasin32RegUruguay %>%
    dplyr::mutate(name="mapIntersectGCAMBasin32RegUruguay",
                  source = paste0(source_GCAMBasin))%>%
    dplyr::select(subRegion,region,subRegionAlt,source, area, name, geometry,
                  subRegion_GCAMBasin,subRegionAlt_GCAMBasin,source_GCAMBasin,
                  subRegion_GCAMReg32Uruguay, subRegionAlt_GCAMReg32Uruguay, source_GCAMReg32Uruguay)%>%
    st_set_crs(st_crs(4326))
  mapIntersectGCAMBasin32RegUruguay <- mapIntersectGCAMBasin32RegUruguay %>%
    dplyr::mutate(region=subRegion)
  sf::st_crs(mapIntersectGCAMBasin32RegUruguay) <- 4326
  use_data(mapIntersectGCAMBasin32RegUruguay, version=3, overwrite=T)
}

# Cropped Files
#------------------------------


rgdal::writeOGR(obj=rmap::mapGCAMBasinsUS52,dsn=paste0("C:/Z/data/mapFiles/gis/gcam"),
                layer="mapGCAMBasinsUS52", driver="ESRI Shapefile", overwrite_layer=TRUE)
rgdal::writeOGR(obj=rmap::mapGCAMBasinsUS49,dsn=paste0("C:/Z/data/mapFiles/gis/gcam"),
                layer="mapGCAMBasinsUS49", driver="ESRI Shapefile", overwrite_layer=TRUE)

# Cropped GCAM Basins and US 52
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/gcam",sep="")
  examplePolyFile<-paste("mapGCAMBasinsUS52",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  mapx <- x
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area,
                  source = "https://doi.org/10.5281/zenodo.4688451")
  #mapx <- mapx[!is.na(mapx$subRegion),]
  #---------------------
  mapGCAMBasinsUS52 <- mapx
  mapGCAMBasinsUS52 <- mapGCAMBasinsUS52 %>%
    dplyr::mutate(name="mapGCAMBasinsUS52")%>%
    dplyr::select(subRegion = subRegn,region,subRegionAlt=sbRgnAl,source, area, name, geometry)%>%
    st_set_crs(st_crs(4326))
  mapGCAMBasinsUS52 <- mapGCAMBasinsUS52 %>%
    dplyr::mutate(region=subRegion)
  sf::st_crs(mapGCAMBasinsUS52) <- 4326
  use_data(mapGCAMBasinsUS52, version=3, overwrite=T)
}

# Cropped GCAM Basins and US 49 States
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/gcam",sep="")
  examplePolyFile<-paste("mapGCAMBasinsUS49",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  mapx <- x
  # convert to sf
  mapx <- sf::st_as_sf(mapx); mapx
  area_thresh <- units::set_units(50, km^2)
  sf::sf_use_s2(FALSE)
  mapx_area<-sf::st_area(mapx)
  mapx <- mapx%>%
    dplyr::mutate(area=mapx_area,
                  source = "https://doi.org/10.5281/zenodo.4688451")
  #mapx <- mapx[!is.na(mapx$subRegion),]
  #---------------------
  mapGCAMBasinsUS49 <- mapx
  mapGCAMBasinsUS49 <- mapGCAMBasinsUS49 %>%
    dplyr::mutate(name="mapGCAMBasinsUS49")%>%
    dplyr::select(subRegion = subRegn,region,subRegionAlt=sbRgnAl,source, area, name, geometry)%>%
    st_set_crs(st_crs(4326))
  mapGCAMBasinsUS49 <- mapGCAMBasinsUS49 %>%
    dplyr::mutate(region=subRegion)
  sf::st_crs(mapGCAMBasinsUS49) <- 4326
  use_data(mapGCAMBasinsUS49, version=3, overwrite=T)
}


#
# # Grid Files
# #-------------------
# if(!exists("grid025")){
#   gridx <-  tibble::as_tibble(data.table::fread(paste(dataFileFolder,"/grids/emptyGrids/grid_025.csv",sep="")));
#   grid025<-gridx
#   use_data(grid025, version=3, overwrite=T)
# }
#
# if(!exists("grid050")){
#   gridx <-  tibble::as_tibble(data.table::fread(paste(dataFileFolder,"/grids/emptyGrids/grid_050.csv",sep="")));
#   grid050<-gridx
#   use_data(grid050, version=3, overwrite=T)
# }

#-------------------
# Save Plots
#-------------------

# GCAM Maps (Regions, Basins, Land)
# US Maps (States, Counties)
# HydroShedMaps (HydroShed1,HydroShed2,HydroShed3)
# HUCMaps (HUC2, HUC4, HUC6)
# Grids (Grid0p5, Grid0p25)

if(F){

  library(rmap);

  # Check Holes or Invalid shapes
  #------------
  # World
  rgeos::gIsValid(rmap::mapCountries)
  rgeos::gIsValid(rmap::mapStates)
  # GCAM
  rgeos::gIsValid(rmap::mapGCAMReg32)
  rgeos::gIsValid(rmap::mapGCAMBasins)
  rgeos::gIsValid(rmap::mapGCAMLand)
  # US
  rgeos::gIsValid(rmap::mapUS52)
  rgeos::gIsValid(rmap::mapUS52County)
  rgeos::gIsValid(rmap::mapUS49)
  rgeos::gIsValid(rmap::mapUS49County)
  # HydroSheds
  rgeos::gIsValid(rmap::mapHydroShed1)
  rgeos::gIsValid(rmap::mapHydroShed2)
  rgeos::gIsValid(rmap::mapHydroShed3)
  # USGS HUC
  rgeos::gIsValid(rmap::mapUS52HUC2)
  rgeos::gIsValid(rmap::mapUS52HUC4)
  rgeos::gIsValid(rmap::mapUS49HUC2)
  rgeos::gIsValid(rmap::mapUS49HUC4)

  # Plotting
  #-------------
  # World
  rmap.map(data=rmap::mapCountries,fillColumn = "subRegion",labels=F,save=F)
  rmap.map(data=rmap::mapStates,fillColumn = "subRegion",labels=F,save=F)

  # GCAM
  rmap.map(data=rmap::mapGCAMReg32,fillColumn = "subRegion",labels=F,save=F)
  rmap.map(data=rmap::mapGCAMBasins,fillColumn = "subRegion",labels=F,save=F)
  rmap.map(data=rmap::mapGCAMLand,fillColumn = "subRegion",labels=F,save=F)
  # US
  rmap.map(data=rmap::mapUS52,fillColumn = "subRegion",labels=F,save=F)
  rmap.map(data=rmap::mapUS52County,fillColumn = "subRegion",labels=F,save=F)
  rmap.map(data=rmap::mapUS49,fillColumn = "subRegion",labels=F,save=F)
  rmap.map(data=rmap::mapUS49County,fillColumn = "subRegion",labels=F,save=F)
  # HydroSheds
  rmap.map(data=rmap::mapHydroShed1,fillColumn = "subRegion",labels=F,save=F)
  rmap.map(data=rmap::mapHydroShed2,fillColumn = "subRegion",labels=F,save=F)
  rmap.map(data=rmap::mapHydroShed3,fillColumn = "subRegion",labels=F,save=F)
  # USGS HUC
  rmap.map(data=rmap::mapUS52HUC2,fillColumn = "subRegion",labels=F,save=F)
  rmap.map(data=rmap::mapUS52HUC4,fillColumn = "subRegion",labels=F,save=F)
  rmap.map(data=rmap::mapUS49HUC2,fillColumn = "subRegion",labels=F,save=F)
  rmap.map(data=rmap::mapUS49HUC4,fillColumn = "subRegion",labels=F,save=F)
  # Merge
  rmap.map(data=rmap::mapGCAMReg32US52,fillColumn = "subRegion",labels=F,save=F)
  # Intersections
  rmap.map(data=rmap::mapIntersectGCAMBasin32Reg,fillColumn = "subRegion",labels=F,save=F)
  rmap.map(data=rmap::mapIntersectGCAMBasinCountry,fillColumn = "subRegion",labels=F,save=F)
  # Grids
  rmap::grid025
  rmap::grid050

}

#-----------------
# mappings
#-----------------

if(T){
  mapping_tethys_grid_basin_region_country <- readRDS("./inst/extras/rmap_tethys_grid_basin_region_country.rds") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(basinName = gsub('Madasgacar', 'Madagascar', basinName)) %>%
    dplyr::rename(lat=Latitude,
                  lon=Longitude); mapping_tethys_grid_basin_region_country
  use_data(mapping_tethys_grid_basin_region_country, version=3, overwrite=T)

  mapping_US49 <- c("AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","IA","ID","IL","IN","KS",
                    "KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC",
                    "ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD",
                    "TN","TX","UT","VA","VT","WA","WI","WV","WY")
  use_data(mapping_US49, version=3, overwrite=T)

  mapping_US52 <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA",
                    "MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ",
                    "NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT",
                    "VA","VT","WA","WI","WV","WY")
  use_data(mapping_US52, version=3, overwrite=T)

  mapping_country_gcam32 <- tibble::tribble(
    ~region_code,~ctry_code,~region,~ctry_name,
    "29","238","Southeast Asia","Fiji",
    "23","103","Russia","Russia",
    "13","17","EU-15","Wallis & Futuna",
    "6","223","Australia_NZ","New Zealand",
    "1","150","USA","United States",
    "29","40","Southeast Asia","Midway Is.",
    "29","16","Southeast Asia","Tonga",
    "29","38","Southeast Asia","Kiribati",
    "29","14","Southeast Asia","Samoa",
    "29","15","Southeast Asia","Tokelau",
    "29","9","Southeast Asia","American Samoa",
    "29","13","Southeast Asia","Niue",
    "29","37","Southeast Asia","Johnston Atoll",
    "29","10","Southeast Asia","Cook Is.",
    "29","11","Southeast Asia","French Polynesia",
    "8","34","Canada","Canada",
    "29","3","Southeast Asia","Pitcairn Is.",
    "20","39","Mexico","Mexico",
    "26","21","South America_Southern","Chile",
    "9","35","Central America and Caribbean","Guatemala",
    "26","22","South America_Southern","Ecuador",
    "9","62","Central America and Caribbean","El Salvador",
    "9","63","Central America and Caribbean","Honduras",
    "9","57","Central America and Caribbean","Belize",
    "9","64","Central America and Caribbean","Nicaragua",
    "9","60","Central America and Caribbean","Costa Rica",
    "9","61","Central America and Caribbean","Cuba",
    "9","65","Central America and Caribbean","Panama",
    "32","59","Colombia","Colombia",
    "26","24","South America_Southern","Peru",
    "9","58","Central America and Caribbean","Cayman Is.",
    "9","55","Central America and Caribbean","The Bahamas",
    "9","53","Central America and Caribbean","Jamaica",
    "9","52","Central America and Caribbean","Haiti",
    "7","20","Brazil","Brazil",
    "31","18","Argentina","Argentina",
    "25","8","South America_Northern","Venezuela",
    "13","67","EU-15","Greenland",
    "13","56","EU-15","Turks & Caicos Is.",
    "9","51","Central America and Caribbean","Dominican Republic",
    "26","19","South America_Southern","Bolivia",
    "9","54","Central America and Caribbean","Netherlands Antilles",
    "1","30","USA","Puerto Rico",
    "1","32","USA","Virgin Is.",
    "9","50","Central America and Caribbean","Bermuda",
    "9","26","Central America and Caribbean","Anguilla",
    "9","31","Central America and Caribbean","St. Kitts & Nevis",
    "26","23","South America_Southern","Paraguay",
    "9","29","Central America and Caribbean","Montserrat",
    "9","7","Central America and Caribbean","Trinidad & Tobago",
    "9","43","Central America and Caribbean","Grenada",
    "9","44","Central America and Caribbean","Guadeloupe",
    "9","27","Central America and Caribbean","Antigua & Barbuda",
    "9","48","Central America and Caribbean","St. Vincent & the Grenadines",
    "9","45","Central America and Caribbean","Martinique",
    "9","42","Central America and Caribbean","Dominica",
    "13","0","EU-15","Falkland Is.",
    "25","2","South America_Northern","Guyana",
    "9","41","Central America and Caribbean","Barbados",
    "26","25","South America_Southern","Uruguay",
    "25","6","South America_Northern","Suriname",
    "13","47","EU-15","St. Pierre & Miquelon",
    "25","1","South America_Northern","French Guiana",
    "26","4","South America_Southern","South Georgia & the South Sandwich Is.",
    "13","81","EU-15","Portugal",
    "5","75","Africa_Western","Cape Verde",
    "16","69","European Free Trade Association","Iceland",
    "13","82","EU-15","Spain",
    "5","89","Africa_Western","Senegal",
    "5","88","Africa_Western","Mauritania",
    "3","83","Africa_Northern","Western Sahara",
    "5","91","Africa_Western","The Gambia",
    "5","86","Africa_Western","Guinea-Bissau",
    "5","85","Africa_Western","Guinea",
    "13","5","EU-15","St. Helena",
    "5","90","Africa_Western","Sierra Leone",
    "3","80","Africa_Northern","Morocco",
    "5","87","Africa_Western","Mali",
    "5","79","Africa_Western","Liberia",
    "13","70","EU-15","Ireland",
    "16","72","European Free Trade Association","Jan Mayen",
    "5","76","Africa_Western","Cote d'Ivoire",
    "3","106","Africa_Northern","Algeria",
    "13","74","EU-15","United Kingdom",
    "13","66","EU-15","Faroe Is.",
    "5","84","Africa_Western","Burkina Faso",
    "13","145","EU-15","France",
    "13","71","EU-15","Isle of Man",
    "5","77","Africa_Western","Ghana",
    "13","68","EU-15","Guernsey",
    "13","73","EU-15","Jersey",
    "5","119","Africa_Western","Togo",
    "5","116","Africa_Western","Niger",
    "5","111","Africa_Western","Benin",
    "5","117","Africa_Western","Nigeria",
    "13","144","EU-15","Belgium",
    "13","248","EU-15","Netherlands",
    "16","102","European Free Trade Association","Norway",
    "5","118","Africa_Western","Sao Tome & Principe",
    "13","148","EU-15","Luxembourg",
    "13","146","EU-15","Germany",
    "16","149","European Free Trade Association","Switzerland",
    "13","123","EU-15","Italy",
    "3","110","Africa_Northern","Tunisia",
    "5","115","Africa_Western","Equatorial Guinea",
    "13","138","EU-15","Denmark",
    "5","220","Africa_Western","Gabon",
    "5","112","Africa_Western","Cameroon",
    "3","108","Africa_Northern","Libya",
    "13","136","EU-15","Austria",
    "13","104","EU-15","Sweden",
    "16","143","European Free Trade Association","Svalbard",
    "5","218","Africa_Western","Congo Rep.",
    "4","222","Africa_Southern","Namibia",
    "4","217","Africa_Southern","Angola",
    "5","219","Africa_Western","Congo DRC",
    "12","137","EU-12","Czech Republic",
    "5","114","Africa_Western","Chad",
    "15","122","Europe_Non_EU","Croatia",
    "12","142","EU-12","Slovenia",
    "12","125","EU-12","Malta",
    "5","113","Africa_Western","Central African Republic",
    "12","140","EU-12","Poland",
    "15","121","Europe_Non_EU","Bosnia & Herzegovina",
    "24","224","South Africa","South Africa",
    "12","139","EU-12","Hungary",
    "12","141","EU-12","Slovakia",
    "15","247","Europe_Non_EU","Montenegro",
    "15","120","Europe_Non_EU","Albania",
    "15","246","Europe_Non_EU","Serbia",
    "13","132","EU-15","Greece",
    "13","153","EU-15","Finland",
    "4","202","Africa_Southern","Botswana",
    "15","124","Europe_Non_EU","Macedonia",
    "12","157","EU-12","Romania",
    "12","155","EU-12","Lithuania",
    "12","154","EU-12","Latvia",
    "12","152","EU-12","Estonia",
    "4","207","Africa_Southern","Zambia",
    "2","95","Africa_Eastern","Sudan",
    "12","128","EU-12","Bulgaria",
    "14","158","Europe_Eastern","Ukraine",
    "14","151","Europe_Eastern","Belarus",
    "3","130","Africa_Northern","Egypt",
    "4","208","Africa_Southern","Zimbabwe",
    "15","135","Europe_Non_EU","Turkey",
    "4","221","Africa_Southern","Lesotho",
    "14","156","Europe_Eastern","Moldova",
    "2","203","Africa_Eastern","Burundi",
    "2","205","Africa_Eastern","Rwanda",
    "4","206","Africa_Southern","Tanzania",
    "2","96","Africa_Eastern","Uganda",
    "4","216","Africa_Southern","Mozambique",
    "4","225","Africa_Southern","Swaziland",
    "12","129","EU-12","Cyprus",
    "4","215","Africa_Southern","Malawi",
    "2","94","Africa_Eastern","Ethiopia",
    "2","204","Africa_Eastern","Kenya",
    "21","97","Middle East","Gaza Strip",
    "21","99","Middle East","Israel",
    "21","173","Middle East","Saudi Arabia",
    "21","100","Middle East","Jordan",
    "21","105","Middle East","West Bank",
    "21","133","Middle East","Lebanon",
    "21","134","Middle East","Syria",
    "2","93","Africa_Eastern","Eritrea",
    "21","98","Middle East","Iraq",
    "2","212","Africa_Eastern","French Southern & Antarctic Lands",
    "10","131","Central Asia","Georgia",
    "2","162","Africa_Eastern","Somalia",
    "2","92","Africa_Eastern","Djibouti",
    "21","166","Middle East","Yemen",
    "2","214","Africa_Eastern","Juan De Nova I.",
    "2","228","Africa_Eastern","Madagascar",
    "2","211","Africa_Eastern","Comoros",
    "10","167","Central Asia","Armenia",
    "21","170","Middle East","Iran",
    "29","230","Southeast Asia","Mayotte",
    "10","168","Central Asia","Azerbaijan",
    "29","232","Southeast Asia","Seychelles",
    "21","171","Middle East","Kuwait",
    "2","227","Africa_Eastern","Glorioso Is.",
    "10","101","Central Asia","Kazakhstan",
    "21","172","Middle East","Qatar",
    "21","169","Middle East","Bahrain",
    "21","174","Middle East","United Arab Emirates",
    "21","161","Middle East","Oman",
    "10","164","Central Asia","Turkmenistan",
    "2","231","Africa_Eastern","Reunion",
    "10","165","Central Asia","Uzbekistan",
    "2","229","Africa_Eastern","Mauritius",
    "27","175","South Asia","Afghanistan",
    "22","178","Pakistan","Pakistan",
    "10","179","Central Asia","Tajikistan",
    "17","159","India","India",
    "10","176","Central Asia","Kyrgyzstan",
    "27","226","South Asia","British Indian Ocean Territory",
    "6","213","Australia_NZ","Heard I. & McDonald Is.",
    "27","160","South Asia","Maldives",
    "11","183","China","China",
    "27","163","South Asia","Sri Lanka",
    "27","177","South Asia","Nepal",
    "27","180","South Asia","Bangladesh",
    "10","184","Central Asia","Mongolia",
    "27","181","South Asia","Bhutan",
    "29","190","Southeast Asia","Myanmar",
    "18","236","Indonesia","Indonesia",
    "29","235","Southeast Asia","Cocos Is.",
    "29","194","Southeast Asia","Thailand",
    "29","189","Southeast Asia","Malaysia",
    "29","188","Southeast Asia","Laos",
    "29","187","Southeast Asia","Cambodia",
    "29","195","Southeast Asia","Vietnam",
    "29","192","Southeast Asia","Singapore",
    "29","234","Southeast Asia","Christmas I.",
    "6","233","Australia_NZ","Australia",
    "29","182","Southeast Asia","Brunei",
    "29","186","Southeast Asia","Philippines",
    "30","183","Taiwan","China",
    "19","197","Japan","Japan",
    "29","237","Southeast Asia","Timor-Leste",
    "29","191","Southeast Asia","North Korea",
    "28","193","South Korea","South Korea",
    "29","185","Southeast Asia","Palau",
    "29","199","Southeast Asia","Micronesia",
    "29","242","Southeast Asia","Papua New Guinea",
    "29","196","Southeast Asia","Guam",
    "29","200","Southeast Asia","Northern Mariana Is.",
    "29","243","Southeast Asia","Solomon Is.",
    "29","198","Southeast Asia","Marshall Is.",
    "29","240","Southeast Asia","New Caledonia",
    "29","245","Southeast Asia","Vanuatu",
    "29","239","Southeast Asia","Nauru",
    "29","201","Southeast Asia","Wake I.",
    "29","241","Southeast Asia","Norfolk I.",
    "29","244","Southeast Asia","Tuvalu",
    "33","248","Uruguay","Uruguay",
    "33","150","AK","United States",
    "34","150","AL","United States",
    "35","150","AR","United States",
    "36","150","AZ","United States",
    "37","150","CA","United States",
    "38","150","CO","United States",
    "39","150","CT","United States",
    "40","150","DC","United States",
    "41","150","DE","United States",
    "42","150","FL","United States",
    "43","150","GA","United States",
    "44","150","HI","United States",
    "45","150","IA","United States",
    "46","150","ID","United States",
    "47","150","IL","United States",
    "48","150","IN","United States",
    "49","150","KS","United States",
    "50","150","KY","United States",
    "51","150","LA","United States",
    "52","150","MA","United States",
    "53","150","MD","United States",
    "54","150","ME","United States",
    "55","150","MI","United States",
    "56","150","MN","United States",
    "57","150","MO","United States",
    "58","150","MS","United States",
    "59","150","MT","United States",
    "60","150","NC","United States",
    "61","150","ND","United States",
    "62","150","NE","United States",
    "63","150","NH","United States",
    "64","150","NJ","United States",
    "65","150","NM","United States",
    "66","150","NV","United States",
    "67","150","NY","United States",
    "68","150","OH","United States",
    "69","150","OK","United States",
    "70","150","OR","United States",
    "71","150","PA","United States",
    "72","150","RI","United States",
    "73","150","SC","United States",
    "74","150","SD","United States",
    "75","150","TN","United States",
    "76","150","TX","United States",
    "77","150","UT","United States",
    "78","150","VA","United States",
    "79","150","VT","United States",
    "80","150","WA","United States",
    "81","150","WI","United States",
    "82","150","WV","United States",
    "83","150","WY","United States"
  )
  use_data(mapping_country_gcam32, version=3, overwrite=T)

  mapping_gcambasins <- tibble::tribble(
    ~subRegionMap,~subRegion,
    "Arctic_Ocean_Islands","ArcticIsl",
    "Northwest_Territories","NWTerr",
    "Siberia_North_Coast","SiberiaN",
    "Siberia_West_Coast","SiberiaW",
    "Kara_Sea_Coast","KaraSea",
    "Lena","LenaR",
    "Pacific_and_Arctic_Coast","PacArctic",
    "Scandinavia_North_Coast","ScndnvN",
    "Russia_Barents_Sea_Coast","BarentsSea",
    "Mackenzie","Mackenzie",
    "Iceland","Iceland",
    "Sweden","Sweden",
    "Finland","Finland",
    "Northern_Dvina","DvinaRN",
    "Hudson_Bay_Coast","HudsonBay",
    "Scotland","Scotland",
    "Neva","NevaR",
    "Volga","VolgaR",
    "Atlantic_Ocean_Seaboard","CanAtl",
    "Baltic_Sea_Coast","BalticSea",
    "Denmark_Germany_Coast","DnkGrmCst",
    "Narva","NarvaR",
    "Saskatchewan_Nelson","NelsonR",
    "Ireland","Ireland",
    "Daugava","DaugavaR",
    "England_and_Wales","EngWales",
    "Fraser","FraserR",
    "Ems_Weser","EmsWeserR",
    "Oder","OderR",
    "Wisla","WislaR",
    "Elbe","ElbeR",
    "Rhine","RhineR",
    "Poland_Coast","PolandCst",
    "Churchill","ChurchillR",
    "Neman","NemanR",
    "Scheldt","ScheldtR",
    "Russia_South_East_Coast","RusCstSE",
    "Ural","UralR",
    "Dnieper","DnieperR",
    "St_Lawrence","StLwrncR",
    "France_West_Coast","FranceCstW",
    "Gobi_Interior","Gobi",
    "Amur","AmurR",
    "Loire","LoireR",
    "Caspian_Sea_Coast","CaspianNE",
    "Seine","SeineR",
    "Black_Sea_North_Coast","BlackSeaN",
    "Yenisey","YeniseiR",
    "Dniester","DniesterR",
    "Italy_East_Coast","ItalyCstE",
    "Japan","Japan",
    "Caspian_Sea_East_Coast","CaspianE",
    "Don","DonR",
    "Danube","DanubeR",
    "Adriatic_Sea_Greece_Black_Sea_Coast","AdrBlkSea",
    "Ob","ObR",
    "Po","PoR",
    "Amu_Darya","AmuDaryaR",
    "Italy_West_Coast","ItalyCstW",
    "Spain_Portugal_Atlantic_Coast","IberiaCst",
    "France_South_Coast","FranceCstS",
    "Rhone","RhoneR",
    "Mediterranean_Sea_Islands","MeditIsl",
    "Gironde","Gironde",
    "North_and_South_Korea","Korea",
    "Bo_Hai_Korean_Bay_North_Coast","BoHai",
    "Spain_South_and_East_Coast","SpainCstSE",
    "Lake_Balkash","LBalkash",
    "Tiber","TiberR",
    "Black_Sea_South_Coast","BlackSeaS",
    "Tagus","TagusR",
    "Caspian_Sea_South_West_Coast","CaspianSW",
    "Ebro","EbroR",
    "Douro","DouroR",
    "Mediterranean_Sea_East_Coast","MeditE",
    "Syr_Darya","SyrDaryaR",
    "Ziya_He_Interior","ZiyaHe",
    "China_Coast","ChinaCst",
    "Huang_He","HuangHeR",
    "Mediterranean_South_Coast","MeditS",
    "Guadiana","GuadianaR",
    "Central_Iran","Iran",
    "Guadalquivir","GuadalqR",
    "Tigris_Euphrates","TigrEuphR",
    "Tarim_Interior","Tarim",
    "Africa_North_West_Coast","AfrCstNW",
    "Nile","NileR",
    "Persian_Gulf_Coast","PersianGulf",
    "Indus","IndusR",
    "Farahrud","FarahrudR",
    "Baja_California","MexBaja",
    "Plateau_of_Tibet_Interior","Tibet",
    "Red_Sea_East_Coast","RedSeaE",
    "Arabian_Peninsula","ArabianP",
    "Dead_Sea","DeadSea",
    "Mexico_Northwest_Coast","MexCstNW",
    "Helmand","Helmand",
    "Sinai_Peninsula","SinaiP",
    "Eastern_Jordan_Syria","EJrdnSyr",
    "Africa_Red_Sea_Gulf_of_Aden_Coast","AfrCstNE",
    "Caribbean","Caribbean",
    "Hamun_i_Mashkel","HamuMashR",
    "Taiwan","Taiwan",
    "Arabian_Sea_Coast","ArabianSea",
    "North_Gulf","MexGulf",
    "Yangtze","Yangtze",
    "Sabarmati","SabarmatiR",
    "Xun_Jiang","XunJiang",
    "Hong_Red_River","Hong",
    "Ganges_Bramaputra","GangesR",
    "Yucatan_Peninsula","YucatanP",
    "South_China_Sea_Coast","SChinaSea",
    "Mahi","MahiR",
    "Mexico_Interior","MexInt",
    "Pacific_Central_Coast","MexCstW",
    "Bay_of_Bengal_North_East_Coast","BengalBay",
    "Tapti","TaptiR",
    "Yasai","BengalW",
    "Philippines","Phlppns",
    "Brahamani","BrahmaniR",
    "North_Marina_Islands_and_Guam","Guam",
    "Mahandi","MahanadiR",
    "Godavari","GodavariR",
    "Hainan","Hainan",
    "Mekong","Mekong",
    "Viet_Nam_Coast","VietnamCst",
    "Salween","Salween",
    "India_North_East_Coast","IndCstNE",
    "India_West_Coast","IndCstW",
    "Papaloapan","Papaloapan",
    "Rio_Lerma","RioLerma",
    "Rio_Verde","RioVerde",
    "Grijalva_Usumacinta","GrijUsuR",
    "Rio_Balsas","RioBalsas",
    "Southern_Central_America","CntAmer",
    "Isthmus_of_Tehuantepec","Tehuantpc",
    "Irrawaddy","IrrawaddyR",
    "Sittang","SittaungR",
    "Peninsula_Malaysia","MalaysiaP",
    "Krishna","KrishnaR",
    "Andaman_Nicobar_Islands","AdnNicIsl",
    "Africa_West_Coast","AfrCstW",
    "Caribbean_Coast","SAmerCstN",
    "Africa_North_Interior","AfrIntN",
    "India_East_Coast","IndCstE",
    "Chao_Phraya","ChaoPhrR",
    "Pennar","PennarR",
    "Gulf_of_Thailand_Coast","ThaiGulf",
    "Niger","NigerR",
    "Micronesia","Micronesia",
    "Lake_Chad","LChad",
    "Senegal","SenegalR",
    "Cauvery","CauveryR",
    "Sri_Lanka","SriLanka",
    "India_South_Coast","IndCstS",
    "Orinoco","OrinocoR",
    "Colombia_Ecuador_Pacific_Coast","ColEcuaCst",
    "Palau_and_East_Indonesia","IdnE",
    "North_Borneo_Coast","BorneoCstN",
    "Volta","VoltaR",
    "Northeast_South_America_South_Atlantic_Coast","SAmerCstNE",
    "Gulf_of_Guinea","GuineaGulf",
    "Sumatra","Sumatra",
    "Sulawesi","Sulawesi",
    "Kalimantan","Kalimantan",
    "Magdalena","MagdalenaR",
    "Irian_Jaya_Coast","IrianJaya",
    "Amazon","AmazonR",
    "South_Chile_Pacific_Coast","ChileCstS",
    "Shebelli_Juba","ShebJubR",
    "Africa_East_Central_Coast","AfrCstE",
    "North_Brazil_South_Atlantic_Coast","BrzCstN",
    "Papua_New_Guinea_Coast","PapuaCst",
    "Tocantins","TocantinsR",
    "Java_Timor","JavaTimor",
    "Solomon_Islands","SolomonIsl",
    "Madagascar","Madagascar",
    "Sepik","SepikR",
    "Rift_Valley","RiftValley",
    "Peru_Pacific_Coast","PeruCst",
    "Fly","FlyR",
    "Angola_Coast","AngolaCst",
    "Congo","CongoR",
    "Australia_North_Coast","AusCstN",
    "South_Pacific_Islands","NewCaledn",
    "East_Brazil_South_Atlantic_Coast","BrzCstE",
    "Parnaiba","ParnaibaR",
    "Zambezi","ZambeziR",
    "Australia_East_Coast","AusCstE",
    "Africa_Indian_Ocean_Coast","AfrCstSE",
    "Australia_West_Coast","AusCstW",
    "Sao_Francisco","SaoFrancR",
    "Australia_Interior","AusInt",
    "Orange","OrangeR",
    "Uruguay_Brazil_South_Atlantic_Coast","BrzCstS",
    "Namibia_Coast","AfrCstSW",
    "Africa_South_Interior","AfrIntS",
    "South_Africa_South_Coast","AfrCstS",
    "Limpopo","LimpopoR",
    "La_Puna_Region","LaPuna",
    "New_Zealand","NewZealand",
    "Australia_South_Coast","AusCstS",
    "Mar_Chiquita","MarChiq",
    "South_Africa_West_Coast","AfrCstSSW",
    "Salinas_Grandes","Salinas",
    "La_Plata","RioLaPlata",
    "North_Chile_Pacific_Coast","ChileCstN",
    "Murray_Darling","MurrayDrlg",
    "Pampas_Region","Pampas",
    "North_Argentina_South_Atlantic_Coast","ArgCstN",
    "Tasmania","Tasmania",
    "South_America_Colorado","ArgColoR",
    "Negro","NegroR",
    "Central_Patagonia_Highlands","Patagonia",
    "South_Argentina_South_Atlantic_Coast","ArgCstS",
    "Antarctica","Antarctica",
    "California_River","California",
    "Upper_Mississippi","MissppRN",
    "Lower_Mississippi_River","MissppRS",
    "Upper_Colorado_River","UsaColoRN",
    "Lower_Colorado_River","UsaColoRS",
    "Great","GreatBasin",
    "Missouri_River","MissouriR",
    "Arkansas_White_Red","ArkWhtRedR",
    "Texas_Gulf_Coast","TexasCst",
    "South_Atlantic_Gulf","UsaCstSE",
    "Great_Lakes","GreatLakes",
    "Ohio_River","OhioR",
    "Pacific_Northwest","UsaPacNW",
    "Tennessee_River","TennR",
    "Rio_Grande_River","RioGrande",
    "New_England","UsaCstNE",
    "Mid_Atlantic","UsaCstE",
    "Hawaii","Hawaii",
    "Narmada","NarmadaR"
  )
  use_data(mapping_gcambasins, version=3, overwrite=T)
}

#------------
# Pre-built map regions
#--------------

if(T){

  allSubRegions <- list(
    "mapStates" =
      tolower(mapStates$subRegion %>% unique() %>% as.character %>% sort()),
    "mapUS49" =
      tolower(rmap::mapUS49$subRegion %>% unique() %>% as.character %>% sort()),
    "mapUS52" =
      tolower(rmap::mapUS52$subRegion %>% unique() %>% as.character %>% sort()),
    "mapGCAMReg32" =
      tolower(rmap::mapGCAMReg32$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapGCAMReg32Uruguay" =
      tolower(rmap::mapGCAMReg32Uruguay$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapCountries" =
      tolower(rmap::mapCountries$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapGCAMReg32US52" =
      tolower(rmap::mapGCAMReg32US52$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapCountriesUS52" =
      tolower(rmap::mapCountriesUS52$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapUS49County" =
      tolower(rmap::mapUS49County$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapUS52County" =
      tolower(rmap::mapUS52County$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapGCAMBasins" =
      tolower(rmap::mapGCAMBasins$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapGCAMBasinsUS49" =
      tolower(rmap::mapGCAMBasinsUS49$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapGCAMBasinsUS52" =
      tolower(rmap::mapGCAMBasinsUS52$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapUS49HUC2" =
      tolower(rmap::mapUS49HUC2$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapUS52HUC2" =
      tolower(rmap::mapUS52HUC2$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapUS49HUC4" =
      tolower(rmap::mapUS49HUC4$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapUS52HUC4" =
      tolower(rmap::mapUS52HUC4$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapGCAMLand" =
      tolower(rmap::mapGCAMLand$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapIntersectGCAMBasin32RegUruguay" =
      tolower(rmap::mapIntersectGCAMBasin32RegUruguay$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapIntersectGCAMBasinCountry" =
      tolower(rmap::mapIntersectGCAMBasinCountry$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapIntersectGCAMBasinUS52" =
      tolower(rmap::mapIntersectGCAMBasinUS52$subRegion %>% unique() %>% as.character %>%
                sort()),
    "mapIntersectGCAMBasinUS52County" =
      tolower(rmap::mapIntersectGCAMBasinUS52County$subRegion %>% unique() %>% as.character %>%
                sort()),
    # Alt Names
    "mapStatesAlt" =
      tolower(mapStates$subRegionAlt %>% unique() %>% as.character %>% sort()),
    "mapUS49Alt" =
      tolower(rmap::mapUS49$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapUS52Alt" =
      tolower(rmap::mapUS52$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapGCAMReg32Alt" =
      tolower(rmap::mapGCAMReg32$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapCountriesAlt" =
      tolower(rmap::mapCountries$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapGCAMReg32US52Alt" =
      tolower(rmap::mapGCAMReg32US52$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapCountriesUS52Alt" =
      tolower(rmap::mapCountriesUS52$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapUS49CountyAlt" =
      tolower(rmap::mapUS49County$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapUS52CountyAlt" =
      tolower(rmap::mapUS52County$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapGCAMBasinsAlt" =
      tolower(rmap::mapGCAMBasins$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapGCAMBasinsUS49Alt" =
      tolower(rmap::mapGCAMBasinsUS49$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapGCAMBasinsUS52Alt" =
      tolower(rmap::mapGCAMBasinsUS52$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapUS49HUC2Alt" =
      tolower(rmap::mapUS49HUC2$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapUS52HUC2Alt" =
      tolower(rmap::mapUS52HUC2$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapUS49HUC4Alt" =
      tolower(rmap::mapUS49HUC4$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapUS52HUC4Alt" =
      tolower(rmap::mapUS52HUC4$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapGCAMLandAlt" =
      tolower(rmap::mapGCAMLand$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapIntersectGCAMBasin32RegUruguayAlt" =
      tolower(rmap::mapIntersectGCAMBasin32RegUruguay$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapIntersectGCAMBasinCountryAlt" =
      tolower(rmap::mapIntersectGCAMBasinCountry$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapIntersectGCAMBasinUS52Alt" =
      tolower(rmap::mapIntersectGCAMBasinUS52$subRegionAlt %>% unique() %>% as.character %>%
                sort()),
    "mapIntersectGCAMBasinUS52CountyAlt" =
      tolower(rmap::mapIntersectGCAMBasinUS52County$subRegionAlt %>% unique() %>% as.character %>%
                sort())
  )

  use_data(allSubRegions, version=3,  overwrite=T)
}

  #-----------------
  # Gridded Population Data
  #-----------------

  # Data from Tethys FOlder
  # Original data from https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11/data-download#
  # Center for International Earth Science Information Network (CIESIN) - Columbia University. 2016.
  # Gridded Population of the World, Version 4 (GPWv4): Population Count. NASA Socioeconomic Data and Applications Center (SEDAC),
  # Palisades, NY. DOI: http://dx.doi.org/10.7927/H4X63JVC
  # https://github.com/JGCRI/tethys
  #-------------------
  if(redoMaps){
    dfpop <- data.table::fread("C:/Z/models/tethysExampleFolders/example_v1_3_0/Input/harmonized_inputs/GPW_population.csv"); dfpop
    names(dfpop) <- gsub("X","",names(dfpop)); dfpop
    dfcoords <- data.table::fread("C:/Z/models/tethysExampleFolders/example_v1_3_0/Input/coordinates.csv"); dfcoords
    nrow(dfcoords); nrow(dfpop)
    dfx <- dfcoords %>%
      dplyr::select(lon=V2,lat=V3) %>%
      dplyr::bind_cols(dfpop); dfx
    grid_pop_GPWv4To2015 <- dfx
    #use_data(grid_pop_GPWv4To2015, version=3, overwrite=T)

    # Modify to required data format
    example_gridData_GWPv4To2015 <- grid_pop_GPWv4To2015 %>%
      tidyr::gather(key="x",value="value",-lon,-lat) %>%
      tibble::as_tibble() %>%
      dplyr::filter(x %in% c(2000, 2005, 2010, 2015));
    use_data(example_gridData_GWPv4To2015, version=3, overwrite=T)
  }

#-----------
# Test for duplicate subRegions
#----------

if(F){
  library(rmap);

  # Plotting
  #-------------
  # World
  for(i in c("mapCountries","mapStates","mapGCAMReg32","mapGCAMBasins","mapUS52","mapUS52County",
             "mapUS52CountyCompact","mapUS49","mapUS49County","mapGCAMReg32US52",
             "mapIntersectGCAMBasinCountry")){
  mapx = get(i)
  subRegions_duplicated <- (mapx$subRegion)[(mapx$subRegion)%>%duplicated()]; subRegions_duplicated
  print(i)
  print(subRegions_duplicated)
  }

}
