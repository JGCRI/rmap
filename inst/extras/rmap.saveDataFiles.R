
library(tibble);library(dplyr);library(rgdal);library(devtools);library(rmap); library(tmaptools)
library(rgeos); library(rgcam); library(maptools); library(rnaturalearth);library(rmapshaper)

redoMaps = F

dataFileFolder <- "C:/Z/data/mapFiles"

# Current Data
#data(package="rmap")


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
    dplyr::filter(x %in% c(1990,2015));
  use_data(example_gridData_GWPv4To2015, version=3, overwrite=T)
}

#-----------------
# World Maps (Countries, States)
#-----------------

# Worldmap countries
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/metis/naturalEarth",sep="")
  examplePolyFile<-paste("ne_10m_admin_0_countries",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=NAME,subRegionAlt=ADM0_A3, POP_EST) %>%
    dplyr::mutate(region="World",subRegionType="country", source="https://www.naturalearthdata.com/downloads/",
                  subRegion = as.character(subRegion),
                  subRegion=if_else(subRegion=="United States of America","USA",subRegion))
  mapx <- mapx[!grepl("Antarctica",mapx$subRegion),]
  mapx@data <- mapx@data%>%droplevels()%>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  format(object.size(mapx), units="Mb")
  mapx<-as(simplify_shape(mapx, fact = 0.1),Class="Spatial")
  format(object.size(mapx), units="Mb")
  #sp::plot(mapx)
  #rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapCountries <- mapx
  mapCountries@data <- mapCountries@data %>%
    dplyr::mutate(name="mapCountries")
  use_data(mapCountries, version=3, overwrite=T)
}

# Worldmap states
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/metis/naturalEarth",sep="")
  examplePolyFile<-paste("ne_10m_admin_1_states_provinces",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)

  # From rnaturalearth
  #x <- rnaturalearth::ne_states()
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(region=admin,subRegion=name,subRegionAlt=postal) %>%
    dplyr::mutate(subRegionType="states", source="https://www.naturalearthdata.com/downloads/",
                  region = as.character(region),
                  region=if_else(region=="United States of America","USA",region))
  mapx <- mapx[!grepl("Antarctica",mapx$region),]
  mapx@data <- mapx@data%>%droplevels()%>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  format(object.size(mapx), units="Mb")
  mapx<-rmapshaper::ms_simplify(mapx)
  format(object.size(mapx), units="Mb")
  #sp::plot(mapx)
  # rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F, facetsON=F, fileName="factp1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapStates <- mapx
  mapStates@data <- mapStates@data %>%
    dplyr::mutate(name="mapStates")
  use_data(mapStates, version=3, overwrite=T)
}

#-----------------
# GCAM Maps (Regions, Basins, Land)
#-----------------

# GCAM 32 Regions
#------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/metis/gcam",sep="")
  examplePolyFile<-paste("region32_0p5deg",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  idMapping <- tibble::as_tibble(data.table::fread(paste(dataFileFolder,"/gis/metis/gcam/GCAM_region_names.csv",sep=""),skip=4,header=T))
  mapx@data <- mapx@data %>%
    dplyr::mutate(reg32_id=as.character(reg32_id))%>%
    left_join(idMapping%>%dplyr::mutate(reg32_id=as.character(GCAM_region_ID)),by="reg32_id")%>%
    dplyr::mutate(subRegionType="GCAMReg32",
                  source="https://confluence.pnnl.gov/confluence/display/JGCRI/GCAM+Shape+Files",
                  subRegion=region,
                  region = "World",
                  subRegionAlt=reg32_id,
                  region=gsub("-","_",region),
                  subRegion=gsub("-","_",subRegion))%>%
    dplyr::select(-ID,-GRIDCODE,-GCAM_region_ID,-reg32_id)
  mapx@data <- droplevels(mapx@data)%>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  # Add Taiwan
  # a <- rmap::mapCountries
  # a <- a[a$subRegion %in% "Taiwan",];
  # a@data <- droplevels(a@data)
  # b <- mapx
  # a <- sp::spTransform(a,raster::crs(b))
  # mapx1 <- raster::union(a,b)
  format(object.size(mapx), units="Mb")
  #mapx<-as(simplify_shape(mapx, fact = 0.05),Class="Spatial")
  #format(object.size(mapx), units="Mb")
  #sp::plot(mapx)
  # rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F, facetsON=F, fileName="factp1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMReg32 <- mapx
  mapGCAMReg32@data <- mapGCAMReg32@data %>%
    dplyr::mutate(name="mapGCAMReg32")
  use_data(mapGCAMReg32, version=3, overwrite=T)
}


# GCAM Basins
#------------------
if(redoMaps){

  # Get list of basins from a GCAM database
  # Compare with existing shape file
  # Mapping file is available in rmap::mappings.R

  # Changes to make to shapefile basins (in metis.saveDataFiles)
  #   - Remove "_Basin"
  #   - Fix "R(hne" to "Rohne"
  #   - Fix "Yucat_µ„ön_Peninsula"
  #   - Replace all "-" with "_"
  #   - "HamuniMashkel"~"Hamun-i-Mashkel"
  # Changes to make to GCAMdata in metis.readgcam()
  #   - Replace all "-" with "_"

  # dataGCAM<-metis.readgcam (#gcamdatabase = "C:/Z/gcam-v5.2-Windows-Release-Package/output/example",
  #                          dataProjFile = "C:/Z/projects/metis/dataFiles/examples/example.proj")
  #
  # df <- dataGCAM$data
  # head(df); unique(df$subRegion); unique(df$param)
  # df%>%filter(subRegion=="Cauvery")%>%as.data.frame()%>%head()
  # gcamBasins <- as.character(unique((df%>%dplyr::filter(param=="watSupRunoffBasin"))$subRegion)); gcamBasins
  #
  # examplePolyFolder<-paste(dataFileFolder,"/gis/metis/gcam",sep="")
  # examplePolyFile<-paste("Global235_CLM_final_5arcmin_multipart",sep="")
  # x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  # head(x@data); names(x@data)
  #
  # gcamMapBasins <- as.character(unique(x@data$basin_name)); gcamMapBasins
  # basins2Change <- gcamBasins[!gcamBasins %in% gcamMapBasins]%>%sort(); basins2Change
  # data.table::fwrite(as.data.frame(gcamBasins)%>%arrange(gcamBasins),"gcamBasins.csv")
  # data.table::fwrite(as.data.frame(gcamMapBasins)%>%arrange(gcamMapBasins),"gcamMapBasins.csv")
  # data.table::fwrite(as.data.frame(basins2Change)%>%arrange(basins2Change),"basins2Change.csv")

  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=basin_name, subRegionAlt=basin_id) %>%
    dplyr::mutate(region = "World",
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
                                      TRUE~subRegion))
  mapx@data <- mapx@data %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  # gcamMapBasinsNew <- as.character(unique(mapx@data$subRegion)); gcamMapBasinsNew
  # basins2Change1 <- gcamBasins[!gcamBasins %in% gcamMapBasinsNew]%>%sort(); basins2Change1
  format(object.size(mapx), units="Mb")
  #mapx<-as(simplify_shape(mapx, fact = 0.05),Class="Spatial")
  #format(object.size(mapx), units="Mb")
  #sp::plot(mapx)
  # rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F, facetsON=F, fileName="factp1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMBasins <- mapx
  mapGCAMBasins@data <- mapGCAMBasins@data %>%
    dplyr::mutate(name="mapGCAMBasins")
  use_data(mapGCAMBasins, version=3, overwrite=T)
}


# GCAM Land
#------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/metis/gcam",sep="")
  examplePolyFile<-paste("region32glu_moirai_out_vect",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  idMapping <- data.table::fread(paste(dataFileFolder,"/gis/metis/gcam/GCAM_region_names.csv",sep=""),skip=4,header=T)
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=glu_name, subRegionAlt=Rg32Glu_id, glu_id, reg32_id) %>%
    dplyr::mutate(subRegionType="GCAMRegLand",
                  source="https://confluence.pnnl.gov/confluence/display/JGCRI/GCAM+Shape+Files",
                  region = "World",
                  region=gsub("-","_",region),
                  subRegion=gsub("-","_",subRegion),
                  subRegion=gsub("_Basin","",subRegion),
                  subRegion=gsub("Rfo","Rio",subRegion),
                  subRegion=gsub("-","_",subRegion),
                  subRegion=case_when(subRegion=="HamuniMashkel"~"Hamun_i_Mashkel",
                                      subRegion=="Yucat_µ„ön_Peninsula"~"Yucatan_Peninsula",
                                      subRegion=="Rh(ne"~"Rhone",
                                      subRegion=="Hong_(Red_River)"~"Hong_Red_River",
                                      TRUE~subRegion))%>%
    dplyr::left_join(idMapping%>%
                       dplyr::select(reg32_name=region, reg32_id=GCAM_region_ID)%>%
                       unique())
  mapx@data <- mapx@data %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  head(mapx@data)
  format(object.size(mapx), units="Mb")
  #mapx<-as(simplify_shape(mapx, fact = 0.05),Class="Spatial")
  #format(object.size(mapx), units="Mb")
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F, facetsON=F, fileName="factp1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMLand <- mapx
  mapGCAMLand@data <- mapGCAMLand@data %>%
    dplyr::mutate(name="mapGCAMLand")
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
  examplePolyFolder<-paste(dataFileFolder,"/gis/metis/subbasin_hydrobasin",sep="")
  examplePolyFile<-paste("hydrobasins_level_1",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HYBAS_ID) %>%
    dplyr::mutate(region="World",subRegionType="hydroshed1", subRegionAlt=subRegion,source="https://www.naturalearthdata.com/downloads/")
  head(mapx@data); mapx@data%>%distinct(region)%>%arrange(region)
  format(object.size(mapx), units="Mb")
  a<-tmaptools::simplify_shape(mapx, fact = 0.01)
  mapx <- as(sf::st_collection_extract(x = st_geometry(a),
                                       type = "POLYGON"), "Spatial")
  format(object.size(mapx), units="Mb")
  # Need to Covnert this back to an spdf
  p.df <- data.frame( ID=1:length(mapx))
  pid <- sapply(slot(mapx, "polygons"), function(x) slot(x, "ID")) # Extract polygon ID's
  p.df <- data.frame( ID=1:length(mapx), row.names = pid) # Create dataframe with correct rownames
  p <- SpatialPolygonsDataFrame(mapx, p.df)
  p@data <- a%>%as.data.frame()%>%dplyr::select(-geometry)
  mapx<-p
  format(object.size(mapx), units="Mb")
  # sp::plot(mapx)
  # rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F, facetsON=F,fileName = "HydroShed1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapHydroShed1 <- mapx
  mapHydroShed1@data <- mapHydroShed1@data %>%
    dplyr::mutate(name="mapHydroShed1")
  use_data(mapHydroShed1, version=3, overwrite=T)
}

# HydroSheds Level 2#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/metis/subbasin_hydrobasin",sep="")
  examplePolyFile<-paste("hydrobasins_level_2",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HYBAS_ID, SUB_AREA) %>%
    dplyr::mutate(region="World",subRegionType="hydroshed2", subRegionAlt=subRegion,source="https://www.naturalearthdata.com/downloads/")
  head(mapx@data); mapx@data%>%distinct(region)%>%arrange(region)
  a<-tmaptools::simplify_shape(mapx, fact = 0.01)
  mapx <- as(sf::st_collection_extract(x = st_geometry(a),
                                       type = "POLYGON"), "Spatial")
  format(object.size(mapx), units="Mb")
  # Need to Covnert this back to an spdf
  p.df <- data.frame( ID=1:length(mapx))
  pid <- sapply(slot(mapx, "polygons"), function(x) slot(x, "ID")) # Extract polygon ID's
  p.df <- data.frame( ID=1:length(mapx), row.names = pid) # Create dataframe with correct rownames
  p <- SpatialPolygonsDataFrame(mapx, p.df)
  p@data <- a%>%as.data.frame()%>%dplyr::select(-geometry)
  mapx<-p
  format(object.size(mapx), units="Mb")
  # sp::plot(mapx)
  # rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F, facetsON=F,fileName = "HydroShed1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapHydroShed2 <- mapx
  mapHydroShed2@data <- mapHydroShed2@data %>%
    dplyr::mutate(name="mapHydroShed2")
  use_data(mapHydroShed2, version=3, overwrite=T)
}

# HydroSheds Level 3
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/metis/subbasin_hydrobasin",sep="")
  examplePolyFile<-paste("hydrobasins_level_3",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HYBAS_ID, SUB_AREA) %>%
    dplyr::mutate(region="World",subRegionType="hydroshed3", subRegionAlt=subRegion,source="https://www.naturalearthdata.com/downloads/")
  head(mapx@data); mapx@data%>%distinct(region)%>%arrange(region)
  a<-tmaptools::simplify_shape(mapx, fact = 0.01)
  mapx <- as(sf::st_collection_extract(x = st_geometry(a),
                                       type = "POLYGON"), "Spatial")
  format(object.size(mapx), units="Mb")
  # Need to Covnert this back to an spdf
  p.df <- data.frame( ID=1:length(mapx))
  pid <- sapply(slot(mapx, "polygons"), function(x) slot(x, "ID")) # Extract polygon ID's
  p.df <- data.frame( ID=1:length(mapx), row.names = pid) # Create dataframe with correct rownames
  p <- SpatialPolygonsDataFrame(mapx, p.df)
  p@data <- a%>%as.data.frame()%>%dplyr::select(-geometry)
  mapx<-p
  format(object.size(mapx), units="Mb")
  # sp::plot(mapx)
  # rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F, facetsON=F,fileName = "HydroShed1")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapHydroShed3 <- mapx
  mapHydroShed3@data <- mapHydroShed3@data %>%
    dplyr::mutate(name="mapHydroShed3")
  use_data(mapHydroShed3, version=3, overwrite=T)
}

# # HydroSheds Level 4
# #-------------------
# if(redoMaps){
# examplePolyFolder<-paste(dataFileFolder,"/gis/metis/subbasin_hydrobasin",sep="")
# examplePolyFile<-paste("hydrobasins_level_4",sep="")
# x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
# head(x@data); names(x@data)
# mapx <- x
# mapx@data <- mapx@data %>%
#   dplyr::select(subRegion=HYBAS_ID, SUB_AREA) %>%
#   dplyr::mutate(region="World",subRegionType="hydroshed4", subRegionAlt=subRegion,source="https://www.naturalearthdata.com/downloads/")
# head(mapx@data); mapx@data%>%distinct(region)%>%arrange(region)
# a<-tmaptools::simplify_shape(mapx, fact = 0.01)
# mapx <- as(sf::st_collection_extract(x = st_geometry(a),
#                                      type = "POLYGON"), "Spatial")
# format(object.size(mapx), units="Mb")
# # Need to Covnert this back to an spdf
# p.df <- data.frame( ID=1:length(mapx))
# pid <- sapply(slot(mapx, "polygons"), function(x) slot(x, "ID")) # Extract polygon ID's
# p.df <- data.frame( ID=1:length(mapx), row.names = pid) # Create dataframe with correct rownames
# p <- SpatialPolygonsDataFrame(mapx, p.df)
# p@data <- a%>%as.data.frame()%>%dplyr::select(-geometry)
# mapx<-p
# format(object.size(mapx), units="Mb")
# # sp::plot(mapx)
# # rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F, facetsON=F,fileName = "HydroShed1")
# mapHydroShed4 <- mapx
# use_data(mapHydroShed4, version=3, overwrite=T)
# }

# HUC USGS
# https://water.usgs.gov/GIS/huc.html
# https://datagateway.nrcs.usda.gov/Catalog/ProductDescription/WBD.html
# https://nrcs.app.box.com/v/huc

# USGS HUC Levels
# US52 HUC 2
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/metis/USA/WBD_LatestVersion/wbdhu2_a_us_september2019",sep="")
  examplePolyFile<-paste("WBDHU2",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data); x@data%>%distinct(STATES)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HUC2, subRegionAlt=NAME,STATES) %>%
    dplyr::mutate(region="USA",subRegionType="US52HUC2", source="https://water.usgs.gov/GIS/huc.html")
  head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
  format(object.size(mapx), units="Mb")
  mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
  format(object.size(mapx), units="Mb")
  m1 <- mapx
  m2 <- rmap::mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx@data <- mapx@data%>%droplevels()
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  # mapx <- rmap::mapUS52HUC2
  # Crop Eastern edge of Alaska
  # Check US49 and US52 Limits:
  # as.data.frame(sp::bbox(mapUS49))
  # x (bbox1$min) east limit for US49 is -66.94989
  # Check limit for Puerto Rico
  # mapxPR = mapx[mapx@data$subRegion=="PR",]
  # mapxPR@data <- droplevels(mapxPR@data)
  # as.data.frame(sp::bbox(mapxPR))
  # x (bbox1$min) east limit for US49 is -65.22157
  # Set limit to -65
  bbox1<-as.data.frame(sp::bbox(mapx)); bbox1
  bbox1$min;bbox1$max
  bbox1$min[1]<-bbox1$min[1]
  bbox1$max[1]<--65
  bbox1$min;bbox1$max;
  bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
  as.data.frame(sp::bbox(bbox1))
  sp::proj4string(bbox1)<-sp::proj4string(mapx) # ASSIGN COORDINATE SYSTEM
  mapx<-raster::crop(mapx, bbox1)
  mapx@bbox <- bbox1@bbox
  mapx@data <- droplevels(mapx@data)
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=T,save=F)
  #---------------------
  mapUS52HUC2 <- mapx
  mapUS52HUC2@data <- mapUS52HUC2@data %>%
    dplyr::mutate(name="mapUS52HUC2")
  use_data(mapUS52HUC2, version=3, overwrite=T)
}

# US49 HUC 2
#-------------------
if(redoMaps){
  mapx <- mapUS52HUC2
  m1 <- mapx
  m2 <- rmap::mapUS49
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx@data <- mapx@data%>%droplevels()
  head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
  format(object.size(mapx), units="Mb")
  # mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
  # format(object.size(mapx), units="Mb")
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=T,save=F, facetsON=F, fileName = "HUC2")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapx@data <- mapx@data%>%dplyr::mutate(subRegionType="US49HUC2")
  mapUS49HUC2 <- mapx
  mapUS49HUC2@data <- mapUS49HUC2@data %>%
    dplyr::mutate(name="mapUS49HUC2")
  use_data(mapUS49HUC2, version=3, overwrite=T)
}

# US52 HUC 4
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/metis/USA/WBD_LatestVersion/wbdhu4_a_us_september2019",sep="")
  examplePolyFile<-paste("WBDHU4",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data); unique(x@data$STATES)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=HUC4, subRegionAlt=NAME,STATES) %>%
    dplyr::mutate(region="USA",subRegionType="US52HUC4", source="https://water.usgs.gov/GIS/huc.html")
  head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
  format(object.size(mapx), units="Mb")
  mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
  format(object.size(mapx), units="Mb")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapUS52HUC4 <- mapx
  m1 <- mapx
  m2 <- rmap::mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx@data <- mapx@data%>%droplevels()
  # mapx <- rmap::mapUS52HUC4
  # Crop Eastern edge of Alaska
  # Check US49 and US52 Limits:
  # as.data.frame(sp::bbox(mapUS49))
  # x (bbox1$min) east limit for US49 is -66.94989
  # Check limit for Puerto Rico
  # mapxPR = mapx[mapx@data$subRegion=="PR",]
  # mapxPR@data <- droplevels(mapxPR@data)
  # as.data.frame(sp::bbox(mapxPR))
  # x (bbox1$min) east limit for US49 is -65.22157
  # Set limit to -65
  bbox1<-as.data.frame(sp::bbox(mapx)); bbox1
  bbox1$min;bbox1$max
  bbox1$min[1]<-bbox1$min[1]
  bbox1$max[1]<--65
  bbox1$min;bbox1$max;
  bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
  as.data.frame(sp::bbox(bbox1))
  sp::proj4string(bbox1)<-sp::proj4string(mapx) # ASSIGN COORDINATE SYSTEM
  mapx<-raster::crop(mapx, bbox1)
  mapx@bbox <- bbox1@bbox
  mapx@data <- droplevels(mapx@data)
  m1 <- mapx
  m2 <- mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx@data <- mapx@data%>%droplevels()
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=T,save=F)
  #---------------------
  mapUS52HUC4 <- mapx
  mapUS52HUC4@data <- mapUS52HUC4@data %>%
    dplyr::mutate(name="mapUS52HUC4")
  use_data(mapUS52HUC4, version=3, overwrite=T)
}

# US49 HUC 4
#-------------------
if(redoMaps){
  mapx <- mapUS52HUC4
  m1 <- mapx
  m2 <- rmap::mapUS49
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx@data <- mapx@data%>%droplevels()
  head(mapx@data); mapx@data%>%distinct(subRegionAlt)%>%arrange(subRegionAlt)
  format(object.size(mapx), units="Mb")
  #mapx<-as(simplify_shape(mapx, fact = 0.01),Class="Spatial")
  #format(object.size(mapx), units="Mb")
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=T,save=F, facetsON=F, fileName = "HUC4")
  mapx@data<-mapx@data%>%dplyr::mutate(subRegionType="US49HUC4")
  mapUS49HUC4 <- mapx
  mapUS49HUC4@data <- mapUS49HUC4@data %>%
    dplyr::mutate(name="mapUS49HUC4")
  use_data(mapUS49HUC4, version=3, overwrite=T)
}


#-----------------
# US Maps ( 52 State, 49 State, Counties, Regions, Grid Regions)
#-----------------

# US 52 (including Alaska, Hawaii and Puerto Rico)
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/metis/USA/cb_2018_us_state_20m",sep="")
  examplePolyFile<-paste("cb_2018_us_state_20m",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data); nrow(x)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=STUSPS,subRegionAlt=NAME, STATEFP) %>%
    dplyr::mutate(region="USA",subRegionType="US52", source="https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html")
  head(mapx@data); unique(mapx$subRegion)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  # mapx <- rmap::mapUS52
  # Crop Eastern edge of Alaska
  # Check US49 and US52 Limits:
  # as.data.frame(sp::bbox(mapUS49))
  # x (bbox1$min) east limit for US49 is -66.94989
  # Check limit for Puerto Rico
  # mapxPR = mapx[mapx@data$subRegion=="PR",]
  # mapxPR@data <- droplevels(mapxPR@data)
  # as.data.frame(sp::bbox(mapxPR))
  # x (bbox1$min) east limit for US49 is -65.22157
  # Set limit to -65
  bbox1<-as.data.frame(sp::bbox(mapx)); bbox1
  bbox1$min;bbox1$max
  bbox1$min[1]<-bbox1$min[1]
  bbox1$max[1]<--65
  bbox1$min;bbox1$max;
  bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
  as.data.frame(sp::bbox(bbox1))
  sp::proj4string(bbox1)<-sp::proj4string(mapx) # ASSIGN COORDINATE SYSTEM
  mapx<-raster::crop(mapx, bbox1)
  mapx@bbox <- bbox1@bbox
  mapx@data <- droplevels(mapx@data)
  mapx@data <- mapx@data %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=T,save=F)
  #---------------------
  mapUS52 <- mapx
  mapUS52@data <- mapUS52@data %>%
    dplyr::mutate(name="mapUS52")
  use_data(mapUS52, version=3, overwrite=T)
}


# US 52 with Alaska (AK), Hawaii (HI) and Puerto Rico (PR) shrunken and shifted
#-------------------
if(redoMaps){
us <-rmap::mapUS52
# convert it to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)
# Extract polygon ID's
pid <- sapply(slot(us_aea, "polygons"), function(x) slot(x, "ID")); pid
# Create dataframe with correct rownames
p.df <- data.frame( us_aea@data,ID=1:length(us_aea), row.names = pid); p.df
# Try coersion again and check class
us_aea <- SpatialPolygonsDataFrame(us_aea, p.df); us_aea
# extract, then rotate, shrink & move alaska (and reset projection)
# need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
alaska <- us_aea[us_aea$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)
rmap::rmap::map(alaska)
# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea)
rmap::rmap::map(hawaii)
# extract, then rotate & shift Puerto Rico
pr <- us_aea[us_aea$STATEFP=="72",]
#pr <- elide(pr, rotate=-35)
pr <- elide(pr, shift=c(-2500000,0))
proj4string(pr) <- proj4string(us_aea);
rmap::rmap::map(pr)
# remove old states and put new ones back in; note the different order
# we're also removing puerto rico in this example but you can move it
# between texas and florida via similar methods to the ones we just used
mapUS52Compact <- us_aea[!us_aea$STATEFP %in% c("02", "15", "72"),]
mapUS52Compact <- rbind(mapUS52Compact, alaska, hawaii, pr)
mapUS52Compact<-spTransform(mapUS52Compact, CRS(proj4string(mapUS52)))
proj4string(mapUS52Compact)<-proj4string(mapUS52)
proj4string(mapUS52Compact)
mapUS52Compact@data <- mapUS52Compact@data %>% dplyr::mutate(subRegionType="US52Compact")
rmap::map(mapUS52Compact, labels=T)
#---------------------
mapUS52Compact@data <- mapUS52Compact@data %>%
  dplyr::mutate(name="mapUS52Compact")
use_data(mapUS52Compact, version=3, overwrite=T)
}


# US 49 (Excluding Alsaka, Hawaii and Puerto Rico)
#-------------------
if(redoMaps){
  mapx <- mapUS52[(mapUS52$region=="USA" & !mapUS52$subRegion %in% c("AK","HI","PR")),]
  mapx@data <- mapx@data%>%droplevels()
  head(mapx@data); nrow(mapx); mapx@data%>%distinct(subRegion)
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapx@data<-mapx@data%>%dplyr::mutate(subRegionType="US49")
  mapx@data <- mapx@data %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  mapUS49<-mapx
  mapUS49@data <- mapUS49@data %>%
    dplyr::mutate(name="mapUS49")
  use_data(mapUS49, version=3, overwrite=T)
}


# US 52 Counties
#-------------------
if(redoMaps){
  examplePolyFolder<-paste(dataFileFolder,"/gis/metis/USA/cb_2018_us_county_20m",sep="")
  examplePolyFile<-paste("cb_2018_us_county_20m",sep="")
  x=rgdal::readOGR(dsn=examplePolyFolder,layer=examplePolyFile,use_iconv=T,encoding='UTF-8')
  head(x@data); names(x@data); nrow(x)
  mapx <- x
  mapx@data <- mapx@data %>%
    dplyr::select(subRegion=NAME,subRegionAlt=COUNTYFP,STATEFP) %>%
    dplyr::mutate(region="USA",subRegionType="US52county", source="https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html") %>%
    dplyr::left_join(mapUS52@data%>%dplyr::select(STATEFP,STATECODE=subRegion, STATENAME=subRegionAlt))
  head(mapx@data); unique(mapx$subRegion)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  # mapx <- rmap::mapUS52county
  # Crop Eastern edge of Alaska
  # Check US49 and US52 Limits:
  # as.data.frame(sp::bbox(mapUS49))
  # x (bbox1$min) east limit for US49 is -66.94989
  # Check limit for Puerto Rico
  # mapxPR = mapx[mapx@data$subRegion=="PR",]
  # mapxPR@data <- droplevels(mapxPR@data)
  # as.data.frame(sp::bbox(mapxPR))
  # x (bbox1$min) east limit for US49 is -65.22157
  # Set limit to -65
  bbox1<-as.data.frame(sp::bbox(mapx)); bbox1
  bbox1$min;bbox1$max
  bbox1$min[1]<-bbox1$min[1]
  bbox1$max[1]<--65
  bbox1$min;bbox1$max;
  bbox1<-methods::as(raster::extent(as.vector(t(bbox1))), "SpatialPolygons")
  as.data.frame(sp::bbox(bbox1))
  sp::proj4string(bbox1)<-sp::proj4string(mapx) # ASSIGN COORDINATE SYSTEM
  mapx<-raster::crop(mapx, bbox1)
  mapx@bbox <- bbox1@bbox
  mapx@data <- droplevels(mapx@data)
  mapx@data <- mapx@data %>%
    dplyr::mutate(COUNTYCODE=subRegionAlt,
                  subRegionAlt=paste(subRegion,STATECODE,sep="_"))%>%
    dplyr::rename(subRegion=subRegionAlt,
                  subRegionAlt=subRegion); mapx@data
  mapx@data <- mapx@data %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=T,save=F)
  #---------------------
  mapUS52County <- mapx
  mapUS52County@data <- mapUS52County@data %>%
    dplyr::mutate(name="mapUS52County")
  use_data(mapUS52County, version=3, overwrite=T)
}

# US 52 Counties with Alaska (AK), Hawaii (HI) and Puerto Rico (PR) shrunken and shifted
#-------------------
if(redoMaps){
  us <-rmap::mapUS52County
  # convert it to Albers equal area
  us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  us_aea@data$id <- rownames(us_aea@data)
  # Extract polygon ID's
  pid <- sapply(slot(us_aea, "polygons"), function(x) slot(x, "ID")); pid
  # Create dataframe with correct rownames
  p.df <- data.frame( us_aea@data,ID=1:length(us_aea), row.names = pid); p.df
  # Try coersion again and check class
  us_aea <- SpatialPolygonsDataFrame(us_aea, p.df); us_aea
  # extract, then rotate, shrink & move alaska (and reset projection)
  # need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
  alaska <- us_aea[us_aea$STATENAME=="Alaska",]
  alaska <- elide(alaska, rotate=-50)
  alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
  alaska <- elide(alaska, shift=c(-2100000, -2500000))
  proj4string(alaska) <- proj4string(us_aea)
  rmap::rmap::map(alaska)
  # extract, then rotate & shift hawaii
  hawaii <- us_aea[us_aea$STATENAME=="Hawaii",]
  hawaii <- elide(hawaii, rotate=-35)
  hawaii <- elide(hawaii, shift=c(5400000, -1400000))
  proj4string(hawaii) <- proj4string(us_aea)
  rmap::rmap::map(hawaii)
  # extract, then rotate & shift Puerto Rico
  pr <- us_aea[us_aea$STATENAME=="Puerto Rico",]
  #pr <- elide(pr, rotate=-35)
  pr <- elide(pr, shift=c(-2500000,0))
  proj4string(pr) <- proj4string(us_aea);
  rmap::rmap::map(pr)
  # remove old states and put new ones back in; note the different order
  # we're also removing puerto rico in this example but you can move it
  # between texas and florida via similar methods to the ones we just used
  mapUS52CountyCompact <- us_aea[!us_aea$STATENAME %in% c("Alaska", "Hawaii", "Puerto Rico"),]
  mapUS52CountyCompact <- rbind(mapUS52CountyCompact, alaska, hawaii, pr)
  mapUS52CountyCompact<-spTransform(mapUS52CountyCompact, CRS(proj4string(mapUS52)))
  proj4string(mapUS52CountyCompact)<-proj4string(mapUS52)
  proj4string(mapUS52CountyCompact)
  mapUS52CountyCompact@data <- mapUS52CountyCompact@data %>% dplyr::mutate(subRegionType="US52CountyCompact")
  rmap::map(mapUS52CountyCompact)
  #---------------------
  mapUS52CountyCompact@data <- mapUS52CountyCompact@data %>%
    dplyr::mutate(name="mapUS52CountyCompact")
  use_data(mapUS52CountyCompact, version=3, overwrite=T)
}

# US 49 Counties
#-------------------
if(redoMaps){
  mapx <- mapUS52County[(!mapUS52County$STATECODE %in% c("AK","HI","PR")),]
  mapx@data <- mapx@data%>%droplevels()
  head(mapx@data); nrow(mapx); mapx@data%>%distinct(subRegion)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapx@data<-mapx@data%>%dplyr::mutate(subRegionType="US49County")%>%
    dplyr::rename(subRegion=subRegionAlt,
                  subRegionAlt=subRegion)
  mapx@data <- mapx@data %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  mapUS49County<-mapx
  mapUS49County@data <- mapUS49County@data %>%
    dplyr::mutate(name="mapUS49County")
  use_data(mapUS49County, version=3, overwrite=T)
}


# Merge
#-------------------

# Merge US52 with GCAM Regs
if(redoMaps){
  m1 <- mapGCAMReg32
  m2 <- mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::union(m1,m2)
  mapx@data <- mapx@data %>%
    dplyr::mutate(region="World",
                  subRegionType="GCAMReg32US52",
                  subRegion.2=as.character(subRegion.2),
                  subRegion.1=as.character(subRegion.1),
                  subRegionAlt.1=as.numeric(subRegionAlt.1),
                  subRegionAlt.2=as.numeric(subRegionAlt.2),
                  subRegion=case_when(subRegion.2 %in% rmap::mappings()$US52~subRegion.2,
                                      TRUE~subRegion.1),
                  subRegionAlt=case_when(subRegion.2 %in% rmap::mappings()$US52~1,
                                         TRUE~subRegionAlt.1),
                  subRegionAlt=as.integer(subRegionAlt)) %>%
    dplyr::select(region,subRegion,subRegionType, subRegionAlt)
  mapx = raster::aggregate(mapx, by = c("subRegion","region","subRegionType","subRegionAlt"))
  mapx@data <- mapx@data %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMReg32US52<-mapx
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapGCAMReg32US52@data <- mapGCAMReg32US52@data %>%
    dplyr::mutate(name="mapGCAMReg32US52")
  mapGCAMReg32US52 <- mapGCAMReg32US52[!mapGCAMReg32US52@data$subRegion %in% c("USA"),] # Subset the shapefile to Colombia
  mapGCAMReg32US52@data <- droplevels(mapGCAMReg32US52@data)
  use_data(mapGCAMReg32US52, version=3, overwrite=T)
}


# Merge US52 with Countries file
if(redoMaps){
  m1 <- mapCountries
  m2 <- mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::union(m1,m2)
  mapx@data <- mapx@data %>%
    dplyr::mutate(region="World",
                  subRegionType="countriesUS52",
                  subRegion.2=as.character(subRegion.2),
                  subRegion.1=as.character(subRegion.1),
                  subRegionAlt.1=as.numeric(subRegionAlt.1),
                  subRegionAlt.2=as.numeric(subRegionAlt.2),
                  subRegion=case_when(subRegion.2 %in% rmap::mappings()$US52~subRegion.2,
                                      TRUE~subRegion.1),
                  subRegionAlt=case_when(subRegion.2 %in% rmap::mappings()$US52~1,
                                         TRUE~subRegionAlt.1),
                  subRegionAlt=as.integer(subRegionAlt)) %>%
    dplyr::select(region,subRegion,subRegionType, subRegionAlt)
  mapx = raster::aggregate(mapx, by = c("subRegion","region","subRegionType","subRegionAlt"))
  mapx@data <- mapx@data %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapCountriesUS52<-mapx
  sp::plot(mapx)
  rmap::map(mapx,labels=T,save=F)
  mapCountriesUS52@data <- mapCountriesUS52@data %>%
    dplyr::mutate(name="mapCountriesUS52")
  mapCountriesUS52 <- mapCountriesUS52[!mapCountriesUS52@data$subRegion %in% c("USA"),] # Subset the shapefile to Colombia
  mapCountriesUS52@data <- droplevels(mapCountriesUS52@data)
  use_data(mapCountriesUS52, version=3, overwrite=T)
}


# Merge Uruguay with GCAM 32
if(redoMaps){
  m1 <- mapGCAMReg32
  m2 <- mapCountries
  m2 <- m2[m2@data$subRegion %in% c("Uruguay"),] # Subset the shapefile to Colombia
  m2@data <- droplevels(m2@data)
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::union(m1,m2)
  mapx@data <- mapx@data %>%
    dplyr::mutate(region="World",
                  subRegionType="GCAMReg32Uruguay",
                  subRegion.2=as.character(subRegion.2),
                  subRegion.1=as.character(subRegion.1),
                  subRegionAlt.1=as.numeric(subRegionAlt.1),
                  subRegionAlt.2=as.numeric(subRegionAlt.2),
                  subRegion=case_when(subRegion.2 %in% "Uruguay"~subRegion.2,
                                      TRUE~subRegion.1),
                  subRegionAlt=case_when(subRegion.2 %in% "Uruguay"~33,
                                         TRUE~subRegionAlt.1),
                  subRegionAlt=as.integer(subRegionAlt)) %>%
    dplyr::select(region,subRegion,subRegionType,subRegionAlt)
  mapx = raster::aggregate(mapx, by = c("subRegion","region","subRegionType", "subRegionAlt"))
  mapx@data <- mapx@data %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapGCAMReg32Uruguay<-mapx
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapGCAMReg32Uruguay@data <- mapGCAMReg32Uruguay@data %>%
    dplyr::mutate(name="mapGCAMReg32Uruguay")
  use_data(mapGCAMReg32Uruguay, version=3, overwrite=T)
}


# Intersections
#-------------------

# Intersection of GCAM Basins and Countries
if(redoMaps){
  m1 <- mapGCAMBasins
  m2 <- mapCountries
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::intersect(m1,m2)

  mapx@data%>%head()%>%as.data.frame()

  mapx@data <- mapx@data %>%
    dplyr::mutate(subRegion=paste(subRegion.1,subRegion.2,sep="_X_"),
                  region=region.1,
                  subRegionAlt=paste(subRegionAlt.1,subRegionAlt.2,sep="_X_"),
                  subRegionType=paste(subRegionType.1,subRegionType.2,sep="_X_")) %>%
    dplyr::rename(subRegion_GCAMBasin=subRegion.1,
                  subRegion_Country=subRegion.2,
                  subRegionAlt_GCAMBasin=subRegionAlt.1,
                  subRegionAlt_Country=subRegionAlt.2,
                  subRegionType_GCAMBasin=subRegionType.1,
                  subRegionType_Country=subRegionType.2) %>%
    dplyr::select(-region.1,-region.2, -POP_EST) %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000) %>%
    dplyr::group_by(subRegion_Country) %>%
    dplyr::mutate(areaSubRegion_Country = sum(area_sqkm),
                  ratioAreaIntersect = round((area_sqkm/areaSubRegion_Country),4))%>%
    dplyr::ungroup()

  mapx@data%>%head()%>%as.data.frame()
  mapx1 <- mapx[grepl("Colombia",mapx@data$subRegion_Country),]; mapx1@data%>%head()%>%as.data.frame()
  mapx1@data <- droplevels(mapx1@data)
  sp::plot(mapx1)

  maptools::unionSpatialPolygons(rmap::mapUS49, "subRegion")

  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapIntersectGCAMBasinCountry<-mapx
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapIntersectGCAMBasinCountry@data <- mapIntersectGCAMBasinCountry@data %>%
    dplyr::mutate(name="mapIntersectGCAMBasinCountry")
  use_data(mapIntersectGCAMBasinCountry, version=3, overwrite=T)
}

# Intersection of GCAM Basins and 32 GCAM Regions
if(redoMaps){
  m1 <- mapGCAMBasins
  m2 <- mapGCAMReg32
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::intersect(m1,m2)
  mapx@data <- mapx@data %>%
    dplyr::mutate(subRegion=paste(subRegion.1,subRegion.2,sep="_X_"),
                  region=region.1,
                  subRegionAlt=paste(subRegionAlt.1,subRegionAlt.2,sep="_X_"),
                  subRegionType=paste(subRegionType.1,subRegionType.2,sep="_X_")) %>%
    dplyr::rename(subRegion_GCAMBasin=subRegion.1,
                  subRegion_GCAMReg32=subRegion.2,
                  subRegionAlt_GCAMBasin=subRegionAlt.1,
                  subRegionAlt_GCAMreg32=subRegionAlt.2,
                  subRegionType_GCAMBasin=subRegionType.1,
                  subRegionType_GCAMReg32=subRegionType.2) %>%
    dplyr::select(-region.1,-region.2) %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000) %>%
    dplyr::group_by(subRegion_GCAMReg32) %>%
    dplyr::mutate(areaSubRegion_GCAMReg32 = sum(area_sqkm),
                  ratioAreaIntersect = round((area_sqkm/areaSubRegion_GCAMReg32),4))%>%
    dplyr::ungroup()
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapIntersectGCAMBasin32Reg<-mapx
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapIntersectGCAMBasin32Reg@data <- mapIntersectGCAMBasin32Reg@data %>%
    dplyr::mutate(name="mapIntersectGCAMBasin32Reg")
  use_data(mapIntersectGCAMBasin32Reg, version=3, overwrite=T)
}

# Intersection of GCAM Basins and US 52 States
if(redoMaps){
  m1 <- mapGCAMBasins
  m2 <- mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::intersect(m1,m2)
  mapx@data <- mapx@data %>%
    dplyr::mutate(subRegion=paste(subRegion.1,subRegion.2,sep="_X_"),
                  region=region.1,
                  subRegionAlt=paste(subRegionAlt.1,subRegionAlt.2,sep="_X_"),
                  subRegionType=paste(subRegionType.1,subRegionType.2,sep="_X_")) %>%
    dplyr::rename(subRegion_GCAMBasin=subRegion.1,
                  subRegion_US52=subRegion.2,
                  subRegionAlt_GCAMBasin=subRegionAlt.1,
                  subRegionAlt_US52=subRegionAlt.2,
                  subRegionType_GCAMBasin=subRegionType.1,
                  subRegionType_US52=subRegionType.2) %>%
    dplyr::select(-region.1,-region.2, -STATEFP) %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000) %>%
    dplyr::group_by(subRegion_US52) %>%
    dplyr::mutate(areaSubRegion_US52 = sum(area_sqkm),
                  ratioAreaIntersect = round((area_sqkm/areaSubRegion_US52),4))%>%
    dplyr::ungroup()
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapIntersectGCAMBasinUS52<-mapx
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapIntersectGCAMBasinUS52@data <- mapIntersectGCAMBasinUS52@data %>%
    dplyr::mutate(name="mapIntersectGCAMBasinUS52")
  use_data(mapIntersectGCAMBasinUS52, version=3, overwrite=T)
}

# Intersection of GCAM Basins and US 52 County
if(redoMaps){
  m1 <- mapGCAMBasins
  m2 <- mapUS52County
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::intersect(m1,m2)
  mapx@data <- mapx@data %>%
    dplyr::mutate(subRegion=paste(subRegion.1,subRegion.2,sep="_X_"),
                  region=region.1,
                  subRegionAlt=paste(subRegionAlt.1,subRegionAlt.2,sep="_X_"),
                  subRegionType=paste(subRegionType.1,subRegionType.2,sep="_X_")) %>%
    dplyr::rename(subRegion_GCAMBasin=subRegion.1,
                  subRegion_US52County=subRegion.2,
                  subRegionAlt_GCAMBasin=subRegionAlt.1,
                  subRegionAlt_US52County=subRegionAlt.2,
                  subRegionType_GCAMBasin=subRegionType.1,
                  subRegionType_US52County=subRegionType.2) %>%
    dplyr::select(-region.1,-region.2, -STATEFP) %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000) %>%
    dplyr::group_by(subRegion_US52County) %>%
    dplyr::mutate(areaSubRegion_US52County = sum(area_sqkm),
                  ratioAreaIntersect = round((area_sqkm/areaSubRegion_US52County),4))%>%
    dplyr::ungroup()
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapIntersectGCAMBasinUS52County<-mapx
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapIntersectGCAMBasinUS52County@data <- mapIntersectGCAMBasinUS52County@data %>%
    dplyr::mutate(name="mapIntersectGCAMBasinUS52County")
  use_data(mapIntersectGCAMBasinUS52County, version=3, overwrite=T)
}

# Intersection of GCAM Basins and GCAM32Uruguay
if(redoMaps){
  m1 <- mapGCAMBasins
  m2 <- mapGCAMReg32Uruguay
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::intersect(m1,m2)
  mapx@data <- mapx@data %>%
    dplyr::mutate(subRegion=paste(subRegion.1,subRegion.2,sep="_X_"),
                  region=region.1,
                  subRegionAlt=paste(subRegionAlt.1,subRegionAlt.2,sep="_X_"),
                  subRegionType=paste(subRegionType.1,subRegionType.2,sep="_X_")) %>%
    dplyr::rename(subRegion_GCAMBasin=subRegion.1,
                  subRegion_GCAMReg32Uruguay=subRegion.2,
                  subRegionAlt_GCAMBasin=subRegionAlt.1,
                  subRegionAlt_GCAMReg32Uruguay=subRegionAlt.2,
                  subRegionType_GCAMBasin=subRegionType.1,
                  subRegionType_GCAMReg32Uruguay=subRegionType.2) %>%
    dplyr::select(-region.1,-region.2) %>%
    dplyr::mutate(area_sqkm=raster::area(mapx)/1000000) %>%
    dplyr::group_by(subRegion_GCAMReg32Uruguay) %>%
    dplyr::mutate(areaSubRegion_GCAMReg32Uruguay = sum(area_sqkm),
                  ratioAreaIntersect = round((area_sqkm/areaSubRegion_GCAMReg32Uruguay),4))%>%
    dplyr::ungroup()
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  mapIntersectGCAMBasin32RegUruguay<-mapx
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapIntersectGCAMBasin32RegUruguay@data <- mapIntersectGCAMBasin32RegUruguay@data %>%
    dplyr::mutate(name="mapIntersectGCAMBasin32RegUruguay")
  use_data(mapIntersectGCAMBasin32RegUruguay, version=3, overwrite=T)
}


# Cropped Files
#------------------------------

# Cropped GCAM Basins and US 52
if(redoMaps){
  m1 <- rmap::mapGCAMBasins
  m2 <- rmap::mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapx@data<-mapx@data%>%dplyr::mutate(subRegionType="GCAMBasinsUS52")
  mapx@data <- mapx@data %>% dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  mapGCAMBasinsUS52<-mapx
  mapGCAMBasinsUS52@data <- mapGCAMBasinsUS52@data %>%
    dplyr::mutate(name="mapGCAMBasinsUS52")
  use_data(mapGCAMBasinsUS52, version=3, overwrite=T)
}

# Cropped GCAM Basins and US 49 States
if(redoMaps){
  m1 <- rmap::mapGCAMBasins
  m2 <- rmap::mapUS49
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapx@data<-mapx@data%>%dplyr::mutate(subRegionType="GCAMBasinsUS49")
  mapx@data <- mapx@data %>% dplyr::mutate(area_sqkm=raster::area(mapx)/1000000)
  mapGCAMBasinsUS49<-mapx
  mapGCAMBasinsUS49@data <- mapGCAMBasinsUS49@data %>%
    dplyr::mutate(name="mapGCAMBasinsUS49")
  use_data(mapGCAMBasinsUS49, version=3, overwrite=T)
}


# Cropped GCAM Land and US 52
if(redoMaps){
  m1 <- rmap::mapGCAMLand
  m2 <- rmap::mapUS52
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapx@data<-mapx@data%>%dplyr::mutate(subRegionType="GCAMLandUS52")
  mapGCAMLandUS52<-mapx
  mapGCAMLandUS52@data <- mapGCAMLandUS52@data %>%
    dplyr::mutate(name="mapGCAMLandUS52")
  use_data(mapGCAMLandUS52, version=3, overwrite=T)
}

# Cropped GCAM Land and US 49 States
if(redoMaps){
  m1 <- rmap::mapGCAMLand
  m2 <- rmap::mapUS49
  m2 <- sp::spTransform(m2,raster::crs(m1))
  mapx<-raster::crop(m1,m2)
  head(mapx@data)
  mapx<-rgeos::gBuffer(mapx, byid=TRUE, width=0)
  format(object.size(mapx), units="Mb")
  sp::plot(mapx)
  rmap::map(data=mapx,fillColumn = "subRegion",labels=F,save=F)
  mapx@data<-mapx@data%>%dplyr::mutate(subRegionType="GCAMLandUS49")
  mapGCAMLandUS49<-mapx
  mapGCAMLandUS49@data <- mapGCAMLandUS49@data %>%
    dplyr::mutate(name="mapGCAMLandUS49")
  use_data(mapGCAMLandUS49, version=3, overwrite=T)
}


# Grid Files
#-------------------
if(!exists("grid025")){
  gridx <-  tibble::as_tibble(data.table::fread(paste(dataFileFolder,"/grids/emptyGrids/grid_025.csv",sep="")));
  grid025<-gridx
  use_data(grid025, version=3, overwrite=T)
}

if(!exists("grid050")){
  gridx <-  tibble::as_tibble(data.table::fread(paste(dataFileFolder,"/grids/emptyGrids/grid_050.csv",sep="")));
  grid050<-gridx
  use_data(grid050, version=3, overwrite=T)
}


#-------------------
# Example Data
#-------------------

# Example .proj file
#projFile <-"C:/Z/projects/metisGCAMUSA/metisOutputs/readGCAM/exampleGCAMproj.proj"
metis.readgcam(gcamdatabase = "C:/Z/projects/gcam-v5.2-Windows-Release-Package/output/exampleSSP2050",
               paramsSelect =c("elecByTechTWh", "pop","watWithdrawBySec","watSupRunoffBasin",
                                   "landAlloc","agProdByCrop"),
               dataProjFile = "exampleGCAM52releaseSSP3SSP52050.proj",saveData = F,reReadData = T)
projFile <-"C:/Z/projects/metis/dataFiles/examples/exampleGCAM52releaseSSP3SSP52050.proj"
exampleGCAMproj <- rgcam::loadProject(projFile)
use_data(exampleGCAMproj, version=3, overwrite=T)

# Examples .metisMapProcess
dataGCAM<-metis.readgcam (dataProjFile = exampleGCAMproj,
                          paramsSelect =c("elecByTechTWh", "pop","watWithdrawBySec","watSupRunoffBasin",
                                          "landAlloc","agProdByCrop"),saveData = F)
df <- dataGCAM$data;unique(df$scenario); unique(df$param);
exampleMapDataParam <- dataGCAM$dataAggParam %>%
  dplyr::mutate(classPalette=case_when(param=="agProdByCrop"~"pal_green",
                                       param=="elecByTechTWh"~"pal_hot",
                                       param=="landAlloc"~"pal_green",
                                       param=="watWithdrawBySec"~"pal_wet",
                                       param=="watSupRunoffBasin"~"pal_wet",
                                       TRUE~"pal_hot"))
exampleMapDataClass <- dataGCAM$dataAggClass1 %>%
  dplyr::mutate(classPalette=case_when(param=="agProdByCrop"~"pal_green",
                                       param=="elecByTechTWh"~"pal_hot",
                                       param=="landAlloc"~"pal_green",
                                       param=="watWithdrawBySec"~"pal_wet",
                                       param=="watSupRunoffBasin"~"pal_wet",
                                       TRUE~"pal_hot"))
use_data(exampleMapDataParam, version=3, overwrite=T)
use_data(exampleMapDataClass, version=3, overwrite=T)

#-------------------
# Data
#-------------------

# Metis XMl Query Files
xmlFilePath = paste(dataFileFolder,"/gcam/metisQueries.xml",sep="")
xmlfile <- XML::xmlTreeParse(xmlFilePath)
xmltop <- XML::xmlRoot(xmlfile)
top <- XML::xmlNode(XML::xmlName(xmltop))
for(i in 1:length(xmltop)){
      top <- XML::addChildren(top, xmltop[[i]])
}
xmlMetisQueries <- top
use_data(xmlMetisQueries, version=3, overwrite=T)


# Capacity factors
data_capfactors <- data.table::fread(file=paste(dataFileFolder,"/gcam/capacity_factor_by_elec_gen_subsector.csv",sep=""),skip=5,encoding="Latin-1")
data_cap_cost_tech <- data.table::fread(paste(dataFileFolder,"/gcam/investCalcs/L2233.GlobalTechCapital_elecPassthru.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_cap_cost_cool <- data.table::fread(paste(dataFileFolder,"/gcam/investCalcs/L2233.GlobalTechCapital_elec_cool.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_cap_cost_int_tech <- data.table::fread(paste(dataFileFolder,"/gcam/investCalcs/L2233.GlobalIntTechCapital_elec.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_cap_cost_int_cool <- data.table::fread(paste(dataFileFolder,"/gcam/investCalcs/L2233.GlobalIntTechCapital_elec_cool.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_A23.globaltech_retirement <- data.table::fread(paste(dataFileFolder,"/gcam/investCalcs/A23.globaltech_retirement.csv",sep=""), skip=1)
data_capac_fac <- data.table::fread(paste(dataFileFolder,"/gcam/investCalcs/L223.GlobalTechCapFac_elec.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_capac_fac_int <- data.table::fread(paste(dataFileFolder,"/gcam/investCalcs/L223.GlobalIntTechCapFac_elec.csv", sep=""), skip=1, stringsAsFactors = FALSE)
data_tech_mapping <- data.table::fread(paste(dataFileFolder,"/gcam/investCalcs/agg_tech_mapping.csv", sep=""), skip=1)
use_data(data_capfactors, version=3, overwrite=T)
use_data(data_cap_cost_tech, version=3, overwrite=T)
use_data(data_cap_cost_cool, version=3, overwrite=T)
use_data(data_cap_cost_int_tech, version=3, overwrite=T)
use_data(data_cap_cost_int_cool, version=3, overwrite=T)
use_data(data_A23.globaltech_retirement, version=3, overwrite=T)
use_data(data_capac_fac, version=3, overwrite=T)
use_data(data_capac_fac_int, version=3, overwrite=T)
use_data(data_tech_mapping, version=3, overwrite=T)





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

