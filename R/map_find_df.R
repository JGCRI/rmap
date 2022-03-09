#' map_find_df
#'
#' Given a data.frame with a subRegion column, this function searches for an appropriate map from the pre-loaded rmap maps.
#'
#' @param data data table to be processed
#' @keywords map, find
#' @return dataframe with modified subRegions, subRegion shapefile and subRegion type
#' @export


map_find_df <- function(data) {

  #......................................................
  # Initialize
  #.....................................................

    if(T){
    NULL -> subRegShapeFoundx -> subRegShapeTypeFoundx -> subRegNotInShapeFoundx ->
      dataFound -> subRegionShapex -> mapStatesx -> subRegionAlt -> subRegion -> mapFindx -> subRegion1 ->
        subRegNum-> subRegionMap->long}

  #......................................................
  # Check columns and map subRegions to rmap shape regions
  #.....................................................

  if(T){

    # Check data to make sure basic columns are available
    addMissing<-function(data){
      if(any(grepl("\\<subregion\\>",names(data),ignore.case = T))){
        data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subregion\\>",names(data),ignore.case = T)])[1])}
      if(any(grepl("\\<subregions\\>",names(data),ignore.case = T))){
        data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subregions\\>",names(data),ignore.case = T)])[1])}
    return(data)
      }
    data <- addMissing(data)

    if(!all(c("subRegion") %in% names(data))){stop("data must have subRegion columns.")}

    subRegShapeTblOrig <- unique(data$subRegion)

    # Map subRegions to rmap regions
    data <- data %>%
      dplyr::left_join(rmap::mapping_gcambasins,by="subRegion")%>%
      dplyr::mutate(subRegion=dplyr::case_when(!is.na(subRegionMap)~subRegionMap,
                                        TRUE~subRegion))%>%
      dplyr::select(-subRegionMap)

    subRegShapeTbl <- gsub("-", "_", tolower(unique(data$subRegion)))

  }

    #.....................................................
    # Find how many regions in the data belong to different maps
    #.....................................................

    if (T) {

      mapReg <- data.frame()

      for (i in 1:length(rmap::mapsSubRegions)) {
        subRegNum_i <-
          length(subRegShapeTbl[subRegShapeTbl %in% rmap::mapsSubRegions[[i]]])
        dfx <-
          data.frame(map = names(rmap::mapsSubRegions)[i], subRegNum = subRegNum_i)
        mapReg <- mapReg %>% dplyr::bind_rows(dfx)
      }

      mapReg <-
        mapReg %>% dplyr::arrange(-subRegNum) %>% dplyr::filter(subRegNum > 0)

      mapReg
    }

    #.....................................................
    # Use pre ranked maps
    #.....................................................

    if (T) {
      mapRanked <- tibble::tribble(
        ~	map	, ~ rank,
        "mapUS49",1,
        "mapUS52",2,
        "mapCountries",11,
        "mapGCAMReg32",12,
        "mapGCAMReg32Uruguay",12.5,
        "mapGCAMReg32US52",15.5,
        "mapCountriesUS52",15,
        "mapStates",9,
        "mapUS49County",5,
        "mapUS52County",6,
        "mapGCAMBasins",19,
        "mapGCAMBasinsUS49",17,
        "mapGCAMBasinsUS52",18,
        "mapGCAMLand",25,
        "mapGCAMLandUS49",23,
        "mapGCAMLandUS52",24,
        "mapUS49HUC2",29,
        "mapUS52HUC2",30,
        "mapUS49HUC4",33,
        "mapUS52HUC4",34,
        "mapUS49Alt",3,
        "mapUS52Alt",4,
        "mapGCAMReg32Alt",14,
        "mapCountriesAlt",13,
        "mapGCAMReg32US52Alt",16.5,
        "mapCountriesUS52Alt",16,
        "mapStatesAlt",10,
        "mapUS49CountyAlt",7,
        "mapUS52CountyAlt",8,
        "mapGCAMBasinsAlt",22,
        "mapGCAMBasinsUS49Alt",20,
        "mapGCAMBasinsUS52Alt",21,
        "mapGCAMLandAlt",28,
        "mapGCAMLandUS49Alt",26,
        "mapGCAMLandUS52Alt",27,
        "mapUS49HUC2Alt",31,
        "mapUS52HUC2Alt",32,
        "mapUS49HUC4Alt",35,
        "mapUS52HUC4Alt",36,
        "mapIntersectGCAMBasin32Reg",37,
        "mapIntersectGCAMBasinCountry",38,
        "mapIntersectGCAMBasinUS52", 39,
        "mapIntersectGCAMBasinUS52County", 40,
        "mapIntersectGCAMBasin32RegAlt",41,
        "mapIntersectGCAMBasinCountryAlt",42,
        "mapIntersectGCAMBasinUS52Alt", 43,
        "mapIntersectGCAMBasinUS52CountyAlt", 44,
        "mapIntersectGCAMBasin32RegUruguay",45,
        "mapIntersectGCAMBasin32RegAltUruguay",46
      )

      mapRanked %>% dplyr::arrange(rank)
    }

    #.....................................................
    # Choose maps with highest number of regions and if more than one then attach rank and choose highest rnank(lowest rank number)
    #.....................................................

    if (nrow(mapReg)>0) {
      mapMax <- mapReg %>%
        dplyr::filter(subRegNum == max(subRegNum)) %>%
        dplyr::left_join(mapRanked, by = "map")
      mapMax

      if (nrow(mapMax) > 1) {
        rlang::inform("More than one pre-loaded map contain the subRegions in the data provided.")
        rlang::inform("Choosing map based on pre-set map ranking.")
        print(mapMax %>% dplyr::arrange(rank))
        rlang::inform("To choose a different map, please assign it in shape directly.")
      }

      subRegChosen <- (mapMax %>%
                         dplyr::filter(rank == min(rank)))$map
      subRegChosen



    mapFindx <- get(paste(gsub("Alt","",subRegChosen),"df",sep=""))

    if(grepl("Alt",subRegChosen)){
      mapFindx <- mapFindx %>%
        dplyr::mutate(subRegionOrig=subRegion,
                      subRegion = subRegionAlt)
    }

    rlang::inform(paste("Using map: ", unique(mapFindx$subRegionType), sep = ""))
    }


    #.....................................................
    # Check if no subregions in pre-loaded maps
    #.....................................................

    if(T){

      subRegNotInShapeFoundx <- unique(data$subRegion)[!unique(data$subRegion) %in% unique(mapFindx$subRegion)]

      if (!is.null(subRegNotInShapeFoundx)) {
        if (length(subRegNotInShapeFoundx) > 0) {
          rlang::inform(paste(
            "subRegions in data not present in shapefile are: ",
            paste(subRegNotInShapeFoundx, collapse = ", "),
            sep = ""
          ))
        }
      }
    }

    # Check for incorrect "long" name
    if("long" %in% names(mapFindx)){
      mapFindx <- mapFindx %>%
        dplyr::rename(lon=long)
    }

    #.....................................................
    # Return Map
    #.....................................................


    invisible(mapFindx)

  } # CLose map finding function
