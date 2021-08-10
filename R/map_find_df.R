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
      dplyr::left_join(rmap::mappings("mappingGCAMBasins"),by="subRegion")%>%
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
        "subRegUS49",1,
        "subRegUS52",2,
        "subRegGCAMReg32",12,
        "subRegCountries",11,
        "subRegGCAMReg32US52",15.5,
        "subRegCountriesUS52",15,
        "subRegStates",9,
        "subRegUS49County",5,
        "subRegUS52County",6,
        "subRegGCAMBasins",19,
        "subRegGCAMBasinsUS49",17,
        "subRegGCAMBasinsUS52",18,
        "subRegGCAMLand",25,
        "subRegGCAMLandUS49",23,
        "subRegGCAMLandUS52",24,
        "subRegUS49HUC2",29,
        "subRegUS52HUC2",30,
        "subRegUS49HUC4",33,
        "subRegUS52HUC4",34,
        "subRegUS49Alt",3,
        "subRegUS52Alt",4,
        "subRegGCAMReg32Alt",14,
        "subRegCountriesAlt",13,
        "subRegGCAMReg32US52Alt",16.5,
        "subRegCountriesUS52Alt",16,
        "subRegStatesAlt",10,
        "subRegUS49CountyAlt",7,
        "subRegUS52CountyAlt",8,
        "subRegGCAMBasinsAlt",22,
        "subRegGCAMBasinsUS49Alt",20,
        "subRegGCAMBasinsUS52Alt",21,
        "subRegGCAMLandAlt",28,
        "subRegGCAMLandUS49Alt",26,
        "subRegGCAMLandUS52Alt",27,
        "subRegUS49HUC2Alt",31,
        "subRegUS52HUC2Alt",32,
        "subRegUS49HUC4Alt",35,
        "subRegUS52HUC4Alt",36,
      )

      mapRanked %>% dplyr::arrange(rank)
    }

    #.....................................................
    # Choose maps with highest number of regions and if more than one then attach rank and choose highest rnank(lowest rank number)
    #.....................................................

    if (T) {
      mapMax <- mapReg %>%
        dplyr::filter(subRegNum == max(subRegNum)) %>%
        dplyr::left_join(mapRanked, by = "map")
      mapMax

      if (nrow(mapMax) > 1) {
        print("More than one pre-loaded map contain the subRegions in the data provided.")
        print("Choosing map based on pre-set map ranking:")
        print(mapMax %>% dplyr::arrange(rank))
        print("To choose a different map, please assign it in shape directly.")
      }

      subRegChosen <- (mapMax %>%
                         dplyr::filter(rank == min(rank)))$map
      subRegChosen
    }


    mapFindx <- get(paste(gsub("subReg","map",subRegChosen),"df",sep=""))

    print(paste("Using map: ", unique(mapFindx$subRegionType), sep = ""))


    #.....................................................
    # Check if no subregions in pre-loaded maps
    #.....................................................

    if(T){

      subRegNotInShapeFoundx <- unique(data$subRegion)[!unique(data$subRegion) %in% unique(mapFindx$subRegion)]

      if (!is.null(subRegNotInShapeFoundx)) {
        if (length(subRegNotInShapeFoundx) > 0) {
          print(paste(
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
