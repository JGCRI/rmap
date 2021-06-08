#' map
#'
#' This function produce different kinds of maps for the rmap package.
#' Each figure is accompanied with a csv table.
#'
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @param data Default = NULL,
#' @param fillColumn (Optional). Default = NULL. Only for direct map plotting.
#' @param fileName (Optional). Default = "map". Only for direct map plotting.
#' @param printFig (Optional). Default = T. Only for direct map plotting.
#' @param grid Default = NULL,
#' @param theme Default = NULL,
#' @param folder Default = paste(getwd(),"/outputs",sep=""),
#' @param labels Default = F,
#' @param shape Default = NULL,
#' @param subRegShpFolder Default = paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
#' @param subRegShpFile Default = paste("gadm36_1",sep=""),
#' @param subRegCol Default ="subRegion",
#' @param subRegType Default =NULL
#' @param nameAppend Default =""
#' @param legendType Default ="kmeans", Options include c("pretty","kmeans","freescale","all")
#' @param legendBreaksn Default = "5",
#' @param legendFixedBreaks Default = NULL,
#' @param animateOn Default = T,
#' @param fps Default = 1,
#' @param projX Default = projX="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#' @param width Default =9
#' @param height Default =7
#' @param scenRef Reference Scenario. Default = NULL
#' @param scenDiff Scenarios to Diff. Default = NULL
#' @param scaleRange Default NULL. A vector with c(max,min) (Applied to all params) or a dataframe with cols param, max, min
#' @param scaleRangeDiffAbs Default =NULL, A vector with c(max,min) (Applied to all params) or a dataframe with cols param, max, min
#' @param scaleRangeDiffPrcnt Default =NULL, A vector with c(max,min) (Applied to all params) or a dataframe with cols param, max, min
#' @param xRef Reference year. Default = NULL
#' @param xDiff years to Diff. Default = NULL
#' @param scaleRangeDiffxAbs Default =NULL, A vector with c(max,min) (Applied to all params) or a dataframe with cols param, max, min
#' @param scaleRangeDiffxPrcnt Default =NULL, A vector with c(max,min) (Applied to all params) or a dataframe with cols param, max, min
#' @param facetCols Default ="multiFacetRow",
#' @param facetRows Default ="multiFacetCol",
#' @param mapTitle Default=NULL
#' @param numeric2Cat_list Default=NULL,
#' @param underLayer Default = NULL
#' @param underLayerColor Default = "gray40"
#' @param underLayerFill Default = "gray90"
#' @param underLayerLwd Default = 0.5
#' @param underLayerAlpha Default = 1
#' @param overLayer Default = NULL
#' @param overLayerColor Default = "gray40"
#' @param overLayerFill Default = NA
#' @param overLayerLwd Default = 0.5
#' @param overLayerAlpha Default = 0
#' @param zoom Default =-1. Zoom into or out of map. Positive values zoom in and negative out.
#' @param zoomx Default = NULL. Zoom into or out of map along x. Positive values zoom in and negative out.
#' @param zoomy Default = NULL. Zoom into or out of map along y. Positive values zoom in and negative out.
#' @param pdfpng Save IO figures as pdf or png. Type=String. Options: 'pdf' or 'png'. Default = 'png'
#' @param legendSingleValue Default =NULL,
#' @param legendSingleColor Default="white"
#' @param legendDigitsOverride Default=NULL
#' @param classPalette Default = NULL
#' @param classPaletteDiff Default = "pal_div_BrGn"
#' @param cropToBoundary Default = T
#' @param colorNA Default = "gray50"
#' @param showNA Default = T
#' @param ncol Default = 3. Number of columns to wrap maps
#' @param size Default = 12. Text size of plots.
#' @param alpha Default = 1. Transparency of fill colors.
#' @param background Default = F. Add background water color, border and default underlayer map.
#' @return A list with the gridTbl and shapeTbl used to plot the data if any.
#' @importFrom rlang :=
#' @export


map <- function(data = NULL,
                fillColumn = NULL,
                fileName = "map",
                printFig=T,
                grid = NULL,
                theme = NULL,
                folder = paste(getwd(), "/outputs", sep = ""),
                labels = F,
                shape = NULL,
                subRegShpFolder = NULL,
                subRegShpFile = NULL,
                subRegCol = "subRegion",
                subRegType = NULL,
                nameAppend = "",
                legendType ="kmeans",
                legendBreaksn = 5,
                legendFixedBreaks = NULL,
                animateOn = T,
                fps = 1,
                cropToBoundary = F,
                underLayer = NULL,
                underLayerColor = "gray40",
                underLayerFill = "gray90",
                underLayerLwd = 0.5,
                underLayerAlpha = 1,
                overLayer = NULL,
                overLayerColor = "gray40",
                overLayerFill = NA,
                overLayerLwd = 0.5,
                overLayerAlpha = 0,
                zoom = -1,
                zoomx = NULL,
                zoomy = NULL,
                projX = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                width = 7,
                height = 8,
                scenRef = NULL,
                scenDiff = NULL,
                scaleRange = NULL,
                scaleRangeDiffAbs = NULL,
                scaleRangeDiffPrcnt = NULL,
                xRef = NULL,
                xDiff = NULL,
                scaleRangeDiffxAbs = NULL,
                scaleRangeDiffxPrcnt = NULL,
                facetCols = NULL,
                facetRows = NULL,
                mapTitle=NULL,
                numeric2Cat_list = NULL,
                pdfpng = 'png',
                legendDigitsOverride = NULL,
                legendSingleColor ="white",
                legendSingleValue =NULL,
                classPalette = NULL,
                classPaletteDiff = "pal_div_BluRd",
                colorNA = "gray50",
                showNA = F,
                ncol = 3,
                size = 12,
                alpha = 1,
                background = F) {

  # data = NULL
  # fillColumn = NULL
  # fileName = "map"
  # printFig=T
  # grid = NULL
  # theme = NULL
  # folder = paste(getwd(), "/outputs", sep = "")
  # labels = F
  # shape = NULL
  # subRegShpFolder = NULL
  # subRegShpFile = NULL
  # subRegCol = "subRegion"
  # subRegType = NULL
  # nameAppend = ""
  # legendType ="kmeans"
  # legendBreaksn = 5
  # legendFixedBreaks = NULL
  # animateOn = T
  # fps = 1
  # cropToBoundary = F
  # projX = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # width = 6
  # height = 7
  # scenRef = NULL
  # scenDiff = NULL
  # scaleRange = NULL
  # scaleRangeDiffAbs = NULL
  # scaleRangeDiffPrcnt = NULL
  # xRef = NULL
  # xDiff = NULL
  # scaleRangeDiffxAbs = NULL
  # scaleRangeDiffxPrcnt = NULL
  # facetCols = NULL
  # facetRows = NULL
  # mapTitle=NULL
  # numeric2Cat_list = NULL
  # pdfpng = 'png'
  # legendDigitsOverride = NULL
  # legendSingleColor ="white"
  # legendSingleValue =NULL
  # classPalette = NULL
  # classPaletteDiff = "pal_div_BluRd"
  # size = 12
  # showNA = F
  # ncol = 3

  #print("Starting map...")

  #.................-
  # Initialize variables
  # .................

  if(T){
  NULL->lat->lon->param->region->scenario->subRegion->value ->
    x->year->gridID->maxScale->minScale->
    valueDiff->rowid->include->Var1->Var2->Var3->maxX->minX->shapeTblScenMultiABRef->
    shapeTblDiff -> gridTblDiff -> shapeTblDiffx -> gridTblDiffx -> shapeTblMultiOrig->countCheck->
      multiFacetCol -> multiFacetRow->classPaletteOrig->
      xLabel->vintage->aggregate->query->subRegNotInShape ->gridTblOrig -> shapeTblOrig -> subRegionAlt -> subRegion1 ->
      paramsGrid -> paramsShape -> scaleRange_i ->shapex

    return_i = 1; # Index for return maps list
    mapsReturn = list(); # Return maps list

  tibble::tibble() -> gridTblReturn -> shapeTblReturn

  classPaletteOrig <- classPalette
  subRegColOrig <- subRegCol
  subRegShapeOrig <- shape
  subRegShpFileOrig <- subRegShpFile
  subRegShpFolderOrig <- subRegShpFolder
  animateOnOrig <- animateOn

  if(!is.null(legendFixedBreaks)){
    # Must be a vector of more than one number
    if(length(legendFixedBreaks)<2){
      stop("legendFixedBreaks must be a vector of more than one number")
    }

    if(any(!is.numeric(legendFixedBreaks))){
      stop("legendFixedBreaks must be a vector of more than one number")
    }

  }

  if(!dir.exists(folder)){dir.create(folder)}

  dirOutputsX <- folder;

}


  if(!is.null(subRegShapeOrig)){
    if(is.null(subRegType)){
      if(class(subRegShapeOrig)=="SpatialPolygonsDataFrame"){
        if(any("subRegionType" %in% names(subRegShapeOrig@data))){
          subRegType = unique(subRegShapeOrig@data$subRegionType)
        }
      }
    }
  }; subRegType

  #.................-
  # Run mapplot directly if a shpefile is provided
  # .................

  if(F){
    if((class(data)!="character")){

       if(is.null(classPalette)) {
        classPalettex = "Spectral"
      }else{
        classPalettex <- classPalette
      }

      colm=1
      rowm=1

      rmap::mapplot(overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), theme = theme, legendTitle=legendTitle,
        underLayer=underLayer,
        dataPolygon = data,
        fillPalette=classPalettex,
        folder=folder,
        labels=labels,
        fileName = fileName,
        fillColumn = fillColumn,
        printFig = printFig,
        mapTitle=mapTitle) ->
        mapsReturn[[return_i]]; names(mapsReturn)[return_i] <- fileName; return_i = return_i + 1
    }
  } else {

  #.................-
  # Function for adding any missing columns if needed
  # .................

  if(T){

  addMissingScale<-function(data){
      data <- data %>% dplyr::ungroup()
      if(!any(grepl("\\<param\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(param="param")}else{
        data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<param\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(param=as.character(param),param=dplyr::case_when(is.na(param)~"param",TRUE~param))}
      if(!any(grepl("\\<params\\>",names(data),ignore.case = T))){}else{
        data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<params\\>",names(data),ignore.case = T)])[1])
        data<-data%>%dplyr::mutate(param=as.character(param),param=dplyr::case_when(is.na(param)~"params",TRUE~param))}
      if(!any(grepl("max",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(maxScale=NA_real_)}else{
        data <- data %>% dplyr::rename(!!"maxScale" := (names(data)[grepl("max",names(data),ignore.case = T)])[1])
        data<-data %>%dplyr::mutate(maxScale=as.numeric(maxScale))
        data<-data %>%dplyr::mutate(maxScale=dplyr::case_when(is.na(maxScale)~NA_real_,TRUE~maxScale))}
      if(!any(grepl("min",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(minScale=NA_real_)}else{
        data <- data %>% dplyr::rename(!!"minScale" := (names(data)[grepl("min",names(data),ignore.case = T)])[1])
        data<-data %>%dplyr::mutate(minScale=as.numeric(minScale))
        data<-data %>%dplyr::mutate(minScale=dplyr::case_when(is.na(minScale)~NA_real_,TRUE~minScale))}
      data = data %>% dplyr::select(param,maxScale,minScale)
       return(data)
    }

  addMissing<-function(data){
    if(!any(grepl("\\<scenario\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(scenario="scenario")}else{
      data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenario\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(scenario=as.character(scenario),scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
    if(!any(grepl("\\<scenarios\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenarios\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(scenario=as.character(scenario),scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
    if(!"x"%in%names(data)){if("year"%in%names(data)){
      data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}
    if(any(grepl("\\<subregion\\>",names(data),ignore.case = T))){
      data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subregion\\>",names(data),ignore.case = T)])[1])}
    if(!any(grepl("\\<subregtype\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(subRegType="subRegType")}else{
      data <- data %>% dplyr::rename(!!"subRegType" := (names(data)[grepl("\\<subregtype\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(subRegType=as.character(subRegType),subRegType=dplyr::case_when(is.na(subRegType)~"subRegType",TRUE~subRegType))}
    if(!any(grepl("\\<unit\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"units" := (names(data)[grepl("\\<unit\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(units=as.character(units),units=dplyr::case_when(is.na(units)~"units",TRUE~units))}
    if(!any(grepl("\\<units\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(units="units")}else{
      data <- data %>% dplyr::rename(!!"units" := (names(data)[grepl("\\<units\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(units=as.character(units),units=dplyr::case_when(is.na(units)~"units",TRUE~units))}
    if(!any(grepl("\\<region\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(region="region")}else{
      data <- data %>% dplyr::rename(!!"region" := (names(data)[grepl("\\<region\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(region=as.character(region),region=dplyr::case_when(is.na(region)~"region",TRUE~region))}
    if(!any(grepl("\\<class\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(class="class")}else{
      data <- data %>% dplyr::rename(!!"class" := (names(data)[grepl("\\<class\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(class=as.character(class),class=dplyr::case_when(is.na(class)~"class",TRUE~class))}
    if(!any(grepl("\\<regions\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"region" := (names(data)[grepl("\\<regions\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(region=as.character(region),region=dplyr::case_when(is.na(region)~"region",TRUE~region))}
    if(!any(grepl("\\<classpalette\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(classPalette="pal_hot")}else{
      data <- data %>% dplyr::rename(!!"classPalette" := (names(data)[grepl("\\<classpalette\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(classPalette=as.character(classPalette),classPalette=dplyr::case_when(is.na(classPalette)~"pal_hot",TRUE~classPalette))}
    if(!any(grepl("\\<param\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(param="param")}else{
      data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<param\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(param=as.character(param),param=dplyr::case_when(is.na(param)~"param",TRUE~param))}
    if(!any(grepl("\\<params\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"param" := (names(data)[grepl("\\<params\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(param=as.character(param),param=dplyr::case_when(is.na(param)~"params",TRUE~param))}
    if(!any(grepl("\\<multiFacetCol\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(multiFacetCol="multiFacetCol")}else{
      data <- data %>% dplyr::rename(!!"multiFacetCol" := (names(data)[grepl("\\<multiFacetCol\\>",names(data),ignore.case = T)])[1])
      data<-data %>%dplyr::mutate(multiFacetCol=dplyr::case_when(is.na(multiFacetCol)~"multiFacetCol",TRUE~multiFacetCol))}
    if(!any(grepl("\\<multiFacetRow\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(multiFacetRow="multiFacetRow")}else{
      data <- data %>% dplyr::rename(!!"multiFacetRow" := (names(data)[grepl("\\<multiFacetRow\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(multiFacetRow=dplyr::case_when(is.na(multiFacetRow)~"multiFacetRow",TRUE~multiFacetRow))}
    return(data)
  }


  if(is.null(grid) & is.null(data)){
    stop ("Both grid and data are Null. Need to provide atleast one of the two.")
  }


  } # Close custom functions

  #.................-
  # Create Folders
  #.................-

  if(T){

  if(!dir.exists(dirOutputsX)){dir.create(dirOutputsX)}
  } # Close create folders

  #.................-
  # Read in grid Tables (Either csv tables or an R Table)
  #.................-

  if(T){

  gridTbl<-tibble::tibble()


  if(!is.null(grid)){

    if(all(!class(grid) %in% c("tbl_df","tbl","data.frame"))){

      for(grid_i in grid){
        if(file.exists(grid_i)){
          gridTblNew<-data.table::fread(paste(grid_i),encoding="Latin-1")%>%tibble::as_tibble()
          gridTbl<-dplyr::bind_rows(gridTbl,gridTblNew)
          rm(gridTblNew)
        } else {stop(paste(grid_i," does not exist"))}
      }

      # Join relevant colors and classes using the mapping file if it exists
      if(!"classPalette" %in% names(gridTbl)){
        if(file.exists(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""))){
          map<-data.table::fread(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""),encoding="Latin-1")%>%tibble::as_tibble()
          gridTbl<-gridTbl%>%dplyr::left_join(map,by=c("param","units","class"))
        }}

    }else{gridTbl<-grid}

  }else{gridTbl=grid}


  if(!is.null(gridTbl)){
  if(nrow(gridTbl)>0){

    # Add missing columns
    gridTbl<-addMissing(gridTbl)

    if(!"value" %in% names(gridTbl)){stop("'value' column not present in grid data provided. Need to have values. Check data.",sep="")}
    if(!"lat" %in% names(gridTbl)){stop("'lat' column not present in grid data provided. Need to have lat. Check data.",sep="")}
    if(!"lon" %in% names(gridTbl)){stop("'lon' column not present in grid data provided. Need to have lat. Check data.",sep="")}


    # Set classPalette if given
    if(!is.null(classPaletteOrig) & (length(classPaletteOrig)==1)){
    gridTbl <- gridTbl %>% dplyr::mutate(classPalette = classPaletteOrig)
    }

  }}

  if(!is.null(gridTbl)){
    if(nrow(gridTbl)==0){
      gridTbl = NULL
    } else {
      gridTbl_scenariosOrig <- unique(gridTbl$scenario)
    }
  }

  } # Close read in Grid Tables

  #.................-
  # Read in shapex Tables (Either csv tables or R table
  #.................-

  if(T){

  shapeTbl<-tibble::tibble()

  if(!is.null(data)){

    if(all(!class(data) %in% c("tbl_df","tbl","data.frame"))){
      if(class(data)!="character"){
        stop("data neither .csv file path nor dataframe or tibble")}
      for(i in data){
        if(file.exists(i)){
          shapeTblNew<-data.table::fread(paste(i),encoding="Latin-1")%>%tibble::as_tibble()

          # Join relevant colors and classes using the mapping file if it exists
          if(!"classPalette" %in% names(shapeTblNew)){
            if(file.exists(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""))){
              map<-data.table::fread(paste(getwd(),"/dataFiles/mapping/template_subRegional_mapping.csv", sep = ""),encoding="Latin-1")%>%tibble::as_tibble()
              shapeTblNew<-shapeTblNew%>%dplyr::left_join(map,by=c("param","units","class"))
            }else{"subregional mapping not found. Using defaults."}}

          shapeTbl<-dplyr::bind_rows(shapeTbl,shapeTblNew)

          if(!"subRegion" %in% names(shapeTbl)){stop(paste("Column subRegion not present in data ",i,sep=""))}

        } else {stop(paste(i," does not exist"))}
      }

    }else{shapeTbl<-data}
    }


  if(!is.null(shapeTbl)){
    if(nrow(shapeTbl)>0){

      if(!subRegCol %in% names(shapeTbl)){
        stop("Please rename polygon column in data 'subRegion' or provide a subRegCol name.")
      }

      # Add missing columns
      shapeTbl<-addMissing(shapeTbl)

    if(!"value" %in% names(shapeTbl)){stop("'value' column not present in polygon data provided. Need to have values. Check data.",sep="")}

    # Set classPalette if given
    #if(!is.null(classPaletteOrig)){shapeTbl <- shapeTbl %>% dplyr::mutate(classPalette = classPaletteOrig)}

  }}

  if(!is.null(shapeTbl)){
    if(nrow(shapeTbl)==0){
      shapeTbl = NULL
    }
  }

  } # Read in SHape Tables

  #.................-
  # Subset Data
  #.................-

  if(T){


  # Remove NA's & Keep only Unique Values
  if(!is.null(gridTbl)){
    if(nrow(gridTbl)>0){
    #print("Removing NA's and keeping only unique values in gridTbl...")
    gridTbl<-gridTbl%>%dplyr::filter(!is.na(value))%>%dplyr::mutate(value = signif(value,10))%>%dplyr::ungroup()%>%dplyr::distinct()
    #print("Complete.")
    }
  }

  if(!is.null(shapeTbl)){
    if(nrow(shapeTbl)>0){
    #print("Removing NA's and keeping only unique values in shapeTbl...")
    shapeTbl<-shapeTbl%>%
      dplyr::filter(!is.na(value))%>%
      dplyr::filter(!is.na(!!subRegCol))%>%
      dplyr::mutate(value = signif(value,10))%>%
      dplyr::ungroup()%>%
      dplyr::distinct()
    #print("Complete.")
    }
  }
  } # Subset data

  #.................-
  # xRange
  #.................-

  if(T){
    if(!is.null(shapeTbl) & !is.null(gridTbl)){
      xRange = unique(c(unique(shapeTbl$x),unique(gridTbl$x)))
    } else if(!is.null(shapeTbl)){
      xRange = unique(c(unique(shapeTbl$x)))
    } else if(!is.null(gridTbl)){
      xRange = unique(c(unique(gridTbl$x)))
    }
  }

  #.................-
  # Compare Scenarios
  #.................-

  if(T){
  # Get Params and Scenarios
  if(!is.null(shapeTbl) & !is.null(gridTbl)){
    if(nrow(shapeTbl)>0 & nrow(gridTbl)>0){
      params <- unique(c(unique(shapeTbl$param),unique(gridTbl$param)))
      scenarios <- unique(c(unique(shapeTbl$scenario),unique(gridTbl$scenario)))
    }
  }else{
    if(!is.null(shapeTbl)){
      if(nrow(shapeTbl)>0){
        params <- unique(c(unique(shapeTbl$param)))
        scenarios <- unique(c(unique(shapeTbl$scenario)))
      }
    } else {
      if(!is.null(gridTbl)){
        if(nrow(gridTbl)>0){
          params <- unique(c(unique(gridTbl$param)))
          scenarios <- unique(c(unique(gridTbl$scenario)))
        }
      }
    }
  }

    if(length(scenarios)>1){
    if(!is.null(scenRef)){

      if(!any(scenRef %in% scenarios)){
        print(paste("scenRef chosen: ", scenRef, " is not in any of the available scenarios: ",sep=""))
        print(paste(scenarios,collapse=", "))
        print(paste("Setting scenRef to first scenario: ", scenarios[1],".",sep=""))
        scenRef <- scenarios[1]
      }


      if(is.null(scenDiff)){
        scenDiff <- scenarios[!scenarios %in% scenRef]
        print(paste("Running difference against all available scenarios:",sep=""))
        print(paste(scenDiff,collapse=", "))
      }else{
        if(!any(scenDiff %in% scenarios)){
          print(paste("None of the scenDiff are in any of the available scenarios: "))
          print(paste(scenarios[!scenarios %in% scenRef],collapse=", "))
          print(paste("Skipping Diff.",sep=""))
        }
      }

      shapeTblDiff <- tibble::tibble()
      gridTblDiff <- tibble::tibble()

      for(i in 1:length(params)){

        NULL -> param_i -> scenRef_i -> scenDiff_i

        param_i <- params[i]
        scenRef_i <- scenRef
        scenDiff_i <- scenDiff

        if(!is.null(param_i) & !is.null(scenRef_i) & !is.null(scenDiff_i)){

    # Compare Gridded Data
    if(!is.null(gridTbl)){

      gridTblDiffa <- gridTbl %>% dplyr::filter(param==param_i & (scenario %in% c(scenRef_i,scenDiff_i)))

      if(length(unique(gridTblDiffa$scenario))>1){

        if(scenRef_i %in% unique(gridTblDiffa$scenario)){
          print(paste("Ref scenario chosen for param: ", param_i, " is ", paste(scenRef_i,collapse=", "),sep=""))
        if(any(scenDiff_i %in% unique(gridTblDiffa$scenario))){
          print(paste("Diff scenarios chosen for param: ", param_i, " are ",
                      paste(scenDiff_i[scenDiff_i %in% unique(gridTblDiffa$scenario)],collapse=", "),sep=""))}


        scenDiff_i <- scenDiff_i[scenDiff_i %in% unique(gridTblDiffa$scenario)]

       # Calculate Diff Values

      gridTblDiffb<-gridTblDiffa%>%dplyr::filter(param==param_i, scenario %in% dplyr::all_of(c(scenRef_i,scenDiff_i)))%>%
        dplyr::select(lat,lon,subRegType,param,x,xLabel,vintage,units,aggregate,classPalette,class,scenario,value)%>%
        tidyr::spread(scenario,value)


      for (scenario_i in unique(gridTblDiffa$scenario)[unique(gridTblDiffa$scenario) %in% scenDiff_i]){
        tbl_temp1 <-gridTblDiffb%>%
          dplyr::mutate(!!paste("DiffAbs_",scenario_i,"_",scenRef_i,sep=""):=get(scenario_i)-get(scenRef_i),
                        classPalette=classPaletteDiff)%>%
          dplyr::select(-dplyr::one_of(as.vector(unique(gridTblDiffa$scenario))))
        tbl_temp1<-tbl_temp1%>%
          tidyr::gather(key=scenario,value=value,
                        -c(names(tbl_temp1)[!names(tbl_temp1) %in% paste("DiffAbs_",scenario_i,"_",scenRef_i,sep="")]))%>%
          dplyr::filter(!is.na(value))

        tbl_temp2 <-gridTblDiffb%>%
          dplyr::mutate(!!paste("DiffPrcnt_",scenario_i,"_",scenRef_i,sep=""):=((get(scenario_i)-get(scenRef_i))*100/get(scenRef_i)),
                        classPalette=classPaletteDiff)%>%
          dplyr::select(-dplyr::one_of(as.vector(unique(gridTblDiffa$scenario))))
        tbl_temp2<-tbl_temp2%>%
          tidyr::gather(key=scenario,value=value,
                        -c(names(tbl_temp2)[!names(tbl_temp2) %in% paste("DiffPrcnt_",scenario_i,"_",scenRef_i,sep="")]))%>%
          dplyr::filter(!is.na(value))

        gridTblDiff<-dplyr::bind_rows(gridTblDiff,tbl_temp1,tbl_temp2)
      }
    }}
  }


  # Compare Shape Data

  if(!is.null(shapeTbl)){
    if(nrow(shapeTbl)>0){

      shapeTblDiffa <- shapeTbl %>% dplyr::filter(param==param_i & (scenario %in% c(scenRef_i,scenDiff_i)));shapeTblDiffa

      if(length(unique(shapeTblDiffa$scenario))>1){
      # Calculate Diff Values

        if(scenRef_i %in% unique(shapeTblDiffa$scenario)){
          print(paste("Ref scenario chosen for param: ", param_i, " is ", paste(scenRef_i,collapse=", "),sep=""))
        if(any(scenDiff_i %in% unique(shapeTblDiffa$scenario))){
          print(paste("Diff scenarios chosen for param: ", param_i, " are ",
                      paste(scenDiff_i[scenDiff_i %in% unique(shapeTblDiffa$scenario)],collapse=", "),sep=""))}

        scenDiff_i <- scenDiff_i[scenDiff_i %in% unique(shapeTblDiffa$scenario)]

      shapeTblDiffb<-shapeTbl%>%dplyr::filter(param==param_i, scenario %in% c(scenRef_i,scenDiff_i))%>%
        dplyr::select(region,subRegion,subRegType,param,x,xLabel,vintage,units,aggregate,classPalette,class,scenario,value)%>%
        tidyr::spread(scenario,value);shapeTblDiffb%>%as.data.frame(); names(shapeTblDiffb)

      for (scenario_i in unique(shapeTbl$scenario)[(unique(shapeTbl$scenario) %in% scenDiff_i)]){
        tbl_temp1 <-shapeTblDiffb%>%
          dplyr::mutate(!!paste("DiffAbs_",scenario_i,"_",scenRef_i,sep=""):=get(scenario_i)-get(scenRef_i),
                        classPalette=classPaletteDiff)%>%
          dplyr::select(-dplyr::one_of(as.vector(unique(shapeTblDiffa$scenario)[unique(shapeTblDiffa$scenario) %in% c(scenRef_i,scenDiff_i)])))
        tbl_temp1<-tbl_temp1%>%
          tidyr::gather(key=scenario,value=value,
                        -c(names(tbl_temp1)[!names(tbl_temp1) %in% paste("DiffAbs_",scenario_i,"_",scenRef_i,sep="")]))%>%
          dplyr::filter(!is.na(value))

        tbl_temp2 <-shapeTblDiffb%>%
          dplyr::mutate(!!paste("DiffPrcnt_",scenario_i,"_",scenRef_i,sep=""):=((get(scenario_i)-get(scenRef_i))*100/get(scenRef_i)),
                        classPalette=classPaletteDiff)%>%
          dplyr::select(-dplyr::one_of(as.vector(unique(shapeTblDiffa$scenario)[unique(shapeTblDiffa$scenario) %in% c(scenRef_i,scenDiff_i)])))
        tbl_temp2<-tbl_temp2%>%
          tidyr::gather(key=scenario,value=value,
                        -c(names(tbl_temp2)[!names(tbl_temp2) %in% paste("DiffPrcnt_",scenario_i,"_",scenRef_i,sep="")]))%>%
          dplyr::filter(!is.na(value))

        shapeTblDiff<-dplyr::bind_rows(shapeTblDiff,tbl_temp1,tbl_temp2)
      }
        }
      }
  }}
        }
      }

    }
    }

    if(!is.null(xRef)){

      if(!any(xRef %in% xRange)){
        print(paste("xRef chosen: ", xRef, " is not in any of the available x values: ",sep=""))
        print(paste(xRange,collapse=", "))
        print(paste("Setting xRef to first x value: ", xRange[1],".",sep=""))
        xRef <- xRange[1]
      }


      if(is.null(xDiff)){
        xDiff <- xRange[!xRange %in% xRef]
        print(paste("Running difference against all available x:",sep=""))
        print(paste(xDiff,collapse=", "))
      }else{
        if(!any(xDiff %in% xRange)){
          print(paste("None of the xDiff are in any of the available scenarios: "))
          print(paste(xRange[!xRange %in% xRef],collapse=", "))
          print(paste("Skipping x Diff.",sep=""))
        }
      }

      shapeTblDiffx <- tibble::tibble()
      gridTblDiffx <- tibble::tibble()

      for(i in 1:length(params)){

        NULL -> param_i -> xRef_i -> xDiff_i

        param_i <- params[i]
        xRef_i <- xRef
        xDiff_i <- xDiff

        if(!is.null(param_i) & !is.null(xRef_i) & !is.null(xDiff_i)){

          # Compare Shape Data
          if(!is.null(shapeTbl)){
            if(nrow(shapeTbl)>0){

            shapeTblDiffa <- shapeTbl %>% dplyr::filter(param==param_i & (x %in% c(xRef_i,xDiff_i)));shapeTblDiffa

            if(nrow(shapeTblDiffa)>0){
              # Calculate Diff Values

              if(xRef_i %in% unique(shapeTblDiffa$x)){
                print(paste("Ref x chosen for param: ", param_i, " is ", paste(xRef_i,collapse=", "),sep=""))
              if(any(xDiff_i %in% unique(shapeTblDiffa$x))){
                print(paste("Diff x chosen for param: ", param_i, " are ",
                            paste(xDiff_i[xDiff_i %in% unique(shapeTblDiffa$x)],collapse=", "),sep=""))}

              xDiff_i <- xDiff_i[xDiff_i %in% unique(shapeTblDiffa$x)]

              colsx <- c("region","subRegion","subRegType","param","x","xLabel","units","aggregate","classPalette","class","scenario","value")
              colsx1 <- names(shapeTbl)[names(shapeTbl) %in% colsx]; colsx1
              colsx2 <- colsx1[!colsx1 %in% "value"]; colsx2
              shapeTblDiffb<-shapeTbl%>%dplyr::filter(param==param_i, x %in% c(xRef_i,xDiff_i))%>%
                dplyr::select(dplyr::all_of(colsx1))%>%
                dplyr::group_by_at(colsx2)%>%
                dplyr::summarize(value=sum(value,na.rm=T))%>%
                tidyr::spread(x,value);shapeTblDiffb%>%as.data.frame(); names(shapeTblDiffb)

              for (scen_i in scenarios){
              for (x_i in unique(shapeTbl$x)[(unique(shapeTbl$x) %in% xDiff_i)]){
                tbl_temp1 <-shapeTblDiffb%>%
                  dplyr::filter(scenario==scen_i)%>%
                  dplyr::mutate(!!paste(scen_i,"_DiffxAbs_",xRef_i,sep=""):=(!!as.name(x_i)-!!as.name(xRef_i)),
                                classPalette=classPaletteDiff)%>%
                  dplyr::select(-!!as.name(xDiff_i),-!!as.name(xRef_i))
                tbl_temp1<-tbl_temp1%>%
                  tidyr::gather(key=scenario,value=value,
                                -c(names(tbl_temp1)[!names(tbl_temp1) %in% paste(scen_i,"_DiffxAbs_",xRef_i,sep="")]))%>%
                  dplyr::filter(!is.na(value))%>%
                  dplyr::mutate(x=x_i)

                tbl_temp2 <-shapeTblDiffb%>%
                  dplyr::filter(scenario==scen_i)%>%
                  dplyr::mutate(!!paste(scen_i,"_DiffxPrcnt_",xRef_i,sep=""):=((!!as.name(x_i)-!!as.name(xRef_i))*100/!!as.name(x_i)),
                                classPalette=classPaletteDiff)%>%
                  dplyr::select(-!!as.name(xDiff_i),-!!as.name(xRef_i))
                tbl_temp2<-tbl_temp2%>%
                  tidyr::gather(key=scenario,value=value,
                                -c(names(tbl_temp2)[!names(tbl_temp2) %in% paste(scen_i,"_DiffxPrcnt_",xRef_i,sep="")]))%>%
                  dplyr::filter(!is.na(value))%>%
                  dplyr::mutate(x=x_i)

                shapeTblDiffx<-dplyr::bind_rows(shapeTblDiffx,tbl_temp1,tbl_temp2)
              }
            }# Close Scenario
              }
              }
            }
          }

          # Compare Grid Data
          if(!is.null(gridTbl)){
            if(nrow(gridTbl)>0){

              gridTblDiffa <- gridTbl %>% dplyr::filter(param==param_i & (x %in% c(xRef_i,xDiff_i)));gridTblDiffa

              if(nrow(gridTblDiffa)>0){
                # Calculate Diff Values

                if(xRef_i %in% unique(gridTblDiffa$x)){
                  print(paste("Ref x chosen for param: ", param_i, " is ", paste(xRef_i,collapse=", "),sep=""))
                  if(any(xDiff_i %in% unique(gridTblDiffa$x))){
                    print(paste("Diff x chosen for param: ", param_i, " are ",
                                paste(xDiff_i[xDiff_i %in% unique(gridTblDiffa$x)],collapse=", "),sep=""))}

                  xDiff_i <- xDiff_i[xDiff_i %in% unique(gridTblDiffa$x)]

                  colsx <- c("region","lat","lon","subRegType","param","x","xLabel","units","aggregate","classPalette","class","scenario","value")
                  colsx1 <- names(gridTbl)[names(gridTbl) %in% colsx]; colsx1
                  colsx2 <- colsx1[!colsx1 %in% "value"]; colsx2
                  gridTblDiffb<-gridTbl%>%dplyr::filter(param==param_i, x %in% c(xRef_i,xDiff_i))%>%
                    dplyr::select(dplyr::all_of(colsx1))%>%
                    dplyr::group_by_at(colsx2)%>%
                    dplyr::summarize(value=sum(value,na.rm=T))%>%
                    tidyr::spread(x,value);gridTblDiffb%>%as.data.frame(); names(gridTblDiffb)

                  for (scen_i in scenarios){
                    for (x_i in unique(gridTbl$x)[(unique(gridTbl$x) %in% xDiff_i)]){
                      tbl_temp1 <-gridTblDiffb%>%
                        dplyr::filter(scenario==scen_i)%>%
                        dplyr::mutate(!!paste(scen_i,"_DiffxAbs_",xRef_i,sep=""):=(!!as.name(x_i)-!!as.name(xRef_i)),
                                      classPalette=classPaletteDiff)%>%
                        dplyr::select(-!!as.character(xDiff_i),-!!as.character(xRef_i))
                      tbl_temp1<-tbl_temp1%>%
                        tidyr::gather(key=scenario,value=value,
                                      -c(names(tbl_temp1)[!names(tbl_temp1) %in% paste(scen_i,"_DiffxAbs_",xRef_i,sep="")]))%>%
                        dplyr::filter(!is.na(value))%>%
                        dplyr::mutate(x=x_i)

                      tbl_temp2 <-gridTblDiffb%>%
                        dplyr::filter(scenario==scen_i)%>%
                        dplyr::mutate(!!paste(scen_i,"_DiffxPrcnt_",xRef_i,sep=""):=((!!as.name(x_i)-!!as.name(xRef_i))*100/!!as.name(x_i)),
                                      classPalette=classPaletteDiff)%>%
                        dplyr::select(-!!as.character(xDiff_i),-!!as.character(xRef_i))
                      tbl_temp2<-tbl_temp2%>%
                        tidyr::gather(key=scenario,value=value,
                                      -c(names(tbl_temp2)[!names(tbl_temp2) %in% paste(scen_i,"_DiffxPrcnt_",xRef_i,sep="")]))%>%
                        dplyr::filter(!is.na(value))%>%
                        dplyr::mutate(x=x_i)

                      gridTblDiffx<-dplyr::bind_rows(gridTblDiffx,tbl_temp1,tbl_temp2)
                    }
                  } # Close Scenario
                }
              }
            }
          }

        }
      }

    }

  shapeTbl <- shapeTbl %>% dplyr::bind_rows(shapeTblDiffx)%>% dplyr::bind_rows(shapeTblDiff) %>% dplyr::ungroup() %>% dplyr::distinct();
  gridTbl <- gridTbl %>% dplyr::bind_rows(gridTblDiffx)%>% dplyr::bind_rows(gridTblDiff) %>% dplyr::ungroup() %>% dplyr::distinct();

} # Compare Scenarios


  #.................-
  # Check MultiFacet Columns
  #.................-

  if(F){ # Check facet column/Rows selected exist

  # Shape Table

  # Grid table
  if(!is.null(gridTbl)){

    if(!facetCols %in% names(gridTbl)){
      print(paste0("facetCols chosen: ",  facetCols ,"do not exist:"))
      facetCols <- NULL
    }

    if(!facetRows %in% names(gridTbl)){
      print(paste0("facetRows chosen: ",  facetRows ,"do not exist:"))
      facetRows <- NULL
    }
  }

  } # Check facet column/rows selected exist


  #................
  # Check scaleRanges
  #.............--

  if(T){

    # Get list of params in grid or shapex data
    if(!is.null(gridTbl)){if(nrow(gridTbl)>0){paramsGrid <- unique(gridTbl$param)}}
    if(!is.null(shapeTbl)){if(nrow(shapeTbl)>0){paramsShape <- unique(shapeTbl$param)}}
    paramsRange <- unique(c(paramsGrid,paramsShape)); paramsRange


    if(!is.null(scaleRange)){
      # Scale Range
      scaleRange[is.na(scaleRange)]<-NA_real_
      scaleRange[scaleRange=="NA"]<-NA_real_
      # If scale range is a vector of two numbers set as limits for all params
      if(is.numeric(scaleRange) & length(scaleRange)==2){
        scaleRange = data.frame(param=paramsRange,maxScale=max(scaleRange),minScale=min(scaleRange))
      } else {
        # Else format the scaleRange data frame as needed
        if(!is.null(nrow(scaleRange))){
          scaleRange = addMissingScale(scaleRange)
          if(!any(unique(scaleRange$param) %in% paramsRange)){
            print(paste("None of the params in scaleRange: ",
                        paste(unique(scaleRange$param),collapse=", "),sep=""))
            print("are present in the data params:")
            print(paste(paramsRange,collapse=", "))
            print("Setting scaleRange to NULL")
            scaleRange=NULL
          }
        }else{scaleRange=NULL}
      }
    }

      # Scale Range Diff Abs
    if(!is.null(scaleRangeDiffAbs)){

      scaleRangeDiffAbs[is.na(scaleRangeDiffAbs)]<-NA_real_
      scaleRangeDiffAbs[scaleRangeDiffAbs=="NA"]<-NA_real_
      # If scale range is a vector of two numbers set as limits for all params
      if(is.numeric(scaleRangeDiffAbs) & length(scaleRangeDiffAbs)==2){
        scaleRangeDiffAbs = data.frame(param=paramsRange,maxScale=max(scaleRangeDiffAbs),minScale=min(scaleRangeDiffAbs))
      } else {
        if(!is.null(nrow(scaleRangeDiffAbs))){
          scaleRangeDiffAbs = addMissingScale(scaleRangeDiffAbs)
          if(!any(unique(scaleRangeDiffAbs$param) %in% paramsRange)){
            print(paste("None of the params in scaleRangeDiffAbs: ",
                        paste(unique(scaleRangeDiffAbs$param),collapse=", "),sep=""))
            print("are present in the data params:")
            print(paste(paramsRange,collapse=", "))
            print("Setting scaleRangeDiffAbs to NULL")
            scaleRangeDiffAbs=NULL
          }
        }else{scaleRangeDiffAbs=NULL}
      }
    }

    if(!is.null(scaleRangeDiffPrcnt)){
      # Scale Range Diff Prcnt
      scaleRangeDiffPrcnt[is.na(scaleRangeDiffPrcnt)]<-NA_real_
      scaleRangeDiffPrcnt[scaleRangeDiffPrcnt=="NA"]<-NA_real_
      # If scale range is a vector of two numbers set as limits for all params
      if(is.numeric(scaleRangeDiffPrcnt) & length(scaleRangeDiffPrcnt)==2){
        scaleRangeDiffPrcnt = data.frame(param=paramsRange,maxScale=max(scaleRangeDiffPrcnt),minScale=min(scaleRangeDiffPrcnt))
      } else {
        # Else format the scaleRangeDiffPrcnt data frame as needed
        if(!is.null(nrow(scaleRangeDiffPrcnt))){
          scaleRangeDiffPrcnt = addMissingScale(scaleRangeDiffPrcnt)
          if(!any(unique(scaleRangeDiffPrcnt$param) %in% paramsRange)){
            print(paste("None of the params in scaleRangeDiffPrcnt: ",
                        paste(unique(scaleRangeDiffPrcnt$param),collapse=", "),sep=""))
            print("are present in the data params:")
            print(paste(paramsRange,collapse=", "))
            print("Setting scaleRangeDiffPrcnt to NULL")
            scaleRangeDiffPrcnt=NULL
            }
        }else{scaleRangeDiffPrcnt=NULL}
      }
  }

    # Scale Range Diff Abs
    if(!is.null(scaleRangeDiffxAbs)){

      scaleRangeDiffxAbs[is.na(scaleRangeDiffxAbs)]<-NA_real_
      scaleRangeDiffxAbs[scaleRangeDiffxAbs=="NA"]<-NA_real_
      # If scale range is a vector of two numbers set as limits for all params
      if(is.numeric(scaleRangeDiffxAbs) & length(scaleRangeDiffxAbs)==2){
        scaleRangeDiffxAbs = data.frame(param=paramsRange,maxScale=max(scaleRangeDiffxAbs),minScale=min(scaleRangeDiffxAbs))
      } else {
        if(!is.null(nrow(scaleRangeDiffxAbs))){
          scaleRangeDiffxAbs = addMissingScale(scaleRangeDiffxAbs)
          if(!any(unique(scaleRangeDiffxAbs$param) %in% paramsRange)){
            print(paste("None of the params in scaleRangeDiffxAbs: ",
                        paste(unique(scaleRangeDiffxAbs$param),collapse=", "),sep=""))
            print("are present in the data params:")
            print(paste(paramsRange,collapse=", "))
            print("Setting scaleRangeDiffxAbs to NULL")
            scaleRangeDiffxAbs=NULL
          }
        }else{scaleRangeDiffxAbs=NULL}
      }
    }

    if(!is.null(scaleRangeDiffxPrcnt)){
      # Scale Range Diff Prcnt
      scaleRangeDiffxPrcnt[is.na(scaleRangeDiffxPrcnt)]<-NA_real_
      scaleRangeDiffxPrcnt[scaleRangeDiffxPrcnt=="NA"]<-NA_real_
      # If scale range is a vector of two numbers set as limits for all params
      if(is.numeric(scaleRangeDiffxPrcnt) & length(scaleRangeDiffxPrcnt)==2){
        scaleRangeDiffxPrcnt = data.frame(param=paramsRange,maxScale=max(scaleRangeDiffxPrcnt),minScale=min(scaleRangeDiffxPrcnt))
      } else {
        # Else format the scaleRangeDiffxPrcnt data frame as needed
        if(!is.null(nrow(scaleRangeDiffxPrcnt))){
          scaleRangeDiffxPrcnt = addMissingScale(scaleRangeDiffxPrcnt)
          if(!any(unique(scaleRangeDiffxPrcnt$param) %in% paramsRange)){
            print(paste("None of the params in scaleRangeDiffxPrcnt: ",
                        paste(unique(scaleRangeDiffxPrcnt$param),collapse=", "),sep=""))
            print("are present in the data params:")
            print(paste(paramsRange,collapse=", "))
            print("Setting scaleRangeDiffxPrcnt to NULL")
            scaleRangeDiffxPrcnt=NULL
          }
        }else{scaleRangeDiffxPrcnt=NULL}
      }
    }

    }# Close Check Scale Range


  #....................
  # Crop To Boundary
  #....................

  if(cropToBoundary){
    if(is.null(boundaryRegionsSelect)){
      if(!is.null(shapeTbl)){
        if(nrow(shapeTbl)>0){
          boundaryRegionsSelect <- unique(shapeTbl$subRegion)
        }
      }
    }
  }

  #.................-
  # Rename SubRegions and SubRegtype
  #.................-

  if(T){
    if(!is.null(shapeTbl)){
      if(nrow(shapeTbl)>0){
        if(!is.null(subRegCol)){
        shapeTbl <- shapeTbl %>% dplyr::rename(subRegion=!!as.name(subRegCol))
        subRegCol <- "subRegion"
        }
        if(!is.null(subRegType)){
          shapeTbl <- shapeTbl %>% dplyr::mutate(subRegType=!!subRegType)
        }

      }
    }
  }

  # .................--
  # Create Raster Plots
  # .................--

  if(T){ # Raster Plots
  if(!is.null(gridTbl)){
    if(nrow(gridTbl)>0){

      gridTblOrig <- gridTbl

      if(!length(unique(gridTblOrig$x))>1){animateOn=F}

      for (param_i in unique(gridTblOrig$param)){

        # Combined Scenarios
        if(length(gridTbl_scenariosOrig)>1){

            if(length(unique(gridTblOrig$param))==1){param_if=NULL}else{param_if=param_i}

            if(nrow(gridTblOrig%>%dplyr::filter(param==param_i))>0){

              gridTblx <- gridTblOrig%>%dplyr::filter(param==param_i,
                                                     scenario %in% scenarios)

                if(nrow(gridTblx)>0){

              #.................-
              # Create Grid Table Folders If Needed
              #.................-
              if(T){

                if(!dir.exists(paste(dirOutputsX,"/",sep = ""))){
                  dir.create(paste(dirOutputsX,"/",sep = ""))}

                if(!dir.exists(paste(dirOutputsX,"/",param_if,sep = ""))){
                  dir.create(paste(dirOutputsX,"/",param_if,sep = ""))}

                if(!dir.exists(paste(dirOutputsX,"/",param_if,"/combScenario",sep = ""))){
                  dir.create(paste(dirOutputsX, "/",param_if,"/combScenario",sep = ""))}

                if(length(unique(gridTblx$x))>1){
                if(!dir.exists(paste(dirOutputsX,"/",param_if,"/combScenario/byYear",sep = ""))){
                  dir.create(paste(dirOutputsX, "/",param_if,"/combScenario/byYear",sep = ""))}
                }
              } # Create grid table folder if needed

              #.................--
              # Save Map related Data Table
              #.................--

              if(printFig){
                if(nrow(gridTblx %>% dplyr::filter(param==param_i))>0){
                  data.table::fwrite(gridTblx %>% dplyr::filter(param==param_i)%>%
                                       dplyr::select(scenario,lat,lon,param,class,x,value,units),
                                     paste(dirOutputsX,"/",param_if,"/combScenario/","map_","raster_",param_i,nameAppend,".csv",sep = ""))
                  print(paste("Map data table written to ",dirOutputsX,"/",param_if,"/combScenario/","map_","raster_",param_i,nameAppend,".csv",sep = ""))
                }
              }

              #.............................
              # By Year
              #.............................

              # Set Legends
              if(T){
                animScaleGrid<-gridTblx$value
                animScaleGrid <- animScaleGrid[!is.infinite(animScaleGrid)]
                animScaleGrid <- animScaleGrid[!is.nan(animScaleGrid)]
                animScaleGrid <- animScaleGrid[!is.na(animScaleGrid)]

                # Choose correct scaleRange
                scaleRange_i=scaleRange

                if(!is.null(scaleRange_i)){
                  if(any(param_i %in% unique(scaleRange_i$param))){
                    if(max(animScaleGrid) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                      animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                        animScaleGrid <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                           animScaleGrid[animScaleGrid<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                      }
                    if(min(animScaleGrid) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                      animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                        animScaleGrid <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                            animScaleGrid[animScaleGrid>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                      }
                  }
                }
                animPrettyBreaksGrid<-scales::pretty_breaks(n=legendBreaksn)(animScaleGrid); animPrettyBreaksGrid
                animKmeanBreaksGrid<-sort(as.vector((stats::kmeans(animScaleGrid,
                                                                   centers=max(1,min(length(unique(animScaleGrid))-1,(legendBreaksn-1)))))$centers[,1]));animKmeanBreaksGrid
                if(!min(animScaleGrid) %in% animKmeanBreaksGrid){
                  animKmeanBreaksGrid <- sort(c(min(animScaleGrid),animKmeanBreaksGrid))}
                if(!max(animScaleGrid) %in% animKmeanBreaksGrid){
                  animKmeanBreaksGrid <- sort(c(animKmeanBreaksGrid,max(animScaleGrid)))};animKmeanBreaksGrid


                if((max(range(animScaleGrid))-min(range(animScaleGrid)))<1E-10 &
                   (max(range(animScaleGrid))-min(range(animScaleGrid)))>-1E-10){animScaleGridRange=min(animScaleGrid)}else{
                     animScaleGridRange=range(animScaleGrid)
                   }

                if(abs(min(animScaleGridRange,na.rm = T))==abs(max(animScaleGridRange,na.rm = T))){animScaleGridRange=abs(min(animScaleGridRange,na.rm = T))}
                if(mean(animScaleGridRange,na.rm = T)<0.01 & mean(animScaleGridRange,na.rm = T)>(-0.01)){animLegendDigits<-5}else{
                  if(mean(animScaleGridRange,na.rm = T)<0.1 & mean(animScaleGridRange,na.rm = T)>(-0.1)){animLegendDigits<-4}else{
                    if(mean(animScaleGridRange,na.rm = T)<1 & mean(animScaleGridRange,na.rm = T)>(-1)){animLegendDigits<-3}else{
                      if(mean(animScaleGridRange,na.rm = T)<10 & mean(animScaleGridRange,na.rm = T)>(-10)){animLegendDigits<-2}else{animLegendDigits<-1}}}}

              }

              # By Year
              if(length(unique(gridTblx$x))>1){

              for (x_i in unique(gridTblx$x)){

                datax<-gridTblx%>%dplyr::filter(x==x_i)

                if(nrow(datax)>0){
                  legendTitle<-unique(datax$units)
                  fillPalette<-as.character(unique(datax$classPalette))

                  # Set Facets
                  if(length(unique(datax$scenario))>1){
                    multiFacetColsx <- "scenario"
                    colm <- length(unique(datax$scenario))
                    if((length(unique(datax$class))>1)){
                      multiFacetRowsx <- c("class")
                      rowm <- length(unique(datax$scenario))
                    }else{
                        multiFacetRowsx <- NULL
                        rowm = 1
                        colm = colm/((colm + ncol-1)%/%ncol);
                        rowm = (colm + ncol-1)%/%ncol
                        }
                  }else{
                    if((length(unique(datax$class))>1)){
                      multiFacetColsx <- c("class")
                      multiFacetRowsx <- NULL
                      colm = length(unique(datax$class))
                      rowm = 1
                      colm = colm/((colm + ncol-1)%/%ncol);
                      rowm = (colm + ncol-1)%/%ncol
                    }else{
                      multiFacetColsx <- NULL
                      multiFacetRowsx <- NULL
                      colm = 1
                      rowm = 1
                      }
                  }

                  # Add facet or Rows if selected
                  if(!is.null(facetCols)){
                    if(!is.null(multiFacetColsx)){
                      multiFacetColsx <- c(multiFacetColsx,facetCols)
                      colm <- colm + length(facetCols)
                    } else { multiFacetColsx <- facetCols; colm <- length(facetCols)}
                  }

                  if(!is.null(facetRows)){
                    if(!is.null(multiFacetRowsx)){
                      multiFacetRowsx <- c(multiFacetRowsx,facetRows)
                      rowm <- rowm + length(facetRows)
                    } else { multiFacetRowsx <- facetRows; rowm <- length(facetRows)}
                  }


                  # Check for Duplicates
                  if(duplicated(datax %>%
                                dplyr::select(lat,lon,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                     any()){stop("Input grid data has multiple values for the same lat and lon. Please check your data.")}


                  if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                    rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                  theme = theme, legendTitle=legendTitle,
                                  legendDigitsOverride=legendDigitsOverride,
                                  numeric2Cat_list=numeric2Cat_list,
                                  underLayer=underLayer,
                                  dataPolygon=shapex,
                                  dataGrid=datax,
                                  legendBreaksn=legendBreaksn,
                                  legendDigits = animLegendDigits,
                                  fillPalette = fillPalette,
                                  width=width*max(1,colm/1),
                                  height=height*max(1,rowm/1),
                                  pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                  labels=labels,
                                  legendBreaks = animKmeanBreaksGrid,
                                  fillColumn = "value",
                                  facetCols = multiFacetColsx,
                                  facetRows = multiFacetRowsx,
                                  mapTitle=paste(param_i," ",x_i,sep="") ,
                                  fileName = paste("map_","raster_",param_i,"_",x_i,nameAppend,"_KMEANS",sep=""),
                                  folder = paste(dirOutputsX,"/",param_if,"/combScenario/byYear",sep = "")) ->
                      mapsReturn[[return_i]];
                      names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,"_",x_i,nameAppend,"_KMEANS",sep="");
                      return_i = return_i + 1

                      # theme_ggplot = theme_ggplot
                      # theme_custom = theme_custom
                      # theme_rmap = theme_rmap
                      # legendDigitsOverride=legendDigitsOverride
                      # numeric2Cat_list=numeric2Cat_list
                      # underLayer=underLayer
                      # dataPolygon=shapex
                      # dataGrid=datax
                      # legendBreaksn=legendBreaksn
                      # legendDigits = animLegendDigits
                      # fillPalette = fillPalette
                      # width=width*max(1,colm/1),
                      # height=height*max(1,rowm/1),
                      # pdfpng = pdfpng
                      # legendSingleColor = legendSingleColor
                      # legendSingleValue =  legendSingleValue
                      # labels=labels
                      # legendBreaks = animKmeanBreaksGrid
                      # fillColumn = "value"
                      # facetCols = multiFacetColsx
                      # facetRows = multiFacetRowsx
                      # mapTitle=paste(param_i," ",x_i,sep="")
                      # fileName = paste("map_","raster_",param_i,"_",x_i,nameAppend,"_KMEANS",sep="")
                      # folder = paste(dirOutputsX,"/",param_if,"/combScenario/byYear",sep = "")

                    }

                  if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                    rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                  theme = theme, legendTitle=legendTitle,
                                  legendDigitsOverride=legendDigitsOverride,
                                  numeric2Cat_list=numeric2Cat_list,
                                  underLayer=underLayer,
                                  dataPolygon=shapex,
                                  dataGrid=datax,
                                  legendBreaksn=legendBreaksn,
                                  legendDigits = animLegendDigits,
                                  fillPalette = fillPalette,
                                  width=width*max(1,colm/1),
                                  height=height*max(1,rowm/1),
                                  pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                  labels=labels,
                                  legendBreaks = animPrettyBreaksGrid,
                                  fillColumn = "value",
                                  facetCols = multiFacetColsx,
                                  facetRows = multiFacetRowsx,
                                  mapTitle=paste(param_i," ",x_i,sep="") ,
                                  fileName = paste("map_","raster_",param_i,"_",x_i,nameAppend,"_PRETTY",sep=""),
                                  folder = paste(dirOutputsX,"/",param_if,"/combScenario/byYear",sep = "")) ->
                      mapsReturn[[return_i]];
                    names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,"_",x_i,nameAppend,"_PRETTY",sep="");
                    return_i = return_i + 1
                  }

                  if(!is.null(legendFixedBreaks)){
                    rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                  theme = theme, legendTitle=legendTitle,
                                  legendDigitsOverride=legendDigitsOverride,
                                  numeric2Cat_list=numeric2Cat_list,
                                  underLayer=underLayer,
                                  dataPolygon=shapex,
                                  dataGrid=datax,
                                  legendBreaksn=legendBreaksn,
                                  legendDigits = animLegendDigits,
                                  fillPalette = fillPalette,
                                  width=width*max(1,colm/1),
                                  height=height*max(1,rowm/1),
                                  pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                  labels=labels,
                                  legendBreaks = legendFixedBreaks,
                                  fillColumn = "value",
                                  facetCols = multiFacetColsx,
                                  facetRows = multiFacetRowsx,
                                  mapTitle=paste(param_i," ",x_i,sep="") ,
                                  fileName = paste("map_","raster_",param_i,"_",x_i,nameAppend,"_FIXED",sep=""),
                                  folder = paste(dirOutputsX,"/",param_if,"/combScenario/byYear",sep = "")) ->
                      mapsReturn[[return_i]];
                    names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,"_",x_i,nameAppend,"_FIXED",sep="");
                    return_i = return_i + 1
                  }

                }
                } # Close years x_i loop

              # Animations
              if(animateOn==T){

                if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  animName<-paste("anim_","raster_",param_i,nameAppend,"_PRETTY.gif",sep="")
                  animFiles <- list.files(path = paste(dirOutputsX,"/",param_if,"/combScenario/byYear",sep=""),
                                          pattern = paste(".*",param_i,".*",nameAppend,".*PRETTY", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
                  animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                  magick::image_write(animation,paste(dirOutputsX,"/",param_if,"/combScenario/",
                                                      animName,sep = ""))
                  print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",param_if,"/combScenario/",
                                            animName,sep = "")))
                }

                if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  animName<-paste("anim_","raster_",param_i,nameAppend,"_KMEANS.gif",sep="")
                  animFiles <- list.files(path = paste(dirOutputsX,"/",param_if,"/combScenario/byYear",sep=""),
                                          pattern = paste(".*",param_i,".*",nameAppend,".*KMEANS", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
                  animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                  magick::image_write(animation,paste(dirOutputsX,"/",param_if,"/combScenario/",
                                                      animName,sep = ""))
                  print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",param_if,"/combScenario/",
                                            animName,sep = "")))
                }

                if(!is.null(legendFixedBreaks)){
                  animName<-paste("anim_","raster_",param_i,nameAppend,"_FIXED.gif",sep="")
                  animFiles <- list.files(path = paste(dirOutputsX,"/",param_if,"/combScenario/byYear",sep=""),
                                          pattern = paste(".*",param_i,".*",nameAppend,".*FIXED", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
                  animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                  magick::image_write(animation,paste(dirOutputsX,"/",param_if,"/combScenario/",
                                                      animName,sep = ""))
                  print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",param_if,"/combScenario/",
                                            animName,sep = "")))
                }

              }
              }

              # Multi-Year-Single Chart
              datax<-gridTblx%>%dplyr::filter(param==param_i)

              if(nrow(datax)>0){

                legendTitle<-unique(datax$units)
                fillPalette<-as.character(unique(datax$classPalette))

                animScaleGrid<-datax$value
                animScaleGrid <- animScaleGrid[!is.infinite(animScaleGrid)]
                animScaleGrid <- animScaleGrid[!is.nan(animScaleGrid)]
                animScaleGrid <- animScaleGrid[!is.na(animScaleGrid)]

                # Choose correct scaleRange
                if(T){
                  scaleRange_i=scaleRange

                  if(!is.null(scaleRange_i)){
                    if(any(param_i %in% unique(scaleRange_i$param))){
                      if(max(animScaleGrid) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                        animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                          animScaleGrid <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                             animScaleGrid[animScaleGrid<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                        }
                      if(min(animScaleGrid) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                        animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                          animScaleGrid <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                              animScaleGrid[animScaleGrid>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                        }
                    }
                  }
                  animPrettyBreaksGrid<-scales::pretty_breaks(n=legendBreaksn)(animScaleGrid)
                  animKmeanBreaksGrid<-sort(as.vector((stats::kmeans(animScaleGrid,
                                                                     centers=max(1,min(length(unique(animScaleGrid))-1,(legendBreaksn-1)))))$centers[,1]))
                  if(!min(animScaleGrid) %in% animKmeanBreaksGrid){
                    animKmeanBreaksGrid <- sort(c(min(animScaleGrid),animKmeanBreaksGrid))}
                  if(!max(animScaleGrid) %in% animKmeanBreaksGrid){
                    animKmeanBreaksGrid <- sort(c(animKmeanBreaksGrid,max(animScaleGrid)))}

                  if((max(range(animScaleGrid))-min(range(animScaleGrid)))<1E-10 &
                     (max(range(animScaleGrid))-min(range(animScaleGrid)))>-1E-10){animScaleGridRange=min(animScaleGrid)}else{
                       animScaleGridRange=range(animScaleGrid)
                     }
                  if(abs(min(animScaleGridRange,na.rm = T))==abs(max(animScaleGridRange,na.rm = T))){animScaleGridRange=abs(min(animScaleGridRange,na.rm = T))}
                  if(mean(animScaleGridRange,na.rm = T)<0.01 & mean(animScaleGridRange,na.rm = T)>(-0.01)){animLegendDigits<-5}else{
                    if(mean(animScaleGridRange,na.rm = T)<0.1 & mean(animScaleGridRange,na.rm = T)>(-0.1)){animLegendDigits<-4}else{
                      if(mean(animScaleGridRange,na.rm = T)<1 & mean(animScaleGridRange,na.rm = T)>(-1)){animLegendDigits<-3}else{
                        if(mean(animScaleGridRange,na.rm = T)<10 & mean(animScaleGridRange,na.rm = T)>(-10)){animLegendDigits<-2}else{animLegendDigits<-1}}}}
                }

                # Set Facets
                if(length(unique(datax$x))>1){
                  multiFacetColsx <- "x"
                  colm <- length(unique(datax$x))
                  if((length(unique(datax$scenario))>1) & (length(unique(datax$class))>1)){
                    multiFacetRowsx <- c("scenario","class")
                    rowm <- length(unique(datax$scenario))*length(unique(datax$class))
                    }
                  if((length(unique(datax$scenario))>1) & (!length(unique(datax$class))>1)){
                    multiFacetRowsx <- c("scenario")
                    rowm <- length(unique(datax$scenario))
                    }
                  if((!length(unique(datax$scenario))>1) & (length(unique(datax$class))>1)){
                    multiFacetRowsx <- c("class")
                    rowm <- length(unique(datax$class))
                    }
                  if((!length(unique(datax$scenario))>1) & (!length(unique(datax$class))>1)){
                    multiFacetRowsx <- NULL
                    rowm <- 1
                    colm = colm/((colm + ncol-1)%/%ncol);
                    rowm = (colm + ncol-1)%/%ncol
                    }
                  }else{
                    if(length(unique(datax$scenario))>1){
                      multiFacetColsx <- "scenario"
                      colm <- length(unique(datax$scenario))
                      if((length(unique(datax$class))>1)){
                        multiFacetRowsx <- c("class")
                        rowm <- length(unique(datax$class))
                      }else{
                        multiFacetRowsx <- NULL
                        rowm <- 1
                        colm = colm/((colm + ncol-1)%/%ncol);
                        rowm = (colm + ncol-1)%/%ncol
                        }
                    }else{
                      if((length(unique(datax$class))>1)){
                        multiFacetColsx <- c("class")
                        colm <- length(unique(datax$class))
                        multiFacetRowsx <- NULL
                        rowm <- 1
                        colm = colm/((colm + ncol-1)%/%ncol);
                        rowm = (colm + ncol-1)%/%ncol
                      }else{
                        multiFacetColsx <- NULL
                        multiFacetRowsx <- NULL
                        colm <- 1
                        rowm <- 1
                      }
                    }
                  }


                # Add facet or Rows if selected
                if(!is.null(facetCols)){
                  if(!is.null(multiFacetColsx)){
                    multiFacetColsx <- c(multiFacetColsx,facetCols)
                    colm <- colm + length(facetCols)
                  } else { multiFacetColsx <- facetCols; colm <- length(facetCols)}
                }

                if(!is.null(facetRows)){
                  if(!is.null(multiFacetRowsx)){
                    multiFacetRowsx <- c(multiFacetRowsx,facetRows)
                    rowm <- rowm + length(facetRows)
                  } else { multiFacetRowsx <- facetRows; rowm <- length(facetRows)}
                }



                # Check for Duplicates
                if(duplicated(datax %>%
                              dplyr::select(lat,lon,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                   any()){stop("Input grid data has multiple values for the same lat and lon. Please check your data.")}

                if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                theme = theme, legendTitle=legendTitle,
                                legendDigitsOverride=legendDigitsOverride,
                                numeric2Cat_list=numeric2Cat_list,
                                underLayer=underLayer,
                                dataPolygon=shapex,
                                dataGrid=datax,
                                legendBreaksn=legendBreaksn,
                                legendDigits = animLegendDigits,
                                fillPalette = fillPalette,
                                width=width*max(1,colm/1),
                                height=height*max(1,rowm/1),
                                pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                labels=labels,
                                legendBreaks = animKmeanBreaksGrid,
                                fillColumn = "value",
                                facetCols = multiFacetColsx,
                                facetRows = multiFacetRowsx,
                                mapTitle=paste(param_i,sep=""),
                                fileName = paste("map_","raster_",param_i,nameAppend,"_KMEANS",sep=""),
                                folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/combScenario",sep = ""))) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,nameAppend,"_KMEANS",sep="");
                  return_i = return_i + 1
                  }

                if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                theme = theme, legendTitle=legendTitle,
                                legendDigitsOverride=legendDigitsOverride,
                                numeric2Cat_list=numeric2Cat_list,
                                underLayer=underLayer,
                                dataPolygon=shapex,
                                dataGrid=datax,
                                legendBreaksn=legendBreaksn,
                                legendDigits = animLegendDigits,
                                fillPalette = fillPalette,
                                width=width*max(1,colm/1),
                                height=height*max(1,rowm/1),
                                pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                labels=labels,
                                legendBreaks = animPrettyBreaksGrid,
                                fillColumn = "value",
                                facetCols = multiFacetColsx,
                                facetRows = multiFacetRowsx,
                                mapTitle=paste(param_i,sep=""),
                                fileName = paste("map_","raster_",param_i,nameAppend,"_PRETTY",sep=""),
                                folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/combScenario",sep = ""))) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,nameAppend,"_PRETTY",sep="");
                  return_i = return_i + 1
                  }

                if(!is.null(legendFixedBreaks)){
                  rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                theme = theme, legendTitle=legendTitle,
                                legendDigitsOverride=legendDigitsOverride,
                                numeric2Cat_list=numeric2Cat_list,
                                underLayer=underLayer,
                                dataPolygon=shapex,
                                dataGrid=datax,
                                legendBreaksn=legendBreaksn,
                                legendDigits = animLegendDigits,
                                fillPalette = fillPalette,
                                width=width*max(1,colm/1),
                                height=height*max(1,rowm/1),
                                pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                labels=labels,
                                legendBreaks = legendFixedBreaks,
                                fillColumn = "value",
                                facetCols = multiFacetColsx,
                                facetRows = multiFacetRowsx,
                                mapTitle=paste(param_i,sep=""),
                                fileName = paste("map_","raster_",param_i,nameAppend,"_FIXED",sep=""),
                                folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/combScenario",sep = ""))) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,nameAppend,"_FIXED",sep="");
                  return_i = return_i + 1
                }

              } # if(nrow(datax)>0){

              # Mean for all years provided
              datax<-gridTblx%>%dplyr::filter(param==param_i)

              if(length(unique(datax$x))>1){

                if(nrow(datax)>0){
                  legendTitle<-unique(datax$units)
                  fillPalette<-as.character(unique(datax$classPalette))

                  meanCol = paste("Mean_",min(datax$x),"to",max(datax$x),sep="")

                  datax<-datax%>%dplyr::select(lat,lon,scenario,class,x,value)%>%
                    dplyr::group_by(lat,lon,scenario,class)%>%
                    dplyr::summarize(!!meanCol:=mean(value))%>%
                    dplyr::ungroup()

                  animScaleGrid<-datax[[meanCol]];animScaleGrid
                  animScaleGrid <- animScaleGrid[!is.infinite(animScaleGrid)]
                  animScaleGrid <- animScaleGrid[!is.nan(animScaleGrid)]
                  animScaleGrid <- animScaleGrid[!is.na(animScaleGrid)]

                  # Choose correct scaleRange
                  if(T){
                    scaleRange_i=scaleRange

                  if(!is.null(scaleRange_i)){
                    if(any(param_i %in% unique(scaleRange_i$param))){
                      if(max(animScaleGrid) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                        animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                          animScaleGrid <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                             animScaleGrid[animScaleGrid<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                        }
                      if(min(animScaleGrid) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                        animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                          animScaleGrid <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                              animScaleGrid[animScaleGrid>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                        }
                    }
                  }
                  animPrettyBreaksGrid<-scales::pretty_breaks(n=legendBreaksn)(animScaleGrid)
                  animKmeanBreaksGrid<-sort(as.vector((stats::kmeans(animScaleGrid,
                                                                     centers=max(1,min(length(unique(animScaleGrid))-1,(legendBreaksn-1)))))$centers[,1]))
                  if(!min(animScaleGrid) %in% animKmeanBreaksGrid){
                    animKmeanBreaksGrid <- sort(c(min(animScaleGrid),animKmeanBreaksGrid))}
                  if(!max(animScaleGrid) %in% animKmeanBreaksGrid){
                    animKmeanBreaksGrid <- sort(c(animKmeanBreaksGrid,max(animScaleGrid)))}

                  if((max(range(animScaleGrid))-min(range(animScaleGrid)))<1E-10 &
                     (max(range(animScaleGrid))-min(range(animScaleGrid)))>-1E-10){animScaleGridRange=min(animScaleGrid)}else{
                       animScaleGridRange=range(animScaleGrid)
                     }
                  if(abs(min(animScaleGridRange,na.rm = T))==abs(max(animScaleGridRange,na.rm = T))){animScaleGridRange=abs(min(animScaleGridRange,na.rm = T))}
                  if(mean(animScaleGridRange,na.rm = T)<0.01 & mean(animScaleGridRange,na.rm = T)>(-0.01)){animLegendDigits<-5}else{
                    if(mean(animScaleGridRange,na.rm = T)<0.1 & mean(animScaleGridRange,na.rm = T)>(-0.1)){animLegendDigits<-4}else{
                      if(mean(animScaleGridRange,na.rm = T)<1 & mean(animScaleGridRange,na.rm = T)>(-1)){animLegendDigits<-3}else{
                        if(mean(animScaleGridRange,na.rm = T)<10 & mean(animScaleGridRange,na.rm = T)>(-10)){animLegendDigits<-2}else{animLegendDigits<-1}}}}
                }

                  # Set Facets
                  if(length(unique(datax$scenario))>1){
                    multiFacetColsx <- "scenario"
                    colm <- length(unique(datax$scenario))
                    if((length(unique(datax$class))>1)){
                      multiFacetRowsx <- c("class")
                      rowm <- length(unique(datax$class))
                    }else{
                        multiFacetRowsx <- NULL
                        rowm <- 1
                        colm = colm/((colm + ncol-1)%/%ncol);
                        rowm = (colm + ncol-1)%/%ncol
                        }
                  }else{
                    if((length(unique(datax$class))>1)){
                      multiFacetColsx <- c("class")
                      colm <- length(unique(datax$class))
                      multiFacetRowsx <- NULL
                      rowm <- 1
                      colm = colm/((colm + ncol-1)%/%ncol);
                      rowm = (colm + ncol-1)%/%ncol
                    }else{
                      multiFacetColsx <- NULL
                      multiFacetRowsx <- NULL
                      colm <- 1
                      rowm <- 1
                    }
                  }

                  # Add facet or Rows if selected
                  if(!is.null(facetCols)){
                    if(!is.null(multiFacetColsx)){
                      multiFacetColsx <- c(multiFacetColsx,facetCols)
                      colm <- colm + length(facetCols)
                    } else { multiFacetColsx <- facetCols; colm <- length(facetCols)}
                  }

                  if(!is.null(facetRows)){
                    if(!is.null(multiFacetRowsx)){
                      multiFacetRowsx <- c(multiFacetRowsx,facetRows)
                      rowm <- rowm + length(facetRows)
                    } else { multiFacetRowsx <- facetRows; rowm <- length(facetRows)}
                  }



                  # Check for Duplicates
                  if(duplicated(datax %>%
                                dplyr::select(lat,lon,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                     any()){stop("Input grid data has multiple values for the same lat and lon. Please check your data.")}

                  if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                    rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                  theme = theme, legendTitle=legendTitle,
                                  legendDigitsOverride=legendDigitsOverride,
                                  numeric2Cat_list=numeric2Cat_list,
                                  underLayer=underLayer,
                                  dataPolygon=shapex,
                                  dataGrid=datax,
                                  legendBreaksn=legendBreaksn,
                                  legendDigits = animLegendDigits,
                                  fillPalette = fillPalette,
                                  width=width*max(1,colm/1),
                                  height=height*max(1,rowm/1),
                                  pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                  labels=labels,
                                  legendBreaks = animKmeanBreaksGrid,
                                  fillColumn = meanCol,
                                  facetCols = multiFacetColsx,
                                  facetRows = multiFacetRowsx,
                                  mapTitle=paste(param_i," ",meanCol,sep=""),
                                  fileName = paste("map_","raster_",param_i,nameAppend,"_MEAN_KMEANS",sep=""),
                                  folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/combScenario",sep = ""))) ->
                      mapsReturn[[return_i]];
                    names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,nameAppend,"_MEAN_KMEANS",sep="");
                    return_i = return_i + 1
                    }

                  if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                    rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                  theme = theme, legendTitle=legendTitle,
                                  legendDigitsOverride=legendDigitsOverride,
                                  numeric2Cat_list=numeric2Cat_list,
                                  underLayer=underLayer,
                                  dataPolygon=shapex,
                                  dataGrid=datax,
                                  legendBreaksn=legendBreaksn,
                                  legendDigits = animLegendDigits,
                                  fillPalette = fillPalette,
                                  width=width*max(1,colm/1),
                                  height=height*max(1,rowm/1),
                                  pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                  labels=labels,
                                  legendBreaks = animPrettyBreaksGrid,
                                  fillColumn = meanCol,
                                  facetCols = multiFacetColsx,
                                  facetRows = multiFacetRowsx,
                                  mapTitle=paste(param_i," ",meanCol,sep=""),
                                  fileName = paste("map_","raster_",param_i,nameAppend,"_MEAN_PRETTY",sep=""),
                                  folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/combScenario",sep = ""))) ->
                      mapsReturn[[return_i]];
                    names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,nameAppend,"_MEAN_PRETTY",sep="");
                    return_i = return_i + 1
                    }

                  if(!is.null(legendFixedBreaks)){
                    rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                  theme = theme, legendTitle=legendTitle,
                                  legendDigitsOverride=legendDigitsOverride,
                                  numeric2Cat_list=numeric2Cat_list,
                                  underLayer=underLayer,
                                  dataPolygon=shapex,
                                  dataGrid=datax,
                                  legendBreaksn=legendBreaksn,
                                  legendDigits = animLegendDigits,
                                  fillPalette = fillPalette,
                                  width=width*max(1,colm/1),
                                  height=height*max(1,rowm/1),
                                  pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                  labels=labels,
                                  legendBreaks = legendFixedBreaks,
                                  fillColumn = meanCol,
                                  facetCols = multiFacetColsx,
                                  facetRows = multiFacetRowsx,
                                  mapTitle=paste(param_i," ",meanCol,sep=""),
                                  fileName = paste("map_","raster_",param_i,nameAppend,"_MEAN_FIXED",sep=""),
                                  folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/combScenario",sep = ""))) ->
                      mapsReturn[[return_i]];
                    names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,nameAppend,"_MEAN_FIXED",sep="");
                    return_i = return_i + 1
                  }

                } # if(nrow(datax)>0){
              }# If multiple years

            } # if nrow of gridTblx dplyr::filtered for Diff scenarios

              }# Close if nrow gridTbl < 0
        } # Close Combined Scenario

        # By Scenario
        for (scenario_i in unique(gridTblOrig$scenario)){

          if(length(unique(gridTblOrig$scenario))==1){scenario_if=NULL}else{scenario_if = scenario_i}
          if(length(unique(gridTblOrig$param))==1){param_if=NULL}else{param_if=param_i}

          if(nrow(gridTblOrig%>%dplyr::filter(param==param_i,scenario==scenario_i))>0){

          gridTbl <- gridTblOrig%>%dplyr::filter(param==param_i,scenario==scenario_i)

          #.................-
          # Create Grid Table Folders If Needed
          #.................-
          if(T){

                if(!dir.exists(paste(dirOutputsX,"/",sep = ""))){
                  dir.create(paste(dirOutputsX,"/",sep = ""))}

                  if(!dir.exists(paste(dirOutputsX,"/",param_if,sep = ""))){
                    dir.create(paste(dirOutputsX,"/",param_if,sep = ""))}

                    if(!dir.exists(gsub("//","/",paste(dirOutputsX,"/",param_if,"/",scenario_if,sep = "")))){
                      dir.create(paste(dirOutputsX, "/",param_if,"/",scenario_if,sep = ""))}

            if(length(unique(gridTbl$x))>1){
                    if(!dir.exists(paste(dirOutputsX,"/",param_if,"/",scenario_if,"/byYear",sep = ""))){
                      dir.create(paste(dirOutputsX, "/",param_if,"/",scenario_if,"/byYear",sep = ""))}
            }
          } # Create grid table folder if needed

          #................
          # Plot mapsReturn
          #.............--
          if(nrow(gridTbl)>0){

            #.................--
            # Save Map related Data Table
            #.................--

            if(printFig){
            if(nrow(gridTbl %>% dplyr::filter(scenario==scenario_i,param==param_i))>0){
              data.table::fwrite(gridTbl %>% dplyr::filter(scenario==scenario_i,param==param_i)%>%
                                   dplyr::select(scenario,lat,lon,param,class,x,value,units),
                                 paste(dirOutputsX,"/",param_if,"/", scenario_if,
                                       "/","map_","raster_",param_i,"_",scenario_i,nameAppend,".csv",sep = ""))
              print(paste("Map data table written to ",dirOutputsX,"/",param_if,"/", scenario_if,
                          "/","map_","raster_",param_i,"_",scenario_i,nameAppend,".csv",sep = ""))
            }
              }

            #.............................
            # By Year
            #.............................

            gridTblx<-gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)

            # Set Legends
            if(T){
              animScaleGrid<-gridTbl$value
              animScaleGrid <- animScaleGrid[!is.infinite(animScaleGrid)]
              animScaleGrid <- animScaleGrid[!is.nan(animScaleGrid)]
              animScaleGrid <- animScaleGrid[!is.na(animScaleGrid)]

              # Choose correct scaleRange
              scaleRange_i=scaleRange
              if(grepl("DiffPrcnt",scenario_i)){
                scaleRange_i=scaleRangeDiffPrcnt
                gridTbl <- gridTbl %>% dplyr::mutate(units="Percent")
              }
              if(grepl("DiffAbs",scenario_i)){
                scaleRange_i=scaleRangeDiffAbs
              }
              if(grepl("DiffxPrcnt",scenario_i)){
                scaleRange_i=scaleRangeDiffxPrcnt
                gridTbl <- gridTbl %>% dplyr::mutate(units="Percent")
              }
              if(grepl("DiffxAbs",scenario_i)){
                scaleRange_i=scaleRangeDiffxAbs
              }

              if(!is.null(scaleRange_i)){
                if(any(param_i %in% unique(scaleRange_i$param))){
                  if(max(animScaleGrid) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                    animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                      animScaleGrid <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                         animScaleGrid[animScaleGrid<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                    }
                  if(min(animScaleGrid) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                    animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                      animScaleGrid <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                          animScaleGrid[animScaleGrid>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                    }
                }
              }
              animPrettyBreaksGrid<-scales::pretty_breaks(n=legendBreaksn)(animScaleGrid); animPrettyBreaksGrid
              animKmeanBreaksGrid<-sort(as.vector((stats::kmeans(animScaleGrid,
                                                                 centers=max(1,min(length(unique(animScaleGrid))-1,(legendBreaksn-1)))))$centers[,1]));animKmeanBreaksGrid
              if(!min(animScaleGrid) %in% animKmeanBreaksGrid){
                animKmeanBreaksGrid <- sort(c(min(animScaleGrid),animKmeanBreaksGrid))}
              if(!max(animScaleGrid) %in% animKmeanBreaksGrid){
                animKmeanBreaksGrid <- sort(c(animKmeanBreaksGrid,max(animScaleGrid)))};animKmeanBreaksGrid


              if((max(range(animScaleGrid))-min(range(animScaleGrid)))<1E-10 &
                 (max(range(animScaleGrid))-min(range(animScaleGrid)))>-1E-10){animScaleGridRange=min(animScaleGrid)}else{
                   animScaleGridRange=range(animScaleGrid)
                 }

              if(abs(min(animScaleGridRange,na.rm = T))==abs(max(animScaleGridRange,na.rm = T))){animScaleGridRange=abs(min(animScaleGridRange,na.rm = T))}
              if(mean(animScaleGridRange,na.rm = T)<0.01 & mean(animScaleGridRange,na.rm = T)>(-0.01)){animLegendDigits<-5}else{
                if(mean(animScaleGridRange,na.rm = T)<0.1 & mean(animScaleGridRange,na.rm = T)>(-0.1)){animLegendDigits<-4}else{
                  if(mean(animScaleGridRange,na.rm = T)<1 & mean(animScaleGridRange,na.rm = T)>(-1)){animLegendDigits<-3}else{
                    if(mean(animScaleGridRange,na.rm = T)<10 & mean(animScaleGridRange,na.rm = T)>(-10)){animLegendDigits<-2}else{animLegendDigits<-1}}}}

            }


            # By Year
            if(length(unique(gridTblx$x))>1){
            for (x_i in unique(gridTblx$x)){

              datax<-gridTblx%>%dplyr::filter(x==x_i)
              if(nrow(datax)>0){

                legendTitle<-unique(datax$units)
                fillPalette<-as.character(unique(datax$classPalette))

                # Set Facets
                if(length(unique(gridTblx$class))>1){
                  multiFacetColsx = "class"
                  colm <- length(unique(datax$class))
                  rowm <- 1
                } else {
                    multiFacetColsx = NULL
                    colm <- 1
                    rowm <- 1
                }

                # Add facet or Rows if selected
                if(!is.null(facetCols)){
                  if(!is.null(multiFacetColsx)){
                    multiFacetColsx <- c(multiFacetColsx,facetCols)
                    colm <- colm + length(facetCols)
                  } else { multiFacetColsx <- facetCols; colm <- length(facetCols)}
                }

                if(!is.null(facetRows)){
                  if(!is.null(multiFacetRowsx)){
                    multiFacetRowsx <- c(multiFacetRowsx,facetRows)
                    rowm <- rowm + length(facetRows)
                  } else { multiFacetRowsx <- facetRows; rowm <- length(facetRows)}
                }



                # Check for Duplicates
                if(duplicated(datax %>%
                              dplyr::select(lat,lon,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                   any()){stop("Input grid data has multiple values for the same lat and lon. Please check your data.")}


                if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                theme = theme, legendTitle=legendTitle,
                                legendDigitsOverride=legendDigitsOverride,
                                numeric2Cat_list=numeric2Cat_list,
                                underLayer=underLayer,
                                dataPolygon=shapex,
                                dataGrid=datax,
                                legendBreaksn=legendBreaksn,
                                legendDigits = animLegendDigits,
                                fillPalette = fillPalette,
                                width=width*max(1,colm/1),
                                height=height,
                                pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                labels=labels,
                                legendBreaks = animKmeanBreaksGrid,
                                fillColumn = "value",
                                facetCols = multiFacetColsx,
                                mapTitle=paste(param_i," ",scenario_i," ",x_i,sep=""),
                                fileName = paste("map_","raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
                                folder = paste(dirOutputsX,"/",param_if,"/", scenario_if,"/byYear",sep = "")) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep="");
                  return_i = return_i + 1
                  }

                # ncol=ncol
                # theme_ggplot = theme_ggplot
                # theme_custom = theme_custom
                # theme_rmap = theme_rmap
                # legendDigitsOverride=legendDigitsOverride
                # numeric2Cat_list=numeric2Cat_list
                # underLayer=underLayer
                # dataPolygon=shapex
                # dataGrid=datax
                # legendBreaksn=legendBreaksn
                # legendDigits = animLegendDigits
                # fillPalette = fillPalette
                # width=width*max(1,colm/1)
                # height=height
                # pdfpng = pdfpng
                # legendSingleColor = legendSingleColor
                # legendSingleValue =  legendSingleValue
                # labels=labels
                # legendBreaks = animKmeanBreaksGrid
                # fillColumn = "value"
                # facetCols = multiFacetColsx
                # mapTitle=paste(param_i," ",scenario_i," ",x_i,sep="")
                # fileName = paste("map_","raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_KMEANS",sep="")
                # folder = paste(dirOutputsX,"/",param_if,"/", scenario_if,"/byYear",sep = "")

                if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                theme = theme, legendTitle=legendTitle,
                                legendDigitsOverride=legendDigitsOverride,
                                numeric2Cat_list=numeric2Cat_list,
                                underLayer=underLayer,
                                dataPolygon=shapex,
                                dataGrid=datax,
                                legendBreaksn=legendBreaksn,
                                legendDigits = animLegendDigits,
                                fillPalette = fillPalette,
                                width=width*max(1,colm/1),
                                height=height,
                                pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                labels=labels,
                                legendBreaks = animPrettyBreaksGrid,
                                fillColumn = "value",
                                facetCols = multiFacetColsx,
                                mapTitle=paste(param_i," ",scenario_i," ",x_i,sep=""),
                                fileName = paste("map_","raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
                                folder = paste(dirOutputsX,"/",param_if,"/", scenario_if,"/byYear",sep = "")) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_PRETTY",sep="");
                  return_i = return_i + 1
                  }


                  if(!is.null(legendFixedBreaks)){
                    rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                  theme = theme, legendTitle=legendTitle,
                                  legendDigitsOverride=legendDigitsOverride,
                                  numeric2Cat_list=numeric2Cat_list,
                                  underLayer=underLayer,
                                  dataPolygon=shapex,
                                  dataGrid=datax,
                                  legendBreaksn=legendBreaksn,
                                  legendDigits = animLegendDigits,
                                  fillPalette = fillPalette,
                                  width=width*max(1,colm/1),
                                  height=height,
                                  pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                  labels=labels,
                                  legendBreaks = legendFixedBreaks,
                                  fillColumn = "value",
                                  facetCols = multiFacetColsx,
                                  mapTitle=paste(param_i," ",scenario_i," ",x_i,sep=""),
                                  fileName = paste("map_","raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_FIXED",sep=""),
                                  folder = paste(dirOutputsX,"/",param_if,"/", scenario_if,"/byYear",sep = "")) ->
                      mapsReturn[[return_i]];
                    names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,"_",x_i,"_",scenario_i,nameAppend,"_FIXED",sep="");
                    return_i = return_i + 1
                  }



              }} # Close years x_i loop

            # Animations
            if(animateOn==T){

              if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
              animName<-paste("anim_","raster_",param_i,"_",scenario_i,nameAppend,"_PRETTY.gif",sep="")
              animFiles <- list.files(path = paste(dirOutputsX,"/",param_if,"/", scenario_if,"/byYear",sep=""),
                                      pattern = paste(".*",param_i,".*",nameAppend,".*PRETTY", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
              animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
              magick::image_write(animation,paste(dirOutputsX,"/",param_if,"/", scenario_if,"/",
                                        animName,sep = ""))
              print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",param_if,"/", scenario_if,"/",
                                                       animName,sep = "")))
              }

              if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
              animName<-paste("anim_","raster_",param_i,"_",scenario_i,nameAppend,"_KMEANS.gif",sep="")
              animFiles <- list.files(path = paste(dirOutputsX,"/",param_if,"/", scenario_if,"/byYear",sep=""),
                                      pattern = paste(".*",param_i,".*",nameAppend,".*KMEANS", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
              animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
              magick::image_write(animation,paste(dirOutputsX,"/",param_if,"/", scenario_if,"/",
                                          animName,sep = ""))
              print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",param_if,"/", scenario_if,"/",
                          animName,sep = "")))
              }

              if(!is.null(legendFixedBreaks)){
                animName<-paste("anim_","raster_",param_i,"_",scenario_i,nameAppend,"_FIXED.gif",sep="")
                animFiles <- list.files(path = paste(dirOutputsX,"/",param_if,"/", scenario_if,"/byYear",sep=""),
                                        pattern = paste(".*",param_i,".*",nameAppend,".*FIXED", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
                animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                magick::image_write(animation,paste(dirOutputsX,"/",param_if,"/", scenario_if,"/",
                                                    animName,sep = ""))
                print(gsub("//","/",paste("animation saved in :",dirOutputsX,"/",param_if,"/", scenario_if,"/",
                                          animName,sep = "")))
              }
            }
            }


            # Multi-Columns

              datax<-gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)

              if(nrow(datax)>0){

                legendTitle<-unique(datax$units)
                fillPalette<-as.character(unique(datax$classPalette))

                animScaleGrid<-datax$value
                animScaleGrid <- animScaleGrid[!is.infinite(animScaleGrid)]
                animScaleGrid <- animScaleGrid[!is.nan(animScaleGrid)]
                animScaleGrid <- animScaleGrid[!is.na(animScaleGrid)]

                # Choose correct scaleRange
                if(T){
                scaleRange_i=scaleRange
                if(grepl("DiffPrcnt",scenario_i)){
                  scaleRange_i=scaleRangeDiffPrcnt
                  datax <- datax %>% dplyr::mutate(units="Percent")
                }
                if(grepl("DiffAbs",scenario_i)){
                  scaleRange_i=scaleRangeDiffAbs
                }
                if(grepl("DiffxPrcnt",scenario_i)){
                  scaleRange_i=scaleRangeDiffxPrcnt
                  datax <- datax %>% dplyr::mutate(units="Percent")
                }
                if(grepl("DiffxAbs",scenario_i)){
                  scaleRange_i=scaleRangeDiffxAbs
                }

                if(!is.null(scaleRange_i)){
                   if(any(param_i %in% unique(scaleRange_i$param))){
                    if(max(animScaleGrid) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                      animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                        animScaleGrid <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                           animScaleGrid[animScaleGrid<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                      }
                    if(min(animScaleGrid) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                      animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                        animScaleGrid <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                            animScaleGrid[animScaleGrid>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                      }
                  }
                }
                animPrettyBreaksGrid<-scales::pretty_breaks(n=legendBreaksn)(animScaleGrid)
                animKmeanBreaksGrid<-sort(as.vector((stats::kmeans(animScaleGrid,
                                                                   centers=max(1,min(length(unique(animScaleGrid))-1,(legendBreaksn-1)))))$centers[,1]))
                if(!min(animScaleGrid) %in% animKmeanBreaksGrid){
                  animKmeanBreaksGrid <- sort(c(min(animScaleGrid),animKmeanBreaksGrid))}
                if(!max(animScaleGrid) %in% animKmeanBreaksGrid){
                  animKmeanBreaksGrid <- sort(c(animKmeanBreaksGrid,max(animScaleGrid)))}

                if((max(range(animScaleGrid))-min(range(animScaleGrid)))<1E-10 &
                   (max(range(animScaleGrid))-min(range(animScaleGrid)))>-1E-10){animScaleGridRange=min(animScaleGrid)}else{
                     animScaleGridRange=range(animScaleGrid)
                   }
                if(abs(min(animScaleGridRange,na.rm = T))==abs(max(animScaleGridRange,na.rm = T))){animScaleGridRange=abs(min(animScaleGridRange,na.rm = T))}
                if(mean(animScaleGridRange,na.rm = T)<0.01 & mean(animScaleGridRange,na.rm = T)>(-0.01)){animLegendDigits<-5}else{
                  if(mean(animScaleGridRange,na.rm = T)<0.1 & mean(animScaleGridRange,na.rm = T)>(-0.1)){animLegendDigits<-4}else{
                    if(mean(animScaleGridRange,na.rm = T)<1 & mean(animScaleGridRange,na.rm = T)>(-1)){animLegendDigits<-3}else{
                      if(mean(animScaleGridRange,na.rm = T)<10 & mean(animScaleGridRange,na.rm = T)>(-10)){animLegendDigits<-2}else{animLegendDigits<-1}}}}
                  }

                # Set Facets
                if(length(unique(datax$x))>1){
                  multiFacetColsx <- "x"
                  colm <- length(unique(datax$x))
                  if((length(unique(datax$class))>1)){
                    multiFacetRowsx <- c("class")
                    rowm <- length(unique(datax$class))
                  }else{
                      multiFacetRowsx <- NULL
                      rowm <- 1
                      colm = colm/((colm + ncol-1)%/%ncol);
                      rowm = (colm + ncol-1)%/%ncol
                      }
                }else{
                  if((length(unique(datax$class))>1)){
                    multiFacetColsx <- c("class")
                    colm <- length(unique(datax$class))
                    multiFacetRowsx <- NULL
                    rowm <- 1
                    colm = colm/((colm + ncol-1)%/%ncol);
                    rowm = (colm + ncol-1)%/%ncol
                  }else{
                    multiFacetColsx <- NULL
                    multiFacetRowsx <- NULL
                    rowm <- 1
                    colm <- 1
                  }
                }

                # Add facet or Rows if selected
                if(!is.null(facetCols)){
                  if(!is.null(multiFacetColsx)){
                    multiFacetColsx <- c(multiFacetColsx,facetCols)
                    colm <- colm + length(facetCols)
                  } else { multiFacetColsx <- facetCols; colm <- length(facetCols)}
                }

                if(!is.null(facetRows)){
                  if(!is.null(multiFacetRowsx)){
                    multiFacetRowsx <- c(multiFacetRowsx,facetRows)
                    rowm <- rowm + length(facetRows)
                  } else { multiFacetRowsx <- facetRows; rowm <- length(facetRows)}
                }



                # Check for Duplicates
                if(duplicated(datax %>%
                              dplyr::select(lat,lon,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                   any()){stop("Input grid data has multiple values for the same lat and lon. Please check your data.")}

                if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                theme = theme, legendTitle=legendTitle,
                                legendDigitsOverride=legendDigitsOverride,
                                numeric2Cat_list=numeric2Cat_list,
                                underLayer=underLayer,
                                dataPolygon=shapex,
                                dataGrid=datax,
                                legendBreaksn=legendBreaksn,
                                legendDigits = animLegendDigits,
                                fillPalette = fillPalette,
                                width=width*max(1,colm/1),
                                height=height*max(1,rowm/1),
                                pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                labels=labels,
                                legendBreaks = animKmeanBreaksGrid,
                                fillColumn = "value",
                                facetCols = multiFacetColsx,
                                facetRows = multiFacetRowsx,
                                mapTitle=paste(param_i," ",scenario_i,sep=""),
                                fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_KMEANS",sep=""),
                                folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/", scenario_if,sep = ""))) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_KMEANS",sep="");
                  return_i = return_i + 1

                      # size=max(1,(size+(colm+rowm)*3 - 12))
                      # ncol=ncol
                      # showNA=showNA
                      # colorNA=colorNA
                      # theme_ggplot = theme_ggplot
                      # theme_custom = theme_custom
                      # theme_rmap = theme_rmap
                      # legendTitle=legendTitle
                      # legendDigitsOverride=legendDigitsOverride
                      # numeric2Cat_list=numeric2Cat_list
                      # underLayer=underLayer
                      # dataPolygon=shapex
                      # dataGrid=datax
                      # legendBreaksn=legendBreaksn
                      # legendDigits = animLegendDigits
                      # fillPalette = fillPalette
                      # width=width*max(1,colm/1)
                      # height=height*max(1,rowm/1)
                      # pdfpng = pdfpng
                      # legendSingleColor = legendSingleColor
                      # legendSingleValue =  legendSingleValue
                      # labels=labels
                      # legendBreaks = animKmeanBreaksGrid
                      # fillColumn = "value"
                      # facetCols = multiFacetColsx
                      # facetRows = multiFacetRowsx
                      # mapTitle=paste(param_i," ",scenario_i,sep="")
                      # fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_KMEANS",sep="")
                      # folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/", scenario_if,sep = ""))

                  }

                if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                theme = theme, legendTitle=legendTitle,
                                legendDigitsOverride=legendDigitsOverride,
                                numeric2Cat_list=numeric2Cat_list,
                                underLayer=underLayer,
                                dataPolygon=shapex,
                                dataGrid=datax,
                                legendBreaksn=legendBreaksn,
                                legendDigits = animLegendDigits,
                                fillPalette = fillPalette,
                                width=width*max(1,colm/1),
                                height=height*max(1,rowm/1),
                                pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                labels=labels,
                                legendBreaks = animPrettyBreaksGrid,
                                fillColumn = "value",
                                facetCols = multiFacetColsx,
                                facetRows = multiFacetRowsx,
                                mapTitle=paste(param_i," ",scenario_i,sep=""),
                                fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_PRETTY",sep=""),
                                folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/", scenario_if,sep = ""))) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_PRETTY",sep="");
                  return_i = return_i + 1
                }


                if(!is.null(legendFixedBreaks)){
                  rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                theme = theme, legendTitle=legendTitle,
                                legendDigitsOverride=legendDigitsOverride,
                                numeric2Cat_list=numeric2Cat_list,
                                underLayer=underLayer,
                                dataPolygon=shapex,
                                dataGrid=datax,
                                legendBreaksn=legendBreaksn,
                                legendDigits = animLegendDigits,
                                fillPalette = fillPalette,
                                width=width*max(1,colm/1),
                                height=height*max(1,rowm/1),
                                pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                labels=labels,
                                legendBreaks = legendFixedBreaks,
                                fillColumn = "value",
                                facetCols = multiFacetColsx,
                                facetRows = multiFacetRowsx,
                                mapTitle=paste(param_i," ",scenario_i,sep=""),
                                fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_FIXED",sep=""),
                                folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/", scenario_if,sep = ""))) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_FIXED",sep="");
                  return_i = return_i + 1
                }

              } # if(nrow(datax)>0){

              # Mean for all years provided

              datax<-gridTbl%>%dplyr::filter(scenario==scenario_i,param==param_i)

              if(length(unique(datax$x))>1){

              if(nrow(datax)>0){
                legendTitle<-unique(datax$units)
                fillPalette<-as.character(unique(datax$classPalette))

                meanCol = paste("Mean_",min(datax$x),"to",max(datax$x),sep="")

                datax<-datax%>%dplyr::select(lat,lon,class,x,value)%>%
                  dplyr::group_by(lat,lon, class)%>%
                  dplyr::summarize(!!meanCol:=mean(value))%>%
                  dplyr::ungroup()

                  animScaleGrid<-datax[[meanCol]];animScaleGrid
                  animScaleGrid <- animScaleGrid[!is.infinite(animScaleGrid)]
                  animScaleGrid <- animScaleGrid[!is.nan(animScaleGrid)]
                  animScaleGrid <- animScaleGrid[!is.na(animScaleGrid)]

                  # Choose correct scaleRange
                  if(T){
                  scaleRange_i=scaleRange
                  if(grepl("DiffPrcnt",scenario_i)){
                    scaleRange_i=scaleRangeDiffPrcnt
                    datax <- datax %>% dplyr::mutate(units="Percent")
                  }
                  if(grepl("DiffAbs",scenario_i)){
                    scaleRange_i=scaleRangeDiffAbs
                  }
                  if(grepl("DiffxPrcnt",scenario_i)){
                    scaleRange_i=scaleRangeDiffxPrcnt
                    datax <- datax %>% dplyr::mutate(units="Percent")
                  }
                  if(grepl("DiffxAbs",scenario_i)){
                    scaleRange_i=scaleRangeDiffxAbs
                  }

                  if(!is.null(scaleRange_i)){
                     if(any(param_i %in% unique(scaleRange_i$param))){
                      if(max(animScaleGrid) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                        animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                          animScaleGrid <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                             animScaleGrid[animScaleGrid<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                        }
                      if(min(animScaleGrid) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                        animScaleGrid<-c(animScaleGrid,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                          animScaleGrid <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                              animScaleGrid[animScaleGrid>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                        }
                    }
                  }
                  animPrettyBreaksGrid<-scales::pretty_breaks(n=legendBreaksn)(animScaleGrid)
                  animKmeanBreaksGrid<-sort(as.vector((stats::kmeans(animScaleGrid,
                                                                     centers=max(1,min(length(unique(animScaleGrid))-1,(legendBreaksn-1)))))$centers[,1]))
                  if(!min(animScaleGrid) %in% animKmeanBreaksGrid){
                    animKmeanBreaksGrid <- sort(c(min(animScaleGrid),animKmeanBreaksGrid))}
                  if(!max(animScaleGrid) %in% animKmeanBreaksGrid){
                    animKmeanBreaksGrid <- sort(c(animKmeanBreaksGrid,max(animScaleGrid)))}


                  if((max(range(animScaleGrid))-min(range(animScaleGrid)))<1E-10 &
                     (max(range(animScaleGrid))-min(range(animScaleGrid)))>-1E-10){animScaleGridRange=min(animScaleGrid)}else{
                       animScaleGridRange=range(animScaleGrid)
                     }
                  if(abs(min(animScaleGridRange,na.rm = T))==abs(max(animScaleGridRange,na.rm = T))){animScaleGridRange=abs(min(animScaleGridRange,na.rm = T))}
                  if(mean(animScaleGridRange,na.rm = T)<0.01 & mean(animScaleGridRange,na.rm = T)>(-0.01)){animLegendDigits<-5}else{
                    if(mean(animScaleGridRange,na.rm = T)<0.1 & mean(animScaleGridRange,na.rm = T)>(-0.1)){animLegendDigits<-4}else{
                      if(mean(animScaleGridRange,na.rm = T)<1 & mean(animScaleGridRange,na.rm = T)>(-1)){animLegendDigits<-3}else{
                        if(mean(animScaleGridRange,na.rm = T)<10 & mean(animScaleGridRange,na.rm = T)>(-10)){animLegendDigits<-2}else{animLegendDigits<-1}}}}
                  }

                  # Set Facets
                  if((length(unique(datax$class))>1)){
                    multiFacetColsx <- c("class")
                    colm <- length(unique(datax$class))
                    rowm <- 1
                  }else{
                      multiFacetColsx <- NULL
                      colm <- 1
                      rowm <- 1
                  }

                  # Add facet or Rows if selected
                  if(!is.null(facetCols)){
                    if(!is.null(multiFacetColsx)){
                      multiFacetColsx <- c(multiFacetColsx,facetCols)
                      colm <- colm + length(facetCols)
                    } else { multiFacetColsx <- facetCols; colm <- length(facetCols)}
                  }

                  if(!is.null(facetRows)){
                    if(!is.null(multiFacetRowsx)){
                      multiFacetRowsx <- c(multiFacetRowsx,facetRows)
                      rowm <- rowm + length(facetRows)
                    } else { multiFacetRowsx <- facetRows; rowm <- length(facetRows)}
                  }



                  # Check for Duplicates
                  if(duplicated(datax %>%
                                dplyr::select(lat,lon,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                     any()){stop("Input grid data has multiple values for the same lat and lon. Please check your data.")}


                if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                theme = theme, legendTitle=legendTitle,
                                legendDigitsOverride=legendDigitsOverride,
                                numeric2Cat_list=numeric2Cat_list,
                                underLayer=underLayer,
                                dataPolygon=shapex,
                                dataGrid=datax,
                                legendBreaksn=legendBreaksn,
                                legendDigits = animLegendDigits,
                                fillPalette = fillPalette,
                                width=width*max(1,colm/1),
                                height=height,
                                pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                labels=labels,
                                legendBreaks = animKmeanBreaksGrid,
                                fillColumn = meanCol,
                                facetCols = multiFacetColsx,
                                mapTitle=paste(param_i," ",scenario_i," ",meanCol,sep=""),
                                fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_MEAN_KMEANS",sep=""),
                                folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/", scenario_if,sep = ""))) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_MEAN_KMEANS",sep="");
                  return_i = return_i + 1
                  }

                if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                theme = theme, legendTitle=legendTitle,
                                legendDigitsOverride=legendDigitsOverride,
                                numeric2Cat_list=numeric2Cat_list,
                                underLayer=underLayer,
                                dataPolygon=shapex,
                                dataGrid=datax,
                                legendBreaksn=legendBreaksn,
                                legendDigits = animLegendDigits,
                                fillPalette = fillPalette,
                                width=width*max(1,colm/1),
                                height=height,
                                pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                labels=labels,
                                legendBreaks = animPrettyBreaksGrid,
                                fillColumn = meanCol,
                                facetCols = multiFacetColsx,
                                mapTitle=paste(param_i," ",scenario_i," ",meanCol,sep=""),
                                fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_MEAN_PRETTY",sep=""),
                                folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/", scenario_if,sep = ""))) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_MEAN_PRETTY",sep="");
                  return_i = return_i + 1
                }

                  if(!is.null(legendFixedBreaks)){
                    rmap::mapplot(printFig=printFig, overLayer=overLayer, overLayerColor=overLayerColor, overLayerFill = overLayerFill, overLayerLwd = overLayerLwd, overLayerAlpha = overLayerAlpha, underLayerColor, underLayerFill = underLayerFill, underLayerLwd = underLayerLwd, underLayerAlpha = underLayerAlpha, background=background, zoom=zoom, zoomx = zoomx, zoomy=zoomy, alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol, showNA=showNA, colorNA=colorNA,
                                  theme = theme, legendTitle=legendTitle,
                                  legendDigitsOverride=legendDigitsOverride,
                                  numeric2Cat_list=numeric2Cat_list,
                                  underLayer=underLayer,
                                  dataPolygon=shapex,
                                  dataGrid=datax,
                                  legendBreaksn=legendBreaksn,
                                  legendDigits = animLegendDigits,
                                  fillPalette = fillPalette,
                                  width=width*max(1,colm/1),
                                  height=height,
                                  pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                  labels=labels,
                                  legendBreaks = legendFixedBreaks,
                                  fillColumn = meanCol,
                                  facetCols = multiFacetColsx,
                                  mapTitle=paste(param_i," ",scenario_i," ",meanCol,sep=""),
                                  fileName = paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_MEAN_FIXED",sep=""),
                                  folder = sub("/$","",paste(dirOutputsX,"/",param_if,"/", scenario_if,sep = ""))) ->
                      mapsReturn[[return_i]];
                    names(mapsReturn)[return_i] <- paste("map_","raster_",param_i,"_",scenario_i,nameAppend,"_MEAN_FIXED",sep="");
                    return_i = return_i + 1
                  }

              } # if(nrow(datax)>0){
            }# If multiple years


          } # Close if nrow gridTbl < 0

          }# Close if nrow gridTbl < 0
        } # close Scenarios

      } # Close params loop

      } # Close if nrow gridTbl < 0

    animateOn <- animateOnOrig

    }# Close if gridTbl is Null
  } # Close Raster Plots

  # .................
  # Return Data
  # .................

  print("map run completed.")

  invisible(mapsReturn)

  } # Close direct plots

} # close function
