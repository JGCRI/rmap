#' map
#'
#' This function produce different kinds of maps for the rmap package.
#' Each figure is accompanied with a csv table.
#'
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @param data Default = NULL,
#' @param region Default = NULL. Set the boundary region for subRegion maps. Useful when multiple subRegions in different regions.
#' @param fillColumn (Optional). Default = NULL. Only for direct map plotting.
#' @param shape Default = NULL, Cusotm shape can be provided as a SpatialPolygonDataFrame with features corresponding to subRegion columns in the data provided.
#' @param fileName (Optional). Default = "map". Only for direct map plotting.
#' @param save (Optional). Default = T. Only for direct map plotting.
#' @param theme Default = NULL,
#' @param show Default = T. Print maps in console as they are processed.
#' @param folder Default = paste(getwd(),"/outputs",sep=""),
#' @param labels Default = F,
#' @param labelCol Default = NULL,
#' @param labelRepel Default = 0,
#' @param labelColor Default = "black",
#' @param labelSize Default = 3
#' @param labelAlpha Default = 0.7,
#' @param labelFill Default = NA,
#' @param labelBorderSize Default = NA
#' @param shapeFolder Default = paste(getwd(),"/dataFiles/gis/admin_gadm36",sep=""),
#' @param shapeFile Default = paste("gadm36_1",sep=""),
#' @param subRegCol Default ="subRegion",
#' @param valueCol Default = "value",
#' @param nameAppend Default =""
#' @param legendTitle Default = NULL
#' @param legendType Default ="kmeans", Options include c("pretty","kmeans","freescale","all")
#' @param legendBreaksn Default = "5",
#' @param legendFixedBreaks Default = NULL,
#' @param animate Default = T,
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
#' @param scaleRangexDiffAbs Default =NULL, A vector with c(max,min) (Applied to all params) or a dataframe with cols param, max, min
#' @param scaleRangexDiffPrcnt Default =NULL, A vector with c(max,min) (Applied to all params) or a dataframe with cols param, max, min
#' @param col Default ="multiFacetRow",
#' @param row Default ="multiFacetCol",
#' @param title Default=NULL
#' @param numeric2Cat_list Default=NULL,
#' @param fill Default = NULL. Fill of polygon shapes. Same as palette.
#' @param color Default = "grey40". Color of polygon lines.
#' @param lwd Default = 0.1. Line width of polygon boundaries.
#' @param underLayer Default = NULL
#' @param underLayerLabelCol Default = NULL
#' @param underLayerColor Default = "gray40"
#' @param underLayerFill Default = "gray90"
#' @param underLayerLwd Default = 0.5
#' @param underLayerAlpha Default = 1
#' @param underLayerLabels Default = F
#' @param overLayerLabels Default = F
#' @param overLayerLabelCol Default = NULL
#' @param overLayer Default = NULL
#' @param overLayerColor Default = "gray40"
#' @param overLayerFill Default = NA
#' @param overLayerLwd Default = 0.5
#' @param overLayerAlpha Default = 0
#' @param zoom Default =-1. Zoom into or out of map. Positive values zoom in and negative out.
#' @param zoomx Default = NULL. Zoom into or out of map along x. Positive values zoom in and negative out.
#' @param zoomy Default = NULL. Zoom into or out of map along y. Positive values zoom in and negative out.
#' @param asp Default = 1.2. Aspect ratio of lat and lon.
#' @param pdfpng Save IO figures as pdf or png. Type=String. Options: 'pdf' or 'png'. Default = 'png'
#' @param legendSingleValue Default=F. Change to True to get default single value or provide a numeric value.
#' @param legendSingleColor Default="white"
#' @param legendDigitsOverride Default=NULL
#' @param palette Default = NULL
#' @param paletteDiff Default = "pal_div_BrGn"
#' @param crop Default = T. This crops the map to the extent of your data regions. If false will zoom out to the extent of the larget layer.
#' @param crop_to_underLayer Default = F. Crop to the underLayer boundary provided.
#' @param crop_to_overLayer Default = F. Crop to the overLayer boundary provided.
#' @param colorNA Default = "gray50"
#' @param showNA Default = T
#' @param ncol Default = 3. Number of columns to wrap maps
#' @param size Default = 12. Text size of plots.
#' @param alpha Default = 1. Transparency of fill colors.
#' @param background Default = F. Add background water color, border and default underlayer map.
#' @param transparent Default = T. To make map background transparent for maps without backgrounds.
#' @param legendShow Default = T
#' @param diffOnly Default = F. Only run diff plots and not individual scenarios.
#' @param forceFacets Default = F. Used to force facet label for single scenario which is usually dropped.
#' @param crs Default = 4326, WGS84
#' @return A list of maps
#' @import sf
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#' @export


map <- function(data = NULL,
                region = NULL,
                fillColumn = NULL,
                shape = NULL,
                fileName = "map",
                save=T,
                theme = NULL,
                show = T,
                folder = paste(getwd(), "/outputs", sep = ""),
                labels = F,
                labelCol = NULL,
                labelRepel = 0,
                labelColor = "black",
                labelSize = 2,
                labelAlpha = 1,
                labelFill = NA,
                labelBorderSize = NA,
                shapeFolder = NULL,
                shapeFile = NULL,
                subRegCol = "subRegion",
                valueCol = "value",
                nameAppend = "",
                legendTitle = NULL,
                legendType ="kmeans",
                legendBreaksn = 5,
                legendFixedBreaks = NULL,
                animate = T,
                fps = 1,
                crop = T,
                crop_to_underLayer = F,
                crop_to_overLayer = F,
                fill = NULL,
                color = "grey40",
                lwd = 0.1,
                underLayer = NULL,
                underLayerColor = "gray40",
                underLayerFill = "gray90",
                underLayerLwd = 0.1,
                underLayerAlpha = 1,
                underLayerLabels= F,
                underLayerLabelCol = NULL,
                overLayerLabelCol = NULL,
                overLayerLabels = F,
                overLayer = NULL,
                overLayerColor = "gray40",
                overLayerFill = NA,
                overLayerLwd = 0.2,
                overLayerAlpha = 0,
                zoom = 0,
                zoomx = NULL,
                zoomy = NULL,
                asp = 1.2,
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
                scaleRangexDiffAbs = NULL,
                scaleRangexDiffPrcnt = NULL,
                col = NULL,
                row = NULL,
                title=NULL,
                numeric2Cat_list = NULL,
                pdfpng = 'png',
                legendDigitsOverride = NULL,
                legendSingleColor ="white",
                legendSingleValue =F,
                palette = NULL,
                paletteDiff = "pal_div_BluRd",
                colorNA = "gray50",
                showNA = F,
                ncol = 3,
                size = 16,
                alpha = 1,
                background = F,
                transparent = T,
                legendShow = T,
                diffOnly = F,
                forceFacets = F,
                crs = 4326) {

  # # data = NULL
  # crs = 4326
  # legendSingleValue =F
  # show = T
  # fill = NULL
  # lwd = 0.1
  # color = "grey40"
  # fillColumn = NULL
  # fileName = "map"
  # save=T
  # theme = NULL
  # folder = paste(getwd(), "/outputs", sep = "")
  # labels = F
  # labelCol = NULL,
  # shapeFolder = NULL
  # shapeFile = NULL
  # subRegCol = "subRegion"
  # valueCol = "value"
  # nameAppend = ""
  # legendType ="kmeans"
  # legendBreaksn = 5
  # legendFixedBreaks = NULL
  # animate = T
  # fps = 1
  # crop = F
  # underLayer = NULL
  # underLayerColor = "gray40"
  # underLayerFill = "gray90"
  # underLayerLwd = 0.5
  # underLayerAlpha = 1
  # overLayer = NULL
  # overLayerColor = "gray40"
  # overLayerFill = NA
  # overLayerLwd = 0.5
  # overLayerAlpha = 0
  # overLayerLabels = F
  # zoom = 0
  # zoomx = NULL
  # zoomy = NULL
  # projX = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # width = 7
  # height = 8
  # scenRef = NULL
  # scenDiff = NULL
  # scaleRange = NULL
  # scaleRangeDiffAbs = NULL
  # scaleRangeDiffPrcnt = NULL
  # xRef = NULL
  # xDiff = NULL
  # scaleRangexDiffAbs = NULL
  # scaleRangexDiffPrcnt = NULL
  # col = NULL
  # row = NULL
  # title=NULL
  # numeric2Cat_list = NULL
  # pdfpng = 'png'
  # legendDigitsOverride = NULL
  # legendSingleColor ="white"
  # palette = NULL
  # paletteDiff = "pal_div_BluRd"
  # colorNA = "gray50"
  # showNA = F
  # ncol = 3
  # size = 12
  # alpha = 1
  # background = F
  # asp = 1.2
  # legendTitle = NULL
  # transparent = F
  # legendShow = T
  # underLayerLabels=F
  # shape = NULL
  # diffOnly = F
  # forceFacets = F
  # labelCol = NULL
  # labels = F
  # labelRepel = 0
  # labelColor = "black"
  # labelSize = 2
  # labelAlpha = 1
  # labelFill = NA
  # labelBorderSize = NA
  # crop_to_underLayer = F
  # crop_to_overLayer = F
  # region = NULL
  # underLayerLabelCol = NULL
  # overLayerLabelCol = NULL

  rlang::inform("Starting map...")

  #.................-
  # Initialize variables
  # .................

  if(T){

    NULL->lat->lon->param->scenario->subRegion->value ->
      x->year->gridID->maxScale->minScale->
      valueDiff->rowid->catParam->include->Var1->Var2->Var3->maxX->minX->
      dataTblDiff -> dataTblxDiff -> countCheck->
      multiFacetCol -> multiFacetRow->paletteOrig->
      xLabel->vintage->aggregate->query->subRegNotInShape ->dataTblOrig -> subRegionAlt -> subRegion1 ->
      paramsGrid -> paramsShape -> scaleRange_i -> boundaryRegShapeLimits -> dataTbl -> subRegType -> paramsdata

    if(!is.null(fill) & is.null(palette)){ palette = fill}

  if(!save){animate=F}

  return_i = 1; # Index for return maps list
  mapsReturn = list(); # Return maps list

  paletteOrig <- palette
  shapeFileOrig <- shapeFile
  shapeFolderOrig <- shapeFolder
  animateOrig <- animate
  legendTitleOrig <- legendTitle
  forceFacetsOrig = forceFacets

  if(!is.null(legendFixedBreaks)){
    # Must be a vector of more than one number
    if(length(legendFixedBreaks)<2){
      stop("legendFixedBreaks must be a vector of more than one number")
    }

    if(any(!is.numeric(legendFixedBreaks))){
      stop("legendFixedBreaks must be a vector of more than one number")
    }

  }

  # Rename SubRegCol
  if(T){
    if(!is.null(data)){
      if(nrow(data)>0){
        if(subRegCol != "subRegion"){
          if(any(subRegCol %in% names(data))){
            if(any("subRegion" %in% names(data))){
              data <- data %>%
                dplyr::select(-subRegion)
            }
            data <- data %>%
              dplyr::rename("subRegion" = subRegCol) %>%
              dplyr::mutate(subRegion = as.character(subRegion))
          }
        }
      }
    }
  }

  # Remove NA subRegion
  if(T){
    if(any(grepl("tbl_df|tbl|data.frame",class(data)))){
    if(!is.null(data)){
      if(nrow(data)>0){
        if(any("subRegion" %in% names(data))){
          data <- data %>%
            dplyr::filter(!is.na(subRegion))
        }
      }
      }
    }
  }

  # Rename valueCol
  if(T){
    if(!is.null(data)){
      if(nrow(data)>0){
        if(valueCol != "value"){
          if(any(valueCol %in% names(data))){
            if(any("value" %in% names(data))){
              data <- data %>%
                dplyr::select(-value)
            }
            data <- data %>%
              dplyr::rename("value" = valueCol) %>%
              dplyr::mutate(value = as.numeric(value))
          }
        }
      }
    }
  }

  }

  #.................-
  # Check for Shape files
  # .................

  if(any(grepl("SpatialPolygonsDataFrame",class(data)))){
    data <- sf::st_as_sf(data)
  }

  # Custom Shape
  if(!is.null(shape)){
    if(!any(grepl("sf",class(shape)))){
      shapex <- sf::st_as_sf(shape)
    } else {
      shapex = shape
    }
  }else{
    shapex = NULL
  }

  #.................-
  # Run map_plot directly if a shpefile is provided
  # .................

  if(any(grepl("tbl_df|tbl|data.frame",class(data))) &
     !"value" %in% names(data)){

    if(!"subRegion" %in% names(data)){stop("Data must have 'subRegion' column or specify another column using the `subRegCol` argument instead.")}

     if(is.null(palette)) {
      palettex = "Set3"
    }else{
      palettex <- palette
    }

    colm=1
    rowm=1

    rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType,
      pdfpng=pdfpng,
      overLayer=overLayer,
      overLayerColor=overLayerColor,
      overLayerFill = overLayerFill,
      overLayerLwd = overLayerLwd,
      overLayerAlpha = overLayerAlpha,
      underLayerColor,
      underLayerFill = underLayerFill,
      underLayerLwd = underLayerLwd,
      underLayerAlpha = underLayerAlpha,
      background=background,
      zoom=zoom, zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=F,  crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
      alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)),
      theme = theme,
      legendTitle=NULL,
      underLayer=underLayer,
      data = data,
      palette=palettex,
      folder=folder,
      labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
      labelColor=labelColor,
      labelSize=labelSize,
      labelAlpha=labelAlpha,
      labelFill=labelFill,
      labelBorderSize=labelBorderSize,
      fileName = fileName,
      fillColumn = fillColumn,
      show=show,
      save = save,
      width = width,
      height = height,
      title=title) ->
      mapsReturn[[return_i]];

    names(mapsReturn)[return_i] <- fileName; return_i = return_i + 1

    # labelCol = labelCol
    # overLayer=overLayer
    # overLayerColor=overLayerColor
    # overLayerFill = overLayerFill
    # overLayerLwd = overLayerLwd
    # overLayerAlpha = overLayerAlpha
    # underLayerColor
    # underLayerFill = underLayerFill
    # underLayerLwd = underLayerLwd
    # underLayerAlpha = underLayerAlpha
    # background=background
    # zoom=zoom
    # zoomx = zoomx
    # zoomy=zoomy
    # asp=asp
    # legendShow=legendShow
    # crop = crop
    # crop_to_underLayer = crop_to_underLayer
    # crop_to_overLayer = crop_to_overLayer
    # transparent=transparent
    # alpha = alpha
    # size=max(1,(size+(colm+rowm)*3 - 12))
    # theme = theme
    # legendTitle=NULL
    # underLayer=underLayer
    # data = data
    # palette=palettex
    # folder=folder
    # labels=labels
    # labelRepel=labelRepel
    # underLayerLabels=underLayerLabels
    # overLayerLabels=overLayerLabels
    # labelColor=labelColor
    # labelSize=labelSize
    # labelAlpha=labelAlpha
    # labelFill=labelFill
    # labelBorderSize=labelBorderSize
    # fileName = fileName
    # fillColumn = fillColumn
    # save = save
    # width = width
    # height = height
    # title=title

    # .................
    # Return Data
    # .................

    rlang::inform("map run completed.")

    invisible(mapsReturn)

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
      if(!class(data$scenario)=="factor"){
      data<-data%>%dplyr::mutate(scenario=as.character(scenario),scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}}
    if(!any(grepl("\\<scenarios\\>",names(data),ignore.case = T))){}else{
      data <- data %>% dplyr::rename(!!"scenario" := (names(data)[grepl("\\<scenarios\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(scenario=as.character(scenario),scenario=dplyr::case_when(is.na(scenario)~"scenario",TRUE~scenario))}
    if(!"x"%in%names(data)){if("year"%in%names(data)){
      data<-data%>%dplyr::mutate(x=year)}else{data<-data%>%dplyr::mutate(x="x")}}
    if(any(grepl("\\<subregion\\>",names(data),ignore.case = T))){
      data <- data %>% dplyr::rename(!!"subRegion" := (names(data)[grepl("\\<subregion\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(subRegion=as.character(subRegion))}
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
    if(!any(grepl("\\<palette\\>",names(data),ignore.case = T))){data<-data%>%dplyr::mutate(palette="pal_hot")}else{
      data <- data %>% dplyr::rename(!!"palette" := (names(data)[grepl("\\<palette\\>",names(data),ignore.case = T)])[1])
      data<-data%>%dplyr::mutate(palette=as.character(palette),palette=dplyr::case_when(is.na(palette)~"pal_hot",TRUE~palette))}
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


  if(is.null(data)){
    stop ("Data is NULL. Need to provide valid data.")
  } else{
    if("subRegion" %in% names(data)){
    # Make sure subRegion is character
    data <- data %>%
      dplyr::mutate(subRegion = as.character(subRegion))
    }
  }


  } # Close custom functions

  #.................-
  # Create Folders
  #.................-

  if(save){

  if(!dir.exists(folder)){dir.create(folder)}

  } # Close create folders

  #.................-
  # Read in data Tables (Either csv tables or an R Table)
  #.................-

  if(T){

  dataTbl<-data.frame()


  if(!is.null(data)){

    if(all(!class(data) %in% c("tbl_df","tbl","data.frame"))){

      for(data_i in data){
        if(file.exists(data_i)){
          dataTblNew<-data.table::fread(paste(data_i),encoding="Latin-1")%>%as.data.frame()
          dataTbl<-dplyr::bind_rows(dataTbl,dataTblNew)
          rm(dataTblNew)
        } else {stop(paste(data_i," does not exist"))}
      }

    }else{dataTbl<-data}

  }else{dataTbl=data}


  if(!is.null(dataTbl)){
  if(nrow(dataTbl)>0){

    # Add missing columns
    dataTbl<-addMissing(dataTbl)

    if(!"value" %in% names(dataTbl)){stop("'value' column not present in data provided. Check data.")}
    if(!"lat" %in% names(dataTbl) &
       !"lon" %in% names(dataTbl) &
       !"subRegion" %in% names(dataTbl)){
      stop(paste0("Must have atleast either 'lat' and 'lon columns or subRegion column in data."))}

    # Set palette if given
    if(!is.null(paletteOrig) & (length(paletteOrig)==1)){
    dataTbl <- dataTbl %>% dplyr::mutate(palette = paletteOrig)
    }

  }}

  if(!is.null(dataTbl)){
    if(nrow(dataTbl)==0){
      dataTbl = NULL
    }
  }

  } # Close read in data Tables

  #.................-
  # Subset Data
  #.................-

  if(T){

  # Keep only Unique Values
  if(!is.null(dataTbl)){
    if(nrow(dataTbl)>0){
    dataTbl<-dataTbl %>%
      #dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(value = signif(value,10)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()
    }
  }

  } # Subset data

  #.................-
  # xRange
  #.................-

  if(T){
    if(!is.null(dataTbl)){
      xRange = unique(c(unique(dataTbl$x)))
    }
  }

  #.................-
  # Compare Scenarios & X diff
  #.................-

  if(T){
  # Get Params and Scenarios
  if(!is.null(dataTbl)){


        if(nrow(dataTbl)>0){
          params <- unique(c(unique(dataTbl$param)))
          scenarios <- unique(c(unique(dataTbl$scenario)))
        }

  # Compare Scenarios
  if(length(scenarios)>1){
    if(!is.null(scenRef)){

      if(!any(scenRef %in% scenarios)){
        rlang::inform(paste0("scenRef chosen: ", scenRef, " is not in any of the available scenarios: "))
        rlang::inform(paste(scenarios,collapse=", "))
        rlang::inform(paste0("Setting scenRef to first scenario: ", scenarios[1],"."))
        scenRef <- scenarios[1]
      }


      if(is.null(scenDiff)){
        scenDiff <- scenarios[!scenarios %in% scenRef]
        rlang::inform(paste0("Running difference against all available scenarios:"))
        rlang::inform(paste(scenDiff,collapse=", "))
      }else{
        if(!any(scenDiff %in% scenarios)){
          rlang::inform(paste("None of the scenDiff are in any of the available scenarios: "))
          rlang::inform(paste(scenarios[!scenarios %in% scenRef],collapse=", "))
          rlang::inform(paste0("Skipping Diff."))
        }
      }

      dataTblDiff <- data.frame()

      for(i in 1:length(params)){

        NULL -> param_i -> scenRef_i -> scenDiff_i

        param_i <- params[i]
        scenRef_i <- as.character(scenRef)
        scenDiff_i <- as.character(scenDiff)

        if(!is.null(param_i) & !is.null(scenRef_i) & !is.null(scenDiff_i)){

    # Compare  Data
    if(!is.null(dataTbl)){

      dataTblDiffa <- dataTbl %>% dplyr::filter(param==param_i & (scenario %in% c(scenRef_i,scenDiff_i)))

      if(length(unique(dataTblDiffa$scenario))>1){

        if(scenRef_i %in% unique(dataTblDiffa$scenario)){
          rlang::inform(paste("Ref scenario chosen for param: ", param_i, " is ", paste(scenRef_i,collapse=", "),sep=""))
        if(any(scenDiff_i %in% unique(dataTblDiffa$scenario))){
          rlang::inform(paste("Diff scenarios chosen for param: ", param_i, " are ",
                      paste(scenDiff_i[scenDiff_i %in% unique(dataTblDiffa$scenario)],collapse=", "),sep=""))}


        scenDiff_i <- scenDiff_i[scenDiff_i %in% unique(dataTblDiffa$scenario)]

       # Calculate Diff Values

      dataTblDiffb<-dataTblDiffa%>%dplyr::filter(param==param_i, scenario %in% dplyr::all_of(c(scenRef_i,scenDiff_i)))%>%
        dplyr::select(lat,lon,subRegion,region,subRegType,param,x,xLabel,vintage,units,aggregate,palette,class,scenario,value)%>%
        tidyr::spread(scenario,value)

      # If paletteDiff is a character vector collapse it to a string.
      if(length(paletteDiff)>1){
        paletteDiff <-paste(paletteDiff,collapse=",")
        }

      for (scenario_i in unique(dataTblDiffa$scenario)[unique(dataTblDiffa$scenario) %in% scenDiff_i]){
        tbl_temp1 <-dataTblDiffb%>%
          dplyr::mutate(!!paste(scenario_i,"_DiffAbs_",scenRef_i,sep=""):=get(scenario_i)-get(scenRef_i),
                        palette=paletteDiff)%>%
          dplyr::select(-dplyr::one_of(as.vector(unique(dataTblDiffa$scenario))))
        tbl_temp1<-tbl_temp1%>%
          tidyr::gather(key=scenario,value=value,
                        -c(names(tbl_temp1)[!names(tbl_temp1) %in% paste(scenario_i,"_DiffAbs_",scenRef_i,sep="")]))%>%
          dplyr::filter(!is.na(value))

        tbl_temp2 <-dataTblDiffb%>%
          dplyr::mutate(!!paste(scenario_i,"_DiffPrcnt_",scenRef_i,sep=""):=((get(scenario_i)-get(scenRef_i))*100/get(scenRef_i)),
                        palette=paletteDiff)%>%
          dplyr::select(-dplyr::one_of(as.vector(unique(dataTblDiffa$scenario))))
        tbl_temp2<-tbl_temp2%>%
          tidyr::gather(key=scenario,value=value,
                        -c(names(tbl_temp2)[!names(tbl_temp2) %in% paste(scenario_i,"_DiffPrcnt_",scenRef_i,sep="")]))%>%
          dplyr::filter(!is.na(value))

        dataTblDiff<-dplyr::bind_rows(dataTblDiff,tbl_temp1,tbl_temp2)
      }
    }}
  }

      }

    }
    }

    dataTbl <- dataTbl %>%
      dplyr::bind_rows(dataTblDiff) %>%
      dplyr::ungroup() %>%
      dplyr::distinct();

  }else{
    scenRef = NULL
    scenDiff = NULL
  } # Compare Scenarios

    # Compare X
  if(length(unique(dataTbl$x))>1){

    if(!is.null(xRef)){

      if(!any(xRef %in% xRange)){
        rlang::inform(paste("xRef chosen: ", xRef, " is not in any of the available x values: ",sep=""))
        rlang::inform(paste(xRange,collapse=", "))
        rlang::inform(paste("Setting xRef to first x value: ", xRange[1],".",sep=""))
        xRef <- xRange[1]
      }

      if(is.null(xDiff)){
        xDiff <- xRange[!xRange %in% xRef]
        rlang::inform(paste("Running difference against all available x:",sep=""))
        rlang::inform(paste(xDiff,collapse=", "))
      }else{
        if(!any(xDiff %in% xRange)){
          rlang::inform(paste("None of the xDiff are in any of the available scenarios: "))
          rlang::inform(paste(xRange[!xRange %in% xRef],collapse=", "))
          rlang::inform(paste("Skipping x Diff.",sep=""))
        } else {
          if(!all(xDiff %in% xRange)){
            rlang::inform(paste0("Not all xDiff chosen: ",paste(xDiff,collapse=",")))
            xDiff <- xDiff[xDiff %in% xRange]
            rlang::inform(paste0("Setting xDiff to available x: ",xDiff))
          }
        }

      }

      dataTblxDiff <- data.frame()

      for(i in 1:length(params)){

        NULL -> param_i -> xRef_i -> xDiff_i

        param_i <- params[i]
        xRef_i <- xRef
        xDiff_i <- xDiff

        if(!is.null(param_i) & !is.null(xRef_i) & !is.null(xDiff_i)){

          # Compare Data
          if(nrow(dataTbl)>0){

            dataTblDiffa <- dataTbl %>% dplyr::filter(param==param_i & (x %in% c(xRef_i,xDiff_i)));dataTblDiffa

            if(nrow(dataTblDiffa)>0){
              # Calculate Diff Values

              if(xRef_i %in% unique(dataTblDiffa$x)){
                rlang::inform(paste("Ref x chosen for param: ", param_i, " is ", paste(xRef_i,collapse=", "),sep=""))
                if(any(xDiff_i %in% unique(dataTblDiffa$x))){
                  rlang::inform(paste("Diff x chosen for param: ", param_i, " are ",
                              paste(xDiff_i[xDiff_i %in% unique(dataTblDiffa$x)],collapse=", "),sep=""))
                  }

                xDiff_i <- xDiff_i[xDiff_i %in% unique(dataTblDiffa$x)]

                colsx <- c("region","subRegion","lat","lon","subRegType","param","x","xLabel","units","aggregate","palette","class","scenario","value")
                colsx1 <- names(dataTbl)[names(dataTbl) %in% colsx]; colsx1
                colsx2 <- colsx1[!colsx1 %in% "value"]; colsx2
                dataTblDiffb<-dataTbl%>%dplyr::filter(param==param_i, x %in% c(xRef_i,xDiff_i))%>%
                  dplyr::select(dplyr::all_of(colsx1))%>%
                  dplyr::group_by_at(colsx2)%>%
                  dplyr::summarize(value=sum(value,na.rm=T))%>%
                  tidyr::spread(x,value);dataTblDiffb%>%as.data.frame(); names(dataTblDiffb)

                for (scen_i in scenarios){
                  for (x_i in unique(dataTbl$x)[(unique(dataTbl$x) %in% xDiff_i)]){
                    tbl_temp1 <-dataTblDiffb%>%
                      dplyr::filter(scenario==scen_i)%>%
                      dplyr::mutate(!!paste(scen_i,"_xDiffAbs_",xRef_i,sep=""):=(!!as.name(x_i)-!!as.name(xRef_i)),
                                    palette=paletteDiff)%>%
                      dplyr::select(-!!as.character(xDiff_i),-!!as.character(xRef_i))
                    tbl_temp1<-tbl_temp1%>%
                      tidyr::gather(key=scenario,value=value,
                                    -c(names(tbl_temp1)[!names(tbl_temp1) %in% paste(scen_i,"_xDiffAbs_",xRef_i,sep="")]))%>%
                      dplyr::filter(!is.na(value))%>%
                      dplyr::mutate(x=x_i)

                    tbl_temp2 <-dataTblDiffb%>%
                      dplyr::filter(scenario==scen_i)%>%
                      dplyr::mutate(!!paste(scen_i,"_xDiffPrcnt_",xRef_i,sep=""):=((!!as.name(x_i)-!!as.name(xRef_i))*100/!!as.name(xRef_i)),
                                    palette=paletteDiff)%>%
                      dplyr::select(-!!as.character(xDiff_i),-!!as.character(xRef_i))
                    tbl_temp2<-tbl_temp2%>%
                      tidyr::gather(key=scenario,value=value,
                                    -c(names(tbl_temp2)[!names(tbl_temp2) %in% paste(scen_i,"_xDiffPrcnt_",xRef_i,sep="")]))%>%
                      dplyr::filter(!is.na(value))%>%
                      dplyr::mutate(x=x_i)

                    dataTblxDiff<-dplyr::bind_rows(dataTblxDiff,tbl_temp1,tbl_temp2)
                  }
                } # Close Scenario
              }
            }
          }


        }
      }

    }

    dataTbl <- dataTbl %>%
      dplyr::bind_rows(dataTblxDiff) %>%
      dplyr::ungroup() %>%
      dplyr::distinct();

  }else{
    xRef = NULL
    xDiff = NULL
  } # Compare X

  }
  }

  #.................-
  # Check MultiFacet Columns
  #.................-

  if(T){ # Check facet column/Rows selected exist

  # Shape Table

  # data table
  if(!is.null(dataTbl)){

    if(!is.null(col)){
    if(!col %in% names(dataTbl)){
      rlang::inform(paste0("col chosen: ",  col ,"do not exist:"))
      col <- NULL
    }}

    if(!is.null(row)){
    if(!row %in% names(dataTbl)){
      rlang::inform(paste0("row chosen: ",  row ,"do not exist:"))
      row <- NULL
    }
    }
  }

  } # Check facet column/rows selected exist


  #................
  # Check scaleRanges
  #.............--

  if(T){

    # Get list of params in data or shapex data
    if(!is.null(dataTbl)){if(nrow(dataTbl)>0){paramsdata <- unique(dataTbl$param)}}
    paramsRange <- unique(c(paramsdata)); paramsRange


    # Scale Range
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
            rlang::inform(paste("None of the params in scaleRange: ",
                        paste(unique(scaleRange$param),collapse=", "),sep=""))
            rlang::inform("are present in the data params:")
            rlang::inform(paste(paramsRange,collapse=", "))
            rlang::inform("Setting scaleRange to NULL")
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
            rlang::inform(paste("None of the params in scaleRangeDiffAbs: ",
                        paste(unique(scaleRangeDiffAbs$param),collapse=", "),sep=""))
            rlang::inform("are present in the data params:")
            rlang::inform(paste(paramsRange,collapse=", "))
            rlang::inform("Setting scaleRangeDiffAbs to NULL")
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
            rlang::inform(paste("None of the params in scaleRangeDiffPrcnt: ",
                        paste(unique(scaleRangeDiffPrcnt$param),collapse=", "),sep=""))
            rlang::inform("are present in the data params:")
            rlang::inform(paste(paramsRange,collapse=", "))
            rlang::inform("Setting scaleRangeDiffPrcnt to NULL")
            scaleRangeDiffPrcnt=NULL
            }
        }else{scaleRangeDiffPrcnt=NULL}
      }
  }

    # Scale Range Diff X Abs
    if(!is.null(scaleRangexDiffAbs)){
      scaleRangexDiffAbs[is.na(scaleRangexDiffAbs)]<-NA_real_
      scaleRangexDiffAbs[scaleRangexDiffAbs=="NA"]<-NA_real_
      # If scale range is a vector of two numbers set as limits for all params
      if(is.numeric(scaleRangexDiffAbs) & length(scaleRangexDiffAbs)==2){
        scaleRangexDiffAbs = data.frame(param=paramsRange,maxScale=max(scaleRangexDiffAbs),minScale=min(scaleRangexDiffAbs))
      } else {
        if(!is.null(nrow(scaleRangexDiffAbs))){
          scaleRangexDiffAbs = addMissingScale(scaleRangexDiffAbs)
          if(!any(unique(scaleRangexDiffAbs$param) %in% paramsRange)){
            rlang::inform(paste("None of the params in scaleRangexDiffAbs: ",
                        paste(unique(scaleRangexDiffAbs$param),collapse=", "),sep=""))
            rlang::inform("are present in the data params:")
            rlang::inform(paste(paramsRange,collapse=", "))
            rlang::inform("Setting scaleRangexDiffAbs to NULL")
            scaleRangexDiffAbs=NULL
          }
        }else{scaleRangexDiffAbs=NULL}
      }
    }

    # Scale Range Diff X Prcnt
    if(!is.null(scaleRangexDiffPrcnt)){
      scaleRangexDiffPrcnt[is.na(scaleRangexDiffPrcnt)]<-NA_real_
      scaleRangexDiffPrcnt[scaleRangexDiffPrcnt=="NA"]<-NA_real_
      # If scale range is a vector of two numbers set as limits for all params
      if(is.numeric(scaleRangexDiffPrcnt) & length(scaleRangexDiffPrcnt)==2){
        scaleRangexDiffPrcnt = data.frame(param=paramsRange,maxScale=max(scaleRangexDiffPrcnt),minScale=min(scaleRangexDiffPrcnt))
      } else {
        # Else format the scaleRangexDiffPrcnt data frame as needed
        if(!is.null(nrow(scaleRangexDiffPrcnt))){
          scaleRangexDiffPrcnt = addMissingScale(scaleRangexDiffPrcnt)
          if(!any(unique(scaleRangexDiffPrcnt$param) %in% paramsRange)){
            rlang::inform(paste("None of the params in scaleRangexDiffPrcnt: ",
                        paste(unique(scaleRangexDiffPrcnt$param),collapse=", "),sep=""))
            rlang::inform("are present in the data params:")
            rlang::inform(paste(paramsRange,collapse=", "))
            rlang::inform("Setting scaleRangexDiffPrcnt to NULL")
            scaleRangexDiffPrcnt=NULL
          }
        }else{scaleRangexDiffPrcnt=NULL}
      }
    }

    }# Close Check Scale Range

  #.................-
  # Set Palettes
  #.................-

    if(T){
      if(!is.null(dataTbl)){
        if(nrow(dataTbl)>0){

          if(length(paletteOrig)>1){
            dataTbl <- dataTbl %>%
              dplyr::mutate(palette = dplyr::case_when(palette=="pal_hot" ~ paste(paletteOrig,collapse=","),
                                                       TRUE ~ palette))
          }

          if(length(paletteDiff)>1){
            dataTbl <- dataTbl %>%
              dplyr::mutate(palette = dplyr::case_when(palette=="pal_div_BluRd" ~ paste(paletteDiff,collapse=","),
                                                       TRUE ~ palette))
          }
        }
      }
    }

  # .................--
  # Create Plots
  # .................--

  if(T){ # Create Plots

  if(!is.null(dataTbl)){
    if(nrow(dataTbl)>0){

      dataTblOrig <- dataTbl
      dataTbl_scenariosOrig <- unique(dataTbl$scenario)

      if(!length(unique(dataTblOrig$x))>1){animate=F}

      for (param_i in unique(dataTblOrig$param)){

        # If not diffOnly
        if(!diffOnly){

            if(length(unique(dataTblOrig$param))==1){param_if=NULL}else{param_if=param_i}

            if(nrow(dataTblOrig%>%dplyr::filter(param==param_i))>0){

              dataTblx <- dataTblOrig%>%dplyr::filter(param==param_i,
                                                     scenario %in% dataTbl_scenariosOrig[!grepl("_xDiff|_DiffAbs|_DiffPrcnt",dataTbl_scenariosOrig)])

                if(nrow(dataTblx)>0){

              #.................-
              # Create data Table Folders If Needed
              #.................-
              if(save){

                if(!dir.exists(paste(folder,"/",sep = ""))){
                  dir.create(paste(folder,"/",sep = ""))}

                if(!dir.exists(paste(folder,"/",param_if,sep = ""))){
                  dir.create(paste(folder,"/",param_if,sep = ""))}

                if(!dir.exists(paste(folder,"/",param_if,sep = ""))){
                  dir.create(paste(folder, "/",param_if,sep = ""))}

                if(length(unique(dataTblx$x))>1){
                if(!dir.exists(paste(folder,"/",param_if,"/byYear",sep = ""))){
                  dir.create(paste(folder, "/",param_if,"/byYear",sep = ""))}
                }
              } # Create data table folder if needed

              #.................--
              # Save Map related Data Table
              #.................--

              if(save){
                if(nrow(dataTblx %>% dplyr::filter(param==param_i))>0){
                  data.table::fwrite(dataTblx %>% dplyr::filter(param==param_i)%>%
                                       dplyr::select(scenario,lat,lon,subRegion,region,param,class,x,value,units),
                                     paste(folder,"/",param_if,"/map_",param_i,nameAppend,".csv",sep = ""))
                  rlang::inform(paste("Map data table written to ",folder,"/",param_if,"/map_",param_i,nameAppend,".csv",sep = ""))
                }
              }

              #.............................
              # By Year
              #.............................

              # Set Legends
              if(T){
                scalex<-dataTblx$value
                scalex <- scalex[!is.infinite(scalex)]
                scalex <- scalex[!is.nan(scalex)]
                scalex <- scalex[!is.na(scalex)]

                # Choose correct scaleRange
                scaleRange_i=scaleRange

                if(!is.null(scaleRange_i)){
                  if(any(param_i %in% unique(scaleRange_i$param))){
                    if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                      scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                        scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                           scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                      }
                    if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                      scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                        scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                            scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                      }
                  }
                }

                prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex); prettyBreaks
                kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                   centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]));kmeanBreaks
                if(!min(scalex) %in% kmeanBreaks){
                  kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                if(!max(scalex) %in% kmeanBreaks){
                  kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))};kmeanBreaks

                if(!is.null(legendFixedBreaks)){
                  if(min(scalex) < min(legendFixedBreaks)){
                    legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                  if(max(scalex) > max(legendFixedBreaks)){
                    legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                }

                if((max(range(scalex))-min(range(scalex)))<1E-10 &
                   (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                     scaleRangex=range(scalex)
                   }

                if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                  if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                    if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                      if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}

              }

              # By Year
              if(length(unique(dataTblx$x))>1){

              for (x_i in unique(dataTblx$x)){

                datax<-dataTblx%>%dplyr::filter(x==x_i)

                if(nrow(datax)>0){
                  if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                  palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}


                  # Set Facets
                  if(length(unique(datax$scenario))>1){
                    multiFacetColsx <- "scenario"
                    colm <- length(unique(datax$scenario))
                    if((length(unique(datax$class))>1)){
                      multiFacetColsx <- "class"
                      colm <- length(unique(datax$class))
                      multiFacetRowsx <- c("scenario")
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
                      if(forceFacets){
                        multiFacetColsx <- "scenario"
                      } else {
                        multiFacetColsx <- NULL
                      }
                      multiFacetRowsx <- NULL
                      colm = 1
                      rowm = 1
                      }
                  }

                  # Add facet or Rows if selected
                  if(!is.null(col)){
                    if(!is.null(multiFacetColsx)){
                      multiFacetColsx <- c(multiFacetColsx,col)
                      colm <- colm + length(col)
                    } else { multiFacetColsx <- col; colm <- length(col)}
                  }

                  if(!is.null(row)){
                    if(!is.null(multiFacetRowsx)){
                      multiFacetRowsx <- c(multiFacetRowsx,row)
                      rowm <- rowm + length(row)
                    } else { multiFacetRowsx <- row; rowm <- length(row)}
                  }


                  # Check for Duplicates
                  if(duplicated(datax %>%
                                dplyr::select(lat,lon,subRegion,region,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                     any()){stop("Input data data has multiple values. Please check your data.")}

                  # Set title
                  if(is.null(title)){
                    if(param_i == "param"){
                      titlex <- paste(x_i,sep="")
                    } else {
                      titlex <- paste(param_i," ",x_i,sep="")
                      }
                  } else if(title == F){
                    titlex <- NULL
                  } else {
                    titlex <- title
                  }

                  # Assign variables based on legend type choice
                  if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                    fileNameTag <- "KMEANS"
                    legendBreaksx <- kmeanBreaks
                  } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                    fileNameTag <- "CONTINUOUS"
                    legendBreaksx <- prettyBreaks
                  } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                    fileNameTag <- "PRETTY"
                    legendBreaksx <- prettyBreaks
                  } else if(!is.null(legendFixedBreaks)){
                    fileNameTag <- "FIXED"
                    legendBreaksx <- legendFixedBreaks
                  }


                    rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                  overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                  overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                  underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                  underLayerAlpha = underLayerAlpha, background=background,
                                  zoom=zoom, zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                  crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                  alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)),
                                  ncol=ncol, showNA=showNA, colorNA=colorNA,
                                  theme = theme, legendTitle=legendTitle,
                                  legendDigitsOverride=legendDigitsOverride,
                                  numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                  underLayer=underLayer,
                                  data=datax,
                                  labelColor=labelColor,
                                  labelSize=labelSize,
                                  labelAlpha=labelAlpha,
                                  labelFill=labelFill,
                                  labelBorderSize=labelBorderSize,
                                  legendBreaksn=legendBreaksn,
                                  legendDigits = legendDigits,
                                  palette = palette,
                                  width=width*max(1,colm/1),
                                  height=height*max(1,rowm/1),
                                  pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                  labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                  legendBreaks = legendBreaksx,
                                  fillColumn = "value",
                                  col = multiFacetColsx,
                                  row = multiFacetRowsx,
                                  title=titlex ,
                                  fileName = paste("map_",param_i,"_",x_i,"_",fileNameTag,nameAppend,sep=""),
                                  folder = paste(folder,"/",param_if,"/byYear",sep = "")) ->
                      mapsReturn[[return_i]];
                      names(mapsReturn)[return_i] <- paste("map_",param_i,"_",x_i,"_",fileNameTag,nameAppend,sep="");
                      return_i = return_i + 1

                      # theme_ggplot = theme_ggplot
                      # theme_custom = theme_custom
                      # theme_rmap = theme_rmap
                      # legendDigitsOverride=legendDigitsOverride
                      # numeric2Cat_list=numeric2Cat_list
                      # underLayer=underLayer
                      # data=datax
                      # legendBreaksn=legendBreaksn
                      # legendDigits = legendDigits
                      # palette = palette
                      # width=width*max(1,colm/1),
                      # height=height*max(1,rowm/1),
                      # pdfpng = pdfpng
                      # legendSingleColor = legendSingleColor
                      # legendSingleValue =  legendSingleValue
                      # labels=labels
                      # legendBreaks = kmeanBreaks
                      # fillColumn = "value"
                      # col = multiFacetColsx
                      # row = multiFacetRowsx
                      # title=paste(param_i," ",x_i,sep="")
                      # fileName = paste("map_",param_i,"_",x_i,"_",fileNameTag,nameAppend,sep="")
                      # folder = paste(folder,"/",param_if,"/byYear",sep = "")



                }
                } # Close years x_i loop

              # Animations
              if(animate==T){
                  animName<-paste("anim_",param_i,"_",fileNameTag,nameAppend,".gif",sep="")
                  animFiles <- list.files(path = paste(folder,"/",param_if,"/byYear",sep=""),
                                          pattern = paste(".*",param_i,".*",fileNameTag,nameAppend,"", ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
                  animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                  magick::image_write(animation,paste(folder,"/",param_if,"/",
                                                      animName,sep = ""))
                  rlang::inform(gsub("//","/",paste("animation saved in :",folder,"/",param_if,"/",
                                            animName,sep = "")))

              }
              }

              # Multi-Year-Single Chart
              datax<-dataTblx%>%dplyr::filter(param==param_i)
              if(nrow(datax)>0){

                if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                scalex<-datax$value
                scalex <- scalex[!is.infinite(scalex)]
                scalex <- scalex[!is.nan(scalex)]
                scalex <- scalex[!is.na(scalex)]

                # Choose correct scaleRange
                if(T){
                  scaleRange_i=scaleRange

                  if(!is.null(scaleRange_i)){
                    if(any(param_i %in% unique(scaleRange_i$param))){
                      if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                        scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                          scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                             scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                        }
                      if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                        scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                          scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                              scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                        }
                    }
                  }
                  prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex)
                  kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                     centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]))
                  if(!min(scalex) %in% kmeanBreaks){
                    kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                  if(!max(scalex) %in% kmeanBreaks){
                    kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))}

                  if(!is.null(legendFixedBreaks)){
                    if(min(scalex) < min(legendFixedBreaks)){
                      legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                    if(max(scalex) > max(legendFixedBreaks)){
                      legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                  }

                  if((max(range(scalex))-min(range(scalex)))<1E-10 &
                     (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                       scaleRangex=range(scalex)
                     }

                  if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                  if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                    if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                      if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                        if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}
                }

                # Set Facets
                if(length(unique(datax$x))>1){
                  multiFacetColsx <- "x"
                  colm <- length(unique(datax$x))
                  if((length(unique(datax$scenario))>1) & (length(unique(datax$class))>1)){
                    multiFacetRowsx <- "x"
                    rowm <- length(unique(datax$x))
                    multiFacetColsx <- c("scenario","class")
                    colm <- length(unique(datax$scenario))*length(unique(datax$class))
                    }
                  if((length(unique(datax$scenario))>1) & (!length(unique(datax$class))>1)){
                    multiFacetRowsx <- c("scenario")
                    rowm <- length(unique(datax$scenario))
                    }
                  if((!length(unique(datax$scenario))>1) & (length(unique(datax$class))>1)){
                    multiFacetRowsx <- "x"
                    rowm <- length(unique(datax$x))
                    multiFacetColsx <- c("class")
                    colm <- length(unique(datax$class))
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
                        multiFacetRowsx <- "scenario"
                        rowm <- length(unique(datax$scenario))
                        multiFacetColsx <- c("class")
                        colm <- length(unique(datax$class))
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
                        if(forceFacets){
                          multiFacetColsx <- "scenario"
                        } else {
                          multiFacetColsx <- NULL
                        }
                        multiFacetRowsx <- NULL
                        colm <- 1
                        rowm <- 1
                      }
                    }
                  }


                # Add facet or Rows if selected
                if(!is.null(col)){
                  if(!is.null(multiFacetColsx)){
                    multiFacetColsx <- c(multiFacetColsx,col)
                    colm <- colm + length(col)
                  } else { multiFacetColsx <- col; colm <- length(col)}
                }

                if(!is.null(row)){
                  if(!is.null(multiFacetRowsx)){
                    multiFacetRowsx <- c(multiFacetRowsx,row)
                    rowm <- rowm + length(row)
                  } else { multiFacetRowsx <- row; rowm <- length(row)}
                }

                # Check for Duplicates
                if(duplicated(datax %>%
                              dplyr::select(subRegion,region,lat,lon,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                   any()){stop("Input data data has multiple values. Please check your data.")}

                # Set title
                if(is.null(title)){
                  if(param_i != "param"){
                  titlex <- paste(param_i,sep="")
                  } else {
                    titlex <- NULL
                  }
                } else if(title == F){
                  titlex <- NULL
                } else {
                  titlex <- title
                }

                # Assign variables based on legend type choice
                if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  fileNameTag <- "KMEANS"
                  legendBreaksx <- kmeanBreaks
                } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                  fileNameTag <- "CONTINUOUS"
                  legendBreaksx <- prettyBreaks
                } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                  fileNameTag <- "PRETTY"
                  legendBreaksx <- prettyBreaks
                } else if(!is.null(legendFixedBreaks)){
                  fileNameTag <- "FIXED"
                  legendBreaksx <- legendFixedBreaks
                }

                  rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                 overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                 overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                 underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                 underLayerAlpha = underLayerAlpha, background=background, zoom=zoom,
                                 zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                 crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                 alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol,
                                 showNA=showNA, colorNA=colorNA,
                                 labelColor=labelColor,
                                 labelSize=labelSize,
                                 labelAlpha=labelAlpha,
                                 labelFill=labelFill,
                                 labelBorderSize=labelBorderSize,
                                theme = theme, legendTitle=legendTitle,
                                legendDigitsOverride=legendDigitsOverride,
                                numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                underLayer=underLayer,
                                data=datax,
                                legendBreaksn=legendBreaksn,
                                legendDigits = legendDigits,
                                palette = palette,
                                width=width*max(1,colm/1),
                                height=height*max(1,rowm/1),
                                pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                legendBreaks = legendBreaksx,
                                fillColumn = "value",
                                col = multiFacetColsx,
                                row = multiFacetRowsx,
                                title= titlex,
                                fileName = paste("map_",param_i,"_",fileNameTag,nameAppend,sep=""),
                                folder = sub("/$","",paste(folder,"/",param_if,sep = ""))) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_",param_i,"_",fileNameTag,nameAppend,sep="");
                  return_i = return_i + 1

                  # color=color; lwd=lwd; legendType=legendType; save=save;  show=show; shape = shapex; overLayer=overLayer; overLayerColor=overLayerColor;
                  # overLayerFill = overLayerFill; overLayerLwd = overLayerLwd;
                  # overLayerAlpha = overLayerAlpha; underLayerColor=underLayerColor;
                  # underLayerFill = underLayerFill; underLayerLwd = underLayerLwd;
                  # underLayerAlpha = underLayerAlpha; background=background; zoom=zoom;
                  # zoomx = zoomx; zoomy=zoomy; asp=asp; legendShow=legendShow;
                  # crop = crop; crop_to_underLayer = crop_to_underLayer; crop_to_overLayer = crop_to_overLayer; transparent=transparent;
                  # alpha = alpha; size=max(1,(size+(colm+rowm)*3 - 12)); ncol=ncol;
                  # showNA=showNA; colorNA=colorNA;
                  # labelColor=labelColor;
                  # labelSize=labelSize;
                  # labelAlpha=labelAlpha;
                  # labelFill=labelFill;
                  # labelBorderSize=labelBorderSize;
                  # theme = theme; legendTitle=legendTitle;
                  # legendDigitsOverride=legendDigitsOverride;
                  # numeric2Cat_list=numeric2Cat_list; catParam = param_i;
                  # underLayer=underLayer;
                  # data=datax;
                  # legendBreaksn=legendBreaksn;
                  # legendDigits = legendDigits;
                  # palette = palette;
                  # width=width*max(1,colm/1);
                  # height=height*max(1,rowm/1);
                  # pdfpng = pdfpng; legendSingleColor = legendSingleColor; legendSingleValue =  legendSingleValue;
                  # labels=labels; labelRepel=labelRepel; underLayerLabels=underLayerLabels; overLayerLabels=overLayerLabels;
                  # legendBreaks = legendBreaksx;
                  # fillColumn = "value";
                  # col = multiFacetColsx;
                  # row = multiFacetRowsx;
                  # title= titlex;
                  # fileName = paste("map_",param_i,"_",fileNameTag,nameAppend,sep="");
                  # folder = sub("/$","",paste(folder,"/",param_if,sep = ""))

                  } # if(nrow(datax)>0){

              # Mean for all years provided
              datax<-dataTblx%>%dplyr::filter(param==param_i)

              if(length(unique(datax$x))>1){

                if(nrow(datax)>0){

                  if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                  palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                  meanCol = paste("Mean_",min(datax$x),"to",max(datax$x),sep="")


                  colsPresentGroup =  c("lon","lat","subRegion","region","scenario","class")
                  colsPresentGroup = colsPresentGroup[colsPresentGroup %in% names(datax)]

                  datax<-datax%>%
                    dplyr::select(lat,lon,subRegion,region,scenario,class,x,value)%>%
                    dplyr::group_by_at(dplyr::all_of(colsPresentGroup))%>%
                    dplyr::summarize(!!meanCol:=mean(value))%>%
                    dplyr::ungroup()

                  scalex<-datax[[meanCol]];scalex
                  scalex <- scalex[!is.infinite(scalex)]
                  scalex <- scalex[!is.nan(scalex)]
                  scalex <- scalex[!is.na(scalex)]

                  # Choose correct scaleRange
                  if(T){
                    scaleRange_i=scaleRange

                  if(!is.null(scaleRange_i)){
                    if(any(param_i %in% unique(scaleRange_i$param))){
                      if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                        scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                          scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                             scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                        }
                      if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                        scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                          scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                              scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                        }
                    }
                  }
                  prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex)
                  kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                     centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]))
                  if(!min(scalex) %in% kmeanBreaks){
                    kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                  if(!max(scalex) %in% kmeanBreaks){
                    kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))}

                  if(!is.null(legendFixedBreaks)){
                    if(min(scalex) < min(legendFixedBreaks)){
                      legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                    if(max(scalex) > max(legendFixedBreaks)){
                      legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                  }

                  if((max(range(scalex))-min(range(scalex)))<1E-10 &
                     (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                       scaleRangex=range(scalex)
                     }

                  if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                  if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                    if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                      if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                        if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}
                }

                  # Set Facets
                  if(length(unique(datax$scenario))>1){
                    multiFacetColsx <- "scenario"
                    colm <- length(unique(datax$scenario))
                    if((length(unique(datax$class))>1)){
                      multiFacetRowsx <- "scenario"
                      rowm <- length(unique(datax$scenario))
                      multiFacetColsx <- c("class")
                      colm <- length(unique(datax$class))
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
                      if(forceFacets){
                        multiFacetColsx <- "scenario"
                      } else {
                        multiFacetColsx <- NULL
                      }
                      multiFacetRowsx <- NULL
                      colm <- 1
                      rowm <- 1
                    }
                  }

                  # Add facet or Rows if selected
                  if(!is.null(col)){
                    if(!is.null(multiFacetColsx)){
                      multiFacetColsx <- c(multiFacetColsx,col)
                      colm <- colm + length(col)
                    } else { multiFacetColsx <- col; colm <- length(col)}
                  }

                  if(!is.null(row)){
                    if(!is.null(multiFacetRowsx)){
                      multiFacetRowsx <- c(multiFacetRowsx,row)
                      rowm <- rowm + length(row)
                    } else { multiFacetRowsx <- row; rowm <- length(row)}
                  }



                  # Check for Duplicates
                  if(duplicated(datax %>%
                                dplyr::select(lat,lon,subRegion,region,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                     any()){stop("Input data data has multiple values. Please check your data.")}

                  # Set title
                  if(is.null(title)){
                    if(param_i == "param"){
                      titlex <- paste(meanCol,sep="")
                    } else {
                      titlex <- paste(param_i," ",meanCol,sep="")
                    }
                  } else if(title == F){
                    titlex <- NULL
                  } else {
                    titlex <- title
                  }

                  # Assign variables based on legend type choice
                  if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                    fileNameTag <- "KMEANS"
                    legendBreaksx <- kmeanBreaks
                  } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                    fileNameTag <- "CONTINUOUS"
                    legendBreaksx <- prettyBreaks
                  } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                    fileNameTag <- "PRETTY"
                    legendBreaksx <- prettyBreaks
                  } else if(!is.null(legendFixedBreaks)){
                    fileNameTag <- "FIXED"
                    legendBreaksx <- legendFixedBreaks
                  }

                    rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                   overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                   overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                   underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                   underLayerAlpha = underLayerAlpha, background=background,
                                   zoom=zoom, zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                   crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                   alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)),
                                   ncol=ncol, showNA=showNA, colorNA=colorNA,
                                   labelColor=labelColor,
                                   labelSize=labelSize,
                                   labelAlpha=labelAlpha,
                                   labelFill=labelFill,
                                   labelBorderSize=labelBorderSize,
                                  theme = theme, legendTitle=legendTitle,
                                  legendDigitsOverride=legendDigitsOverride,
                                  numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                  underLayer=underLayer,
                                  data=datax,
                                  legendBreaksn=legendBreaksn,
                                  legendDigits = legendDigits,
                                  palette = palette,
                                  width=width*max(1,colm/1),
                                  height=height*max(1,rowm/1),
                                  pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                  labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                  legendBreaks = legendBreaksx,
                                  fillColumn = meanCol,
                                  col = multiFacetColsx,
                                  row = multiFacetRowsx,
                                  title = titlex,
                                  fileName = paste("map_",param_i,"_MEAN_",fileNameTag,nameAppend,sep=""),
                                  folder = sub("/$","",paste(folder,"/",param_if,sep = ""))) ->
                      mapsReturn[[return_i]];
                    names(mapsReturn)[return_i] <- paste("map_",param_i,"_MEAN_",fileNameTag,nameAppend,sep="");
                    return_i = return_i + 1


                } # if(nrow(datax)>0){
              }# If multiple years

            } # if nrow of dataTblx dplyr::filtered for Diff scenarios

              }# Close if nrow dataTbl < 0

        }

        # if scenRef chosen
        if(!is.null(scenRef)){

        # Diff Abs
        if(T){
          if(length(unique(dataTblOrig$param))==1){param_if=NULL}else{param_if=param_i}

          if(nrow(dataTblOrig%>%dplyr::filter(param==param_i))>0){

            dataTblx <- dataTblOrig%>%dplyr::filter(param==param_i,
                                                    scenario %in% dataTbl_scenariosOrig[grepl("_DiffAbs",dataTbl_scenariosOrig)])

            if(nrow(dataTblx)>0){

              #.................-
              # Create data Table Folders If Needed
              #.................-
              if(save){

                if(!dir.exists(paste(folder,"/",sep = ""))){
                  dir.create(paste(folder,"/",sep = ""))}

                if(!dir.exists(paste(folder,"/",param_if,sep = ""))){
                  dir.create(paste(folder,"/",param_if,sep = ""))}

                if(!dir.exists(paste(folder,"/",param_if,sep = ""))){
                  dir.create(paste(folder, "/",param_if,sep = ""))}

                if(length(unique(dataTblx$x))>1){
                  if(!dir.exists(paste(folder,"/",param_if,"/byYear",sep = ""))){
                    dir.create(paste(folder, "/",param_if,"/byYear",sep = ""))}
                }
              } # Create data table folder if needed

              #.................--
              # Save Map related Data Table
              #.................--

              if(save){
                if(nrow(dataTblx %>% dplyr::filter(param==param_i))>0){
                  data.table::fwrite(dataTblx %>% dplyr::filter(param==param_i)%>%
                                       dplyr::select(scenario,lat,lon,subRegion,region,param,class,x,value,units),
                                     paste(folder,"/",param_if,"/map_",param_i,nameAppend,".csv",sep = ""))
                  rlang::inform(paste("Map data table written to ",folder,"/",param_if,"/map_",param_i,nameAppend,".csv",sep = ""))
                }
              }

              #.............................
              # By Year
              #.............................

              # Set Legends
              if(T){
                scalex<-dataTblx$value
                scalex <- scalex[!is.infinite(scalex)]
                scalex <- scalex[!is.nan(scalex)]
                scalex <- scalex[!is.na(scalex)]

                # Choose correct scaleRange
                scaleRange_i=scaleRangeDiffAbs

                if(!is.null(scaleRange_i)){
                  if(any(param_i %in% unique(scaleRange_i$param))){
                    if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                      scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                        scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                       scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                      }
                    if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                      scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                        scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                        scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                      }
                  }
                }

                prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex); prettyBreaks
                kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                               centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]));kmeanBreaks
                if(!min(scalex) %in% kmeanBreaks){
                  kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                if(!max(scalex) %in% kmeanBreaks){
                  kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))};kmeanBreaks

                if(!is.null(legendFixedBreaks)){
                  if(min(scalex) < min(legendFixedBreaks)){
                    legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                  if(max(scalex) > max(legendFixedBreaks)){
                    legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                }


                if((max(range(scalex))-min(range(scalex)))<1E-10 &
                   (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                     scaleRangex=range(scalex)
                   }

                if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                  if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                    if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                      if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}

              }

              # By Year
              if(length(unique(dataTblx$x))>1){

                for (x_i in unique(dataTblx$x)){

                  datax<-dataTblx%>%dplyr::filter(x==x_i)

                  if(nrow(datax)>0){
                    if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                    palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                    # Set Facets
                    if(length(unique(datax$scenario))>1){
                      multiFacetColsx <- "scenario"
                      colm <- length(unique(datax$scenario))
                      if((length(unique(datax$class))>1)){
                        multiFacetRowsx <- "scenario"
                        rowm <- length(unique(datax$scenario))
                        multiFacetColsx <- c("class")
                        colm <- length(unique(datax$class))
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
                        if(forceFacets){
                          multiFacetColsx <- "scenario"
                        } else {
                          multiFacetColsx <- NULL
                        }
                        multiFacetRowsx <- NULL
                        colm = 1
                        rowm = 1
                      }
                    }

                    # Add facet or Rows if selected
                    if(!is.null(col)){
                      if(!is.null(multiFacetColsx)){
                        multiFacetColsx <- c(multiFacetColsx,col)
                        colm <- colm + length(col)
                      } else { multiFacetColsx <- col; colm <- length(col)}
                    }

                    if(!is.null(row)){
                      if(!is.null(multiFacetRowsx)){
                        multiFacetRowsx <- c(multiFacetRowsx,row)
                        rowm <- rowm + length(row)
                      } else { multiFacetRowsx <- row; rowm <- length(row)}
                    }


                    # Check for Duplicates
                    if(duplicated(datax %>%
                                  dplyr::select(lat,lon,subRegion,region,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                       any()){stop("Input data data has multiple values. Please check your data.")}


                    # Set title
                    if(is.null(title)){
                      if(param_i == "param"){
                        if(length(scenDiff)==1){
                          titlex <- paste(x_i," ",scenDiff," diffAbs ", scenRef,sep="")
                        }else{
                          titlex <- paste(x_i,sep="")
                        }
                      } else {
                        if(length(scenDiff)==1){
                          titlex <- paste(param_i," ",x_i," ",scenDiff," diffAbs ", scenRef,sep="")
                        }else{
                          titlex <- paste(param_i," ",x_i,sep="")
                        }
                      }
                    } else if(title == F){
                      titlex <- NULL
                    } else {
                      titlex <- title
                    }

                    # Assign variables based on legend type choice
                    if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                      fileNameTag <- "KMEANS"
                      legendBreaksx <- kmeanBreaks
                    } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                      fileNameTag <- "CONTINUOUS"
                      legendBreaksx <- prettyBreaks
                    } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                      fileNameTag <- "PRETTY"
                      legendBreaksx <- prettyBreaks
                    } else if(!is.null(legendFixedBreaks)){
                      fileNameTag <- "FIXED"
                      legendBreaksx <- legendFixedBreaks
                    }

                      rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                     overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                     overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                     underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                     underLayerAlpha = underLayerAlpha, background=background,
                                     zoom=zoom, zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                     crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                     alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)),
                                     ncol=ncol, showNA=showNA, colorNA=colorNA,
                                     theme = theme, legendTitle=legendTitle,
                                     legendDigitsOverride=legendDigitsOverride,
                                     numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                     underLayer=underLayer,
                                     data=datax,
                                     labelColor=labelColor,
                                     labelSize=labelSize,
                                     labelAlpha=labelAlpha,
                                     labelFill=labelFill,
                                     labelBorderSize=labelBorderSize,
                                     legendBreaksn=legendBreaksn,
                                     legendDigits = legendDigits,
                                     palette = palette,
                                     width=width*max(1,colm/1),
                                     height=height*max(1,rowm/1),
                                     pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                     labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                     legendBreaks = legendBreaksx,
                                     fillColumn = "value",
                                     col = multiFacetColsx,
                                     row = multiFacetRowsx,
                                     title=titlex ,
                                     fileName = paste("map_",param_i,"_",x_i,"_",fileNameTag,"_DiffAbs",nameAppend,sep=""),
                                     folder = paste(folder,"/",param_if,"/byYear",sep = "")) ->
                        mapsReturn[[return_i]];
                      names(mapsReturn)[return_i] <- paste("map_",param_i,"_",x_i,"_",fileNameTag,"_DiffAbs",nameAppend,sep="");
                      return_i = return_i + 1
                      }
                } # Close years x_i loop

                # Animations
                if(animate==T){

                    animName<-paste("anim_",param_i,"_",fileNameTag,"_DiffAbs",nameAppend,".gif",sep="")
                    animFiles <- list.files(path = paste(folder,"/",param_if,"/byYear",sep=""),
                                            pattern = paste(".*",param_i,".*",fileNameTag,"_DiffAbs",nameAppend, ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
                    animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                    magick::image_write(animation,paste(folder,"/",param_if,"/",
                                                        animName,sep = ""))
                    rlang::inform(gsub("//","/",paste("animation saved in :",folder,"/",param_if,"/",
                                              animName,sep = "")))


                }
              }

              # Multi-Year-Single Chart
              datax<-dataTblx%>%dplyr::filter(param==param_i)
              if(nrow(datax)>0){

                if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                scalex<-datax$value
                scalex <- scalex[!is.infinite(scalex)]
                scalex <- scalex[!is.nan(scalex)]
                scalex <- scalex[!is.na(scalex)]

                # Choose correct scaleRange
                if(T){
                  scaleRange_i=scaleRangeDiffAbs

                  if(!is.null(scaleRange_i)){
                    if(any(param_i %in% unique(scaleRange_i$param))){
                      if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                        scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                          scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                         scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                        }
                      if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                        scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                          scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                          scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                        }
                    }
                  }
                  prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex)
                  kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                 centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]))
                  if(!min(scalex) %in% kmeanBreaks){
                    kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                  if(!max(scalex) %in% kmeanBreaks){
                    kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))}

                  if(!is.null(legendFixedBreaks)){
                    if(min(scalex) < min(legendFixedBreaks)){
                      legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                    if(max(scalex) > max(legendFixedBreaks)){
                      legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                  }

                  if((max(range(scalex))-min(range(scalex)))<1E-10 &
                     (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                       scaleRangex=range(scalex)
                     }

                  if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                  if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                    if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                      if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                        if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}
                }

                # Set Facets
                if(length(unique(datax$x))>1){
                  multiFacetColsx <- "x"
                  colm <- length(unique(datax$x))
                  if((length(unique(datax$scenario))>1) & (length(unique(datax$class))>1)){
                    multiFacetRowsx <- "x"
                    rowm <- length(unique(datax$x))
                    multiFacetColsx <- c("scenario","class")
                    colm <- length(unique(datax$scenario))*length(unique(datax$class))
                  }
                  if((length(unique(datax$scenario))>1) & (!length(unique(datax$class))>1)){
                    multiFacetRowsx <- c("scenario")
                    rowm <- length(unique(datax$scenario))
                  }
                  if((!length(unique(datax$scenario))>1) & (length(unique(datax$class))>1)){
                    multiFacetRowsx <- "x"
                    rowm <- length(unique(datax$x))
                    multiFacetColsx <- c("class")
                    colm <- length(unique(datax$class))
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
                      multiFacetRowsx <- "scenario"
                      rowm <- length(unique(datax$scenario))
                      multiFacetColsx <- c("class")
                      colm <- length(unique(datax$class))
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
                      if(forceFacets | length(scenDiff)==1){
                        multiFacetColsx <- "scenario"
                      } else {
                        multiFacetColsx <- NULL
                      }
                      multiFacetRowsx <- NULL
                      colm <- 1
                      rowm <- 1
                    }
                  }
                }


                # Add facet or Rows if selected
                if(!is.null(col)){
                  if(!is.null(multiFacetColsx)){
                    multiFacetColsx <- c(multiFacetColsx,col)
                    colm <- colm + length(col)
                  } else { multiFacetColsx <- col; colm <- length(col)}
                }

                if(!is.null(row)){
                  if(!is.null(multiFacetRowsx)){
                    multiFacetRowsx <- c(multiFacetRowsx,row)
                    rowm <- rowm + length(row)
                  } else { multiFacetRowsx <- row; rowm <- length(row)}
                }

                # Check for Duplicates
                if(duplicated(datax %>%
                              dplyr::select(subRegion,region,lat,lon,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                   any()){stop("Input data data has multiple values. Please check your data.")}

                # Set title
                if(is.null(title)){
                  if(param_i != "param"){
                    if(length(scenDiff)==1){
                      titlex <- paste(param_i," ",scenDiff," diffAbs ", scenRef,sep="")
                    }else{
                      titlex <- paste(param_i,sep="")
                    }
                  } else {
                    if(length(scenDiff)==1){
                      titlex <- paste(scenDiff," diffAbs ", scenRef,sep="")
                    }else{
                      titlex <- NULL
                    }
                  }
                } else if(title == F){
                  titlex <- NULL
                } else {
                  titlex <- title
                }

                # Assign variables based on legend type choice
                if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  fileNameTag <- "KMEANS"
                  legendBreaksx <- kmeanBreaks
                } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                  fileNameTag <- "CONTINUOUS"
                  legendBreaksx <- prettyBreaks
                } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                  fileNameTag <- "PRETTY"
                  legendBreaksx <- prettyBreaks
                } else if(!is.null(legendFixedBreaks)){
                  fileNameTag <- "FIXED"
                  legendBreaksx <- legendFixedBreaks
                }

                  rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                 overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                 overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                 underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                 underLayerAlpha = underLayerAlpha, background=background, zoom=zoom,
                                 zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                 crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                 alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol,
                                 showNA=showNA, colorNA=colorNA,
                                 labelColor=labelColor,
                                 labelSize=labelSize,
                                 labelAlpha=labelAlpha,
                                 labelFill=labelFill,
                                 labelBorderSize=labelBorderSize,
                                 theme = theme, legendTitle=legendTitle,
                                 legendDigitsOverride=legendDigitsOverride,
                                 numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                 underLayer=underLayer,
                                 data=datax,
                                 legendBreaksn=legendBreaksn,
                                 legendDigits = legendDigits,
                                 palette = palette,
                                 width=width*max(1,colm/1),
                                 height=height*max(1,rowm/1),
                                 pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                 labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                 legendBreaks = legendBreaksx,
                                 fillColumn = "value",
                                 col = multiFacetColsx,
                                 row = multiFacetRowsx,
                                 title= titlex,
                                 fileName = paste("map_",param_i,"_",fileNameTag,"_DiffAbs",nameAppend,sep=""),
                                 folder = sub("/$","",paste(folder,"/",param_if,sep = ""))) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_",param_i,"_",fileNameTag,"_DiffAbs",nameAppend,sep="");
                  return_i = return_i + 1
                  } # if(nrow(datax)>0){

              # Mean for all years provided
              datax<-dataTblx%>%dplyr::filter(param==param_i)

              if(length(unique(datax$x))>1){

                if(nrow(datax)>0){

                  if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                  palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                  meanCol = paste("Mean_",min(datax$x),"to",max(datax$x),sep="")


                  colsPresentGroup =  c("lon","lat","subRegion","region","scenario","class")
                  colsPresentGroup = colsPresentGroup[colsPresentGroup %in% names(datax)]

                  datax<-datax%>%
                    dplyr::select(lat,lon,subRegion,region,scenario,class,x,value)%>%
                    dplyr::group_by_at(dplyr::all_of(colsPresentGroup))%>%
                    dplyr::summarize(!!meanCol:=mean(value))%>%
                    dplyr::ungroup()

                  scalex<-datax[[meanCol]];scalex
                  scalex <- scalex[!is.infinite(scalex)]
                  scalex <- scalex[!is.nan(scalex)]
                  scalex <- scalex[!is.na(scalex)]

                  # Choose correct scaleRange
                  if(T){
                    scaleRange_i=scaleRangeDiffAbs

                    if(!is.null(scaleRange_i)){
                      if(any(param_i %in% unique(scaleRange_i$param))){
                        if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                          scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                            scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                           scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                          }
                        if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                          scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                            scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                            scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                          }
                      }
                    }
                    prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex)
                    kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                   centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]))
                    if(!min(scalex) %in% kmeanBreaks){
                      kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                    if(!max(scalex) %in% kmeanBreaks){
                      kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))}

                    if(!is.null(legendFixedBreaks)){
                      if(min(scalex) < min(legendFixedBreaks)){
                        legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                      if(max(scalex) > max(legendFixedBreaks)){
                        legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                    }

                    if((max(range(scalex))-min(range(scalex)))<1E-10 &
                       (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                         scaleRangex=range(scalex)
                       }

                    if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                    if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                      if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                        if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                          if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}
                  }

                  # Set Facets
                  if(length(unique(datax$scenario))>1){
                    multiFacetColsx <- "scenario"
                    colm <- length(unique(datax$scenario))
                    if((length(unique(datax$class))>1)){
                      multiFacetRowsx <- "scenario"
                      rowm <- length(unique(datax$scenario))
                      multiFacetColsx <- c("class")
                      colm <- length(unique(datax$class))
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
                      if(forceFacets){
                        multiFacetColsx <- "scenario"
                      } else {
                        multiFacetColsx <- NULL
                      }
                      multiFacetRowsx <- NULL
                      colm <- 1
                      rowm <- 1
                    }
                  }

                  # Add facet or Rows if selected
                  if(!is.null(col)){
                    if(!is.null(multiFacetColsx)){
                      multiFacetColsx <- c(multiFacetColsx,col)
                      colm <- colm + length(col)
                    } else { multiFacetColsx <- col; colm <- length(col)}
                  }

                  if(!is.null(row)){
                    if(!is.null(multiFacetRowsx)){
                      multiFacetRowsx <- c(multiFacetRowsx,row)
                      rowm <- rowm + length(row)
                    } else { multiFacetRowsx <- row; rowm <- length(row)}
                  }



                  # Check for Duplicates
                  if(duplicated(datax %>%
                                dplyr::select(lat,lon,subRegion,region,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                     any()){stop("Input data data has multiple values. Please check your data.")}


                  if(length(scenDiff)==1){
                    titlex <- paste(param_i," ",scenDiff," diffAbs ", scenRef,sep="")
                  }else{
                    titlex <- paste(param_i,sep="")
                  }

                  # Set title
                  if(is.null(title)){
                    if(param_i == "param"){
                      if(length(scenDiff)==1){
                        titlex <- paste(meanCol," ",scenDiff," diffAbs ", scenRef,sep="")
                      }else{
                        titlex <- paste(meanCol," diffAbs",sep="")
                      }
                    } else {
                      if(length(scenDiff)==1){
                        titlex <- paste(param_i," ",meanCol," ",scenDiff," diffAbs ", scenRef,sep="")
                      }else{
                        titlex <- paste(param_i," ",meanCol," diffAbs",sep="")
                      }
                    }
                  } else if(title == F){
                    titlex <- NULL
                  } else {
                    titlex <- title
                  }

                  # Assign variables based on legend type choice
                  if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                    fileNameTag <- "KMEANS"
                    legendBreaksx <- kmeanBreaks
                  } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                    fileNameTag <- "CONTINUOUS"
                    legendBreaksx <- prettyBreaks
                  } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                    fileNameTag <- "PRETTY"
                    legendBreaksx <- prettyBreaks
                  } else if(!is.null(legendFixedBreaks)){
                    fileNameTag <- "FIXED"
                    legendBreaksx <- legendFixedBreaks
                  }

                    rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                   overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                   overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                   underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                   underLayerAlpha = underLayerAlpha, background=background,
                                   zoom=zoom, zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                   crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                   alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)),
                                   ncol=ncol, showNA=showNA, colorNA=colorNA,
                                   labelColor=labelColor,
                                   labelSize=labelSize,
                                   labelAlpha=labelAlpha,
                                   labelFill=labelFill,
                                   labelBorderSize=labelBorderSize,
                                   theme = theme, legendTitle=legendTitle,
                                   legendDigitsOverride=legendDigitsOverride,
                                   numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                   underLayer=underLayer,
                                   data=datax,
                                   legendBreaksn=legendBreaksn,
                                   legendDigits = legendDigits,
                                   palette = palette,
                                   width=width*max(1,colm/1),
                                   height=height*max(1,rowm/1),
                                   pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                   labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                   legendBreaks = legendBreaksx,
                                   fillColumn = meanCol,
                                   col = multiFacetColsx,
                                   row = multiFacetRowsx,
                                   title = titlex,
                                   fileName = paste("map_",param_i,"_MEAN_",fileNameTag,"_DiffAbs",nameAppend,sep=""),
                                   folder = sub("/$","",paste(folder,"/",param_if,sep = ""))) ->
                      mapsReturn[[return_i]];
                    names(mapsReturn)[return_i] <- paste("map_",param_i,"_MEAN_",fileNameTag,"_DiffAbs",nameAppend,sep="");
                    return_i = return_i + 1


                } # if(nrow(datax)>0){
              }# If multiple years

            } # if nrow of dataTblx dplyr::filtered for Diff scenarios

          }# Close if nrow dataTbl < 0
        } # Close Diff Abs

        # DiffPrcnt Scenarios
        if(T){

          if(length(unique(dataTblOrig$param))==1){param_if=NULL}else{param_if=param_i}

          if(nrow(dataTblOrig%>%dplyr::filter(param==param_i))>0){

            dataTblx <- dataTblOrig%>%dplyr::filter(param==param_i,
                                                    scenario %in% dataTbl_scenariosOrig[grepl("_DiffPrcnt",dataTbl_scenariosOrig)])

            if(nrow(dataTblx)>0){

              #.................-
              # Create data Table Folders If Needed
              #.................-
              if(save){

                if(!dir.exists(paste(folder,"/",sep = ""))){
                  dir.create(paste(folder,"/",sep = ""))}

                if(!dir.exists(paste(folder,"/",param_if,sep = ""))){
                  dir.create(paste(folder,"/",param_if,sep = ""))}

                if(!dir.exists(paste(folder,"/",param_if,sep = ""))){
                  dir.create(paste(folder, "/",param_if,sep = ""))}

                if(length(unique(dataTblx$x))>1){
                  if(!dir.exists(paste(folder,"/",param_if,"/byYear",sep = ""))){
                    dir.create(paste(folder, "/",param_if,"/byYear",sep = ""))}
                }
              } # Create data table folder if needed

              #.................--
              # Save Map related Data Table
              #.................--

              if(save){
                if(nrow(dataTblx %>% dplyr::filter(param==param_i))>0){
                  data.table::fwrite(dataTblx %>% dplyr::filter(param==param_i)%>%
                                       dplyr::select(scenario,lat,lon,subRegion,region,param,class,x,value,units),
                                     paste(folder,"/",param_if,"/map_",param_i,nameAppend,".csv",sep = ""))
                  rlang::inform(paste("Map data table written to ",folder,"/",param_if,"/map_",param_i,nameAppend,".csv",sep = ""))
                }
              }

              #.............................
              # By Year
              #.............................

              # Set Legends
              if(T){
                scalex<-dataTblx$value
                scalex <- scalex[!is.infinite(scalex)]
                scalex <- scalex[!is.nan(scalex)]
                scalex <- scalex[!is.na(scalex)]

                # Choose correct scaleRange
                scaleRange_i=scaleRangeDiffPrcnt

                if(!is.null(scaleRange_i)){
                  if(any(param_i %in% unique(scaleRange_i$param))){
                    if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                      scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                        scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                       scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                      }
                    if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                      scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                        scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                        scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                      }
                  }
                }

                prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex); prettyBreaks
                kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                               centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]));kmeanBreaks
                if(!min(scalex) %in% kmeanBreaks){
                  kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                if(!max(scalex) %in% kmeanBreaks){
                  kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))};kmeanBreaks

                if(!is.null(legendFixedBreaks)){
                  if(min(scalex) < min(legendFixedBreaks)){
                    legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                  if(max(scalex) > max(legendFixedBreaks)){
                    legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                }

                if((max(range(scalex))-min(range(scalex)))<1E-10 &
                   (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                     scaleRangex=range(scalex)
                   }

                if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                  if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                    if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                      if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}

              }

              # By Year
              if(length(unique(dataTblx$x))>1){

                for (x_i in unique(dataTblx$x)){

                  datax<-dataTblx%>%dplyr::filter(x==x_i)

                  if(nrow(datax)>0){
                    if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                    palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                    # Set Facets
                    if(length(unique(datax$scenario))>1){
                      multiFacetColsx <- "scenario"
                      colm <- length(unique(datax$scenario))
                      if((length(unique(datax$class))>1)){
                        multiFacetRowsx <- "scenario"
                        rowm <- length(unique(datax$scenario))
                        multiFacetColsx <- c("class")
                        colm <- length(unique(datax$class))
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
                        if(forceFacets){
                          multiFacetColsx <- "scenario"
                        } else {
                          multiFacetColsx <- NULL
                        }
                        multiFacetRowsx <- NULL
                        colm = 1
                        rowm = 1
                      }
                    }

                    # Add facet or Rows if selected
                    if(!is.null(col)){
                      if(!is.null(multiFacetColsx)){
                        multiFacetColsx <- c(multiFacetColsx,col)
                        colm <- colm + length(col)
                      } else { multiFacetColsx <- col; colm <- length(col)}
                    }

                    if(!is.null(row)){
                      if(!is.null(multiFacetRowsx)){
                        multiFacetRowsx <- c(multiFacetRowsx,row)
                        rowm <- rowm + length(row)
                      } else { multiFacetRowsx <- row; rowm <- length(row)}
                    }


                    # Check for Duplicates
                    if(duplicated(datax %>%
                                  dplyr::select(lat,lon,subRegion,region,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                       any()){stop("Input data data has multiple values. Please check your data.")}

                    # Set title
                    if(is.null(title)){
                      if(param_i == "param"){
                        if(length(scenDiff)==1){
                          titlex <- paste(x_i," ",scenDiff," diffPrcnt ", scenRef,sep="")
                        }else{
                          titlex <- paste(x_i,sep="")
                        }
                      } else {
                        if(length(scenDiff)==1){
                          titlex <- paste(param_i," ",x_i," ",scenDiff," diffPrcnt ", scenRef,sep="")
                        }else{
                          titlex <- paste(param_i," ",x_i,sep="")
                        }
                      }
                    } else if(title == F){
                      titlex <- NULL
                    } else {
                      titlex <- title
                    }

                    # Assign variables based on legend type choice
                    if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                      fileNameTag <- "KMEANS"
                      legendBreaksx <- kmeanBreaks
                    } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                      fileNameTag <- "CONTINUOUS"
                      legendBreaksx <- prettyBreaks
                    } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                      fileNameTag <- "PRETTY"
                      legendBreaksx <- prettyBreaks
                    } else if(!is.null(legendFixedBreaks)){
                      fileNameTag <- "FIXED"
                      legendBreaksx <- legendFixedBreaks
                    }

                      rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                     overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                     overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                     underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                     underLayerAlpha = underLayerAlpha, background=background,
                                     zoom=zoom, zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                     crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                     alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)),
                                     ncol=ncol, showNA=showNA, colorNA=colorNA,
                                     theme = theme, legendTitle=legendTitle,
                                     legendDigitsOverride=legendDigitsOverride,
                                     numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                     underLayer=underLayer,
                                     data=datax,
                                     labelColor=labelColor,
                                     labelSize=labelSize,
                                     labelAlpha=labelAlpha,
                                     labelFill=labelFill,
                                     labelBorderSize=labelBorderSize,
                                     legendBreaksn=legendBreaksn,
                                     legendDigits = legendDigits,
                                     palette = palette,
                                     width=width*max(1,colm/1),
                                     height=height*max(1,rowm/1),
                                     pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                     labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                     legendBreaks = legendBreaksx,
                                     fillColumn = "value",
                                     col = multiFacetColsx,
                                     row = multiFacetRowsx,
                                     title=titlex ,
                                     fileName = paste("map_",param_i,"_",x_i,"_",fileNameTag,"_DiffPrcnt",nameAppend,sep=""),
                                     folder = paste(folder,"/",param_if,"/byYear",sep = "")) ->
                        mapsReturn[[return_i]];
                      names(mapsReturn)[return_i] <- paste("map_",param_i,"_",x_i,"_",fileNameTag,"_DiffPrcnt",nameAppend,sep="");
                      return_i = return_i + 1
                      }
                } # Close years x_i loop

                # Animations
                if(animate==T){

                    animName<-paste("anim_",param_i,"_",fileNameTag,"_DiffPrcnt",nameAppend,".gif",sep="")
                    animFiles <- list.files(path = paste(folder,"/",param_if,"/byYear",sep=""),
                                            pattern = paste(".*",param_i,".*",fileNameTag,"_DiffPrcnt",nameAppend, ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
                    animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                    magick::image_write(animation,paste(folder,"/",param_if,"/",
                                                        animName,sep = ""))
                    rlang::inform(gsub("//","/",paste("animation saved in :",folder,"/",param_if,"/",
                                              animName,sep = "")))


                }
              }

              # Multi-Year-Single Chart
              datax<-dataTblx%>%dplyr::filter(param==param_i)
              if(nrow(datax)>0){

                if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                scalex<-datax$value
                scalex <- scalex[!is.infinite(scalex)]
                scalex <- scalex[!is.nan(scalex)]
                scalex <- scalex[!is.na(scalex)]

                # Choose correct scaleRange
                if(T){
                  scaleRange_i=scaleRangeDiffPrcnt

                  if(!is.null(scaleRange_i)){
                    if(any(param_i %in% unique(scaleRange_i$param))){
                      if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                        scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                          scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                         scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                        }
                      if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                        scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                          scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                          scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                        }
                    }
                  }
                  prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex)
                  kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                 centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]))
                  if(!min(scalex) %in% kmeanBreaks){
                    kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                  if(!max(scalex) %in% kmeanBreaks){
                    kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))}

                  if(!is.null(legendFixedBreaks)){
                    if(min(scalex) < min(legendFixedBreaks)){
                      legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                    if(max(scalex) > max(legendFixedBreaks)){
                      legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                  }

                  if((max(range(scalex))-min(range(scalex)))<1E-10 &
                     (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                       scaleRangex=range(scalex)
                     }

                  if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                  if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                    if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                      if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                        if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}
                }

                # Set Facets
                if(length(unique(datax$x))>1){
                  multiFacetColsx <- "x"
                  colm <- length(unique(datax$x))
                  if((length(unique(datax$scenario))>1) & (length(unique(datax$class))>1)){
                    multiFacetRowsx <- "x"
                    rowm <- length(unique(datax$x))
                    multiFacetColsx <- c("scenario","class")
                    colm <- length(unique(datax$scenario))*length(unique(datax$class))
                  }
                  if((length(unique(datax$scenario))>1) & (!length(unique(datax$class))>1)){
                    multiFacetRowsx <- c("scenario")
                    rowm <- length(unique(datax$scenario))
                  }
                  if((!length(unique(datax$scenario))>1) & (length(unique(datax$class))>1)){
                    multiFacetRowsx <- "x"
                    rowm <- length(unique(datax$x))
                    multiFacetColsx <- c("class")
                    colm <- length(unique(datax$class))
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
                      multiFacetRowsx <- "scenario"
                      rowm <- length(unique(datax$scenario))
                      multiFacetColsx <- c("class")
                      colm <- length(unique(datax$class))
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
                      if(forceFacets | length(scenDiff)==1){
                        multiFacetColsx <- "scenario"
                      } else {
                        multiFacetColsx <- NULL
                      }
                      multiFacetRowsx <- NULL
                      colm <- 1
                      rowm <- 1
                    }
                  }
                }


                # Add facet or Rows if selected
                if(!is.null(col)){
                  if(!is.null(multiFacetColsx)){
                    multiFacetColsx <- c(multiFacetColsx,col)
                    colm <- colm + length(col)
                  } else { multiFacetColsx <- col; colm <- length(col)}
                }

                if(!is.null(row)){
                  if(!is.null(multiFacetRowsx)){
                    multiFacetRowsx <- c(multiFacetRowsx,row)
                    rowm <- rowm + length(row)
                  } else { multiFacetRowsx <- row; rowm <- length(row)}
                }

                # Check for Duplicates
                if(duplicated(datax %>%
                              dplyr::select(subRegion,region,lat,lon,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                   any()){stop("Input data data has multiple values. Please check your data.")}

                # Set title
                if(is.null(title)){
                  if(param_i != "param"){
                    if(length(scenDiff)==1){
                      titlex <- paste(param_i," ",scenDiff," diffPrcnt ", scenRef,sep="")
                    }else{
                      titlex <- paste(param_i,sep="")
                    }
                  } else {
                    if(length(scenDiff)==1){
                      titlex <- paste(scenDiff," diffPrcnt ", scenRef,sep="")
                    }else{
                      titlex <- NULL
                    }
                  }
                } else if(title == F){
                  titlex <- NULL
                } else {
                  titlex <- title
                }

                # Assign variables based on legend type choice
                if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                  fileNameTag <- "KMEANS"
                  legendBreaksx <- kmeanBreaks
                } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                  fileNameTag <- "CONTINUOUS"
                  legendBreaksx <- prettyBreaks
                } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                  fileNameTag <- "PRETTY"
                  legendBreaksx <- prettyBreaks
                } else if(!is.null(legendFixedBreaks)){
                  fileNameTag <- "FIXED"
                  legendBreaksx <- legendFixedBreaks
                }

                  rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                 overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                 overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                 underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                 underLayerAlpha = underLayerAlpha, background=background, zoom=zoom,
                                 zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                 crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                 alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol,
                                 showNA=showNA, colorNA=colorNA,
                                 labelColor=labelColor,
                                 labelSize=labelSize,
                                 labelAlpha=labelAlpha,
                                 labelFill=labelFill,
                                 labelBorderSize=labelBorderSize,
                                 theme = theme, legendTitle=legendTitle,
                                 legendDigitsOverride=legendDigitsOverride,
                                 numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                 underLayer=underLayer,
                                 data=datax,
                                 legendBreaksn=legendBreaksn,
                                 legendDigits = legendDigits,
                                 palette = palette,
                                 width=width*max(1,colm/1),
                                 height=height*max(1,rowm/1),
                                 pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                 labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                 legendBreaks = legendBreaksx,
                                 fillColumn = "value",
                                 col = multiFacetColsx,
                                 row = multiFacetRowsx,
                                 title= titlex,
                                 fileName = paste("map_",param_i,"_",fileNameTag,"_DiffPrcnt",nameAppend,sep=""),
                                 folder = sub("/$","",paste(folder,"/",param_if,sep = ""))) ->
                    mapsReturn[[return_i]];
                  names(mapsReturn)[return_i] <- paste("map_",param_i,"_",fileNameTag,"_DiffPrcnt",nameAppend,sep="");
                  return_i = return_i + 1


                  # save=save
                  # overLayer=overLayer
                  # overLayerColor=overLayerColor
                  # overLayerFill = overLayerFill
                  # overLayerLwd = overLayerLwd
                  # overLayerAlpha = overLayerAlpha
                  # underLayerColor = underLayerColor
                  # underLayerFill = underLayerFill
                  # underLayerLwd = underLayerLwd
                  # underLayerAlpha = underLayerAlpha
                  # background=background
                  # zoom=zoom
                  # zoomx = zoomx
                  # zoomy=zoomy
                  # alpha = alpha
                  # size=max(1,(size+(colm+rowm)*3 - 12))
                  # ncol=ncol
                  # showNA=showNA
                  # colorNA=colorNA
                  # theme = theme
                  # legendTitle=legendTitle
                  # legendDigitsOverride=legendDigitsOverride
                  # numeric2Cat_list=numeric2Cat_list
                  # underLayer=underLayer
                  # data=datax
                  # legendBreaksn=legendBreaksn
                  # legendDigits = legendDigits
                  # palette = palette
                  # width=width*max(1,colm/1)
                  # height=height*max(1,rowm/1)
                  # pdfpng = pdfpng
                  # legendSingleColor = legendSingleColor
                  # legendSingleValue =  legendSingleValue
                  # labels=labels
                  # legendBreaks = kmeanBreaks
                  # fillColumn = "value"
                  # col = multiFacetColsx
                  # row = multiFacetRowsx
                  # title=paste(param_i,sep="")
                  # fileName = paste("map_",param_i,"_",fileNameTag,nameAppend,sep="")
                  # folder = sub("/$","",paste(folder,"/",param_if,sep = ""))



              } # if(nrow(datax)>0){

              # Mean for all years provided
              datax<-dataTblx%>%dplyr::filter(param==param_i)

              if(length(unique(datax$x))>1){

                if(nrow(datax)>0){

                  if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                  palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                  meanCol = paste("Mean_",min(datax$x),"to",max(datax$x),sep="")


                  colsPresentGroup =  c("lon","lat","subRegion","region","scenario","class")
                  colsPresentGroup = colsPresentGroup[colsPresentGroup %in% names(datax)]

                  datax<-datax%>%
                    dplyr::select(lat,lon,subRegion,region,scenario,class,x,value)%>%
                    dplyr::group_by_at(dplyr::all_of(colsPresentGroup))%>%
                    dplyr::summarize(!!meanCol:=mean(value))%>%
                    dplyr::ungroup()

                  scalex<-datax[[meanCol]];scalex
                  scalex <- scalex[!is.infinite(scalex)]
                  scalex <- scalex[!is.nan(scalex)]
                  scalex <- scalex[!is.na(scalex)]

                  # Choose correct scaleRange
                  if(T){
                    scaleRange_i=scaleRangeDiffPrcnt

                    if(!is.null(scaleRange_i)){
                      if(any(param_i %in% unique(scaleRange_i$param))){
                        if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                          scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                            scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                           scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                          }
                        if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                          scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                            scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                            scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                          }
                      }
                    }
                    prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex)
                    kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                   centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]))
                    if(!min(scalex) %in% kmeanBreaks){
                      kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                    if(!max(scalex) %in% kmeanBreaks){
                      kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))}

                    if(!is.null(legendFixedBreaks)){
                      if(min(scalex) < min(legendFixedBreaks)){
                        legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                      if(max(scalex) > max(legendFixedBreaks)){
                        legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                    }

                    if((max(range(scalex))-min(range(scalex)))<1E-10 &
                       (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                         scaleRangex=range(scalex)
                       }

                    if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                    if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                      if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                        if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                          if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}
                  }

                  # Set Facets
                  if(length(unique(datax$scenario))>1){
                    multiFacetColsx <- "scenario"
                    colm <- length(unique(datax$scenario))
                    if((length(unique(datax$class))>1)){
                      multiFacetRowsx <- "scenario"
                      rowm <- length(unique(datax$scenario))
                      multiFacetColsx <- c("class")
                      colm <- length(unique(datax$class))
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
                      if(forceFacets){
                        multiFacetColsx <- "scenario"
                      } else {
                        multiFacetColsx <- NULL
                      }
                      multiFacetRowsx <- NULL
                      colm <- 1
                      rowm <- 1
                    }
                  }

                  # Add facet or Rows if selected
                  if(!is.null(col)){
                    if(!is.null(multiFacetColsx)){
                      multiFacetColsx <- c(multiFacetColsx,col)
                      colm <- colm + length(col)
                    } else { multiFacetColsx <- col; colm <- length(col)}
                  }

                  if(!is.null(row)){
                    if(!is.null(multiFacetRowsx)){
                      multiFacetRowsx <- c(multiFacetRowsx,row)
                      rowm <- rowm + length(row)
                    } else { multiFacetRowsx <- row; rowm <- length(row)}
                  }



                  # Check for Duplicates
                  if(duplicated(datax %>%
                                dplyr::select(lat,lon,subRegion,region,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                     any()){stop("Input data data has multiple values. Please check your data.")}

                  # Set title
                  if(is.null(title)){
                    if(param_i == "param"){
                      if(length(scenDiff)==1){
                        titlex <- paste(meanCol," diffPrcnt"," ",scenDiff," diffPrcnt ", scenRef,sep="")
                      }else{
                        titlex <- paste(meanCol," diffPrcnt",sep="")
                      }
                    } else {
                      if(length(scenDiff)==1){
                        titlex <- paste(param_i," ",meanCol," diffPrcnt"," ",scenDiff," diffPrcnt ", scenRef,sep="")
                      }else{
                        titlex <- paste(param_i," ",meanCol," diffPrcnt",sep="")
                      }
                    }
                  } else if(title == F){
                    titlex <- NULL
                  } else {
                    titlex <- title
                  }

                  # Assign variables based on legend type choice
                  if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                    fileNameTag <- "KMEANS"
                    legendBreaksx <- kmeanBreaks
                  } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                    fileNameTag <- "CONTINUOUS"
                    legendBreaksx <- prettyBreaks
                  } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                    fileNameTag <- "PRETTY"
                    legendBreaksx <- prettyBreaks
                  } else if(!is.null(legendFixedBreaks)){
                    fileNameTag <- "FIXED"
                    legendBreaksx <- legendFixedBreaks
                  }

                    rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                   overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                   overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                   underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                   underLayerAlpha = underLayerAlpha, background=background,
                                   zoom=zoom, zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                   crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                   alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)),
                                   ncol=ncol, showNA=showNA, colorNA=colorNA,
                                   labelColor=labelColor,
                                   labelSize=labelSize,
                                   labelAlpha=labelAlpha,
                                   labelFill=labelFill,
                                   labelBorderSize=labelBorderSize,
                                   theme = theme, legendTitle=legendTitle,
                                   legendDigitsOverride=legendDigitsOverride,
                                   numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                   underLayer=underLayer,
                                   data=datax,
                                   legendBreaksn=legendBreaksn,
                                   legendDigits = legendDigits,
                                   palette = palette,
                                   width=width*max(1,colm/1),
                                   height=height*max(1,rowm/1),
                                   pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                   labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                   legendBreaks = legendBreaksx,
                                   fillColumn = meanCol,
                                   col = multiFacetColsx,
                                   row = multiFacetRowsx,
                                   title = titlex,
                                   fileName = paste("map_",param_i,"_MEAN_",fileNameTag,"_DiffPrcnt",nameAppend,sep=""),
                                   folder = sub("/$","",paste(folder,"/",param_if,sep = ""))) ->
                      mapsReturn[[return_i]];
                    names(mapsReturn)[return_i] <- paste("map_",param_i,"_MEAN_",fileNameTag,"_DiffPrcnt",nameAppend,sep="");
                    return_i = return_i + 1


                } # if(nrow(datax)>0){
              }# If multiple years

            } # if nrow of dataTblx dplyr::filtered for Diff scenarios

          }# Close if nrow dataTbl < 0
        } # Close Diff Prcnt Scenario

        } # Close if scenRef

        # if xRef chosen
        if(!is.null(xRef)){

          # xDiff Abs Scenarios
          if(T){

            if(length(unique(dataTblOrig$param))==1){param_if=NULL}else{param_if=param_i}

            if(nrow(dataTblOrig%>%dplyr::filter(param==param_i))>0){

              dataTblx <- dataTblOrig%>%dplyr::filter(param==param_i,
                                                      scenario %in% dataTbl_scenariosOrig[grepl("_xDiffAbs",dataTbl_scenariosOrig)])

              if(nrow(dataTblx)>0){

                #.................-
                # Create data Table Folders If Needed
                #.................-
                if(save){

                  if(!dir.exists(paste(folder,"/",sep = ""))){
                    dir.create(paste(folder,"/",sep = ""))}

                  if(!dir.exists(paste(folder,"/",param_if,sep = ""))){
                    dir.create(paste(folder,"/",param_if,sep = ""))}

                  if(!dir.exists(paste(folder,"/",param_if,sep = ""))){
                    dir.create(paste(folder, "/",param_if,sep = ""))}

                  if(length(unique(dataTblx$x))>1){
                    if(!dir.exists(paste(folder,"/",param_if,"/byYear",sep = ""))){
                      dir.create(paste(folder, "/",param_if,"/byYear",sep = ""))}
                  }
                } # Create data table folder if needed

                #.................--
                # Save Map related Data Table
                #.................--

                if(save){
                  if(nrow(dataTblx %>% dplyr::filter(param==param_i))>0){
                    data.table::fwrite(dataTblx %>% dplyr::filter(param==param_i)%>%
                                         dplyr::select(scenario,region,lat,lon,subRegion,param,class,x,value,units),
                                       paste(folder,"/",param_if,"/map_",param_i,nameAppend,".csv",sep = ""))
                    rlang::inform(paste("Map data table written to ",folder,"/",param_if,"/map_",param_i,nameAppend,".csv",sep = ""))
                  }
                }

                #.............................
                # By Year
                #.............................

                # Set Legends
                if(T){
                  scalex<-dataTblx$value
                  scalex <- scalex[!is.infinite(scalex)]
                  scalex <- scalex[!is.nan(scalex)]
                  scalex <- scalex[!is.na(scalex)]

                  # Choose correct scaleRange
                  scaleRange_i=scaleRangexDiffAbs

                  if(!is.null(scaleRange_i)){
                    if(any(param_i %in% unique(scaleRange_i$param))){
                      if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                        scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                          scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                         scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                        }
                      if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                        scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                          scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                          scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                        }
                    }
                  }

                  prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex); prettyBreaks
                  kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                 centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]));kmeanBreaks
                  if(!min(scalex) %in% kmeanBreaks){
                    kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                  if(!max(scalex) %in% kmeanBreaks){
                    kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))};kmeanBreaks

                  if(!is.null(legendFixedBreaks)){
                    if(min(scalex) < min(legendFixedBreaks)){
                      legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                    if(max(scalex) > max(legendFixedBreaks)){
                      legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                  }


                  if((max(range(scalex))-min(range(scalex)))<1E-10 &
                     (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                       scaleRangex=range(scalex)
                     }

                  if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                  if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                    if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                      if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                        if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}

                }

                # By Year
                if(length(unique(dataTblx$x))>1){

                  for (x_i in unique(dataTblx$x)){

                    datax<-dataTblx%>%dplyr::filter(x==x_i)

                    if(nrow(datax)>0){
                      if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                      palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                      # Set Facets
                      if(length(unique(datax$scenario))>1){
                        multiFacetColsx <- "scenario"
                        colm <- length(unique(datax$scenario))
                        if((length(unique(datax$class))>1)){
                          multiFacetRowsx <- "scenario"
                          rowm <- length(unique(datax$scenario))
                          multiFacetColsx <- c("class")
                          colm <- length(unique(datax$class))
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
                          if(forceFacets){
                            multiFacetColsx <- "scenario"
                          } else {
                            multiFacetColsx <- NULL
                          }
                          multiFacetRowsx <- NULL
                          colm = 1
                          rowm = 1
                        }
                      }

                      # Add Multi facet Cols or Rows if selected
                      if(!is.null(col)){
                        if(!is.null(multiFacetColsx)){
                          multiFacetColsx <- c(multiFacetColsx,col)
                          colm <- colm + length(col)
                        } else { multiFacetColsx <- col; colm <- length(col)}
                      }

                      if(!is.null(row)){
                        if(!is.null(multiFacetRowsx)){
                          multiFacetRowsx <- c(multiFacetRowsx,row)
                          rowm <- rowm + length(row)
                        } else { multiFacetRowsx <- row; rowm <- length(row)}
                      }

                      # Check for Duplicates
                      if(duplicated(datax %>%
                                    dplyr::select(lat,lon,subRegion,region,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                         any()){stop("Input data data has multiple values. Please check your data.")}

                      # Set title
                      if(is.null(title)){
                        if(param_i == "param"){
                          titlex <- paste(x_i," xDiffAbs ",xRef,sep="")
                        } else {
                          titlex <- paste(param_i," - ",x_i," xDiffAbs ",xRef,sep="")
                        }
                      } else if(title == F){
                        titlex <- NULL
                      } else {
                        titlex <- title
                      }


                      # Assign variables based on legend type choice
                      if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                        fileNameTag <- "KMEANS"
                        legendBreaksx <- kmeanBreaks
                      } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                        fileNameTag <- "CONTINUOUS"
                        legendBreaksx <- prettyBreaks
                      } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                        fileNameTag <- "PRETTY"
                        legendBreaksx <- prettyBreaks
                      } else if(!is.null(legendFixedBreaks)){
                        fileNameTag <- "FIXED"
                        legendBreaksx <- legendFixedBreaks
                      }

                      # gsub scenario name for figures
                      datax <- datax %>%
                        dplyr::mutate(scenario= gsub("_xDiff.*","",scenario))

                      rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                     overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                     overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                     underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                     underLayerAlpha = underLayerAlpha, background=background,
                                     zoom=zoom, zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                     crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                     alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)),
                                     ncol=ncol, showNA=showNA, colorNA=colorNA,
                                     theme = theme, legendTitle=legendTitle,
                                     legendDigitsOverride=legendDigitsOverride,
                                     numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                     underLayer=underLayer,
                                     data=datax,
                                     labelColor=labelColor,
                                     labelSize=labelSize,
                                     labelAlpha=labelAlpha,
                                     labelFill=labelFill,
                                     labelBorderSize=labelBorderSize,
                                     legendBreaksn=legendBreaksn,
                                     legendDigits = legendDigits,
                                     palette = palette,
                                     width=width*max(1,colm/1),
                                     height=height*max(1,rowm/1),
                                     pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                     labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                     legendBreaks = legendBreaksx,
                                     fillColumn = "value",
                                     col = multiFacetColsx,
                                     row = multiFacetRowsx,
                                     title=titlex ,
                                     fileName = paste("map_",param_i,"_",x_i,"_",fileNameTag,"_xDiffAbs",nameAppend,sep=""),
                                     folder = paste(folder,"/",param_if,"/byYear",sep = "")) ->
                        mapsReturn[[return_i]];
                      names(mapsReturn)[return_i] <- paste("map_",param_i,"_",x_i,"_",fileNameTag,"_xDiffAbs",nameAppend,sep="");
                      return_i = return_i + 1
                    }
                  } # Close years x_i loop

                  # Animations
                  if(animate==T){

                      animName<-paste("anim_",param_i,"_",fileNameTag,"_xDiffAbs",nameAppend,".gif",sep="")
                      animFiles <- list.files(path = paste(folder,"/",param_if,"/byYear",sep=""),
                                              pattern = paste(".*",param_i,".*",fileNameTag,"_xDiffAbs",nameAppend, ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
                      animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                      magick::image_write(animation,paste(folder,"/",param_if,"/",
                                                          animName,sep = ""))
                      rlang::inform(gsub("//","/",paste("animation saved in :",folder,"/",param_if,"/",
                                                animName,sep = "")))

                  }
                }

                # Multi-Year-Single Chart
                datax<-dataTblx%>%dplyr::filter(param==param_i)
                if(nrow(datax)>0){

                  if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                  palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                  scalex<-datax$value
                  scalex <- scalex[!is.infinite(scalex)]
                  scalex <- scalex[!is.nan(scalex)]
                  scalex <- scalex[!is.na(scalex)]

                  # Choose correct scaleRange
                  if(T){
                    scaleRange_i=scaleRangexDiffAbs

                    if(!is.null(scaleRange_i)){
                      if(any(param_i %in% unique(scaleRange_i$param))){
                        if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                          scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                            scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                           scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                          }
                        if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                          scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                            scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                            scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                          }
                      }
                    }
                    prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex)
                    kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                   centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]))
                    if(!min(scalex) %in% kmeanBreaks){
                      kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                    if(!max(scalex) %in% kmeanBreaks){
                      kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))}

                    if(!is.null(legendFixedBreaks)){
                      if(min(scalex) < min(legendFixedBreaks)){
                        legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                      if(max(scalex) > max(legendFixedBreaks)){
                        legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                    }

                    if((max(range(scalex))-min(range(scalex)))<1E-10 &
                       (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                         scaleRangex=range(scalex)
                       }

                    if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                    if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                      if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                        if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                          if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}
                  }

                  # Set Facets
                  if(length(unique(datax$x))>1){
                    multiFacetColsx <- "x"
                    colm <- length(unique(datax$x))
                    if((length(unique(datax$scenario))>1) & (length(unique(datax$class))>1)){
                      multiFacetRowsx <- "x"
                      rowm <- length(unique(datax$x))
                      multiFacetColsx <- c("scenario","class")
                      colm <- length(unique(datax$scenario))*length(unique(datax$class))
                    }
                    if((length(unique(datax$scenario))>1) & (!length(unique(datax$class))>1)){
                      multiFacetRowsx <- c("scenario")
                      rowm <- length(unique(datax$scenario))
                    }
                    if((!length(unique(datax$scenario))>1) & (length(unique(datax$class))>1)){
                      multiFacetRowsx <- "x"
                      rowm <- length(unique(datax$x))
                      multiFacetColsx <- c("class")
                      colm <- length(unique(datax$class))
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
                        multiFacetRowsx <- "scenario"
                        rowm <- length(unique(datax$scenario))
                        multiFacetColsx <- c("class")
                        colm <- length(unique(datax$class))
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
                        if(forceFacets){
                          multiFacetColsx <- "scenario"
                        } else {
                          multiFacetColsx <- NULL
                        }
                        multiFacetRowsx <- NULL
                        colm <- 1
                        rowm <- 1
                      }
                    }
                  }


                  # Add facet or Rows if selected
                  if(!is.null(col)){
                    if(!is.null(multiFacetColsx)){
                      multiFacetColsx <- c(multiFacetColsx,col)
                      colm <- colm + length(col)
                    } else { multiFacetColsx <- col; colm <- length(col)}
                  }

                  if(!is.null(row)){
                    if(!is.null(multiFacetRowsx)){
                      multiFacetRowsx <- c(multiFacetRowsx,row)
                      rowm <- rowm + length(row)
                    } else { multiFacetRowsx <- row; rowm <- length(row)}
                  }

                  # Check for Duplicates
                  if(duplicated(datax %>%
                                dplyr::select(subRegion,region,lat,lon,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                     any()){stop("Input data data has multiple values. Please check your data.")}

                  # Set title
                  if(is.null(title)){
                    if(param_i == "param"){
                      if(length(xDiff)==1){
                        titlex <- paste(xDiff," xDiffAbs ",xRef,sep="")
                      }else{
                      titlex <- paste(xRef," xDiffAbs",sep="")
                      }
                    } else {
                      if(length(xDiff)==1){
                        titlex <- paste(param_i," - ",xDiff," xDiffAbs ",xRef,sep="")
                      }else{
                      titlex <- paste(param_i," - ",xRef," xDiffAbs",sep="")
                      }
                    }
                  } else if(title == F){
                    titlex <- NULL
                  } else {
                    titlex <- title
                  }

                  # Assign variables based on legend type choice
                  if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                    fileNameTag <- "KMEANS"
                    legendBreaksx <- kmeanBreaks
                  } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                    fileNameTag <- "CONTINUOUS"
                    legendBreaksx <- prettyBreaks
                  } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                    fileNameTag <- "PRETTY"
                    legendBreaksx <- prettyBreaks
                  } else if(!is.null(legendFixedBreaks)){
                    fileNameTag <- "FIXED"
                    legendBreaksx <- legendFixedBreaks
                  }

                  # gsub scenario name for figures
                  datax <- datax %>%
                    dplyr::mutate(scenario= gsub("_xDiff.*","",scenario))

                    rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                   overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                   overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                   underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                   underLayerAlpha = underLayerAlpha, background=background, zoom=zoom,
                                   zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                   crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                   alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol,
                                   showNA=showNA, colorNA=colorNA,
                                   labelColor=labelColor,
                                   labelSize=labelSize,
                                   labelAlpha=labelAlpha,
                                   labelFill=labelFill,
                                   labelBorderSize=labelBorderSize,
                                   theme = theme, legendTitle=legendTitle,
                                   legendDigitsOverride=legendDigitsOverride,
                                   numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                   underLayer=underLayer,
                                   data=datax,
                                   legendBreaksn=legendBreaksn,
                                   legendDigits = legendDigits,
                                   palette = palette,
                                   width=width*max(1,colm/1),
                                   height=height*max(1,rowm/1),
                                   pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                   labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                   legendBreaks = legendBreaksx,
                                   fillColumn = "value",
                                   col = multiFacetColsx,
                                   row = multiFacetRowsx,
                                   title= titlex,
                                   fileName = paste("map_",param_i,"_",fileNameTag,"_xDiffAbs",nameAppend,sep=""),
                                   folder = sub("/$","",paste(folder,"/",param_if,sep = ""))) ->
                      mapsReturn[[return_i]];
                    names(mapsReturn)[return_i] <- paste("map_",param_i,"_",fileNameTag,"_xDiffAbs",nameAppend,sep="");
                    return_i = return_i + 1


                    # save=save
                    # overLayer=overLayer
                    # overLayerColor=overLayerColor
                    # overLayerFill = overLayerFill
                    # overLayerLwd = overLayerLwd
                    # overLayerAlpha = overLayerAlpha
                    # underLayerColor = underLayerColor
                    # underLayerFill = underLayerFill
                    # underLayerLwd = underLayerLwd
                    # underLayerAlpha = underLayerAlpha
                    # background=background
                    # zoom=zoom
                    # zoomx = zoomx
                    # zoomy=zoomy
                    # alpha = alpha
                    # size=max(1,(size+(colm+rowm)*3 - 12))
                    # ncol=ncol
                    # showNA=showNA
                    # colorNA=colorNA
                    # theme = theme
                    # legendTitle=legendTitle
                    # legendDigitsOverride=legendDigitsOverride
                    # numeric2Cat_list=numeric2Cat_list
                    # underLayer=underLayer
                    # data=datax
                    # legendBreaksn=legendBreaksn
                    # legendDigits = legendDigits
                    # palette = palette
                    # width=width*max(1,colm/1)
                    # height=height*max(1,rowm/1)
                    # pdfpng = pdfpng
                    # legendSingleColor = legendSingleColor
                    # legendSingleValue =  legendSingleValue
                    # labels=labels
                    # legendBreaks = kmeanBreaks
                    # fillColumn = "value"
                    # col = multiFacetColsx
                    # row = multiFacetRowsx
                    # title=paste(param_i,sep="")
                    # fileName = paste("map_",param_i,"_",fileNameTag,nameAppend,sep="")
                    # folder = sub("/$","",paste(folder,"/",param_if,sep = ""))


                } # if(nrow(datax)>0){

                # Mean for all years provided
                datax<-dataTblx%>%dplyr::filter(param==param_i)

                if(length(unique(datax$x))>1){

                  if(nrow(datax)>0){

                    if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                    palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                    meanCol = paste("Mean_",min(datax$x),"to",max(datax$x),sep="")


                    colsPresentGroup =  c("lon","lat","subRegion","region","scenario","class")
                    colsPresentGroup = colsPresentGroup[colsPresentGroup %in% names(datax)]

                    datax<-datax%>%
                      dplyr::select(lat,lon,subRegion,region,scenario,class,x,value)%>%
                      dplyr::group_by_at(dplyr::all_of(colsPresentGroup))%>%
                      dplyr::summarize(!!meanCol:=mean(value))%>%
                      dplyr::ungroup()

                    scalex<-datax[[meanCol]];scalex
                    scalex <- scalex[!is.infinite(scalex)]
                    scalex <- scalex[!is.nan(scalex)]
                    scalex <- scalex[!is.na(scalex)]

                    # Choose correct scaleRange
                    if(T){
                      scaleRange_i=scaleRangexDiffAbs

                      if(!is.null(scaleRange_i)){
                        if(any(param_i %in% unique(scaleRange_i$param))){
                          if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                            scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                              scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                             scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                            }
                          if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                            scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                              scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                              scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                            }
                        }
                      }
                      prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex)
                      kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                     centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]))
                      if(!min(scalex) %in% kmeanBreaks){
                        kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                      if(!max(scalex) %in% kmeanBreaks){
                        kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))}

                      if(!is.null(legendFixedBreaks)){
                        if(min(scalex) < min(legendFixedBreaks)){
                          legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                        if(max(scalex) > max(legendFixedBreaks)){
                          legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                      }

                      if((max(range(scalex))-min(range(scalex)))<1E-10 &
                         (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                           scaleRangex=range(scalex)
                         }

                      if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                      if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                        if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                          if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                            if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}
                    }

                    # Set Facets
                    if(length(unique(datax$scenario))>1){
                      multiFacetColsx <- "scenario"
                      colm <- length(unique(datax$scenario))
                      if((length(unique(datax$class))>1)){
                        multiFacetRowsx <- "scenario"
                        rowm <- length(unique(datax$scenario))
                        multiFacetColsx <- c("class")
                        colm <- length(unique(datax$class))
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
                        if(forceFacets){
                          multiFacetColsx <- "scenario"
                        } else {
                          multiFacetColsx <- NULL
                        }
                        multiFacetRowsx <- NULL
                        colm <- 1
                        rowm <- 1
                      }
                    }

                    # Add facet or Rows if selected
                    if(!is.null(col)){
                      if(!is.null(multiFacetColsx)){
                        multiFacetColsx <- c(multiFacetColsx,col)
                        colm <- colm + length(col)
                      } else { multiFacetColsx <- col; colm <- length(col)}
                    }

                    if(!is.null(row)){
                      if(!is.null(multiFacetRowsx)){
                        multiFacetRowsx <- c(multiFacetRowsx,row)
                        rowm <- rowm + length(row)
                      } else { multiFacetRowsx <- row; rowm <- length(row)}
                    }



                    # Check for Duplicates
                    if(duplicated(datax %>%
                                  dplyr::select(lat,lon,subRegion,region,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                       any()){stop("Input data data has multiple values. Please check your data.")}

                    # Set title
                    if(is.null(title)){
                      if(param_i == "param"){
                        titlex <- paste(xRef," xDiffAbs ", meanCol,sep="")
                      } else {
                        titlex <- paste(param_i," - ",xRef," xDiffAbs ", meanCol,sep="")
                      }
                    } else if(title == F){
                      titlex <- NULL
                    } else {
                      titlex <- title
                    }

                    # Assign variables based on legend type choice
                    if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                      fileNameTag <- "KMEANS"
                      legendBreaksx <- kmeanBreaks
                    } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                      fileNameTag <- "CONTINUOUS"
                      legendBreaksx <- prettyBreaks
                    } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                      fileNameTag <- "PRETTY"
                      legendBreaksx <- prettyBreaks
                    } else if(!is.null(legendFixedBreaks)){
                      fileNameTag <- "FIXED"
                      legendBreaksx <- legendFixedBreaks
                    }

                    # gsub scenario name for figures
                    datax <- datax %>%
                      dplyr::mutate(scenario= gsub("_xDiff.*","",scenario))

                      rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                     overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                     overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                     underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                     underLayerAlpha = underLayerAlpha, background=background,
                                     zoom=zoom, zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                     crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                     alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)),
                                     ncol=ncol, showNA=showNA, colorNA=colorNA,
                                     labelColor=labelColor,
                                     labelSize=labelSize,
                                     labelAlpha=labelAlpha,
                                     labelFill=labelFill,
                                     labelBorderSize=labelBorderSize,
                                     theme = theme, legendTitle=legendTitle,
                                     legendDigitsOverride=legendDigitsOverride,
                                     numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                     underLayer=underLayer,
                                     data=datax,
                                     legendBreaksn=legendBreaksn,
                                     legendDigits = legendDigits,
                                     palette = palette,
                                     width=width*max(1,colm/1),
                                     height=height*max(1,rowm/1),
                                     pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                     labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                     legendBreaks = legendBreaksx,
                                     fillColumn = meanCol,
                                     col = multiFacetColsx,
                                     row = multiFacetRowsx,
                                     title = titlex,
                                     fileName = paste("map_",param_i,"_MEAN_",fileNameTag,"_xDiffAbs",nameAppend,sep=""),
                                     folder = sub("/$","",paste(folder,"/",param_if,sep = ""))) ->
                        mapsReturn[[return_i]];
                      names(mapsReturn)[return_i] <- paste("map_",param_i,"_MEAN_",fileNameTag,"_xDiffAbs",nameAppend,sep="");
                      return_i = return_i + 1


                  } # if(nrow(datax)>0){
                }# If multiple years

              } # if nrow of dataTblx dplyr::filtered for Diff scenarios

            }# Close if nrow dataTbl < 0
          } # Close xDiff Abs Scenario


          # xDiff Percent Scenarios
          if(T){

            if(length(unique(dataTblOrig$param))==1){param_if=NULL}else{param_if=param_i}

            if(nrow(dataTblOrig%>%dplyr::filter(param==param_i))>0){

              dataTblx <- dataTblOrig%>%dplyr::filter(param==param_i,
                                                      scenario %in% dataTbl_scenariosOrig[grepl("_xDiffPrcnt",dataTbl_scenariosOrig)])

              if(nrow(dataTblx)>0){

                #.................-
                # Create data Table Folders If Needed
                #.................-
                if(save){

                  if(!dir.exists(paste(folder,"/",sep = ""))){
                    dir.create(paste(folder,"/",sep = ""))}

                  if(!dir.exists(paste(folder,"/",param_if,sep = ""))){
                    dir.create(paste(folder,"/",param_if,sep = ""))}

                  if(!dir.exists(paste(folder,"/",param_if,sep = ""))){
                    dir.create(paste(folder, "/",param_if,sep = ""))}

                  if(length(unique(dataTblx$x))>1){
                    if(!dir.exists(paste(folder,"/",param_if,"/byYear",sep = ""))){
                      dir.create(paste(folder, "/",param_if,"/byYear",sep = ""))}
                  }
                } # Create data table folder if needed

                #.................--
                # Save Map related Data Table
                #.................--
                if(save){
                  if(nrow(dataTblx %>% dplyr::filter(param==param_i))>0){
                    data.table::fwrite(dataTblx %>% dplyr::filter(param==param_i)%>%
                                         dplyr::select(scenario,lat,lon,subRegion,region,param,class,x,value,units),
                                       paste(folder,"/",param_if,"/map_",param_i,nameAppend,".csv",sep = ""))
                    rlang::inform(paste("Map data table written to ",folder,"/",param_if,"/map_",param_i,nameAppend,".csv",sep = ""))
                  }
                }

                #.............................
                # By Year
                #.............................

                # Set Legends
                if(T){
                  scalex<-dataTblx$value
                  scalex <- scalex[!is.infinite(scalex)]
                  scalex <- scalex[!is.nan(scalex)]
                  scalex <- scalex[!is.na(scalex)]

                  # Choose correct scaleRange
                  scaleRange_i=scaleRangexDiffPrcnt

                  if(!is.null(scaleRange_i)){
                    if(any(param_i %in% unique(scaleRange_i$param))){
                      if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                        scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                          scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                         scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                        }
                      if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                        scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                          scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                          scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                        }
                    }
                  }

                  prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex); prettyBreaks
                  kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                 centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]));kmeanBreaks
                  if(!min(scalex) %in% kmeanBreaks){
                    kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                  if(!max(scalex) %in% kmeanBreaks){
                    kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))};kmeanBreaks

                  if(!is.null(legendFixedBreaks)){
                    if(min(scalex) < min(legendFixedBreaks)){
                      legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                    if(max(scalex) > max(legendFixedBreaks)){
                      legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                  }


                  if((max(range(scalex))-min(range(scalex)))<1E-10 &
                     (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                       scaleRangex=range(scalex)
                     }

                  if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                  if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                    if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                      if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                        if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}

                }

                # By Year
                if(length(unique(dataTblx$x))>1){

                  for (x_i in unique(dataTblx$x)){

                    datax<-dataTblx%>%dplyr::filter(x==x_i)

                    if(nrow(datax)>0){
                      if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                      palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                      # Set Facets
                      if(length(unique(datax$scenario))>1){
                        multiFacetColsx <- "scenario"
                        colm <- length(unique(datax$scenario))
                        if((length(unique(datax$class))>1)){
                          multiFacetRowsx <- "scenario"
                          rowm <- length(unique(datax$scenario))
                          multiFacetColsx <- c("class")
                          colm <- length(unique(datax$class))
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
                          if(forceFacets){
                            multiFacetColsx <- "scenario"
                          } else {
                            multiFacetColsx <- NULL
                          }
                          multiFacetRowsx <- NULL
                          colm = 1
                          rowm = 1
                        }
                      }

                      # Add facet or Rows if selected
                      if(!is.null(col)){
                        if(!is.null(multiFacetColsx)){
                          multiFacetColsx <- c(multiFacetColsx,col)
                          colm <- colm + length(col)
                        } else { multiFacetColsx <- col; colm <- length(col)}
                      }

                      if(!is.null(row)){
                        if(!is.null(multiFacetRowsx)){
                          multiFacetRowsx <- c(multiFacetRowsx,row)
                          rowm <- rowm + length(row)
                        } else { multiFacetRowsx <- row; rowm <- length(row)}
                      }


                      # Check for Duplicates
                      if(duplicated(datax %>%
                                    dplyr::select(lat,lon,subRegion,region,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                         any()){stop("Input data data has multiple values. Please check your data.")}

                      # Set title
                      if(is.null(title)){
                        if(param_i == "param"){
                          titlex <- paste(x_i," xDiffPrcnt ",xRef,sep="")
                        } else {
                          titlex <- paste(param_i," - ",x_i," xDiffPrcnt ",xRef,sep="")
                        }
                      } else if(title == F){
                        titlex <- NULL
                      } else {
                        titlex <- title
                      }

                      # Assign variables based on legend type choice
                      if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                        fileNameTag <- "KMEANS"
                        legendBreaksx <- kmeanBreaks
                      } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                        fileNameTag <- "CONTINUOUS"
                        legendBreaksx <- prettyBreaks
                      } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                        fileNameTag <- "PRETTY"
                        legendBreaksx <- prettyBreaks
                      } else if(!is.null(legendFixedBreaks)){
                        fileNameTag <- "FIXED"
                        legendBreaksx <- legendFixedBreaks
                      }

                      # gsub scenario name for figures
                      datax <- datax %>%
                        dplyr::mutate(scenario= gsub("_xDiff.*","",scenario))

                        rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                       overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                       overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                       underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                       underLayerAlpha = underLayerAlpha, background=background,
                                       zoom=zoom, zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                       crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                       alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)),
                                       ncol=ncol, showNA=showNA, colorNA=colorNA,
                                       theme = theme, legendTitle=legendTitle,
                                       legendDigitsOverride=legendDigitsOverride,
                                       numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                       underLayer=underLayer,
                                       data=datax,
                                       labelColor=labelColor,
                                       labelSize=labelSize,
                                       labelAlpha=labelAlpha,
                                       labelFill=labelFill,
                                       labelBorderSize=labelBorderSize,
                                       legendBreaksn=legendBreaksn,
                                       legendDigits = legendDigits,
                                       palette = palette,
                                       width=width*max(1,colm/1),
                                       height=height*max(1,rowm/1),
                                       pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                       labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                       legendBreaks = legendBreaksx,
                                       fillColumn = "value",
                                       col = multiFacetColsx,
                                       row = multiFacetRowsx,
                                       title=titlex ,
                                       fileName = paste("map_",param_i,"_",x_i,"_",fileNameTag,"_xDiffPrcnt",nameAppend,sep=""),
                                       folder = paste(folder,"/",param_if,"/byYear",sep = "")) ->
                          mapsReturn[[return_i]];
                        names(mapsReturn)[return_i] <- paste("map_",param_i,"_",x_i,"_",fileNameTag,"_xDiffPrcnt",nameAppend,sep="");
                        return_i = return_i + 1

                        # theme_ggplot = theme_ggplot
                        # theme_custom = theme_custom
                        # theme_rmap = theme_rmap
                        # legendDigitsOverride=legendDigitsOverride
                        # numeric2Cat_list=numeric2Cat_list
                        # underLayer=underLayer
                        # data=datax
                        # legendBreaksn=legendBreaksn
                        # legendDigits = legendDigits
                        # palette = palette
                        # width=width*max(1,colm/1),
                        # height=height*max(1,rowm/1),
                        # pdfpng = pdfpng
                        # legendSingleColor = legendSingleColor
                        # legendSingleValue =  legendSingleValue
                        # labels=labels
                        # legendBreaks = kmeanBreaks
                        # fillColumn = "value"
                        # col = multiFacetColsx
                        # row = multiFacetRowsx
                        # title=paste(param_i," ",x_i,sep="")
                        # fileName = paste("map_",param_i,"_",x_i,"_",fileNameTag,nameAppend,sep="")
                        # folder = paste(folder,"/",param_if,"/byYear",sep = "")



                    }
                  } # Close years x_i loop

                  # Animations
                  if(animate==T){

                      animName<-paste("anim_",param_i,"_",fileNameTag,"_xDiffPrcnt",nameAppend,".gif",sep="")
                      animFiles <- list.files(path = paste(folder,"/",param_if,"/byYear",sep=""),
                                              pattern = paste(".*",param_i,".*",fileNameTag,"_xDiffPrcnt",nameAppend, ".", pdfpng,sep=""), full.names=T,ignore.case = T, include.dirs = T);
                      animation <- magick::image_animate(magick::image_join(lapply(animFiles, magick::image_read)),fps=fps)
                      magick::image_write(animation,paste(folder,"/",param_if,"/",
                                                          animName,sep = ""))
                      rlang::inform(gsub("//","/",paste("animation saved in :",folder,"/",param_if,"/",
                                                animName,sep = "")))


                  }
                }

                # Multi-Year-Single Chart
                datax<-dataTblx%>%dplyr::filter(param==param_i)
                if(nrow(datax)>0){

                  if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                  palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                  scalex<-datax$value
                  scalex <- scalex[!is.infinite(scalex)]
                  scalex <- scalex[!is.nan(scalex)]
                  scalex <- scalex[!is.na(scalex)]

                  # Choose correct scaleRange
                  if(T){
                    scaleRange_i=scaleRangexDiffPrcnt

                    if(!is.null(scaleRange_i)){
                      if(any(param_i %in% unique(scaleRange_i$param))){
                        if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                          scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                            scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                           scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                          }
                        if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                          scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                            scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                            scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                          }
                      }
                    }
                    prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex)
                    kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                   centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]))
                    if(!min(scalex) %in% kmeanBreaks){
                      kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                    if(!max(scalex) %in% kmeanBreaks){
                      kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))}

                    if(!is.null(legendFixedBreaks)){
                      if(min(scalex) < min(legendFixedBreaks)){
                        legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                      if(max(scalex) > max(legendFixedBreaks)){
                        legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                    }

                    if((max(range(scalex))-min(range(scalex)))<1E-10 &
                       (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                         scaleRangex=range(scalex)
                       }

                    if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                    if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                      if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                        if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                          if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}
                  }

                  # Set Facets
                  if(length(unique(datax$x))>1){
                    multiFacetColsx <- "x"
                    colm <- length(unique(datax$x))
                    if((length(unique(datax$scenario))>1) & (length(unique(datax$class))>1)){
                      multiFacetRowsx <- "x"
                      rowm <- length(unique(datax$x))
                      multiFacetColsx <- c("scenario","class")
                      colm <- length(unique(datax$scenario))*length(unique(datax$class))
                    }
                    if((length(unique(datax$scenario))>1) & (!length(unique(datax$class))>1)){
                      multiFacetRowsx <- c("scenario")
                      rowm <- length(unique(datax$scenario))
                    }
                    if((!length(unique(datax$scenario))>1) & (length(unique(datax$class))>1)){
                      multiFacetRowsx <- "x"
                      rowm <- length(unique(datax$x))
                      multiFacetColsx <- c("class")
                      colm <- length(unique(datax$class))
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
                        multiFacetRowsx <- "scenario"
                        rowm <- length(unique(datax$scenario))
                        multiFacetColsx <- c("class")
                        colm <- length(unique(datax$class))
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
                        if(forceFacets){
                          multiFacetColsx <- "scenario"
                        } else {
                          multiFacetColsx <- NULL
                        }
                        multiFacetRowsx <- NULL
                        colm <- 1
                        rowm <- 1
                      }
                    }
                  }


                  # Add facet or Rows if selected
                  if(!is.null(col)){
                    if(!is.null(multiFacetColsx)){
                      multiFacetColsx <- c(multiFacetColsx,col)
                      colm <- colm + length(col)
                    } else { multiFacetColsx <- col; colm <- length(col)}
                  }

                  if(!is.null(row)){
                    if(!is.null(multiFacetRowsx)){
                      multiFacetRowsx <- c(multiFacetRowsx,row)
                      rowm <- rowm + length(row)
                    } else { multiFacetRowsx <- row; rowm <- length(row)}
                  }

                  # Check for Duplicates
                  if(duplicated(datax %>%
                                dplyr::select(subRegion,lat,lon,x,region,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                     any()){stop("Input data data has multiple values. Please check your data.")}

                  # Set title
                  if(is.null(title)){
                    if(param_i == "param"){
                      if(length(xDiff)==1){
                        titlex <- paste(xDiff," xDiffPrcnt ",xRef,sep="")
                      }else{
                        titlex <- paste(xRef," xDiffPrcnt",sep="")
                      }
                    } else {
                      if(length(xDiff)==1){
                        titlex <- paste(param_i," - ",xDiff," xDiffPrcnt ",xRef,sep="")
                      }else{
                        titlex <- paste(param_i," - ",xRef," xDiffPrcnt",sep="")
                      }
                    }
                  } else if(title == F){
                    titlex <- NULL
                  } else {
                    titlex <- title
                  }

                  # Assign variables based on legend type choice
                  if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                    fileNameTag <- "KMEANS"
                    legendBreaksx <- kmeanBreaks
                  } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                    fileNameTag <- "CONTINUOUS"
                    legendBreaksx <- prettyBreaks
                  } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                    fileNameTag <- "PRETTY"
                    legendBreaksx <- prettyBreaks
                  } else if(!is.null(legendFixedBreaks)){
                    fileNameTag <- "FIXED"
                    legendBreaksx <- legendFixedBreaks
                  }

                  # gsub scenario name for figures
                  datax <- datax %>%
                    dplyr::mutate(scenario= gsub("_xDiff.*","",scenario))

                    rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                   overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                   overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                   underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                   underLayerAlpha = underLayerAlpha, background=background, zoom=zoom,
                                   zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                   crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                   alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)), ncol=ncol,
                                   showNA=showNA, colorNA=colorNA,
                                   labelColor=labelColor,
                                   labelSize=labelSize,
                                   labelAlpha=labelAlpha,
                                   labelFill=labelFill,
                                   labelBorderSize=labelBorderSize,
                                   theme = theme, legendTitle=legendTitle,
                                   legendDigitsOverride=legendDigitsOverride,
                                   numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                   underLayer=underLayer,
                                   data=datax,
                                   legendBreaksn=legendBreaksn,
                                   legendDigits = legendDigits,
                                   palette = palette,
                                   width=width*max(1,colm/1),
                                   height=height*max(1,rowm/1),
                                   pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                   labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                   legendBreaks = legendBreaksx,
                                   fillColumn = "value",
                                   col = multiFacetColsx,
                                   row = multiFacetRowsx,
                                   title= titlex,
                                   fileName = paste("map_",param_i,"_",fileNameTag,"_xDiffPrcnt",nameAppend,sep=""),
                                   folder = sub("/$","",paste(folder,"/",param_if,sep = ""))) ->
                      mapsReturn[[return_i]];
                    names(mapsReturn)[return_i] <- paste("map_",param_i,"_",fileNameTag,"_xDiffPrcnt",nameAppend,sep="");
                    return_i = return_i + 1


                    # save=save
                    # overLayer=overLayer
                    # overLayerColor=overLayerColor
                    # overLayerFill = overLayerFill
                    # overLayerLwd = overLayerLwd
                    # overLayerAlpha = overLayerAlpha
                    # underLayerColor = underLayerColor
                    # underLayerFill = underLayerFill
                    # underLayerLwd = underLayerLwd
                    # underLayerAlpha = underLayerAlpha
                    # background=background
                    # zoom=zoom
                    # zoomx = zoomx
                    # zoomy=zoomy
                    # alpha = alpha
                    # size=max(1,(size+(colm+rowm)*3 - 12))
                    # ncol=ncol
                    # showNA=showNA
                    # colorNA=colorNA
                    # theme = theme
                    # legendTitle=legendTitle
                    # legendDigitsOverride=legendDigitsOverride
                    # numeric2Cat_list=numeric2Cat_list
                    # underLayer=underLayer
                    # data=datax
                    # legendBreaksn=legendBreaksn
                    # legendDigits = legendDigits
                    # palette = palette
                    # width=width*max(1,colm/1)
                    # height=height*max(1,rowm/1)
                    # pdfpng = pdfpng
                    # legendSingleColor = legendSingleColor
                    # legendSingleValue =  legendSingleValue
                    # labels=labels
                    # legendBreaks = kmeanBreaks
                    # fillColumn = "value"
                    # col = multiFacetColsx
                    # row = multiFacetRowsx
                    # title=paste(param_i,sep="")
                    # fileName = paste("map_",param_i,"_",fileNameTag,nameAppend,sep="")
                    # folder = sub("/$","",paste(folder,"/",param_if,sep = ""))



                } # if(nrow(datax)>0){

                # Mean for all years provided
                datax<-dataTblx%>%dplyr::filter(param==param_i)

                if(length(unique(datax$x))>1){

                  if(nrow(datax)>0){

                    if(is.null(legendTitleOrig)){legendTitle<-unique(datax$units)}
                    palette<-as.character(unique(datax$palette)); if(grepl(",",palette)){palette = unlist(stringr::str_split(palette,","))}

                    meanCol = paste("Mean_",min(datax$x),"to",max(datax$x),sep="")

                    colsPresentGroup =  c("lon","lat","subRegion","region","scenario","class")
                    colsPresentGroup = colsPresentGroup[colsPresentGroup %in% names(datax)]

                    datax<-datax%>%
                      dplyr::select(lat,lon,subRegion,scenario,region,class,x,value)%>%
                      dplyr::group_by_at(dplyr::all_of(colsPresentGroup))%>%
                      dplyr::summarize(!!meanCol:=mean(value))%>%
                      dplyr::ungroup()

                    scalex<-datax[[meanCol]];scalex
                    scalex <- scalex[!is.infinite(scalex)]
                    scalex <- scalex[!is.nan(scalex)]
                    scalex <- scalex[!is.na(scalex)]

                    # Choose correct scaleRange
                    if(T){
                      scaleRange_i=scaleRangexDiffPrcnt

                      if(!is.null(scaleRange_i)){
                        if(any(param_i %in% unique(scaleRange_i$param))){
                          if(max(scalex) < (scaleRange_i %>% dplyr::filter(param==param_i))$maxScale){
                            scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale)} else {
                              scalex <- c((scaleRange_i %>% dplyr::filter(param==param_i))$maxScale,
                                             scalex[scalex<(scaleRange_i %>% dplyr::filter(param==param_i))$maxScale])
                            }
                          if(min(scalex) > (scaleRange_i %>% dplyr::filter(param==param_i))$minScale){
                            scalex<-c(scalex,(scaleRange_i %>% dplyr::filter(param==param_i))$minScale)} else {
                              scalex <-  c((scaleRange_i %>% dplyr::filter(param==param_i))$minScale,
                                              scalex[scalex>(scaleRange_i %>% dplyr::filter(param==param_i))$minScale])
                            }
                        }
                      }
                      prettyBreaks<-scales::pretty_breaks(n=legendBreaksn)(scalex)
                      kmeanBreaks<-sort(as.vector((stats::kmeans(scalex,
                                                                     centers=max(1,min(length(unique(scalex))-1,(legendBreaksn-1)))))$centers[,1]))
                      if(!min(scalex) %in% kmeanBreaks){
                        kmeanBreaks <- sort(c(min(scalex),kmeanBreaks))}
                      if(!max(scalex) %in% kmeanBreaks){
                        kmeanBreaks <- sort(c(kmeanBreaks,max(scalex)))}

                      if(!is.null(legendFixedBreaks)){
                        if(min(scalex) < min(legendFixedBreaks)){
                          legendFixedBreaks <- sort(c(min(scalex),legendFixedBreaks))}
                        if(max(scalex) > max(legendFixedBreaks)){
                          legendFixedBreaks <- sort(c(legendFixedBreaks,max(scalex)))};legendFixedBreaks
                      }

                      if((max(range(scalex))-min(range(scalex)))<1E-10 &
                         (max(range(scalex))-min(range(scalex)))>-1E-10){scaleRangex=min(scalex)}else{
                           scaleRangex=range(scalex)
                         }

                      if(abs(min(scaleRangex,na.rm = T))==abs(max(scaleRangex,na.rm = T))){scaleRangex=abs(min(scaleRangex,na.rm = T))}
                      if(mean(scaleRangex,na.rm = T)<0.01 & mean(scaleRangex,na.rm = T)>(-0.01)){legendDigits<-5}else{
                        if(mean(scaleRangex,na.rm = T)<0.1 & mean(scaleRangex,na.rm = T)>(-0.1)){legendDigits<-4}else{
                          if(mean(scaleRangex,na.rm = T)<1 & mean(scaleRangex,na.rm = T)>(-1)){legendDigits<-3}else{
                            if(mean(scaleRangex,na.rm = T)<10 & mean(scaleRangex,na.rm = T)>(-10)){legendDigits<-2}else{legendDigits<-1}}}}
                    }

                    # Set Facets
                    if(length(unique(datax$scenario))>1){

                      multiFacetColsx <- "scenario"
                      colm <- length(unique(datax$scenario))

                      if((length(unique(datax$class))>1)){
                        multiFacetRowsx <- "scenario"
                        rowm <- length(unique(datax$scenario))
                        multiFacetColsx <- c("class")
                        colm <- length(unique(datax$class))
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

                        if(forceFacets){
                          multiFacetColsx <- "scenario"
                        } else {
                          multiFacetColsx <- NULL
                        }

                        multiFacetRowsx <- NULL
                        colm <- 1
                        rowm <- 1
                      }
                    }

                    # Add facet or Rows if selected
                    if(!is.null(col)){
                      if(!is.null(multiFacetColsx)){
                        multiFacetColsx <- c(multiFacetColsx,col)
                        colm <- colm + length(col)
                      } else { multiFacetColsx <- col; colm <- length(col)}
                    }

                    if(!is.null(row)){
                      if(!is.null(multiFacetRowsx)){
                        multiFacetRowsx <- c(multiFacetRowsx,row)
                        rowm <- rowm + length(row)
                      } else { multiFacetRowsx <- row; rowm <- length(row)}
                    }



                    # Check for Duplicates
                    if(duplicated(datax %>%
                                  dplyr::select(lat,lon,subRegion,region,x,dplyr::all_of(multiFacetRowsx),dplyr::all_of(multiFacetColsx))) %>%
                       any()){stop("Input data data has multiple values. Please check your data.")}

                    # Set title
                    if(is.null(title)){
                      if(param_i == "param"){
                        titlex <- paste(xRef," xDiffPrcnt ", meanCol,sep="")
                      } else {
                        titlex <- paste(param_i," - ",xRef," xDiffPrcnt ",meanCol,sep="")
                      }
                    } else if(title == F){
                      titlex <- NULL
                    } else {
                      titlex <- title
                    }

                    # Assign variables based on legend type choice
                    if(any(grepl("all|kmean",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))){
                      fileNameTag <- "KMEANS"
                      legendBreaksx <- kmeanBreaks
                    } else if(any(grepl("all|continuous",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                      fileNameTag <- "CONTINUOUS"
                      legendBreaksx <- prettyBreaks
                    } else if(any(grepl("all|pretty",legendType,ignore.case = T)) & (is.null(legendFixedBreaks))) {
                      fileNameTag <- "PRETTY"
                      legendBreaksx <- prettyBreaks
                    } else if(!is.null(legendFixedBreaks)){
                      fileNameTag <- "FIXED"
                      legendBreaksx <- legendFixedBreaks
                    }

                    # gsub scenario name for figures
                    datax <- datax %>%
                      dplyr::mutate(scenario= gsub("_xDiff.*","",scenario))

                      rmap::map_plot(crs=crs, underLayerLabelCol = underLayerLabelCol, overLayerLabelCol = overLayerLabelCol, labelCol=labelCol, region=region,color=color, lwd=lwd, legendType=legendType, save=save,  show=show, shape = shapex, overLayer=overLayer, overLayerColor=overLayerColor,
                                     overLayerFill = overLayerFill, overLayerLwd = overLayerLwd,
                                     overLayerAlpha = overLayerAlpha, underLayerColor=underLayerColor,
                                     underLayerFill = underLayerFill, underLayerLwd = underLayerLwd,
                                     underLayerAlpha = underLayerAlpha, background=background,
                                     zoom=zoom, zoomx = zoomx, zoomy=zoomy, asp=asp, legendShow=legendShow,
                                     crop = crop, crop_to_underLayer = crop_to_underLayer, crop_to_overLayer = crop_to_overLayer, transparent=transparent,
                                     alpha = alpha, size=max(1,(size+(colm+rowm)*3 - 12)),
                                     ncol=ncol, showNA=showNA, colorNA=colorNA,
                                     labelColor=labelColor,
                                     labelSize=labelSize,
                                     labelAlpha=labelAlpha,
                                     labelFill=labelFill,
                                     labelBorderSize=labelBorderSize,
                                     theme = theme, legendTitle=legendTitle,
                                     legendDigitsOverride=legendDigitsOverride,
                                     numeric2Cat_list=numeric2Cat_list, catParam = param_i,
                                     underLayer=underLayer,
                                     data=datax,
                                     legendBreaksn=legendBreaksn,
                                     legendDigits = legendDigits,
                                     palette = palette,
                                     width=width*max(1,colm/1),
                                     height=height*max(1,rowm/1),
                                     pdfpng = pdfpng, legendSingleColor = legendSingleColor, legendSingleValue =  legendSingleValue,
                                     labels=labels, labelRepel=labelRepel, underLayerLabels=underLayerLabels, overLayerLabels=overLayerLabels,
                                     legendBreaks = legendBreaksx,
                                     fillColumn = meanCol,
                                     col = multiFacetColsx,
                                     row = multiFacetRowsx,
                                     title = titlex,
                                     fileName = paste("map_",param_i,"_MEAN_",fileNameTag,"_xDiffPrcnt",nameAppend,sep=""),
                                     folder = sub("/$","",paste(folder,"/",param_if,sep = ""))) ->
                        mapsReturn[[return_i]];
                      names(mapsReturn)[return_i] <- paste("map_",param_i,"_MEAN_",fileNameTag,"_xDiffPrcnt",nameAppend,sep="");
                      return_i = return_i + 1


                  } # if(nrow(datax)>0){
                }# If multiple years

              } # if nrow of dataTblx dplyr::filtered for Diff scenarios

            }# Close if nrow dataTbl < 0
          } # Close xDiff Percent Scenario
        }


      } # Close params loop

      } # Close if nrow dataTbl < 0

    }# Close if dataTbl is Null
  } # Close Plots

  # .................
  # Return Data
  # .................

  rlang::inform("map run completed.")

  invisible(mapsReturn)

  } # Close direct plots

} # close function
