#' map_plot
#'
#' This function produce different kinds of maps for the rmap package.
#' Each figure is accompanied with a csv table.
#'
#' @param data Default = NULL
#' @param region Default = NULL. Set the boundary region for subRegion maps. Useful when multiple subRegions in different regions.
#' @param fileName Default = "map"
#' @param shape Default = NULL, Cusotm shape can be provided as a SpatialPolygonDataFrame with features corresponding to subRegion columns in the data provided.
#' @param folder Default = paste(getwd(),"/outputs",sep Default = "")
#' @param palette Default = "Set3"
#' @param show Default = T. Print maps in console as they are processed.
#' @param theme Default = NULL
#' @param fillColumn Default = NULL # Or give column with data
#' @param width Default = 9
#' @param height Default = 7
#' @param legendShow Default = F
#' @param legendDigits Default = NULL
#' @param legendTitle Default = NULL
#' @param legendBreaksn Default = "5"
#' @param legendBreaks Default = NULL
#' @param labels Default = FALSE
#' @param labelCol Default = NULL,
#' @param labelRepel Default = 0,
#' @param labelColor Default = "black",
#' @param labelSize Default = 3
#' @param labelAlpha Default = 0.7,
#' @param labelFill Default = NA,
#' @param labelBorderSize Default = NA
#' @param pdfpng Default = "png"
#' @param color Default = "grey40". Color of polygon lines.
#' @param lwd Default = 0.1. Line width of polygon boundaries.
#' @param underLayer Default = NULL
#' @param underLayerLabelCol Default = NULL
#' @param underLayerColor Default = "gray40"
#' @param underLayerFill Default = "gray40"
#' @param underLayerLwd Default = 0.5
#' @param underLayerAlpha Default = 1
#' @param underLayerLabels Default = F
#' @param overLayerLabelCol Default = NULL
#' @param overLayerLabels Default = F
#' @param overLayer Default = NULL
#' @param overLayerColor Default = "gray40"
#' @param overLayerFill Default = NA
#' @param overLayerLwd Default = 0.5
#' @param overLayerAlpha Default = 0
#' @param zoom Default =-1. Zoom into or out of map. Positive values zoom in and negative out.
#' @param zoomx Default = NULL. Zoom into or out of map along x. Positive values zoom in and negative out.
#' @param zoomy Default = NULL. Zoom into or out of map along y. Positive values zoom in and negative out.
#' @param asp Default = 1.2. Aspect ratio of lat and lon.
#' @param save Default = T
#' @param row Default=NULL
#' @param col Default=NULL
#' @param title Default=NULL
#' @param catParam Default=NULL
#' @param numeric2Cat_list Default=NULL
#' @param legendDigitsOverride Default=NULL
#' @param legendSingleColor Default="white"
#' @param legendSingleValue Default=F. Change to True to get default single value or provide a numeric value.
#' @param colorNA Default = "gray50"
#' @param showNA Default = T
#' @param ncol Default = 3.  Number of columns to wrap maps
#' @param size Default = 12. Text size of plots.
#' @param alpha Default = 1. Transparency of fill colors.
#' @param background Default = F. T adds background water color, border and default underlayer map.
#' If background is set to a color (e.g. background = "grey10") then map will be produced with
#' water of that color, a border and underlayer map.
#' @param crop Default = T. Crop to boundary data.
#' @param crop_to_underLayer Default = F. Crop to the underLayer boundary provided.
#' @param crop_to_overLayer Default = F. Crop to the overLayer boundary provided.
#' @param transparent Default = F. To make map background transparent for maps without backgrounds.
#' @param legendType Default = "continuous".
#' @param crs Default = "+proj=longlat +datum=WGS84 +no_defs". A proj4 string from EPSG https://epsg.io/
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @import sf
#' @importFrom rlang :=
#' @importFrom jgcricolors jgcricol
#' @importFrom magrittr %>%
#' @export

map_plot<-function(data=NULL,
                  region=NULL,
                  fillColumn=NULL, # Or give column data with
                  shape = NULL,
                  theme = NULL,
                  show = T,
                  palette="Set3",
                  legendType="kmeans",
                  labels=F,
                  labelCol = NULL,
                  labelRepel = 0,
                  labelColor = "black",
                  labelSize = 2,
                  labelAlpha = 1,
                  labelFill = NA,
                  labelBorderSize = NA,
                  width=9,
                  height=7,
                  legendShow=T,
                  legendTitle=NULL,
                  legendBreaksn=5,
                  legendBreaks=NULL,
                  pdfpng="png",
                  underLayer = NULL,
                  color = "grey40",
                  lwd = 0.1,
                  underLayerLabelCol = NULL,
                  underLayerColor = "gray40",
                  underLayerFill = "gray90",
                  underLayerLwd = 0.1,
                  underLayerAlpha = 1,
                  underLayerLabels = F,
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
                  save=T,
                  fileName="map",
                  folder=paste(getwd(),"/outputs",sep=""),
                  row=NULL,
                  col=NULL,
                  title=NULL,
                  numeric2Cat_list=NULL,
                  catParam =NULL,
                  legendDigits = NULL,
                  legendDigitsOverride=NULL,
                  legendSingleColor ="white",
                  legendSingleValue =F,
                  colorNA = "gray50",
                  showNA = F,
                  ncol = 3,
                  size = 16,
                  alpha = 1,
                  background = F,
                  crop = T,
                  crop_to_underLayer = F,
                  crop_to_overLayer = F,
                  transparent = F,
                  crs = "+proj=longlat +datum=WGS84 +no_defs"
                  ){

  # # data=NULL
  # region=NULL
  # fillColumn=NULL # Or give column data with
  # shape = NULL
  # theme = NULL
  # show = T
  # palette="Set3"
  # legendType="kmeans"
  # labels=F
  # labelCol = NULL
  # labelRepel = 0
  # labelColor = "black"
  # labelSize = 2
  # labelAlpha = 1
  # labelFill = NA
  # labelBorderSize = NA
  # width=9
  # height=7
  # legendShow=T
  # legendTitle=NULL
  # legendBreaksn=5
  # legendBreaks=NULL
  # pdfpng="png"
  # underLayer = NULL
  # color = "grey40"
  # lwd = 0.1
  # underLayerLabelCol = NULL
  # underLayerColor = "gray40"
  # underLayerFill = "gray90"
  # underLayerLwd = 0.1
  # underLayerAlpha = 1
  # underLayerLabels = F
  # overLayerLabelCol = NULL
  # overLayerLabels = F
  # overLayer = NULL
  # overLayerColor = "gray40"
  # overLayerFill = NA
  # overLayerLwd = 0.2
  # overLayerAlpha = 0
  # zoom = 0
  # zoomx = NULL
  # zoomy = NULL
  # asp = 1.2
  # save=T
  # fileName="map"
  # folder=paste(getwd(),"/outputs",sep="")
  # row=NULL
  # col=NULL
  # title=NULL
  # numeric2Cat_list=NULL
  # catParam =NULL
  # legendDigits = NULL
  # legendDigitsOverride=NULL
  # legendSingleColor ="white"
  # legendSingleValue =F
  # colorNA = "gray50"
  # showNA = F
  # ncol = 3
  # size = 16
  # alpha = 1
  # background = F
  # crop = T
  # crop_to_underLayer = F
  # crop_to_overLayer = F
  # transparent = F
  # crs = "+proj=longlat +datum=WGS84 +no_defs"

# ALT + 0 here to collapse all code into manageable chunks
# ALT + Shift + O to expand all

#.........................
# Initialize variables to remove binding errors if needed
# .........................

if(T){ # Initialize

  NULL->raster->map->checkFacets->catBreaks->catLabels->catPalette->legendLabelsX->
    singleValLoc->label->long->lat->group->dataShape->dataPolygon->dataGrid->data_shape->
    lon->hole->piece->subRegion->X1->X2->id->name->value->datax->subRegionAlt->datax1->
    data_sf_w_labels->geometry


  if(is.null(data)){stop("data cannot be null.")}

  if(!is.null(legendTitle)){
    legendTitle=gsub(" ","\n",legendTitle)

    if(any(tolower(legendTitle) %in% c("legend","units","unit"))){
      legendTitle = NULL
    }
  }

  if(!is.null(legendBreaks)){legendBreaks<-sort(legendBreaks)}

  paletteOrig <- palette

  if(!is.null(theme)){
  if(!any(grepl("theme",class(theme),ignore.case = T))){
    rlang::inform("Theme provide is not a ggplot theme. Ignoring theme.")
    theme = NULL
  }}

  # Rename certain countries to rmap names
  if("subRegion" %in% names(data)){
    data <- data %>%
      dplyr::mutate(subRegion = dplyr::case_when(grepl("^United States of America$|^United States$",subRegion,ignore.case = T)~"USA",
                                                 grepl("^Tanzania$",subRegion,ignore.case = T)~"United Republic of Tanzania",
                                                 grepl("^Democratic Republic of Congo$",subRegion,ignore.case = T)~"Democratic Republic of the Congo",
                                                 grepl("^Congo$",subRegion,ignore.case = T)~"Republic of the Congo",
                                                 grepl("^Cote d'Ivoire$",subRegion,ignore.case = T)~"Ivory Coast",
                                                 grepl("^Serbia$",subRegion,ignore.case = T)~"Republic of Serbia",
                                                 grepl("^EU-12$",subRegion,ignore.case = T)~"EU_12",
                                                 grepl("^EU-15$",subRegion,ignore.case = T)~"EU_15",
                                                 TRUE ~ subRegion))
  }

  # Disable s2 for sf plots
  sf::sf_use_s2(FALSE)

} # initialize


#.........................
# Set fillColumn, labelColumn and shape
#.........................

if(is.null(fillColumn)){
  if(any("value" %in% names(data))){
    fillColumn = "value"
  } else if("subRegion" %in% names(data)){
    fillColumn = "subRegion"
  }
}

if(is.null(labelCol)){
  if("subRegion" %in% names(data)){
    labelCol = "subRegion"
  } else {
    labelCol = names(data)[1]
  }
}

if(!is.null(shape)){
  if(any(grepl("sf",class(shape)))){
    if(any(grepl("sf",class(data)))){
    data <- shape %>%
      dplyr::left_join(
        data %>% sf::st_drop_geometry(),
        by="subRegion")
    } else if(any(grepl("data.frame",class(data)))){
      if("geometry" %in% names(data)){
        data <- shape %>%
          dplyr::left_join(
            data %>%
              dplyr::select(-geometry),
            by="subRegion")} else {
              data <- shape %>%
                dplyr::left_join(
                  data,by="subRegion")
            }
      }
    }
  }


#......................................................................
# Read data and check inputs
#......................................................................

if(T){ # Read input data


  if (any(grepl("sf", class(data)))) {
    data_sf <- data
    gridded_data=F
  } else if(any(grepl("data.frame", class(data))) & !any(grepl("^lat$",names(data))) & !any(grepl("^lon$",names(data)))){
    # If simple dataframe find map
    if(!any(grepl("^region$",names(data),ignore.case = T))){
      data_sf <-  map_find(data) %>%
        dplyr::left_join(data, by=c("subRegion")) %>%
        dplyr::filter(subRegion %in% (data$subRegion %>% unique()))

    } else if(length(unique(data$region))==1){
      if(all("region" %in% (unique(data$region)))){
        data <- data %>% dplyr::select(-region)

        data_sf <-  map_find(data) %>%
          dplyr::left_join(data, by=c("subRegion")) %>%
          dplyr::filter(subRegion %in% (data$subRegion %>% unique()))
      } else {
        data_sf <-  map_find(data) %>%
          dplyr::left_join(data, by=c("subRegion","region")) %>%
          dplyr::filter(subRegion %in% (data$subRegion %>% unique()))
      }
    } else {
    data_sf <-  map_find(data) %>%
      dplyr::left_join(data, by=c("subRegion","region")) %>%
      dplyr::filter(subRegion %in% (data$subRegion %>% unique()))
    }

    gridded_data=F

  } else if(any(grepl("data.frame", class(data))) & any(grepl("^lat$",names(data))) & any(grepl("^lon$",names(data)))){
    # If simple dataframe with lat lon
    if(!is.null(row) & !is.null(col)){
      data_comb <- data %>%
        dplyr::mutate(key := paste0(!!as.name(row),"xxspreadxx",!!as.name(col))) %>%
        dplyr::select(-row,-col); data_comb
      data_sf_raster <- raster::rasterFromXYZ(data_comb %>%
                                                tidyr::spread(key=key,value="value"))
      names(data_sf_raster) <- data_comb[[key]]%>%unique()%>%sort()
      data_sf_spdf <- data_sf_raster %>%
        methods::as('SpatialPixelsDataFrame') %>%
        methods::as('SpatialPolygonsDataFrame')
      data_sf <- sf::st_as_sf(data_sf_spdf) %>%
        sf::st_set_crs(sf::st_crs(crs)) %>%
        tidyr::gather(key=key,value="value",-names(data_comb)[!names(data_comb) %in% c("lon","lat","key","value")], -geometry) %>%
        dplyr::mutate(!!row := gsub("xxspreadxx.*","",key),
                      !!col := gsub(".*xxspreadxx","",key)) %>%
        dplyr::select(-key)
    } else if(!is.null(col)){
    data_sf_raster <- raster::rasterFromXYZ(data %>%
                                              tidyr::spread(key=col,value="value"))
    names(data_sf_raster) <- data[[col]]%>%unique()%>%sort()
    data_sf_spdf <- data_sf_raster %>%
      methods::as('SpatialPixelsDataFrame') %>%
      methods::as('SpatialPolygonsDataFrame')
    data_sf <- sf::st_as_sf(data_sf_spdf) %>%
      sf::st_set_crs(sf::st_crs(crs)) %>%
      tidyr::gather(key=!!col,value="value",-names(data)[!names(data) %in% c("lon","lat",col,"value")], -geometry)
    } else if(!is.null(row)){
      data_sf_raster <- raster::rasterFromXYZ(data %>%
                                                tidyr::spread(key=row,value="value"))
      names(data_sf_raster) <- data[[row]]%>%unique()%>%sort()
      data_sf_spdf <- data_sf_raster %>%
        methods::as('SpatialPixelsDataFrame') %>%
        methods::as('SpatialPolygonsDataFrame')
      data_sf <- sf::st_as_sf(data_sf_spdf) %>%
        sf::st_set_crs(sf::st_crs(crs)) %>%
        tidyr::gather(key=!!row,value="value",-names(data)[!names(data) %in% c("lon","lat",row,"value")], -geometry)
    } else {
        data_sf_raster <- raster::rasterFromXYZ(data)
        names(data_sf_raster) <- names(data)[!names(data) %in% c("lat","lon")]
        data_sf_spdf <- data_sf_raster %>%
          methods::as('SpatialPixelsDataFrame') %>%
          methods::as('SpatialPolygonsDataFrame')
        data_sf <- sf::st_as_sf(data_sf_spdf) %>%
          sf::st_set_crs(sf::st_crs(crs))
      }
    gridded_data=T
  } else if(any(grepl("raster", class(data),ignore.case = T))){
    # If raster
    data_sf_spdf <- data_sf_raster %>%
      methods::as('SpatialPixelsDataFrame') %>%
      methods::as('SpatialPolygonsDataFrame')
    data_sf <- sf::st_as_sf(data_sf_spdf) %>%
      sf::st_set_crs(sf::st_crs(crs))
    gridded_data=T
  }

  # Set palette
  if(any("value" %in% names(data_sf)) & any((palette == "Set3"))){
    palette = "pal_hot"
  }
  if (length(palette) == 1) {
    if (palette %in% names(jgcricolors::jgcricol())) {
      palette <- jgcricolors::jgcricol()[[palette]]
    } else{
      if (!is.na(RColorBrewer::brewer.pal.info[palette, ]$maxcolors)) {
        palette <-
          RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[palette, ]$maxcolors, palette)
      }
    }
  }

  # Set rows and cols if missing
  if(is.null(row)){data_sf <- data_sf %>% dplyr::mutate(row="row")}
  if(is.null(col)){data_sf <- data_sf %>% dplyr::mutate(col="col")}

}


#....................
# Set Legend Breaks and Labels
#....................

  if(T){
  # Set num2cat
  if (!is.null(numeric2Cat_list)) {

    if (all(
      c(
        "numeric2Cat_param",
        "numeric2Cat_breaks",
        "numeric2Cat_labels",
        "numeric2Cat_palette",
        "numeric2Cat_legendTextSize"
      ) %in% names(numeric2Cat_list)
    )) {
      if (catParam %in% unique(unlist(numeric2Cat_list$numeric2Cat_param))) {
        list_index <- which(numeric2Cat_list$numeric2Cat_param == catParam)
        catBreaks <-
          numeric2Cat_list$numeric2Cat_breaks[[list_index]]
        catLabels <-
          numeric2Cat_list$numeric2Cat_labels[[list_index]]
        if (grepl("c\\(", numeric2Cat_list$numeric2Cat_palette[[list_index]][1])) {
          catPalette <-
            eval(parse(text = paste(
              numeric2Cat_list$numeric2Cat_palette[[list_index]]
            )))
        } else{
          catPalette <- numeric2Cat_list$numeric2Cat_palette[[list_index]]
        }

        legendTextSize <-
          numeric2Cat_list$numeric2Cat_legendTextSize[[list_index]]
      }
    } else {
      rlang::inform(
        "numerc2Cat_list does not contain the appropriate sublists: 'numeric2Cat_param','numeric2Cat_breaks','numeric2Cat_labels','numeric2Cat_catPalette'. Skipping conversion to Categorical"
      )
    }
  }

  # If categorical data then set as factor for data_sf
  if(!is.null(catBreaks) & !is.null(catLabels)){

    if(!is.null(catPalette)){

      if(length(catPalette)==1){
        if(catPalette %in% names(jgcricolors::jgcricol())){
          catPalette<-jgcricolors::jgcricol()[[catPalette]]
          catPalette <- (rep(catPalette,length(catLabels)))[1:length(catLabels)]
          names(catPalette) <- catLabels
        }else if(!is.na(RColorBrewer::brewer.pal.info[catPalette,]$maxcolors)){
          catPalette <- RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[catPalette,]$maxcolors,catPalette)
        } else {
          rlang::inform(paste0("catPalette provided: ", catPalette, "for param: ", catParam, " is invalid. Using jgcricol()$pal_16."))
          catPalette <- (rep(jgcricol()$pal_16,length(catLabels)))[1:length(catLabels)]
          names(catPalette) <- catLabels
          rlang::inform(paste0("New catPalette: ", paste(catPalette,collapse=", ")))

        }
      }

      if(all(catLabels %in% names(catPalette))){
        rlang::inform(paste0("catPalette does not contain names for all labels. Assigning automatically."))
        catPalette <- (rep(catPalette,length(catLabels)))[1:length(catLabels)]
        catPalette <- catPalette[catLabels]
        catPalette <- catPalette[!is.na(catPalette)]
        names(catPalette) <- catLabels
      }

      palette = catPalette

    }


      if(is.numeric(data_sf[[fillColumn]])){

        legendBreaks <- NULL

        data_sf[[fillColumn]] <- cut( data_sf[[fillColumn]],
                                     breaks=catBreaks,
                                     labels=catLabels)
      }


      if(!any(unique(data_sf[[fillColumn]]) %in% names(palette))){
        data_sf %>%
          dplyr::mutate(!!fillColumn := as.factor(data_sf[[fillColumn]])) -> data_sf
      }

      paletteX <- palette;
      data_sf_w_labels <- data_sf %>% dplyr::mutate(label=value)

  }


  if(is.null(catPalette) & is.numeric(data_sf[[fillColumn]])){

  # Setting Legend Breaks
  if(T){

    if(is.null(legendBreaks)){
      if(length(scales::pretty_breaks(n=legendBreaksn)(data_sf[[fillColumn]]))>1){
      legendBreaks=scales::pretty_breaks(n=legendBreaksn)(data_sf[[fillColumn]])
      }else{legendBreaks=NULL}
      }

    if(!is.null(legendDigits)){
      if(!is.null(legendDigitsOverride)){
        legendDigits <- legendDigitsOverride;
      }
    }else{
        legendDigits = 4
      }

    # Adding in a single value (eg. 0 to be set to a single color eg. white)
      if(!is.null(legendDigits)){
          if(length(legendBreaks)>1){

            # Test Palette
            if(T){
              # Legend Labels
              if(T){

                legendBreaksX <- legendBreaks; legendBreaksX

                # New Breaks
                if(!is.numeric(legendSingleValue)){
                  if(max(legendBreaksX)<0){
                    legendSingleValueX=ceiling(max(legendBreaksX))
                  }else{
                    if(min(legendBreaksX)>0){
                      legendSingleValueX=floor(min(legendBreaksX))
                    }else{
                      legendSingleValueX=0
                    }
                  }
                }else {
                  legendSingleValueX=legendSingleValue}; legendSingleValueX


                # Place Single Value in Legend Breaks if not present
                if(!legendSingleValueX %in% legendBreaksX){
                  if(min(legendBreaksX)>legendSingleValueX){
                    legendBreaksX <- c(legendSingleValueX,legendBreaksX)
                  }else{
                    if(max(legendBreaksX)<legendSingleValueX){
                      legendBreaksX <- c(legendBreaksX,legendSingleValueX)
                    }else{
                      if(max(legendBreaksX)==legendSingleValueX | min(legendBreaksX)==legendSingleValueX){
                        legendBreaksX <- legendBreaksX
                      }else{
                        legendBreaksX <- c(legendBreaksX[1:match(max(legendBreaksX[legendBreaksX<legendSingleValueX]),legendBreaksX)],
                                           legendSingleValueX,
                                           legendBreaksX[(match(max(legendBreaksX[legendBreaksX<legendSingleValueX]),legendBreaksX)+1):(length(legendBreaksX))])
                      }
                    }
                  }
                }
                singleValLoc <- max(match(legendSingleValueX,legendBreaksX),1); singleValLoc; legendBreaksX

                # Legend Labels
                a<-c()
                if(length(legendBreaksX)>1){
                  for(i in 1:(length(legendBreaksX)-1)){
                    if(i!=1){lower<-upperLast}else{lower <- round(legendBreaksX[i],(legendDigits))};lower
                    upper <- round(legendBreaksX[i+1],legendDigits); upper
                    countDig <- 1
                    while(upper==lower & countDig<6){upper <- round(legendBreaksX[i+1],(legendDigits+countDig)); countDig=countDig+1};upper
                    upperLast <- upper; upperLast
                    a[i]=paste(format(lower,big.mark = ",")," to ",format(upper,big.mark = ","),sep="")
                  };a

                  # Add in the single Value
                  if(min(legendBreaksX)>=legendSingleValueX){
                    legendLabelsX=c(a)
                    legendLabelsX<-c(paste(legendSingleValueX,sep=""),
                                     legendLabelsX)
                  }else{
                    if(max(legendBreaksX)<=legendSingleValueX){
                      legendLabelsX=c(a)
                      legendLabelsX<-c(legendLabelsX,paste(legendSingleValueX,sep=""))
                    }else{
                      legendLabelsX=c(a)
                      legendLabelsX<-c(legendLabelsX[1:max((singleValLoc-1),1)],
                                       paste(legendSingleValueX,sep=""),
                                       legendLabelsX[(singleValLoc):length(legendLabelsX)])
                    }}; legendLabelsX

                }
              } # Legend Labels

              # Fill palette
              if(T){
                # Split Palettes into halves (to split diverging palettes when range is only one side of 0)
                #graphics::pie(rep(1,length(palette)),label=names(palette),col=palette);palette
                fillColUp<-palette[(round(length(palette)/2,0)+2):length(palette)];fillColUp
                fillColUp <- grDevices::colorRampPalette(c("white",fillColUp))(11)[-1];fillColUp
                #graphics::pie(rep(1,length(fillColUp)),label=names(fillColUp),col=fillColUp)
                fillColDown<-rev(palette[1:(round(length(palette)/2,0)-1)])
                fillColDown <- grDevices::colorRampPalette(c("white",fillColDown))(11)[-1];fillColDown
                #graphics::pie(rep(1,length(fillColDown)),label=names(fillColDown),col=fillColDown)

                # If all less than single color chosen then colDown, if vice versa then colUp else full palette
                if(max(legendBreaksX)<=legendSingleValueX){
                  if(any(grepl("diff|div|ratio",paletteOrig,ignore.case=T))){
                    paletteX<-rev(grDevices::colorRampPalette(fillColDown)(length(legendLabelsX)-1))
                  }else{
                    paletteX<-rev(grDevices::colorRampPalette(palette[-1])(length(legendLabelsX)-1))
                  }
                }else{
                  if(min(legendBreaksX)>=legendSingleValueX){
                    if(any(grepl("diff|div|ratio",paletteOrig,ignore.case=T))){
                      paletteX<-grDevices::colorRampPalette(fillColUp)(length(legendLabelsX)-1)
                    }else{
                      paletteX<-grDevices::colorRampPalette(palette[-1])(length(legendLabelsX)-1)
                    }
                  }else{
                    if(singleValLoc==length(legendLabelsX)){paletteXUp<-c()}else{
                      paletteXUp <- grDevices::colorRampPalette(fillColUp)(round((length(legendLabelsX)-singleValLoc),0))};paletteXUp
                    if(singleValLoc==1){paletteXDown<-c()}else{
                      paletteXDown <- rev(grDevices::colorRampPalette(fillColDown)(singleValLoc))};paletteXDown
                    paletteX <-c(paletteXDown,paletteXUp)
                  }
                }

                # Visualize Palette
                #if(length(paletteX)>0){
                #graphics::pie(rep(1,length(paletteX)),label=names(paletteX),col=paletteX)
                #  }

                # Add in the singleColor
                if(min(legendBreaksX)>=legendSingleValueX){
                  paletteX<-c(paste(legendSingleColor,sep=""),
                                  paletteX)
                }else{
                  if(max(legendBreaksX)<=legendSingleValueX){
                    paletteX<-c(paletteX,paste(legendSingleColor,sep=""))
                  }else{
                    paletteX<-c(paletteX[1:(singleValLoc-1)],
                                    paste(legendSingleColor,sep=""),
                                    paletteX[(singleValLoc+1):length(paletteX)])
                  }
                }

                paletteX;#graphics::pie(rep(1,length(paletteX)),label=1:length(paletteX),col=paletteX)

              }

              length(paletteX); length(legendLabelsX)

              # Add in Label for single Color
              if(T){
                if(legendSingleValueX %in% legendBreaksX){
                  if(max(legendBreaksX)==legendSingleValueX){
                    legendAdder = -(legendSingleValueX+(legendBreaksX[length(legendBreaksX)]-legendBreaksX[length(legendBreaksX)-1])/1000)}else{
                      legendAdder = (legendSingleValueX+(legendBreaksX[singleValLoc+1]-legendBreaksX[singleValLoc])/1000)
                    }
                }else{legendAdder=NULL}; legendAdder

                if(min(legendBreaksX)>legendSingleValueX){
                  legendBreaksX<- sort(c(legendSingleValueX[!legendSingleValueX %in% legendBreaksX],
                                         legendBreaksX[singleValLoc:length(legendBreaksX)]))
                }else{
                  if(max(legendBreaksX)<legendSingleValueX){
                    legendBreaksX<- sort(c(legendBreaksX[1:(singleValLoc)],
                                           legendSingleValueX[!legendSingleValueX %in% legendBreaksX]))
                  }else{
                    if(min(legendBreaksX)==legendSingleValueX){
                      legendBreaksX<- sort(c(legendSingleValueX,
                                             legendAdder,
                                             legendBreaksX[(singleValLoc+1):length(legendBreaksX)]))
                    }else{
                      if(max(legendBreaksX)==legendSingleValueX){
                        legendBreaksX<- sort(c(legendBreaksX[1:(singleValLoc)],
                                               legendSingleValueX[!legendSingleValueX %in% legendBreaksX],
                                               legendAdder))
                      }else{
                        legendBreaksX<- sort(c(legendBreaksX[1:(singleValLoc-1)],
                                               legendSingleValueX[!legendSingleValueX %in% legendBreaksX],
                                               legendAdder,
                                               legendBreaksX[(singleValLoc):length(legendBreaksX)]))
                      }}}};legendBreaksX
              }

              length(legendBreaksX);length(legendLabelsX);legendBreaksX;legendLabelsX

              # Assign new Labels to Palette
              if(length(paletteX)>0){
                if(length(paletteX)==length(legendLabelsX)){
                  names(paletteX)<-legendLabelsX
                }else{
                  paletteX=palette
                }
                #graphics::pie(rep(1,length(paletteX)),label=names(paletteX),col=paletteX)
              }else{paletteX=palette}
            } # Test Palette

          }else{
            legendBreaksX=legendBreaks
            legendLabelsX=NULL
            paletteX=palette[1]
            names(paletteX) = as.character(legendBreaks); paletteX
          }
    }else{
      legendBreaksX=legendBreaks
      legendLabelsX=NULL
      paletteX=palette
    }
  }

  # Remove singleValue if legendsSingleVal == F
  if(legendSingleValue == F){
    if(!is.null(singleValLoc)){
    paletteX = paletteX[-singleValLoc]
    }
    }

  # Setting labels for data_sf
  if(T){
    legVals <- names(paletteX); legVals
    legValsSingle <- legVals[!grepl("to",legVals)]; legValsSingle
    legValsRange <- legVals[grepl("to",legVals)]; legValsRange

    data_sf_w_labels <- data_sf

    if(length(legValsSingle)>0){
    for(legValsSingle_i in legValsSingle){
      data_sf_w_labels <- data_sf_w_labels %>%
        dplyr::mutate(label := dplyr::if_else(round(!!data_sf_w_labels[[fillColumn]],legendDigits)==as.numeric(as.character(legValsSingle)),
                                       legValsSingle,
                                       "temp"))
    }}else{
      data_sf_w_labels <- data_sf_w_labels %>%
        dplyr::mutate(label = "temp")
    }

    if(length(legValsRange)>0){
    for(legValsRange_i in legValsRange){

      range_i <- as.numeric(gsub(",","",unlist(stringr::str_split(legValsRange_i," to ")))); range_i

      data_sf_w_labels <- data_sf_w_labels %>%
        dplyr::mutate(
          label := dplyr::case_when(
          (label == "temp" &
             !!data_sf_w_labels[[fillColumn]] < round(max(range_i),legendDigits) &
             !!data_sf_w_labels[[fillColumn]] >= round(min(range_i),legendDigits)) ~ legValsRange_i,
          TRUE ~ label))
    }

    #Setting labels for values that hit max and min values
    data_sf_w_labels <- data_sf_w_labels %>%
      dplyr::mutate(
        label := dplyr::case_when(
          (label == "temp" & round(!!data_sf_w_labels[[fillColumn]],legendDigits) == round(max(legendBreaksX),legendDigits)) ~ legValsRange[length(legValsRange)],
          (label == "temp" & round(!!data_sf_w_labels[[fillColumn]],legendDigits) == round(min(legendBreaksX),legendDigits)) ~ legValsRange[1],
          TRUE ~ label
        )
      )

    }

    # Set values for greater or less than the scale set
    data_sf_w_labels <- data_sf_w_labels %>%
      dplyr::mutate(
        label := dplyr::case_when(
          (label == "temp" & !!data_sf_w_labels[[fillColumn]] < min(legendBreaksX))~paste0("< ", min(legendBreaksX)),
          (label == "temp" & !!data_sf_w_labels[[fillColumn]] > max(legendBreaksX))~paste0("> ", max(legendBreaksX)),
          TRUE ~ label)
      )

    # Add in any bounds if needed to palette
    labelBounds <- unique(data_sf_w_labels$label); labelBounds
    labelBounds <- labelBounds[grepl(">|<",labelBounds)]; labelBounds

    # If > data_sf point added then add a darker color to the end of palette
    paletteExpand <- grDevices::colorRampPalette(c("white",paletteX,"black"))(length(palette)+10); paletteExpand
    if(any(grepl(">",labelBounds))){
      paletteX <- c(paletteX, paletteExpand[length(paletteExpand)-1]); paletteX
      names(paletteX)[length(paletteX)] <- labelBounds[grepl(">",labelBounds)]; paletteX
    }
    if(any(grepl("<",labelBounds))){
      paletteX <- c(paletteExpand[2],paletteX); paletteX
      names(paletteX)[1] <- labelBounds[grepl("<",labelBounds)]; paletteX
    }

    # If NA value which comes from infinite values in Diff plots
    data_sf_w_labels <- data_sf_w_labels %>%
      dplyr::mutate(
        label := dplyr::case_when(
          (label == "temp" & is.na(!!data_sf_w_labels[[fillColumn]]) ~ paste0("NA")),
          TRUE ~ label)
      )

    if(any(grepl("temp", unique(data_sf_w_labels$label)))){
      #print(data_sf_w_labels %>% as.data_sf.frame() %>% dplyr::filter(grepl("temp",label)))
      stop("Label data not allocated correctly.")
    }

    # Add

  }

  } else {
    data_sf_w_labels <- data_sf %>%
      dplyr::mutate(label = get(fillColumn))
    }

  }

#....................
# Add NA Colors
#....................

  if(showNA) {
    data_sf_w_labels <- data_sf_w_labels %>%
      dplyr::mutate(label = dplyr::if_else(is.na(label), "NA", label))
    paletteX <- c(paletteX, "NA" = colorNA)
  }

#.........................
# Subset to region if provided
#.........................

if(!is.null(region)){
    if(nrow(data_sf_w_labels)>0){
      if(any(grepl("region",names(data_sf_w_labels),ignore.case = T))){
        if(any(region %in% data_sf_w_labels$region%>%unique())){
          data_sf_w_labels <- data_sf_w_labels %>%
            dplyr::filter(region %in% !!region)
        }
      }
    }
  }

#....................
# Plot
#....................

if(T){

  # Convert labels to factors for cat values
  if (!is.null(catPalette)) {
    data_sf_w_labels <- data_sf_w_labels %>%
      dplyr::mutate(label = factor(label, levels = unique(names(paletteX))))
  }

  # UnderLayer
  if (T) {
    if (is.null(underLayer)) {
      underLayer <- ggplot2::ggplot()
    } else if (any(grepl("sf", class(underLayer)))) {

      underLayer_sf <- underLayer

      underLayer <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = underLayer_sf,
          colour = underLayerColor,
          fill = underLayerFill,
          lwd = underLayerLwd,
          alpha = underLayerAlpha
        )

      if (underLayerLabels) {

          if(is.null(underLayerLabelCol)){
            if("subRegion" %in% names(underLayer_sf)){
              underLayerLabelCol = "subRegion"
            } else {
              underLayerLabelCol = names(underLayer_sf)[1]
            }
          }

          underLayer <- underLayer +
            ggrepel::geom_label_repel(
            data = underLayer_sf,
            ggplot2::aes_string(label = underLayerLabelCol, geometry = "geometry"),
            stat = "sf_coordinates",
            colour = labelColor,
            size = labelSize,
            alpha = labelAlpha,
            fill = labelFill,
            label.size = labelBorderSize,
            force = labelRepel)
      }

    } else if (any(grepl("gg", class(underLayer)))) {
      underLayer <- underLayer
    } else {
      underLayer <- ggplot2::ggplot()
    }
  }

  # Plot Shape
  if (T) {

    if(gridded_data){
     colorx = NA
    }else{
      colorx=color
    }

    if (any(grepl("continuous", legendType, ignore.case = T)) & is.numeric(data_sf[[fillColumn]])) {
      map <- underLayer +
        ggplot2::geom_sf(
          data = data_sf_w_labels[, c(fillColumn,row,col)],
          ggplot2::aes_string(fill = fillColumn),
          color = colorx,
          lwd = lwd
        ) +
        ggplot2::scale_fill_gradientn(colors = paletteX, name = legendTitle)
    } else {
      map <- underLayer +
        ggplot2::geom_sf(
          data = data_sf_w_labels[, c("label",row,col)],
          ggplot2::aes_string(fill = "label"),
          color = colorx,
          lwd = lwd
        )

      # Add scales
      if (T) {
        if (any(grepl("numeric", class(data_sf_w_labels[[fillColumn]])))) {
          map <- map +
            ggplot2::scale_fill_manual(
              breaks = names(paletteX),
              values = paletteX,
              drop = F,
              name = legendTitle
            )
        } else if (!is.null(catPalette)) {
          map <- map +
            ggplot2::scale_fill_manual(values = palette, breaks=names(palette),
            drop = F,
            name = legendTitle)
        } else {
          map <- map +
            ggplot2::scale_fill_manual(values = (rep(palette, length(
              unique(data_sf_w_labels$subRegion)
            )))[1:length(unique(data_sf_w_labels$subRegion))],
            drop = F,
            name = legendTitle)
        }
      }

    }
  }

  # Add Labels
  if (labels) {
      map <- map +
        ggrepel::geom_label_repel(
          data = data_sf_w_labels,
          ggplot2::aes_string(label = labelCol, geometry = "geometry"),
          stat = "sf_coordinates",
          colour = labelColor,
          size = labelSize,
          alpha = labelAlpha,
          fill = labelFill,
          label.size = labelBorderSize,
          force = labelRepel)
  }

  # Multi Facet
  if(T){ # Multi Facet

  if(!is.null(data_sf_w_labels)){

  if((!is.null(row) & !is.null(col))){
    if((all(row %in% names(data_sf_w_labels)) & all(col %in% names(data_sf_w_labels)))){

      # Single Col and upto three rows
      # Upto three multifacet rows
      if(length(col)==1){
      if(length(row)==1){
        map <- map +
          ggplot2::facet_grid(get(row[1]) ~ get(col[1]), switch ="y")
        }

      if(length(row)==2){
        map <- map +
          ggplot2::facet_grid(get(row[1]) + get(row[2]) ~ get(col[1]), switch ="y")}

      if(length(row)==3){
        map <- map +
          ggplot2::facet_grid(get(row[1])+ get(row[2])+ get(row[3]) ~ get(col[1]), switch ="y")}
      }

      if(length(col)==2){
        if(length(row)==1){
          map <- map +
            ggplot2::facet_grid(get(row[1]) ~ get(col[1]) +  get(col[2]), switch ="y")}

        if(length(row)==2){
          map <- map +
            ggplot2::facet_grid(get(row[1]) + get(row[2]) ~ get(col[1]) +  get(col[2]), switch ="y")}

        if(length(row)==3){
          map <- map +
            ggplot2::facet_grid(get(row[1])+ get(row[2])+ get(row[3]) ~ get(col[1]) +  get(col[2]), switch ="y")}
      }

      if(length(col)==3){
        if(length(row)==1){
          map <- map +
            ggplot2::facet_grid(get(row[1]) ~ get(col[1]) +  get(col[2]) + get(col[3]), switch ="y")}

        if(length(row)==2){
          map <- map +
            ggplot2::facet_grid(get(row[1]) + get(row[2]) ~ get(col[1]) +  get(col[2]) + get(col[3]), switch ="y")}

        if(length(row)==3){
          map <- map +
            ggplot2::facet_grid(get(row[1])+ get(row[2])+ get(row[3]) ~ get(col[1]) +  get(col[2]) + get(col[3]), switch ="y")}
      }


    }}

    if((!is.null(row) & is.null(col))){
    if(any(row %in% names(data_sf_w_labels))){

      # Upto three multifacet rows
      if(length(row)==1){
        map <- map +
          ggplot2::facet_wrap(get(row[1]) ~ ., ncol = ncol, switch ="y")}

      if(length(row)==2){
        map <- map +
          ggplot2::facet_grid(get(row[1]) + get(row[2]) ~ ., switch ="y")}

      if(length(row)==3){
        map <- map +
          ggplot2::facet_grid(get(row[1])+ get(row[2])+ get(row[3]) ~ ., switch ="y")}

    }
    }

    if((is.null(row) & !is.null(col))){
    if(any(col %in% names(data_sf_w_labels))){

     # Upto three multifacet columns
     if(length(col)==1){
      map <- map +
        ggplot2::facet_wrap(. ~ get(col[1]), ncol = ncol)}

      if(length(col)==2){
        map <- map +
          ggplot2::facet_grid(. ~ get(col[1]) + get(col[2]), switch ="y")}

      if(length(col)==3){
        map <- map +
          ggplot2::facet_grid(. ~ get(col[1]) + get(col[2]) + get(col[3]), switch ="y")}


    }
    }
  }}

  # OverLayer
  if(T){
    if(!is.null(overLayer)){
    if(any(grepl("sf",class(overLayer)))){

      map <- map +
        ggplot2::geom_sf(data = overLayer,
                     colour = overLayerColor,
                     fill = overLayerFill,
                     lwd= overLayerLwd,
                     alpha = overLayerAlpha) +
        ggplot2::coord_sf(expand = FALSE)

      if (overLayerLabels) {

        if(is.null(overLayerLabelCol)){
          if("subRegion" %in% names(overLayer)){
            overLayerLabelCol = "subRegion"
          } else {
            overLayerLabelCol = names(overLayer)[1]
          }
        }

          map <- map +
            ggrepel::geom_label_repel(
              data = overLayer,
              ggplot2::aes_string(label = overLayerLabelCol, geometry = "geometry"),
              stat = "sf_coordinates",
              colour = labelColor,
              size = labelSize,
              alpha = labelAlpha,
              fill = labelFill,
              label.size = labelBorderSize,
              force = labelRepel)
          }
      }

    }
  }

#....................
# Add Titles
#....................

 map <- map + ggplot2::ggtitle(title)

#....................
# Themes
#....................

if(T){

map <- map +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 12),
    panel.background =  ggplot2::element_rect(fill = "transparent",colour = NA)) +
  ggplot2::xlab(NULL) + ggplot2::ylab(NULL) +
  theme


if(background==T){
  map = map +
    ggplot2::theme(
    panel.border = ggplot2::element_rect(color="black",size=0.1, fill = NA),
    panel.background = ggplot2::element_rect(fill="lightcyan2"),
    strip.background = ggplot2::element_rect(color="black",size=0.1, fill="gray30"),
    strip.text = ggplot2::element_text(color = "white"))

}

if(is.character(background)){
  map = map +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color="black",size=0.1, fill = NA),
      panel.background = ggplot2::element_rect(fill=background),
      strip.background = ggplot2::element_rect(color="black",size=0.1, fill="gray30"),
      strip.text = ggplot2::element_text(color = "white"))

}

map <- map +
      ggplot2::theme(text=ggplot2::element_text(size=size))

if(!grepl("continuous",legendType,ignore.case = T)){
  map <- map +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = "black", lwd=0.1)))
      }

if(!legendShow){map = map + ggplot2::guides(fill="none")}
}

# Set Zoom Levels and Crop
if(T){
  if(zoom!=0){crop=T}
  if(is.null(zoomx)){zoomx = zoom}
  if(is.null(zoomy)){zoomy = zoom}

# Set lat lon limits
if(crop){
  bbox_shape <- sf::st_bbox(data_sf_w_labels); bbox_shape
  xMin <- min(180,max(-180,bbox_shape[["xmin"]]+abs(bbox_shape[["xmax"]] - bbox_shape[["xmin"]])*zoomx/10));xMin
  xMax <- max(-180,min(180,bbox_shape[["xmax"]]-abs(bbox_shape[["xmax"]] - bbox_shape[["xmin"]])*zoomx/10));xMax
  yMin <- min(90,max(-90,bbox_shape[["ymin"]]+abs(bbox_shape[["ymax"]] - bbox_shape[["ymin"]])*zoomy/10));yMin
  yMax <- max(-90,min(90,bbox_shape[["ymax"]]-abs(bbox_shape[["ymax"]] - bbox_shape[["ymin"]])*zoomy/10));yMax
  if(xMin>=xMax){xMin<-(bbox_shape[["xmax"]]+bbox_shape[["xmin"]])/2 -1; xMax <- (bbox_shape[["xmax"]]+bbox_shape[["xmin"]])/2 + 1}
  if(yMin>=yMax){yMin<-(bbox_shape[["ymax"]]+bbox_shape[["ymin"]])/2 -1; yMax <- (bbox_shape[["ymax"]]+bbox_shape[["ymin"]])/2 + 1}
  xMin;xMax;yMin;yMax
}

if(crop_to_underLayer){
  if(!is.null(underLayer)){
    bbox_shape <- sf::st_bbox(underLayer_sf)
    xMin <- min(180,max(-180,bbox_shape[["xmin"]]+abs(bbox_shape[["xmax"]] - bbox_shape[["xmin"]])*zoomx/10));xMin
    xMax <- max(-180,min(180,bbox_shape[["xmax"]]-abs(bbox_shape[["xmax"]] - bbox_shape[["xmin"]])*zoomx/10));xMax
    yMin <- min(90,max(-90,bbox_shape[["ymin"]]+abs(bbox_shape[["ymax"]] - bbox_shape[["ymin"]])*zoomy/10));yMin
    yMax <- max(-90,min(90,bbox_shape[["ymax"]]-abs(bbox_shape[["ymax"]] - bbox_shape[["ymin"]])*zoomy/10));yMax
    if(xMin>=xMax){xMin<-(bbox_shape[["xmax"]]+bbox_shape[["xmin"]])/2 -1; xMax <- (bbox_shape[["xmax"]]+bbox_shape[["xmin"]])/2 + 1}
    if(yMin>=yMax){yMin<-(bbox_shape[["ymax"]]+bbox_shape[["ymin"]])/2 -1; yMax <- (bbox_shape[["ymax"]]+bbox_shape[["ymin"]])/2 + 1}
    }
    }

if(crop_to_overLayer){
      if(!is.null(overLayer)){
        bbox_shape <- sf::st_bbox(overLayer);
        xMin <- min(180,max(-180,bbox_shape[["xmin"]]+abs(bbox_shape[["xmax"]] - bbox_shape[["xmin"]])*zoomx/10));xMin
        xMax <- max(-180,min(180,bbox_shape[["xmax"]]-abs(bbox_shape[["xmax"]] - bbox_shape[["xmin"]])*zoomx/10));xMax
        yMin <- min(90,max(-90,bbox_shape[["ymin"]]+abs(bbox_shape[["ymax"]] - bbox_shape[["ymin"]])*zoomy/10));yMin
        yMax <- max(-90,min(90,bbox_shape[["ymax"]]-abs(bbox_shape[["ymax"]] - bbox_shape[["ymin"]])*zoomy/10));yMax
        if(xMin>=xMax){xMin<-(bbox_shape[["xmax"]]+bbox_shape[["xmin"]])/2 -1; xMax <- (bbox_shape[["xmax"]]+bbox_shape[["xmin"]])/2 + 1}
        if(yMin>=yMax){yMin<-(bbox_shape[["ymax"]]+bbox_shape[["ymin"]])/2 -1; yMax <- (bbox_shape[["ymax"]]+bbox_shape[["ymin"]])/2 + 1}
      }
    }

if(crop|crop_to_underLayer|crop_to_overLayer){
  xMin; xMax; yMin; yMax
  map <- map +
    ggplot2::coord_sf(ylim=c(yMin,yMax),
                      xlim=c(xMin,xMax),expand=F)
}
}

# Transform
 if(crs != "+proj=longlat +datum=WGS84 +no_defs"){
   sf::sf_use_s2(FALSE)
   map = map +
     ggplot2::coord_sf(crs = sf::st_crs(crs),expand=F)

   # Crop on transformed layer
   # Set lat lon limits
   if(crop){
     bbox_shape <- sf::st_bbox(sf::st_transform(data_sf_w_labels,sf::st_crs(crs))); bbox_shape
     xMin <- bbox_shape[["xmin"]];xMin
     xMax <- bbox_shape[["xmax"]];xMax
     yMin <- bbox_shape[["ymin"]];yMin
     yMax <- bbox_shape[["ymax"]];yMax
     map <- map +
       ggplot2::coord_sf(crs = sf::st_crs(crs),
                         ylim=c(yMin,yMax),
                         xlim=c(xMin,xMax),expand=F)
   }

   if(crop_to_underLayer){
     if(!is.null(underLayer)){
       bbox_shape <- sf::st_bbox(sf::st_transform(underLayer_sf,sf::st_crs(crs))); bbox_shape
       xMin <- bbox_shape[["xmin"]];xMin
       xMax <- bbox_shape[["xmax"]];xMax
       yMin <- bbox_shape[["ymin"]];yMin
       yMax <- bbox_shape[["ymax"]];yMax
       map <- map +
         ggplot2::coord_sf(crs = sf::st_crs(crs),
                           ylim=c(yMin,yMax),
                           xlim=c(xMin,xMax),expand=F)
     }
   }

   if(crop_to_overLayer){
     if(!is.null(overLayer)){
       bbox_shape <- sf::st_bbox(sf::st_transform(overLayer,sf::st_crs(crs))); bbox_shape
       xMin <- bbox_shape[["xmin"]];xMin
       xMax <- bbox_shape[["xmax"]];xMax
       yMin <- bbox_shape[["ymin"]];yMin
       yMax <- bbox_shape[["ymax"]];yMax
       map <- map +
         ggplot2::coord_sf(crs = sf::st_crs(crs),
                           ylim=c(yMin,yMax),
                           xlim=c(xMin,xMax),expand=F)
       }
   }

 }

#....................
# Print
#....................

if(save){

  if(is.null(folder)){
    folder <- paste(getwd(),"/output",sep="")
  }

  if (!dir.exists(folder)){dir.create(folder)}

fname<-paste(fileName,sep="")

if(nchar(paste(folder,"/",fname,sep=""))>250){
  rlang::inform("Save path for figure larger than 250 characters. Clipping name.")
  rlang::inform(paste("Orig name: ",folder,"/",fname,sep=""))
  rlang::inform(paste("New name: ", folder,"/",strtrim(fname, (250-min(249,nchar(paste(folder,"/",sep=""))))),sep=""))
  fname<-strtrim(fname, (250-nchar(paste(folder,"/",sep=""))))
}

if(!dir.exists(folder)){
  rlang::inform(paste("folder provided: ",folder," does not exist. Saving to: ", getwd(), "/outputsTemp",sep=""))

  if (!dir.exists(paste(getwd(), "/outputsTemp", sep = ""))){
    dir.create(paste(getwd(), "/outputstemp", sep = ""))}

  folder = paste(getwd(), "/outputstemp", sep = "")
}

rmap::printPdfPng(figure=map,
                dir=folder,
                filename=fname,
                width=width,
                height=height,
                pdfpng=pdfpng,
                transparent = transparent)

    if(show){print(map)}

    } else {
    #print("save set to F so no figure will be saved.")
      if(show){print(map)}
    }

}

#....................
# Return
#....................

invisible(map)

}
