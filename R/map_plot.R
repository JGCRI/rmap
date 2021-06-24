#' map_plot
#'
#' This function produce different kinds of maps for the rmap package.
#' Each figure is accompanied with a csv table.
#'
#' @param data Default = NULL
#' @param fileName Default = "map"
#' @param folder Default = paste(getwd(),"/outputs",sep Default = "")
#' @param fillPalette Default = "Spectral"
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
#' @param labelForce Default = 0,
#' @param labelColor Default = "black",
#' @param labelSize Default = 3
#' @param labelAlpha Default = 0.7,
#' @param labelFill Default = NA,
#' @param labelBorderSize Default = NA
#' @param pdfpng Default = "png"
#' @param underLayer Default = NULL
#' @param underLayerColor Default = "gray40"
#' @param underLayerFill Default = "gray40"
#' @param underLayerLwd Default = 0.5
#' @param underLayerAlpha Default = 1
#' @param underLayerLabels Default = F
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
#' @param facetRows Default=NULL
#' @param facetCols Default=NULL
#' @param mapTitle Default=NULL
#' @param numeric2Cat_list Default=NULL
#' @param legendDigitsOverride Default=NULL
#' @param legendSingleColor Default="white"
#' @param legendSingleValue Default=NULL
#' @param colorNA Default = "gray50"
#' @param showNA Default = T
#' @param ncol Default = 3.  Number of columns to wrap maps
#' @param size Default = 12. Text size of plots.
#' @param alpha Default = 1. Transparency of fill colors.
#' @param background Default = F. Add background water color, border and default underlayer map.
#' @param cropToBoundary Default = T. Crop to boundary data.
#' @param transparent Default = F. To make map background transparent for maps without backgrounds.
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @importFrom rlang :=
#' @importFrom jgcricolors jgcricol
#' @export

map_plot<-function(data=NULL,
                  fillColumn=NULL, # Or give column data with
                  theme = NULL,
                  fillPalette="Spectral",
                  labels=F,
                  labelForce = 0,
                  labelColor = "black",
                  labelSize = 3,
                  labelAlpha = 1,
                  labelFill = NA,
                  labelBorderSize = NA,
                  width=9,
                  height=7,
                  legendShow=F,
                  legendTitle=NULL,
                  legendBreaksn=5,
                  legendBreaks=NULL,
                  pdfpng="png",
                  underLayer = NULL,
                  underLayerColor = "gray40",
                  underLayerFill = "gray90",
                  underLayerLwd = 0.1,
                  underLayerAlpha = 1,
                  underLayerLabels = F,
                  overLayerLabels = F,
                  overLayer = NULL,
                  overLayerColor = "gray40",
                  overLayerFill = NA,
                  overLayerLwd = 0.2,
                  overLayerAlpha = 0,
                  zoom = -1,
                  zoomx = NULL,
                  zoomy = NULL,
                  asp = 1.2,
                  save=T,
                  fileName="map",
                  folder=paste(getwd(),"/outputs",sep=""),
                  facetRows=NULL,
                  facetCols=NULL,
                  mapTitle=NULL,
                  numeric2Cat_list=NULL,
                  legendDigits = NULL,
                  legendDigitsOverride=NULL,
                  legendSingleColor ="white",
                  legendSingleValue =NULL,
                  colorNA = "gray50",
                  showNA = F,
                  ncol = 3,
                  size = 12,
                  alpha = 1,
                  background = F,
                  cropToBoundary = T,
                  transparent = F
                  ){

  # data=NULL
  # fillColumn=NULL # Or give column data with
  # theme = ggplot2::theme_bw()
  # fillPalette="Spectral"
  # labels=F
  # width=9
  # height=7
  # legendShow=F
  # legendTitle=NULL
  # legendBreaksn=5
  # legendBreaks=NULL
  # pdfpng="png"
  # underLayer=NULL
  # save=T
  # fileName="map"
  # folder=paste(getwd(),"/outputs",sep="")
  # facetRows=NULL
  # facetCols=NULL
  # mapTitle=NULL
  # numeric2Cat_list=NULL
  # legendDigits = NULL
  # legendDigitsOverride=NULL
  # legendSingleColor ="white"
  # legendSingleValue =NULL
  # underLayer = NULL
  # underLayerColor = "gray30"
  # underLayerFill = "gray90"
  # underLayerLwd = 0.5
  # underLayerAlpha = 1
  # overLayer = NULL
  # overLayerColor = "gray40"
  # overLayerFill = NA
  # overLayerLwd = 0.5
  # overLayerAlpha = 0
  # zoom = -1
  # zoomx = NULL
  # zoomy = NULL
  # asp=1.2
  # cropToBoundary=F

# ALT + 0 here to collapse all code into manageable chunks
# ALT + Shift + O to expand all

#.........................
# Initialize variables to remove binding errors if needed
# .........................

if(T){ # Initialize
  NULL->raster->shape->map->checkFacets->catBreaks->catLabels->catPalette->legendLabelsX->
    singleValLoc->label->catParam->long->lat->group->dataShape->dataPolygon->dataGrid->data_shape->
    lon->hole->piece->subRegion->X1->X2->id->name

  if(!is.null(legendTitle)){
    legendTitle=gsub(" ","\n",legendTitle)

    if(any(tolower(legendTitle) %in% c("legend","units","unit"))){
      legendTitle = NULL
    }
  }

  if(!is.null(legendBreaks)){legendBreaks<-sort(legendBreaks)}

  fillPaletteOrig <- fillPalette

  if(!is.null(theme)){
  if(!any(grepl("theme",class(theme),ignore.case = T))){
    print("Theme provide is not a ggplot theme. Ignoring theme.")
    theme = NULL
  }}


  # Convert UnderLayer to df
  if(any(grepl("SpatialPolygonsDataFrame",class(underLayer)))){
    underLayer <- shape_to_df(underLayer) %>%
      dplyr::filter(subRegion %in% unique(underLayer@data$subRegion))
  }

  # COnvert OverLayer to df
  if(any(grepl("SpatialPolygonsDataFrame",class(overLayer)))){
    overLayer <- shape_to_df(overLayer) %>%
      dplyr::filter(subRegion %in% unique(overLayer@data$subRegion))
  }


} # initialize

#......................................................................
# Read data and check inputs
#......................................................................

if(T){ # Read input data

# Assign Data to correct type dataGrid, dataPolygon or dataShape
if(!is.null(data)){

  if(!"value" %in% names(data)){
    dataShape <- data
  } else if(any(grepl("lat|lon",names(data)))){
    dataGrid <- data
  } else if(any(grepl("subRegion",names(data))) & any(!grepl("lat|lon",names(data)))){
    data_shape <- rmap::map_find_df(data) %>%
      dplyr::select(lon, lat, order, hole, piece, group, subRegion, name) %>%
      dplyr::filter(subRegion %in% unique(data$subRegion))
    dataPolygon <-  data_shape %>%
      dplyr::filter(subRegion %in% unique(data$subRegion)) %>%
      dplyr::left_join(data,by="subRegion")
  } else {
    stop("Data provided in not in the correct format. Data should be a shapefile or fortified dataframe.")
  }

}

  if(!is.null(dataShape)){
    datax<-dataShape
    datax1 <- datax
  }

if(!is.null(dataPolygon)){
  datax<-dataPolygon
  }

if(!is.null(dataGrid)){
   if(!(any(grepl("lat|latitude",names(dataGrid),ignore.case=T)) & any(grepl("lon|longitude",names(dataGrid),ignore.case=T)))){
     stop("dataGrid must have lat and lon columns")} else{

        if(any(grepl("\\<lat\\>",names(dataGrid),ignore.case = T))){
          dataGrid <- dataGrid %>% dplyr::rename(!!"lat" := (names(dataGrid)[grepl("\\<lat\\>",names(dataGrid),ignore.case = T)])[1])}
       if(any(grepl("\\<latitude\\>",names(dataGrid),ignore.case = T))){
         dataGrid <- dataGrid %>% dplyr::rename(!!"lat" := (names(dataGrid)[grepl("\\<latitude\\>",names(dataGrid),ignore.case = T)])[1])}
       if(any(grepl("\\<lon\\>",names(dataGrid),ignore.case = T))){
         dataGrid <- dataGrid %>% dplyr::rename(!!"lon" := (names(dataGrid)[grepl("\\<lon\\>",names(dataGrid),ignore.case = T)])[1])}
       if(any(grepl("\\<longitude\\>",names(dataGrid),ignore.case = T))){
         dataGrid <- dataGrid %>% dplyr::rename(!!"lon" := (names(dataGrid)[grepl("\\<longitude\\>",names(dataGrid),ignore.case = T)])[1])}

       datax = dataGrid
     }
   }

# Set FillPalette
if(length(fillPalette)==1){
  if(fillPalette %in% names(jgcricolors::jgcricol())){
    fillPalette<-jgcricolors::jgcricol()[[fillPalette]]}else{
      if(!is.na(RColorBrewer::brewer.pal.info[fillPalette,]$maxcolors)){
        fillPalette <- RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[fillPalette,]$maxcolors,fillPalette)}
    }}

if(!is.null(numeric2Cat_list)){
     if(all(c("numeric2Cat_param","numeric2Cat_breaks","numeric2Cat_labels","numeric2Cat_palette","numeric2Cat_legendTextSize") %in% names(numeric2Cat_list))){
       if(catParam %in% unique(unlist(numeric2Cat_list$numeric2Cat_param))) {
       list_index <- which(numeric2Cat_list$numeric2Cat_param==catParam)
       catBreaks <- numeric2Cat_list$numeric2Cat_breaks[[list_index]]
       catLabels <- numeric2Cat_list$numeric2Cat_labels[[list_index]]
       if(grepl("c\\(",numeric2Cat_list$numeric2Cat_palette[[list_index]][1])){
         catPalette <- eval(parse(text=paste(numeric2Cat_list$numeric2Cat_palette[[list_index]])))}else{
           catPalette <- numeric2Cat_list$numeric2Cat_palette[[list_index]]}

       legendTextSize <- numeric2Cat_list$numeric2Cat_legendTextSize[[list_index]]
       }
     } else {print("numerc2Cat_list does not contain the appropriate sublists: 'numeric2Cat_param','numeric2Cat_breaks','numeric2Cat_labels','numeric2Cat_catPalette'. Skipping conversion to Categorical")}
}

# If categorical data then set as factor for datax
if(!is.null(datax)){
  if(!is.null(catBreaks) & !is.null(catLabels)){

    if(!is.null(catPalette)){
      if(length(catPalette)>1){
        fillPalette <- c(catPalette,jgcricolors::jgcricol()$pal_16)
      }else{
      if(catPalette %in% names(jgcricolors::jgcricol())){
        fillPalette <- jgcricolors::jgcricol()[[catPalette]]}
      }
  }


    for(i in 1:length(fillColumn)){
      fillColumn_i <- fillColumn[i]

      if(is.numeric(datax[[fillColumn_i]])){

        legendBreaks <- NULL

        datax[[fillColumn_i]] <- cut( datax[[fillColumn_i]],
                                           breaks=catBreaks,
                                           labels=catLabels)
      }


      if(any(unique(datax[[fillColumn_i]]) %in% names(fillPalette))){
        # fillPalette<-fillPalette[1:min(length(catPalette),
        #                                length(fillPalette))]
        datax %>%
          dplyr::mutate(!!fillColumn_i := factor(datax[[fillColumn_i]],
                                                 levels = names(fillPalette)))->
          datax
      } else { datax %>%
          dplyr::mutate(!!fillColumn_i := as.factor(datax[[fillColumn_i]])) -> datax}
    }
  }
}

}

#.........................
# Remove Inf values
#.........................

if(T){ # Remove error values

if(is.null(dataShape)){
if(!is.null(datax)){
  if(nrow(datax)>0){
    datax[mapply(is.infinite, datax)] <- NA
  }
  if(is.null(fillColumn)){
    if("subRegion" %in% names(datax)){
      fillColumn = "subRegion"
      }
    }
}
}
  }

#....................
# Set Legend Breaks and Labels
#....................

if(T){
  if(is.null(dataShape)){

  # Setting Legend Breaks
  if(T){

    if(is.null(legendBreaks)){
      if(length(scales::pretty_breaks(n=legendBreaksn)(datax[[fillColumn]]))>1){
      legendBreaks=scales::pretty_breaks(n=legendBreaksn)(datax[[fillColumn]])
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
    if(is.null(catPalette)){
      if(!is.null(legendDigits)){
          if(length(legendBreaks)>1){

            # Test Palette
            if(T){
              # Legend Labels
              if(T){

                legendBreaksX <- legendBreaks; legendBreaksX

                # New Breaks
                if(is.null(legendSingleValue)){
                  if(max(legendBreaksX)<0){
                    legendSingleValueX=ceiling(max(legendBreaksX))
                  }else{
                    if(min(legendBreaksX)>0){
                      legendSingleValueX=floor(min(legendBreaksX))
                    }else{
                      legendSingleValueX=0
                    }
                  }
                }else{legendSingleValueX=legendSingleValue}; legendSingleValueX


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
                        legendBreaksX <- c(legendBreaksX[1:match(max(legendBreaksX[legendBreaksX<0]),legendBreaksX)],
                                           legendSingleValueX,
                                           legendBreaksX[(match(max(legendBreaksX[legendBreaksX<0]),legendBreaksX)+1):(length(legendBreaksX))])
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
                #graphics::pie(rep(1,length(fillPalette)),label=names(fillPalette),col=fillPalette);fillPalette
                fillColUp<-fillPalette[(round(length(fillPalette)/2,0)+2):length(fillPalette)];fillColUp
                fillColUp <- grDevices::colorRampPalette(c("white",fillColUp))(11)[-1];fillColUp
                #graphics::pie(rep(1,length(fillColUp)),label=names(fillColUp),col=fillColUp)
                fillColDown<-rev(fillPalette[1:(round(length(fillPalette)/2,0)-1)])
                fillColDown <- grDevices::colorRampPalette(c("white",fillColDown))(11)[-1];fillColDown
                #graphics::pie(rep(1,length(fillColDown)),label=names(fillColDown),col=fillColDown)

                # If all less than single color chosen then colDown, if vice versa then colUp else full palette
                if(max(legendBreaksX)<=legendSingleValueX){
                  if(any(grepl("diff|div|ratio",fillPaletteOrig,ignore.case=T))){
                    fillPaletteX<-rev(grDevices::colorRampPalette(fillColDown)(length(legendLabelsX)-1))
                  }else{
                    fillPaletteX<-rev(grDevices::colorRampPalette(fillPalette[-1])(length(legendLabelsX)-1))
                  }
                }else{
                  if(min(legendBreaksX)>=legendSingleValueX){
                    if(any(grepl("diff|div|ratio",fillPaletteOrig,ignore.case=T))){
                      fillPaletteX<-grDevices::colorRampPalette(fillColUp)(length(legendLabelsX)-1)
                    }else{
                      fillPaletteX<-grDevices::colorRampPalette(fillPalette[-1])(length(legendLabelsX)-1)
                    }
                  }else{
                    if(singleValLoc==length(legendLabelsX)){fillPaletteXUp<-c()}else{
                      fillPaletteXUp <- grDevices::colorRampPalette(fillColUp)(round((length(legendLabelsX)-singleValLoc),0))};fillPaletteXUp
                    if(singleValLoc==1){fillPaletteXDown<-c()}else{
                      fillPaletteXDown <- rev(grDevices::colorRampPalette(fillColDown)(singleValLoc))};fillPaletteXDown
                    fillPaletteX <-c(fillPaletteXDown,fillPaletteXUp)
                  }
                }

                # Visualize Palette
                #if(length(fillPaletteX)>0){
                #graphics::pie(rep(1,length(fillPaletteX)),label=names(fillPaletteX),col=fillPaletteX)
                #  }

                # Add in the singleColor
                if(min(legendBreaksX)>=legendSingleValueX){
                  fillPaletteX<-c(paste(legendSingleColor,sep=""),
                                  fillPaletteX)
                }else{
                  if(max(legendBreaksX)<=legendSingleValueX){
                    fillPaletteX<-c(fillPaletteX,paste(legendSingleColor,sep=""))
                  }else{
                    fillPaletteX<-c(fillPaletteX[1:(singleValLoc-1)],
                                    paste(legendSingleColor,sep=""),
                                    fillPaletteX[(singleValLoc+1):length(fillPaletteX)])
                  }
                }

                fillPaletteX;#graphics::pie(rep(1,length(fillPaletteX)),label=1:length(fillPaletteX),col=fillPaletteX)

              }

              length(fillPaletteX); length(legendLabelsX)

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

              legendBreaksnX = length(legendBreaksX)
              length(legendBreaksX);length(legendLabelsX);legendBreaksX;legendLabelsX

              # Assign new Labels to Palette
              if(length(fillPaletteX)>0){
                if(length(fillPaletteX)==length(legendLabelsX)){
                  names(fillPaletteX)<-legendLabelsX
                }else{
                  fillPaletteX=fillPalette
                }
                #graphics::pie(rep(1,length(fillPaletteX)),label=names(fillPaletteX),col=fillPaletteX)
              }else{fillPaletteX=fillPalette}
            } # Test Palette

          }else{
            legendBreaksnX=1
            legendBreaksX=legendBreaks
            legendLabelsX=NULL
            fillPaletteX=fillPalette
          }
    }else{
      legendBreaksnX=legendBreaksn
      legendBreaksX=legendBreaks
      legendLabelsX=NULL
      fillPaletteX=fillPalette
    }

    }
  }

  # Setting labels for data
  if(T){
    legVals <- names(fillPaletteX); legVals
    legValsSingle <- legVals[!grepl("to",legVals)]; legValsSingle
    legValsRange <- legVals[grepl("to",legVals)]; legValsRange

    datax1 <- datax

    for(legValsSingle_i in legValsSingle){
      datax1 <- datax1 %>%
        dplyr::mutate(label := dplyr::if_else(round(!!datax1[[fillColumn]],legendDigits)==as.numeric(as.character(legValsSingle)),
                                       legValsSingle,
                                       "temp"))
    }

    for(legValsRange_i in legValsRange){

      range_i <- as.numeric(gsub(",","",unlist(stringr::str_split(legValsRange_i," to ")))); range_i

      datax1 <- datax1 %>%
        dplyr::mutate(
          label := dplyr::if_else(
          (round(!!datax1[[fillColumn]],legendDigits) >= 0 &
             round(!!datax1[[fillColumn]],legendDigits) <= max(range_i) &
             round(!!datax1[[fillColumn]],legendDigits) > min(range_i)),
          legValsRange_i,
          label),
          label := dplyr::if_else(
            (round(!!datax1[[fillColumn]],legendDigits) < 0 &
               round(!!datax1[[fillColumn]],legendDigits) < max(range_i) &
               round(!!datax1[[fillColumn]],legendDigits) >= min(range_i)),
            legValsRange_i,
            label))
    }

    datax1 <- datax1 %>%
      dplyr::mutate(
        label := dplyr::case_when(
          !!datax1[[fillColumn]] == max(!!datax1[[fillColumn]]) ~ legValsRange[length(legValsRange)],
          !!datax1[[fillColumn]] == min(!!datax1[[fillColumn]]) ~ legValsRange[1],
          TRUE ~ label
        )
      )

    if(any(grepl("temp", unique(datax1$label)))){
      print(datax1%>%dplyr::filter(grepl("temp",label)))
      stop("Label data not allocated correctly. Check map_plot.R line 561.")
      }

  }

  # Add NA colors
  if(T){
    # Add NA Value Colors
    if(showNA){
    datax1 <- datax1 %>%
      dplyr::mutate(label=dplyr::if_else(is.na(label),"NA",label))
    fillPaletteX <- c(fillPaletteX,"NA"=colorNA)
    }
  }
  }
}

#....................
# Plot
#....................

if(!is.null(datax)){

  # Convert labels to factors
  if(T){
    if(is.null(dataShape)){
    datax1 <- datax1 %>%
      dplyr::mutate(label = factor(label,levels=unique(names(fillPaletteX))))
    }
  }

  # UnderLayer
  if(T){
  if(is.null(underLayer)){
    if(!is.null(data_shape)){
      underLayer <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = data_shape,
                              ggplot2::aes(x = lon, y = lat, group = group),
                              colour = underLayerColor,
                              fill = underLayerFill,
                              lwd= underLayerLwd,
                              alpha = underLayerAlpha) +
        ggplot2::coord_fixed(ratio = asp)

      if(underLayerLabels){
        shapex <- rmap::df_to_shape(data_shape)
        if(!is.null(shapex)){
        labels_df_under =  shapex@data %>%
          tibble::as_tibble() %>%
          dplyr::bind_cols(sp::coordinates(shapex) %>%
                             data.frame() %>% dplyr::rename(lon=X1,lat=X2)) %>%
          dplyr::filter(subRegion %in% unique(data_shape$subRegion))


        if(labelForce != 0){
          map <- map +
            ggrepel::geom_label_repel(data=labels_df_under, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                                      colour = labelColor,
                                      size=labelSize,
                                      alpha=labelAlpha,
                                      fill = labelFill,
                                      label.size = labelBorderSize,
                                      force = labelForce)}else{

                                        map <- map +
                                          ggplot2::geom_label(data=labels_df_under, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                                                              colour = labelColor,
                                                              size=labelSize,
                                                              alpha=labelAlpha,
                                                              fill = labelFill,
                                                              label.size = labelBorderSize)
                                      }
        }
      }

    }else{
      underLayer <- ggplot2::ggplot()
    }
  } else if(any(grepl("tbl_df|tbl|data.frame",class(underLayer)))){

        underLayerx <- underLayer

        underLayer <- ggplot2::ggplot() +
          ggplot2::geom_polygon(data = underLayer,
                       ggplot2::aes(x = lon, y = lat, group = group),
                       colour = underLayerColor,
                       fill = underLayerFill,
                       lwd= underLayerLwd,
                       alpha = underLayerAlpha) +
          ggplot2::coord_fixed(ratio = asp)

        if(underLayerLabels){
          shapex <- rmap::df_to_shape(underLayerx)
          if(!is.null(shapex)){
          labels_df_under =  shapex@data %>%
            tibble::as_tibble() %>%
            dplyr::bind_cols(sp::coordinates(shapex) %>%
                               data.frame() %>% dplyr::rename(lon=X1,lat=X2))%>%
            dplyr::filter(subRegion %in% unique(underLayerx$subRegion))

          if(labelForce != 0){
            underLayer <- underLayer +
              ggrepel::geom_label_repel(data=labels_df_under, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                                        colour = labelColor,
                                        size=labelSize,
                                        alpha=labelAlpha,
                                        fill = labelFill,
                                        label.size = labelBorderSize,
                                        force = labelForce)}else{

                                          underLayer <- underLayer +
                                            ggplot2::geom_label(data=labels_df_under, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                                                                colour = labelColor,
                                                                size=labelSize,
                                                                alpha=labelAlpha,
                                                                fill = labelFill,
                                                                label.size = labelBorderSize)
                                        }
          }
        }

  } else if(any(grepl("gg",class(underLayer)))){
      underLayer <- underLayer
  } else {
    underLayer <- ggplot2::ggplot()
    }
  }

  # If grid
  if(!is.null(dataGrid)){

  # Zoom out
  if(is.null(zoomx)){zoomx = zoom}
  if(is.null(zoomy)){zoomy = zoom}
  lonLimMin <- min(datax1$lon)-abs(min(datax1$lon))*(-1)*zoomx/10;lonLimMin
  lonLimMax <- max(datax1$lon)+abs(max(datax1$lon))*(-1)*zoomx/10;lonLimMax
  latLimMin <- min(datax1$lat)-abs(min(datax1$lat))*(-1)*zoomy/10;latLimMin
  latLimMax <- max(datax1$lat)+abs(max(datax1$lat))*(-1)*zoomy/10;latLimMax

  map <- underLayer +
    ggplot2::geom_tile(data=datax1, ggplot2::aes_string(x="lon", y="lat", fill="label"), alpha=alpha) +
    ggplot2::coord_fixed(ratio = asp,
                         ylim=c(max(latLimMin,-90),min(latLimMax,90)),
                         xlim=c(max(-180,lonLimMin),min(lonLimMax,180)),
                         expand = c(0, 0)) +
    ggplot2::scale_fill_manual(breaks=names(fillPaletteX), values=fillPaletteX, drop=F,
                               name = legendTitle)
  }

  # If polygon
  if(!is.null(dataPolygon)){
    map <- underLayer +
      ggplot2::geom_polygon(data = datax1,
                            ggplot2::aes_string(x="lon", y="lat", group="group",fill="label"),
                            color = "grey40", lwd=0.1) +
      ggplot2::coord_fixed(ratio = asp) +
      ggplot2::scale_fill_manual(breaks=names(fillPaletteX), values=fillPaletteX, drop=F,
                                 name = legendTitle)

    if(labels){
      shapex <- rmap::df_to_shape(data_shape)
      if(!is.null(shapex)){
        labels_df_shape =  shapex@data %>%
          tibble::as_tibble() %>%
          dplyr::bind_cols(sp::coordinates(shapex) %>%
                             data.frame() %>% dplyr::rename(lon=X1,lat=X2))%>%
          dplyr::filter(subRegion %in% unique(data_shape$subRegion))

        if(labelForce != 0){
          map <- map +
            ggrepel::geom_label_repel(data=labels_df_shape, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                                      colour = labelColor,
                                      size=labelSize,
                                      alpha=labelAlpha,
                                      fill = labelFill,
                                      label.size = labelBorderSize,
                                      force = labelForce)}else{

                                        map <- map +
                                          ggplot2::geom_label(data=labels_df_shape, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                                                              colour = labelColor,
                                                              size=labelSize,
                                                              alpha=labelAlpha,
                                                              fill = labelFill,
                                                              label.size = labelBorderSize)
                                      }
      }
    }

  }

  # If shape
  if(!is.null(dataShape)){
    map <- underLayer +
      ggplot2::geom_polygon(data = dataShape,
                            ggplot2::aes_string(x="lon", y="lat", group="group",fill="subRegion"),
                            color = "grey40", lwd=0.1) +
      ggplot2::coord_fixed(ratio = asp) +
      ggplot2::scale_fill_manual(values=(rep(fillPalette,length(unique(dataShape$subRegion))))[1:length(unique(dataShape$subRegion))],
                                 drop=F,
                                 name = legendTitle)

    if(labels){
      shapex <- rmap::df_to_shape(dataShape)
      if(!is.null(shapex)){
      labels_df_shape =  shapex@data %>%
        tibble::as_tibble() %>%
        dplyr::bind_cols(sp::coordinates(shapex) %>%
                           data.frame() %>% dplyr::rename(lon=X1,lat=X2))%>%
        dplyr::filter(subRegion %in% unique(dataShape$subRegion))

      if(labelForce != 0){
        map <- map +
          ggrepel::geom_label_repel(data=labels_df_shape, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                                    colour = labelColor,
                                    size=labelSize,
                                    alpha=labelAlpha,
                                    fill = labelFill,
                                    label.size = labelBorderSize,
                                    force = labelForce)}else{

                                      map <- map +
                                        ggplot2::geom_label(data=labels_df_shape, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                                                            colour = labelColor,
                                                            size=labelSize,
                                                            alpha=labelAlpha,
                                                            fill = labelFill,
                                                            label.size = labelBorderSize)
                                    }
      }
    }

    legendShow = F
  }

  # Multi Facet
  if(T){ # Multi Facet

  if(is.null(dataShape)){

  if((!is.null(facetRows) & !is.null(facetCols))){
    if((all(facetRows %in% names(datax1)) & all(facetCols %in% names(datax1)))){

      # Single Col and upto three rows
      # Upto three multifacet rows
      if(length(facetCols)==1){
      if(length(facetRows)==1){
        map <- map +
          ggplot2::facet_grid(get(facetRows[1]) ~ get(facetCols[1]))}

      if(length(facetRows)==2){
        map <- map +
          ggplot2::facet_grid(get(facetRows[1]) + get(facetRows[2]) ~ get(facetCols[1]))}

      if(length(facetRows)==3){
        map <- map +
          ggplot2::facet_grid(get(facetRows[1])+ get(facetRows[2])+ get(facetRows[3]) ~ get(facetCols[1]))}
      }

      if(length(facetCols)==2){
        if(length(facetRows)==1){
          map <- map +
            ggplot2::facet_grid(get(facetRows[1]) ~ get(facetCols[1]) +  get(facetCols[2]))}

        if(length(facetRows)==2){
          map <- map +
            ggplot2::facet_grid(get(facetRows[1]) + get(facetRows[2]) ~ get(facetCols[1]) +  get(facetCols[2]))}

        if(length(facetRows)==3){
          map <- map +
            ggplot2::facet_grid(get(facetRows[1])+ get(facetRows[2])+ get(facetRows[3]) ~ get(facetCols[1]) +  get(facetCols[2]))}
      }

      if(length(facetCols)==3){
        if(length(facetRows)==1){
          map <- map +
            ggplot2::facet_grid(get(facetRows[1]) ~ get(facetCols[1]) +  get(facetCols[2]) + get(facetCols[3]))}

        if(length(facetRows)==2){
          map <- map +
            ggplot2::facet_grid(get(facetRows[1]) + get(facetRows[2]) ~ get(facetCols[1]) +  get(facetCols[2]) + get(facetCols[3]))}

        if(length(facetRows)==3){
          map <- map +
            ggplot2::facet_grid(get(facetRows[1])+ get(facetRows[2])+ get(facetRows[3]) ~ get(facetCols[1]) +  get(facetCols[2]) + get(facetCols[3]))}
      }


    }}

    if((!is.null(facetRows) & is.null(facetCols))){
    if((facetRows %in% names(datax1))){

      # Upto three multifacet rows
      if(length(facetRows)==1){
        map <- map +
          ggplot2::facet_grid(get(facetRows[1]) ~ ., ncol = ncol)}

      if(length(facetRows)==2){
        map <- map +
          ggplot2::facet_grid(get(facetRows[1]) + get(facetRows[2]) ~ .)}

      if(length(facetRows)==3){
        map <- map +
          ggplot2::facet_grid(get(facetRows[1])+ get(facetRows[2])+ get(facetRows[3]) ~ .)}

    }
    }

    if((is.null(facetRows) & !is.null(facetCols))){
    if((facetCols %in% names(datax1))){

     # Upto three multifacet columns
     if(length(facetCols)==1){
      map <- map +
        ggplot2::facet_wrap(. ~ get(facetCols[1]), ncol = ncol)}

      if(length(facetCols)==2){
        map <- map +
          ggplot2::facet_grid(. ~ get(facetCols[1]) + get(facetCols[2]))}

      if(length(facetCols)==3){
        map <- map +
          ggplot2::facet_grid(. ~ get(facetCols[1]) + get(facetCols[2]) + get(facetCols[3]))}


    }
    }
  }}

  # OverLayer
  if(T){
    if(!is.null(overLayer)){
    if(any(grepl("tbl_df|tbl|data.frame",class(overLayer)))){
      map <- map +
        ggplot2::geom_polygon(data = overLayer,
                     ggplot2::aes(x = lon, y = lat, group = group),
                     colour = overLayerColor,
                     fill = overLayerFill,
                     lwd= overLayerLwd,
                     alpha = overLayerAlpha) +
        ggplot2::coord_fixed(ratio = asp)

      if(overLayerLabels){
        shapex <- rmap::df_to_shape(overLayer)
        if(!is.null(shapex)){
        labels_df_over =  shapex@data %>%
          tibble::as_tibble() %>%
          dplyr::bind_cols(sp::coordinates(shapex) %>%
                             data.frame() %>% dplyr::rename(lon=X1,lat=X2))%>%
          dplyr::filter(subRegion %in% unique(overLayer$subRegion))

        if(cropToBoundary){
        labels_df_over <- labels_df_over %>%
            dplyr::filter(lat < max(datax1$lat),
                          lat > min(datax1$lat),
                          lon < max(datax1$lon),
                          lon > min(datax1$lon))}

        if(labelForce != 0){
        map <- map +
          ggrepel::geom_label_repel(data=labels_df_over, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                               colour = labelColor,
                                size=labelSize,
                                alpha=labelAlpha,
                                fill = labelFill,
                                label.size = labelBorderSize,
                                force = labelForce)}else{

                                  map <- map +
                                    ggplot2::geom_label(data=labels_df_over, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                                                              colour = labelColor,
                                                              size=labelSize,
                                                              alpha=labelAlpha,
                                                              fill = labelFill,
                                                              label.size = labelBorderSize)
                                }



        }
      }

      }
    }
  }

}

#....................
# Add Titles
#....................

 map <- map +
   ggplot2::ggtitle(mapTitle)

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
    strip.text = ggplot2::element_text(size = 12),
    panel.background =  ggplot2::element_rect(fill = "transparent",colour = NA)) +
  ggplot2::xlab(NULL) + ggplot2::ylab(NULL) +
  theme


if(background){
  map = map +
    ggplot2::theme(
    panel.border = ggplot2::element_rect(color="black",size=0.1, fill = NA),
    panel.background = ggplot2::element_rect(fill="lightcyan2"))
}

map <- map +
  ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = "black", lwd=0.1))) +
  ggplot2::theme(text=ggplot2::element_text(size=size))

if(!legendShow){map = map + ggplot2::guides(fill=FALSE)}

if(cropToBoundary==T){
  map <- map +
    ggplot2::coord_fixed(ratio = asp,
                         xlim = c(min(datax1$lon),max(datax1$lon)),
                         ylim = c(min(datax1$lat),max(datax1$lat)))
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
  print("Save path for figure larger than 250 characters. Clipping name.")
  print(paste("Orig name: ",folder,"/",fname,sep=""))
  print(paste("New name: ", folder,"/",strtrim(fname, (250-min(249,nchar(paste(folder,"/",sep=""))))),sep=""))
  fname<-strtrim(fname, (250-nchar(paste(folder,"/",sep=""))))
}

if(!dir.exists(folder)){
  print(paste("folder provided: ",folder," does not exist. Saving to: ", getwd(), "/outputsTemp",sep=""))

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
    print(map)

    } else {
    #print("save set to F so no figure will be saved.")
    print(map)
    }

#....................
# Return
#....................

invisible(map)

}
