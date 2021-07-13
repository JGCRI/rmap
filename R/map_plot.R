#' map_plot
#'
#' This function produce different kinds of maps for the rmap package.
#' Each figure is accompanied with a csv table.
#'
#' @param data Default = NULL
#' @param fileName Default = "map"
#' @param shape Default = NULL, Cusotm shape can be provided as a SpatialPolygonDataFrame with features corresponding to subRegion columns in the data provided.
#' @param folder Default = paste(getwd(),"/outputs",sep Default = "")
#' @param palette Default = "Spectral"
#' @param show Default = T. Print maps in console as they are processed.
#' @param theme Default = NULL
#' @param fillColumn Default = NULL # Or give column with data
#' @param shapeColumn (Optional). Default = NULL. If different from subRegion.
#' @param width Default = 9
#' @param height Default = 7
#' @param legendShow Default = F
#' @param legendDigits Default = NULL
#' @param legendTitle Default = NULL
#' @param legendBreaksn Default = "5"
#' @param legendBreaks Default = NULL
#' @param labels Default = FALSE
#' @param labelRepel Default = 0,
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
#' @param background Default = F. Add background water color, border and default underlayer map.
#' @param crop Default = T. Crop to boundary data.
#' @param transparent Default = F. To make map background transparent for maps without backgrounds.
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @importFrom rlang :=
#' @importFrom jgcricolors jgcricol
#' @export

map_plot<-function(data=NULL,
                  fillColumn=NULL, # Or give column data with
                  shapeColumn=NULL,
                  shape = NULL,
                  theme = NULL,
                  show = T,
                  palette="Spectral",
                  labels=F,
                  labelRepel = 0,
                  labelColor = "black",
                  labelSize = 2,
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
                  transparent = F
                  ){

  # data=NULL
  # fillColumn=NULL # Or give column data with
  # shapeColumn=NULL
  # theme = ggplot2::theme_bw()
  # palette="Spectral"
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
  # row=NULL
  # col=NULL
  # title=NULL
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
  # crop=F

# ALT + 0 here to collapse all code into manageable chunks
# ALT + Shift + O to expand all

#.........................
# Initialize variables to remove binding errors if needed
# .........................

if(T){ # Initialize
  NULL->raster->map->checkFacets->catBreaks->catLabels->catPalette->legendLabelsX->
    singleValLoc->label->long->lat->group->dataShape->dataPolygon->dataGrid->data_shape->
    lon->hole->piece->subRegion->X1->X2->id->name->value

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
    print("Theme provide is not a ggplot theme. Ignoring theme.")
    theme = NULL
  }}


  # Convert UnderLayer to df
  if(any(grepl("SpatialPolygonsDataFrame",class(underLayer)))){
    underLayer <- shape_to_df(shape=underLayer, shapeColumn = shapeColumn)
  }

  # COnvert OverLayer to df
  if(any(grepl("SpatialPolygonsDataFrame",class(overLayer)))){
    overLayer <- shape_to_df(shape=overLayer, shapeColumn = shapeColumn)
  }


} # initialize

#......................................................................
# Read data and check inputs
#......................................................................

if(T){ # Read input data

# Assign Data to correct type dataGrid, dataPolygon or dataShape
if(!is.null(data)){

  if(is.null(fillColumn)){
    if(!("value" %in% names(data))){
      dataShape <- data
    }
  } else if(!("value" %in% names(data)) & !(fillColumn %in% names(data))){
    dataShape <- data
  } else if(any(grepl("lat|lon",names(data)))){
    dataGrid <- data
  } else if(any(grepl("subRegion",names(data))) & any(!grepl("lat|lon",names(data)))){

    # If shape provided then use shape
    if(!is.null(shape)){
      # If SpatialPolygonsDataFrame then convert to dataframe using broom
      if(any(grepl("SpatialPolygonsDataFrame",class(shape)))){

        # Check that shape file has relevant columns
        if(!any("subRegion" %in% names(shape@data))){
          stop("shape provided must be a SpatialPolygonsDataFrame and have a column named 'subRegion' in shape@data. ")
        }

        shape <- shape[shape@data$subRegion %in% unique(data$subRegion),]
        shape@data <- shape@data %>% droplevels()

        data_shape <- broom::tidy(shape, region="subRegion") %>%
          dplyr::rename(subRegion=id)%>%
          dplyr::inner_join(shape@data, by="subRegion") %>%
          dplyr::rename(lon=long) %>%
          dplyr::mutate(name = paste0("shapedf"));

      } else if(any(grepl("tbl_df|tbl|data.frame",class(shape)))){

        # Check that shape file has relevant columns
        if(!any("subRegion" %in% names(shape))){
          stop("shape provided must have a column named 'subRegion'")
        }

        data_shape <- shape

      } else {
        stop("shape provided must be a SpatialPolygonsDataFrame or a fortified (broom) dataframe and have a column named 'subRegion' in its data.")
      }

      # Make sure subRegions in data are present in shape provided
      if(!any(unique(data_shape$subRegion) %in% unique(data$subRegion))){
        stop("shape provided must contain at least one subRegion corresponding to the data provided.")
      }

      data_shape <- data_shape %>%
        dplyr::select(lon, lat, order, hole, piece, group, subRegion, name) %>%
        dplyr::filter(subRegion %in% unique(data$subRegion))

    } else {
      data_shape <- rmap::map_find_df(data) %>%
        dplyr::select(lon, lat, order, hole, piece, group, subRegion, name) %>%
        dplyr::filter(subRegion %in% unique(data$subRegion))
    }

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

# Set palette
if(length(palette)==1){
  if(palette %in% names(jgcricolors::jgcricol())){
    palette<-jgcricolors::jgcricol()[[palette]]}else{
      if(!is.na(RColorBrewer::brewer.pal.info[palette,]$maxcolors)){
        palette <- RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[palette,]$maxcolors,palette)}
    }}

if(!is.null(numeric2Cat_list)){
     if(all(c("numeric2Cat_param","numeric2Cat_breaks",
              "numeric2Cat_labels","numeric2Cat_palette","numeric2Cat_legendTextSize") %in% names(numeric2Cat_list))){
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

      if(length(catPalette)==1){
        if(catPalette %in% names(jgcricolors::jgcricol())){
          catPalette<-jgcricolors::jgcricol()[[catPalette]]
          }else if(!is.na(RColorBrewer::brewer.pal.info[catPalette,]$maxcolors)){
              catPalette <- RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[catPalette,]$maxcolors,catPalette)
          } else {
              print(paste0("catPalette provided: ", catPalette, "for param: ", catParam, " is invalid. Using jgcricol()$pal_16."))
              catPalette <- (rep(jgcricol()$pal_16,length(catLabels)))[1:length(catLabels)]
              names(catPalette) <- catLabels
              print(paste0("New catPalette: ", paste(catPalette,collapse=", ")))

            }
      }

      palette = catPalette
  }


    for(i in 1:length(fillColumn)){
      fillColumn_i <- fillColumn[i]

      if(is.numeric(datax[[fillColumn_i]])){

        legendBreaks <- NULL

        datax[[fillColumn_i]] <- cut( datax[[fillColumn_i]],
                                           breaks=catBreaks,
                                           labels=catLabels)
      }


      if(any(unique(datax[[fillColumn_i]]) %in% names(palette))){

        palette<-palette[1:min(length(catPalette),length(palette))]

        datax %>%
          dplyr::mutate(!!fillColumn_i := factor(datax[[fillColumn_i]],
                                                 levels = names(palette)))->
          datax
      } else {
        datax %>%
          dplyr::mutate(!!fillColumn_i := as.factor(datax[[fillColumn_i]])) -> datax
        }

      paletteX <- palette;
      datax <- datax %>% dplyr::mutate(label=value)
      datax1 <- datax

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

  if(is.null(catPalette)){

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

              legendBreaksnX = length(legendBreaksX)
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
            legendBreaksnX=1
            legendBreaksX=legendBreaks
            legendLabelsX=NULL
            paletteX=palette
          }
    }else{
      legendBreaksnX=legendBreaksn
      legendBreaksX=legendBreaks
      legendLabelsX=NULL
      paletteX=palette
    }
  }

  # Remove singleValue if legendsSingleVal == F
  if(legendSingleValue == F){
    paletteX = paletteX[-singleValLoc]
  }

  # Setting labels for data
  if(T){
    legVals <- names(paletteX); legVals
    legValsSingle <- legVals[!grepl("to",legVals)]; legValsSingle
    legValsRange <- legVals[grepl("to",legVals)]; legValsRange

    datax1 <- datax

    if(length(legValsSingle)>0){
    for(legValsSingle_i in legValsSingle){
      datax1 <- datax1 %>%
        dplyr::mutate(label := dplyr::if_else(round(!!datax1[[fillColumn]],legendDigits)==as.numeric(as.character(legValsSingle)),
                                       legValsSingle,
                                       "temp"))
    }}else{
      datax1 <- datax1 %>%
        dplyr::mutate(label = "temp")
    }

    for(legValsRange_i in legValsRange){

      range_i <- as.numeric(gsub(",","",unlist(stringr::str_split(legValsRange_i," to ")))); range_i

      datax1 <- datax1 %>%
        dplyr::mutate(
          label := dplyr::case_when(
          (label == "temp" & round(!!datax1[[fillColumn]],legendDigits) <= round(max(range_i),legendDigits) &
           round(!!datax1[[fillColumn]],legendDigits) >= round(min(range_i),legendDigits)) ~ legValsRange_i,
          TRUE ~ label))
    }

    #Setting labels for values that hit max and min values
    datax1 <- datax1 %>%
      dplyr::mutate(
        label := dplyr::case_when(
          (label == "temp" & !!datax1[[fillColumn]] == max(legendBreaksX)) ~ legValsRange[length(legValsRange)],
          (label == "temp" & !!datax1[[fillColumn]] == min(legendBreaksX)) ~ legValsRange[1],
          TRUE ~ label
        )
      )

    # Set values for greater or less than the scale set
    datax1 <- datax1 %>%
      dplyr::mutate(
        label := dplyr::case_when(
          (label == "temp" & !!datax1[[fillColumn]] < min(legendBreaksX))~paste0("< ", min(legendBreaksX)),
          (label == "temp" & !!datax1[[fillColumn]] > max(legendBreaksX))~paste0("> ", max(legendBreaksX)),
          TRUE ~ label)
      )

    # Add in any bounds if needed to palette
    labelBounds <- unique(datax1$label); labelBounds
    labelBounds <- labelBounds[grepl(">|<",labelBounds)]; labelBounds

    # If > data point added then add a darker color to the end of palette
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
    datax1 <- datax1 %>%
      dplyr::mutate(
        label := dplyr::case_when(
          (label == "temp" & is.na(!!datax1[[fillColumn]]) ~ paste0("NA")),
          TRUE ~ label)
      )

    if(any(grepl("temp", unique(datax1$label)))){
      print(datax1 %>% as.data.frame() %>% dplyr::filter(grepl("temp",label)))
      stop("Label data not allocated correctly.")
    }

    # Add

  }

    } # if is.null(catPalette)

  # Add NA colors
  if(T){
    # Add NA Value Colors
    if(showNA){
    datax1 <- datax1 %>%
      dplyr::mutate(label=dplyr::if_else(is.na(label),"NA",label))
    paletteX <- c(paletteX,"NA"=colorNA)
    }
  }
  }
}

#....................
# Plot
#....................

if(!is.null(datax)){

  # Convert labels to factors for cat values
  if(T){
      if(!is.null(catPalette)){
    datax1 <- datax1 %>%
      dplyr::mutate(label = factor(label,levels=unique(names(paletteX))))
      }
    }

  # Zoom out
  if(T){
    if(is.null(zoomx)){zoomx = zoom}
    if(is.null(zoomy)){zoomy = zoom}
    lonLimMin <- min(datax1$lon)+abs(max(range(datax1$lon))-min(range(datax1$lon)))*zoomx/10;lonLimMin
    lonLimMax <- max(datax1$lon)-abs(max(range(datax1$lon))-min(range(datax1$lon)))*zoomx/10;lonLimMax
    latLimMin <- min(datax1$lat)+abs(max(range(datax1$lon))-min(range(datax1$lon)))*zoomy/10;latLimMin
    latLimMax <- max(datax1$lat)-abs(max(range(datax1$lon))-min(range(datax1$lon)))*zoomy/10;latLimMax
  }

  # UnderLayer
  if(T){
  if(is.null(underLayer)){
      underLayer <- ggplot2::ggplot()
  } else if(any(grepl("tbl_df|tbl|data.frame",class(underLayer)))){

        underLayerx <- underLayer

        underLayer <- ggplot2::ggplot() +
          ggplot2::geom_polygon(data = underLayer,
                       ggplot2::aes(x = lon, y = lat, group = group),
                       colour = underLayerColor,
                       fill = underLayerFill,
                       lwd= underLayerLwd,
                       alpha = underLayerAlpha) +
          ggplot2::coord_fixed(ratio = asp,
                               ylim=c(max(latLimMin,-90),min(latLimMax,90)),
                               xlim=c(max(-180,lonLimMin),min(lonLimMax,180)),
                               expand = c(0, 0))

        if(underLayerLabels){
          shapex <- rmap::df_to_shape(underLayerx)
          if(!is.null(shapex)){
          labels_df_under =  shapex@data %>%
            tibble::as_tibble() %>%
            dplyr::bind_cols(sp::coordinates(shapex) %>%
                               data.frame() %>% dplyr::rename(lon=X1,lat=X2))%>%
            dplyr::filter(subRegion %in% unique(underLayerx$subRegion))

          if(labelRepel != 0){
            underLayer <- underLayer +
              ggrepel::geom_label_repel(data=labels_df_under, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                                        colour = labelColor,
                                        size=labelSize,
                                        alpha=labelAlpha,
                                        fill = labelFill,
                                        label.size = labelBorderSize,
                                        force = labelRepel)}else{

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

  map <- underLayer +
    ggplot2::geom_tile(data=datax1, ggplot2::aes_string(x="lon", y="lat", fill="label"), alpha=alpha) +
    ggplot2::coord_fixed(ratio = asp,
                         ylim=c(max(latLimMin,-90),min(latLimMax,90)),
                         xlim=c(max(-180,lonLimMin),min(lonLimMax,180)),
                         expand = c(0, 0)) +
    ggplot2::scale_fill_manual(breaks=names(paletteX), values=paletteX, drop=F,
                               name = legendTitle)
  }

  # If polygon
  if(!is.null(dataPolygon)){
    map <- underLayer +
      ggplot2::geom_polygon(data = datax1,
                            ggplot2::aes_string(x="lon", y="lat", group="group",fill="label"),
                            color = "grey40", lwd=0.1) +
      ggplot2::coord_fixed(ratio = asp) +
      ggplot2::scale_fill_manual(breaks=names(paletteX), values=paletteX, drop=F,
                                 name = legendTitle)

    if(labels){
      if(!is.null(shape)){
      if(!any(grepl("SpatialPolygonsDataFrame",class(shape)))){
        print("To print labels shape must be a SpatialPolygonsDataFrame for e.g. rmap::mapCountries.")
        shapex <- NULL
      } else {
        shapex <- shape
      }} else {
        shapex <- rmap::df_to_shape(data_shape)
      }
      if(!is.null(shapex)){
        labels_df_shape =  shapex@data %>%
          tibble::as_tibble() %>%
          dplyr::bind_cols(sp::coordinates(shapex) %>%
                             data.frame() %>% dplyr::rename(lon=X1,lat=X2))%>%
          dplyr::filter(subRegion %in% unique(data_shape$subRegion))

        if(labelRepel != 0){
          map <- map +
            ggrepel::geom_label_repel(data=labels_df_shape, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                                      colour = labelColor,
                                      size=labelSize,
                                      alpha=labelAlpha,
                                      fill = labelFill,
                                      label.size = labelBorderSize,
                                      force = labelRepel)}else{

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
      ggplot2::scale_fill_manual(values=(rep(palette,length(unique(dataShape$subRegion))))[1:length(unique(dataShape$subRegion))],
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

      if(labelRepel != 0){
        map <- map +
          ggrepel::geom_label_repel(data=labels_df_shape, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                                    colour = labelColor,
                                    size=labelSize,
                                    alpha=labelAlpha,
                                    fill = labelFill,
                                    label.size = labelBorderSize,
                                    force = labelRepel)}else{

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

  if((!is.null(row) & !is.null(col))){
    if((all(row %in% names(datax1)) & all(col %in% names(datax1)))){

      # Single Col and upto three rows
      # Upto three multifacet rows
      if(length(col)==1){
      if(length(row)==1){
        map <- map +
          ggplot2::facet_grid(get(row[1]) ~ get(col[1]))}

      if(length(row)==2){
        map <- map +
          ggplot2::facet_grid(get(row[1]) + get(row[2]) ~ get(col[1]))}

      if(length(row)==3){
        map <- map +
          ggplot2::facet_grid(get(row[1])+ get(row[2])+ get(row[3]) ~ get(col[1]))}
      }

      if(length(col)==2){
        if(length(row)==1){
          map <- map +
            ggplot2::facet_grid(get(row[1]) ~ get(col[1]) +  get(col[2]))}

        if(length(row)==2){
          map <- map +
            ggplot2::facet_grid(get(row[1]) + get(row[2]) ~ get(col[1]) +  get(col[2]))}

        if(length(row)==3){
          map <- map +
            ggplot2::facet_grid(get(row[1])+ get(row[2])+ get(row[3]) ~ get(col[1]) +  get(col[2]))}
      }

      if(length(col)==3){
        if(length(row)==1){
          map <- map +
            ggplot2::facet_grid(get(row[1]) ~ get(col[1]) +  get(col[2]) + get(col[3]))}

        if(length(row)==2){
          map <- map +
            ggplot2::facet_grid(get(row[1]) + get(row[2]) ~ get(col[1]) +  get(col[2]) + get(col[3]))}

        if(length(row)==3){
          map <- map +
            ggplot2::facet_grid(get(row[1])+ get(row[2])+ get(row[3]) ~ get(col[1]) +  get(col[2]) + get(col[3]))}
      }


    }}

    if((!is.null(row) & is.null(col))){
    if((row %in% names(datax1))){

      # Upto three multifacet rows
      if(length(row)==1){
        map <- map +
          ggplot2::facet_grid(get(row[1]) ~ ., ncol = ncol)}

      if(length(row)==2){
        map <- map +
          ggplot2::facet_grid(get(row[1]) + get(row[2]) ~ .)}

      if(length(row)==3){
        map <- map +
          ggplot2::facet_grid(get(row[1])+ get(row[2])+ get(row[3]) ~ .)}

    }
    }

    if((is.null(row) & !is.null(col))){
    if((col %in% names(datax1))){

     # Upto three multifacet columns
     if(length(col)==1){
      map <- map +
        ggplot2::facet_wrap(. ~ get(col[1]), ncol = ncol)}

      if(length(col)==2){
        map <- map +
          ggplot2::facet_grid(. ~ get(col[1]) + get(col[2]))}

      if(length(col)==3){
        map <- map +
          ggplot2::facet_grid(. ~ get(col[1]) + get(col[2]) + get(col[3]))}


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
        ggplot2::coord_fixed(ratio = asp,
                             ylim=c(max(latLimMin,-90),min(latLimMax,90)),
                             xlim=c(max(-180,lonLimMin),min(lonLimMax,180)),
                             expand = c(0, 0))

      if(overLayerLabels){
        shapex <- rmap::df_to_shape(overLayer)
        if(!is.null(shapex)){
        labels_df_over =  shapex@data %>%
          tibble::as_tibble() %>%
          dplyr::bind_cols(sp::coordinates(shapex) %>%
                             data.frame() %>% dplyr::rename(lon=X1,lat=X2))%>%
          dplyr::filter(subRegion %in% unique(overLayer$subRegion))

        if(crop){
        labels_df_over <- labels_df_over %>%
            dplyr::filter(lat < latLimMax,
                          lat > latLimMin,
                          lon < lonLimMax,
                          lon > lonLimMin)}

        if(labelRepel != 0){
        map <- map +
          ggrepel::geom_label_repel(data=labels_df_over, ggplot2::aes(x = lon, y = lat, group = subRegion, label=subRegion),
                               colour = labelColor,
                                size=labelSize,
                                alpha=labelAlpha,
                                fill = labelFill,
                                label.size = labelBorderSize,
                                force = labelRepel)}else{

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
   ggplot2::ggtitle(title)

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
    panel.background = ggplot2::element_rect(fill="lightcyan2"),
    strip.background = ggplot2::element_rect(color="black",size=0.1, fill="gray90"))
}

map <- map +
  ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = "black", lwd=0.1))) +
  ggplot2::theme(text=ggplot2::element_text(size=size))

if(!legendShow){map = map + ggplot2::guides(fill="none")}

if(crop==T){
  map <- map +
    ggplot2::coord_fixed(ratio = asp,
                         ylim=c(max(latLimMin,-90),min(latLimMax,90)),
                         xlim=c(max(-180,lonLimMin),min(lonLimMax,180)),
                         expand = c(0, 0))
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

    if(show){print(map)}

    } else {
    #print("save set to F so no figure will be saved.")
      if(show){print(map)}
    }

#....................
# Return
#....................

invisible(map)

}
