#' map
#'
#' This function produce different kinds of maps for the rmap package.
#' Each figure is accompanied with a csv table.
#'
#' @param dataPolygon Default = NULL
#' @param dataGrid Default = NULL
#' @param fileName Default = "map"
#' @param shpFolder Default = paste(getwd(),"/dataFiles/gis/admin_gadm36_1",sep Default = "")
#' @param shpFile Default = paste("gadm36_1",sep Default = "")
#' @param folder Default = paste(getwd(),"/outputs",sep Default = "")
#' @param fillPalette Default = "Spectral"
#' @param theme_ggplot Default = ggplot2::theme_bw()
#' @param theme_custom Default = NULL
#' @param theme_rmap Default = TRUE
#' @param fillColumn Default = NULL # Or give column with data
#' @param width Default = 9
#' @param height Default = 7
#' @param legendShow Default = F
#' @param legendDigits Default = NULL
#' @param legendTitle Default = NULL
#' @param legendBreaksn Default = "5"
#' @param legendBreaks Default = NULL
#' @param labels Default = FALSE
#' @param pdfpng Default = "png"
#' @param underLayer Default = NULL
#' @param printFig Default = T
#' @param multiFacetRows Default=NULL
#' @param multiFacetCols Default=NULL
#' @param mapTitle Default=NULL
#' @param numeric2Cat_list Default=NULL
#' @param legendDigitsOverride Default=NULL
#' @param legendSingleColor Default="white"
#' @param legendSingleValue Default=NULL
#' @param colorNA Default = "gray50"
#' @param showNA Default = T
#' @param ncol Default = 3.  Number of columns to wrap maps
#' @param size Default = 12. Text size of plots.
#' @keywords charts, diffplots
#' @return Returns the formatted data used to produce chart
#' @importFrom rlang :=
#' @importFrom jgcricolors jgcricol
#' @export

mapplot<-function(dataPolygon=NULL,
                  fillColumn=NULL, # Or give column data with
                  dataGrid=NULL,
                  shpFolder=NULL,
                  shpFile=NULL,
                  theme_ggplot = ggplot2::theme_bw(),
                  theme_custom = NULL,
                  theme_rmap = T,
                  fillPalette="Spectral",
                  labels=F,
                  width=9,
                  height=7,
                  legendShow=F,
                  legendTitle=NULL,
                  legendBreaksn=5,
                  legendBreaks=NULL,
                  pdfpng="png",
                  underLayer=NULL,
                  printFig=T,
                  fileName="map",
                  folder=paste(getwd(),"/outputs",sep=""),
                  multiFacetRows=NULL,
                  multiFacetCols=NULL,
                  mapTitle=NULL,
                  numeric2Cat_list=NULL,
                  legendDigits = NULL,
                  legendDigitsOverride=NULL,
                  legendSingleColor ="white",
                  legendSingleValue =NULL,
                  colorNA = "gray50",
                  showNA = F,
                  ncol = 3,
                  size = 12
                  ){

  # dataPolygon=NULL
  # fillColumn=NULL # Or give column data with
  # dataGrid=NULL
  # shpFolder=NULL
  # shpFile=NULL
  # theme_ggplot = ggplot2::theme_bw()
  # theme_custom = NULL
  # theme_rmap = T
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
  # printFig=T
  # fileName="map"
  # folder=paste(getwd(),"/outputs",sep="")
  # multiFacetRows=NULL
  # multiFacetCols=NULL
  # mapTitle=NULL
  # numeric2Cat_list=NULL
  # legendDigits = NULL
  # legendDigitsOverride=NULL
  # legendSingleColor ="white"
  # legendSingleValue =NULL

# ALT + 0 here to collapse all code into manageable chunks
# ALT + Shift + O to expand all

#.........................
# Initialize variables to remove binding errors if needed
# .........................

if(T){ # Initialize
  NULL->raster->shape->map->checkFacets->catBreaks->catLabels->catPalette->legendLabelsX->
    singleValLoc->label->catParam

  legendTitle=gsub(" ","\n",legendTitle)
  legendBreaks<-sort(legendBreaks)

  if(any(tolower(legendTitle) %in% c("legend","units","unit"))){
    legendTitle = NULL
  }

  fillPaletteOrig <- fillPalette

  if(is.null(theme_custom)){
    theme_custom = ggplot2::theme()
  }

  if(theme_rmap){
    theme_rmapx =  ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill="lightblue1"),
      strip.text = ggplot2::element_text(size = 12))
  } else {
    theme_rmapx = ggplot2::theme()
  }




} # initialize

#......................................................................
# Read data and check inputs
#......................................................................

if(T){ # Read input data
if(!is.null(dataPolygon)){
  # print("Using given dataPolygon file as shape.")
  if(!is.null(shpFolder) & !is.null(shpFile)){print(paste("NOT reading shapefile '",shpFile,"' from folder '",shpFolder,"'",sep=""))}
    shape<-dataPolygon
  }else{
if(!is.null(shpFolder) & !is.null(shpFile)){
  if(!dir.exists(shpFolder)){
    stop("Shapefile folder: ", shpFolder ," is incorrect or doesn't exist.",sep="")}
  if(!file.exists(paste(shpFolder,"/",shpFile,".shp",sep=""))){
    stop("Shape file: ", paste(shpFolder,"/",shpFile,".shp",sep="")," is incorrect or doesn't exist.",sep="")}
    print("Reading shapefile '",shpFile,"' from folder '",shpFolder,"'",sep="")
    shape=rgdal::readOGR(dsn=shpFolder,layer=shpFile,use_iconv=T,encoding='UTF-8')
    }
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

       raster = dataGrid
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

# If categorical data then set as factor for raster
if(!is.null(raster)){
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

      if(is.numeric(raster[[fillColumn_i]])){

        legendBreaks <- NULL

        raster[[fillColumn_i]] <- cut( raster[[fillColumn_i]],
                                           breaks=catBreaks,
                                           labels=catLabels)
      }


      if(any(unique(raster[[fillColumn_i]]) %in% names(fillPalette))){
        # fillPalette<-fillPalette[1:min(length(catPalette),
        #                                length(fillPalette))]
        raster %>%
          dplyr::mutate(!!fillColumn_i := factor(raster[[fillColumn_i]],
                                                 levels = names(fillPalette)))->
          raster
      } else { raster %>%
          dplyr::mutate(!!fillColumn_i := as.factor(raster[[fillColumn_i]])) -> raster}
    }
  }
}

# If categorical data then set as factor for shape
if(!is.null(shape)){
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

        if(is.numeric(shape@data[[fillColumn_i]])){

            legendBreaks <- NULL

          shape@data[[fillColumn_i]] <- cut( shape@data[[fillColumn_i]],
                                              breaks=catBreaks,
                                              labels=catLabels)
        }

        shape@data %>%
          dplyr::mutate(!!fillColumn_i := as.factor(shape@data[[fillColumn_i]])) -> shape@data
      }
  }
}

# Set data to raster or shape
  if(!is.null(raster)){
    datax <- raster
  } else {
    datax <- shape
  }

}

#.........................
# Remove Inf values
#.........................

if(T){ # Remove error values

# if(!is.null(shape)){
#   if(class(shape)!="tmap"){
#   if(nrow(shape@data)>0){
#     shape@data[mapply(is.infinite, shape@data)] <- NA
#     if(is.null(fillColumn)){
#       if("subRegion" %in% names(shape@data)){
#         fillColumn = "subRegion"
#         }
#       }
#     }
#    }
# }

if(!is.null(datax)){
    if(nrow(datax)>0){
      datax[mapply(is.infinite, datax)] <- NA
    }
    # if(is.null(fillColumn)){
    #   if("subRegion" %in% names(datax)){
    #     fillColumn = "subRegion"
    #     }
    #   }
  }
}

#....................
# Set Legend Breaks and Labels
#....................

if(T){
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

    if(any(grepl("temp", unique(datax1$label)))){
      print(datax1%>%dplyr::filter(grepl("temp",label)))
      stop("Label data not allocated correctly. Check mapplot.R line 561.")
      }

  }

  # Add NA colors
  if(T){
    # Add NA Value Colors
    if(showNA){
    datax1 <- datax1 %>%
      mutate(label=if_else(is.na(label),"NA",label))
    fillPaletteX <- c(fillPaletteX,"NA"=colorNA)
    }
  }

}

#....................
# Plot
#....................

if(!is.null(datax)){

  # Convert labels to factors
  if(T){
    datax1 <- datax1 %>%
      mutate(label = factor(label,levels=unique(names(fillPaletteX))))
  }

  # UnderLayer
  if(is.null(underLayer)){
    underLayer <- ggplot2::ggplot()
  }

  # If grid
  if(!is.null(dataGrid)){
  map <- underLayer +
    ggplot2::geom_tile(data=datax1, ggplot2::aes_string(x="lon", y="lat", fill="label")) +
    ggplot2::coord_fixed(ratio = 1.0) +
    ggplot2::scale_fill_manual(breaks=names(fillPaletteX), values=fillPaletteX, drop=F,
                               name = legendTitle)
  }

  # If polygon
  if(!is.null(dataPolygon)){
    map <- underLayer +
      ggplot2::geom_polygon(data = datax1, ggplot2::aes_string(x="lon", y="lat", fill="label")) +
      ggplot2::coord_fixed(ratio = 1.0) +
      ggplot2::scale_fill_manual(breaks=names(fillPaletteX), values=fillPaletteX)
    }

  if(!legendShow){map = map + ggplot2::guides(fill=FALSE)}

  # Multi Facet
  if(T){ # Multi Facet

  if((!is.null(multiFacetRows) & !is.null(multiFacetCols))){
    if((all(multiFacetRows %in% names(datax1)) & all(multiFacetCols %in% names(datax1)))){

      # Single Col and upto three rows
      # Upto three multifacet rows
      if(length(multiFacetCols)==1){
      if(length(multiFacetRows)==1){
        map <- map +
          ggplot2::facet_grid(get(multiFacetRows[1]) ~ get(multiFacetCols[1]))}

      if(length(multiFacetRows)==2){
        map <- map +
          ggplot2::facet_grid(get(multiFacetRows[1]) + get(multiFacetRows[2]) ~ get(multiFacetCols[1]))}

      if(length(multiFacetRows)==3){
        map <- map +
          ggplot2::facet_grid(get(multiFacetRows[1])+ get(multiFacetRows[2])+ get(multiFacetRows[3]) ~ get(multiFacetCols[1]))}
      }

      if(length(multiFacetCols)==2){
        if(length(multiFacetRows)==1){
          map <- map +
            ggplot2::facet_grid(get(multiFacetRows[1]) ~ get(multiFacetCols[1]) +  get(multiFacetCols[2]))}

        if(length(multiFacetRows)==2){
          map <- map +
            ggplot2::facet_grid(get(multiFacetRows[1]) + get(multiFacetRows[2]) ~ get(multiFacetCols[1]) +  get(multiFacetCols[2]))}

        if(length(multiFacetRows)==3){
          map <- map +
            ggplot2::facet_grid(get(multiFacetRows[1])+ get(multiFacetRows[2])+ get(multiFacetRows[3]) ~ get(multiFacetCols[1]) +  get(multiFacetCols[2]))}
      }

      if(length(multiFacetCols)==3){
        if(length(multiFacetRows)==1){
          map <- map +
            ggplot2::facet_grid(get(multiFacetRows[1]) ~ get(multiFacetCols[1]) +  get(multiFacetCols[2]) + get(multiFacetCols[3]))}

        if(length(multiFacetRows)==2){
          map <- map +
            ggplot2::facet_grid(get(multiFacetRows[1]) + get(multiFacetRows[2]) ~ get(multiFacetCols[1]) +  get(multiFacetCols[2]) + get(multiFacetCols[3]))}

        if(length(multiFacetRows)==3){
          map <- map +
            ggplot2::facet_grid(get(multiFacetRows[1])+ get(multiFacetRows[2])+ get(multiFacetRows[3]) ~ get(multiFacetCols[1]) +  get(multiFacetCols[2]) + get(multiFacetCols[3]))}
      }


    }}

    if((!is.null(multiFacetRows) & is.null(multiFacetCols))){
    if((multiFacetRows %in% names(datax1))){

      # Upto three multifacet rows
      if(length(multiFacetRows)==1){
        map <- map +
          ggplot2::facet_grid(get(multiFacetRows[1]) ~ ., ncol = ncol)}

      if(length(multiFacetRows)==2){
        map <- map +
          ggplot2::facet_grid(get(multiFacetRows[1]) + get(multiFacetRows[2]) ~ .)}

      if(length(multiFacetRows)==3){
        map <- map +
          ggplot2::facet_grid(get(multiFacetRows[1])+ get(multiFacetRows[2])+ get(multiFacetRows[3]) ~ .)}

    }
    }

    if((is.null(multiFacetRows) & !is.null(multiFacetCols))){
    if((multiFacetCols %in% names(datax1))){

     # Upto three multifacet columns
     if(length(multiFacetCols)==1){
      map <- map +
        ggplot2::facet_wrap(. ~ get(multiFacetCols[1]), ncol = ncol)}

      if(length(multiFacetCols)==2){
        map <- map +
          ggplot2::facet_grid(. ~ get(multiFacetCols[1]) + get(multiFacetCols[2]))}

      if(length(multiFacetCols)==3){
        map <- map +
          ggplot2::facet_grid(. ~ get(multiFacetCols[1]) + get(multiFacetCols[2]) + get(multiFacetCols[3]))}


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
  theme_ggplot +
  theme_rmapx +
  theme_custom

if(theme_rmap){
  map <- map + ggplot2::xlab(NULL) + ggplot2::ylab(NULL)
}

map <- map +
  ggplot2::guides(fill = guide_legend(override.aes = list(colour = "black", lwd=0.1))) +
  theme(text=element_text(size=size))

}

#....................
# Print
#....................

if(printFig){

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
                pdfpng=pdfpng)
    print(map)

    } else {
    #print("printFig set to F so no figure will be saved.")
    print(map)
    }

#....................
# Return
#....................

invisible(map)

}
