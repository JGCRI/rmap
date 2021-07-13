#' shape_to_df
#'
#' Given a SpatialPolygonDataFrame map (e.g. rmap::mapCountries) will choose the
#' corresponding fortified ggplot dataframe (e.g. rmap::mapCountriesdf)
#'
#' @param shape Default = NULL. SpatialPolygonDataFrame map (e.g. rmap::mapCountries)
#' @param shapeColumn (Optional). Default = NULL. If different from subRegion.
#' @keywords map, find
#' @return Fortified ggplot dataframe (e.g. rmap::mapCountriesdf)
#' @export
#' @examples
#' library(rmap)
#' shape = rmap::mapCountries
#' df = rmap::shape_to_df(shape)


shape_to_df <- function(shape=NULL,
                        shapeColumn=NULL) {

  # Initialize
  NULL->id->long->subRegionOrig->regionOrig->subRegion

  # Convert
  if(!is.null(shape)){
    if("name" %in% names(shape@data) & is.null(shapeColumn)){ # If rmap data maps
      if(nrow(shape@data) == nrow(get(paste0(unique(shape@data$name))))){
      subRegionOrig = unique(shape@data$subRegion)
      shape_df <- get(paste0(unique(shape@data$name),"df"))
      shape_df <- shape_df %>%
        dplyr::filter(subRegion %in% subRegionOrig)
      } else { # Create a fortified map for ggplot
        if(is.null(shapeColumn)){
          shape_df <- broom::tidy(shape, region="subRegion") %>%
            dplyr::rename(subRegion=id) %>%
            dplyr::rename(lon=long)
        } else {
          if(shapeColumn %in% names(shape@data)){
            shape_df <- broom::tidy(shape, region = shapeColumn) %>%
              dplyr::rename(subRegion=id) %>%
              dplyr::rename(lon=long)
          } else {
            stop(paste0("shapeColumn provided: ",shapeColumn, "is not a column in the data provided."))
          }
        }
      }
    } else { # Create a fortified map for ggplot
      if(is.null(shapeColumn)){
      shape_df <- broom::tidy(shape, region="subRegion") %>%
        dplyr::rename(subRegion=id) %>%
        dplyr::rename(lon=long)
      } else {
        if(shapeColumn %in% names(shape@data)){
          shape_df <- broom::tidy(shape, region = shapeColumn) %>%
            dplyr::rename(subRegion=id) %>%
            dplyr::rename(lon=long)
        } else {
          stop(paste0("shapeColumn provided: ",shapeColumn, "is not a column in the data provided."))
        }
      }
    }
  } else {
    stop("No shape provided")
  }

  invisible(shape_df)

  } # Close shape_to_df function
