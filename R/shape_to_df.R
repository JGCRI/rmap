#' shape_to_df
#'
#' Given a SpatialPolygonDataFrame map (e.g. rmap::mapCountries) will choose the
#' corresponding fortified ggplot dataframe (e.g. rmap::mapCountriesdf)
#'
#' @param shape Default = NULL. SpatialPolygonDataFrame map (e.g. rmap::mapCountries)
#' @keywords map, find
#' @return Fortified ggplot dataframe (e.g. rmap::mapCountriesdf)
#' @export
#' @examples
#' library(rmap)
#' shape = rmap::mapCountries
#' df = rmap::shape_to_df(shape)


shape_to_df <- function(shape=NULL) {

  # Initialize
  NULL->id->long

  # Convert
  if(!is.null(shape)){
    if("name" %in% names(shape@data)){ # If rmap data maps
      shape_df <- get(paste0(unique(shape@data$name),"df"))
    } else { # Create a fortified map for ggplot
      shape_df <- broom::tidy(shape) %>%
        dplyr::rename(subRegion=id) %>%
        dplyr::rename(lon=long);
    }
  } else {
    stop("No shape provided")
  }

  invisible(shape_df)

  } # Close shape_to_df function
