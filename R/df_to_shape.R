#' df_to_shape
#'
#' Given a fortified ggplot dataframe (e.g. rmap::mapCountriesdf) will choose the
#' corresponding SpatialPolygonDataFrame map (e.g. rmap::mapCountries)
#'
#' @param df Default = NULL. Fortified ggplot dataframe (e.g. rmap::mapCountriesdf)
#' @keywords map, find
#' @return SpatialPolygonDataFrame
#' @export
#' @examples
#' library(rmap)
#' df = rmap::mapCountriesdf
#' shapex = rmap::df_to_shape(df)


df_to_shape <- function(df=NULL) {

  if(!is.null(df)){
    if("name" %in% names(df)){
      shape_df <- get(gsub("df","",unique(df$name)))
    } else{
      shape_df = NULL
      print(paste0("Can't convert df to shape because 'name' column does not exist."))
      print(paste0("This message produced in df_to_shape line 23."))
    }
  } else {
    stop("No shape provided")
  }

  invisible(shape_df)

  } # Close df_to_shape function
