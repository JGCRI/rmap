#' tidy_shape
#'
#' Given a SpatialPolygonDataFrame map (e.g. rmap::mapCountries) will convert to a tidy dataframe
#'
#' @param shape Default = NULL. SpatialPolygonDataFrame map (e.g. rmap::mapCountries)
#' @param shapeColumn (Optional). Default = NULL. If different from subRegion.
#' @keywords map, find
#' @return Fortified ggplot dataframe (e.g. rmap::mapCountriesdf)
#' @export
#' @examples
#' library(rmap)
#' shape = rmap::mapCountries
#' df = rmap::tidy_shape(shape)


tidy_shape <- function(shape, shapeColumn = NULL, ...) {

  attr <- as.data.frame(shape)
  attr <- attr %>% as.data.frame()
  # If not specified, split into regions based on polygons
  if (is.null(shapeColumn)) {
    coords <- purrr::map_df(shape@polygons, broom::tidy)
    message("Regions defined for each Polygons")
  } else {
    cp <- sp::polygons(shape)

    # Union together all polygons that make up a shapeColumn
    unioned <- maptools::unionSpatialPolygons(cp, attr[, shapeColumn])
    coords <- broom::tidy(unioned)
    coords$order <- 1:nrow(coords)
  }
  tibble::as_tibble(coords)
}
