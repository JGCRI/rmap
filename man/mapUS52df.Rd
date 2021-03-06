% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mapUS52df}
\alias{mapUS52df}
\title{US 52 States
Includes Alaska, Hawaii and Puerto Rico as well as DC.}
\format{
A SpatialPolygonsDataFrame
}
\source{
US Census bureau. \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
}
\usage{
mapUS52df
}
\description{
US 52 States
Includes Alaska, Hawaii and Puerto Rico as well as DC.
}
\examples{
\dontrun{
 library(sp); library(rmap); library(ggplot2)
 ggplot() +
 geom_polygon(data = mapUS52df, aes(x = long, y = lat, group = group),
             colour = "black", fill = NA)
 head(mapUS52df)
}
}
\keyword{datasets}
