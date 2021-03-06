% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mapHydroShed2}
\alias{mapHydroShed2}
\title{HydroSHEDS level 2}
\format{
A SpatialPolygonsDataFrame
}
\source{
Lehner, B., Grill G. (2013): Global river hydrography and network routing:
baseline data and new approaches to study the world’s large river systems.
Hydrological Processes, 27(15): 2171–2186. \url{https://www.hydrosheds.org/page/hydrobasins}
}
\usage{
mapHydroShed2
}
\description{
HydroSHEDS level 2
}
\examples{
\dontrun{
 library(sp); library(rmap)
 sp::plot(mapHydroShed2)
 head(mapHydroShed2@data)
}
}
\keyword{datasets}
