---
title: 'rmap: An R package to easily plot polygon and gridded data on highly customizable built-in and user maps.'
tags:
  - 
authors:
  - name: Zarrar Khan
    orcid: 0000-0002-8147-8553
    affiliation: 1
  - name: Mengqi Zhao
    orcid: 0000-0001-5385-2758
    affiliation: 2 
  - name: Chris R. Vernon
    orcid: 0000-0002-3406-6214
    affiliation: 1  
  - name: Thomas Wild
    orcid: 0000-0002-6045-7729
    affiliation: "1, 2"
affiliations:
 - name: Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, MD, USA
   index: 1
 - name: Earth System Science Interdisciplinary Center (ESSIC), University of Maryland, College Park, MD, USA
   index: 2
date: 3 Aug 2021
bibliography: paper.bib
---

# Summary
`rmap` is an R package that allows uers to very easily plot tabular data (csv or R dataframes) on maps without any Geographical Information Systems (GIS) knowledge. All maps produced by `rmap` are `ggplot` objects and thus capitalize on all the flexibility and advancements of the `ggplot` package and all elements of each map is thus fully customizable. Additionally `rmap` automatically detects and produces comparison maps if the data has multiple scenarios, parameters, classes or time periods as well as animations for time series data. Advanced users can use their own shapefiles if desired. `rmap` comes with a range of prebuilt color palettes but users can also provide any `R` color palette or create their own as needed. Data legends are available in three types of legends which include equal intervals (pretty), kmeans or continuous legend scales to highlight different kinds of data distributions. The input data can be both gridded data with latitude and longitude coordinates or polygon data with data values for different regions. 

The package is available on github at https://github.com/JGCRI/rmap.

# Statement of need
`rmap` is meant to advance the current state of spatial visualization in R. Existing R packages (such as tmap, cartography, rworldmap, GISTools, choroplethr, sp and sf) have been developed to address the need to conduct spatial visualization within R without depending on external software such as ArcGIS, GRASS or QGIS. The existing packages mentioned above have the following limitations which are enhanced in `rmap`:

1. **pre-built maps**: None of the other packages come with more than a few example pre-built maps. `rmap` comes with country, state, mutli-level hydroshed river basin maps as well as other customized maps. One reason why pre-built maps are limited in existing packages is to keep the package sizes small enough to be published on CRAN. `rmap` is not published on CRAN and thus includes larger datasets.
2. **direct data table to map**: None of the other packages is able to plot a map from a simple data.frame or csv table. `rmap` has an automatic `map_find` function that searches for the appropriate built-in map based on the regions provided in a `subRegion` column. This truly frees users from the need for any other data need and they can simply `map()` their own data tables directly as long as the table has a minimum of a `subRegion` and `value` column. 
3. **difference maps**: None of the other packages produces difference maps to compare across scenarios or time periods. `rmap` provides this functionality by automatically recognizing multiple scenarios and time periods to produce difference maps across these dimensions. Often what is most important in spatial data is to see the difference between two scenarios or time periods and `rmap` makes this extremely simple.
4. **post-process customization**: Most of the existing packages do not produce output objects that can be saved and then customized. Customization of the maps is limited to built in functionality. `rmap` produces ggplot objects in which every element (axis, grids, titles, colors, linewidths, facets) can all be customized after the map has been produced. 


# Installation Guide

1. Download and install:
    - R (https://www.r-project.org/)
    - R studio (https://www.rstudio.com/)  
    
    
2. Open R studio:

```r
install.packages("devtools")
devtools::install_github("JGCRI/rmap")
```

Additional steps for UBUNTU from a terminal
```
sudo add-apt-repository ppa:ubuntugis/ppa
sudo apt-get update
sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev libmagick++-dev
```

Additional steps for MACOSX from a terminal
```
brew install pkg-config
brew install gdal
brew install imagemagick@6
```

# Functionality

A detailed [User Guide](https://jgcri.github.io/rmap/articles/vignette_map.html) walks users step-by-step through all the available functionality of `rmap`. A simpler [Cheatsheet](https://jgcri.github.io/rmap/cheatsheet.pdf) is also provided to help users remember some of the key functionality in a single sheet. A list of the key functionality is provided below:

1. [Input Formats]:
2. [Output Formats]:
3. [Built-in Maps]:
4. [Layers]:
5. [Labels]:
6. [Cropping]:
7. [Zooming]:
8. [Background]:
9. [Themes]:
10. Plotting Data:
	a.
	b.
	c.
11. [Multi-year-class-scenario facets]:
12. [Scale Ranges]:
13. [Color Palettes]:
14. [Legend Customization]:
15. [Categorical data]:

**NOTE: Users are encouraged to refer to the [User Guide](https://jgcri.github.io/rmap/articles/vignette_map.html) for the most up-to-date functionality. **


# Acknowledgements
This research was supported by the US Department of Energy, Office of Science, as part of research in MultiSector Dynamics, Earth and Environmental System Modeling Program. The Pacific Northwest National Laboratory is operated for DOE by Battelle Memorial Institute under contract DE-AC05-76RL01830. The views and opinions expressed in this paper are those of the authors alone.

# References
