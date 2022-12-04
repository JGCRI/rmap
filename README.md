<!-- badges: start -->
[![build](https://github.com/JGCRI/rmap/actions/workflows/build.yml/badge.svg)](https://github.com/JGCRI/rmap/actions/workflows/build.yml)
[![test_coverage](https://github.com/JGCRI/rmap/actions/workflows/test_coverage.yml/badge.svg?branch=main)](https://github.com/JGCRI/rmap/actions/workflows/test_coverage.yml)
[![codecov](https://codecov.io/gh/JGCRI/rmap/branch/main/graph/badge.svg?token=XQ913U4IYM)](https://codecov.io/gh/JGCRI/rmap) 
[![docs](https://github.com/JGCRI/rmap/actions/workflows/docs.yaml/badge.svg)](https://github.com/JGCRI/rmap/actions/workflows/docs.yaml)
[![status](https://joss.theoj.org/papers/4cdf462f70681bc335ddebf5868b249c/status.svg)](https://joss.theoj.org/papers/4cdf462f70681bc335ddebf5868b249c)
<!-- badges: end -->


<!-- ------------------------>
<!-- ------------------------>
# <a name="Introduction"></a>Introduction
<!-- ------------------------>
<!-- ------------------------>

`rmap` is a comprehensive mapping package to create easily customizable maps and compare across scenarios, years and data classes. The package allows creation of difference maps as well as customizable legends, color palettes and styles.

<br>

<p align="center">
<a href="https://jgcri.github.io/rmap/articles/vignette_map.html" target="_blank"><img src="https://github.com/JGCRI/jgcricolors/blob/main/vignettes/button_user_guide.PNG?raw=true" alt="https://jgcri.github.io/rmap/articles/vignette_map.html" height="60"/></a>
<img src="https://github.com/JGCRI/jgcricolors/blob/main/vignettes/button_divider.PNG?raw=true" height="40"/>
<a href="https://jgcri.github.io/rmap/cheatsheet.pdf" target="_blank"><img src="https://github.com/JGCRI/jgcricolors/blob/main/vignettes/button_cheatsheet.PNG?raw=true" alt="https://jgcri.github.io/rmap/cheatsheet.pdf" height="60"/></a>  
</p>

<!-- ------------------------>
<!-- ------------------------>
# <a name="Citation"></a>Citation
<!-- ------------------------>
<!-- ------------------------>

Khan, Z., Zhao, M., Vernon, C.R., Wild, T. and Yarlagadda, B., 2022. rmap: An R package to plot and compare tabular data on customizable maps across scenarios and time. Journal of Open Source Software, 7(77), p.4015. DOI: https://doi.org/10.21105/joss.04015

<!-- ------------------------>
<!-- ------------------------>
# <a name="InstallGuide"></a>Installation Guide
<!-- ------------------------>
<!-- ------------------------>

1. Download and install:
    - R (https://www.r-project.org/)
    - R studio (https://www.rstudio.com/) (Optional)
    
    
2. In R or R studio:
```r
install.packages("devtools")
devtools::install_github("JGCRI/rmap")
```

Additional steps for UBUNTU from a terminal
```
sudo add-apt-repository ppa:ubuntugis/ppa
sudo apt-get update
sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev libudunits2-dev libgdal-dev libgeos-dev libproj-dev libavfilter-dev  libmagick++-dev
```

Additional steps for MACOSX from a terminal
```
brew install pkg-config
brew install gdal
brew install geos
brew install imagemagick@6
```

<!-- ------------------------>
<!-- ------------------------>
# <a name="Need"></a>Statement of need
<!-- ------------------------>
<!-- ------------------------>

`rmap` is meant to help users having limited to no GIS knowledge use R for spatial visualization of tabular spatial data. `rmap` is not meant to be a replacement for spatial manipulation or cartographic software but focuses on the simple plotting of polygon and gridded data for spatio-temporal visualization of tabular data with a focus on comparing across scenarios and time periods. Several existing R packages (e.g. tmap, cartography, rworldmap, GISTools, choroplethr, sp and sf) have been developed to conduct spatial visualization and analytics in R without depending on external software. `rmap` enhances the following key capabilities which are limited in these existing packages:

1. **Pre-built maps**: Existing packages come with only a few examples of built-in maps as package data. `rmap` comes with a growing collection of  country, state, river basin, as well as other customized maps that are added into the package data based on user needs and requests. While built-in maps increase the size of the package, having direct access to these allows for automated searching and quick deployment of relevant shapefiles without the need to download any data. A list of pre-built maps in `rmap` can be found in the [Built-in Maps](https://jgcri.github.io/rmap/articles/vignette_map.html#maps) section of the user guide.
2. **Direct data table to map**: Existing packages are not able to plot a map directly given only a simple data frame or a CSV file as an input. `rmap` has an automatic `map_find_df` function that searches for the appropriate built-in map based on the regions provided in a `subRegion` column and values in a `value` column. The sub-regions in the `subRegion` column must be one of the sub-regions in the existing set of `rmap` built-in maps. This truly frees users from the need for any other spatial data needs and they can simply `map()` their own data tables directly. 
3. **Difference maps**: Existing packages do not produce difference maps to compare across scenarios or time periods. `rmap` provides this functionality by automatically recognizing multiple scenarios and time periods to produce difference maps across these dimensions. An important aspect of spatial data is exploring the difference between two scenarios or time periods and `rmap` makes this a seamless process.
4. **Post-process customization**: Existing packages do not produce output objects that can be saved and then customized. Customization of the maps in existing packages is limited to package specific functionality and arguments. `rmap` produces `ggplot` objects in which every element (axis, grids, titles, colors, line widths, facets) can all be customized after the map has been produced. This allows users to capitalize on existing knowledge of the widely used `ggplot2` package and its arguments.

<!-- ------------------------>
<!-- ------------------------>
# <a name="Publications"></a>Related Publications
<!-- ------------------------>
<!-- ------------------------>

- Khan, Z., Wild, T., Carrazzone, M.E.S., Gaudioso, R., Mascari, M.P., Bianchi, F., Weinstein, F., Pérez, F., Pérez, W., Miralles-Wilhelm, F. and Clarke, L., 2020. Integrated energy-water-land nexus planning to guide national policy: an example from Uruguay. Environmental Research Letters. DOI: https://doi.org/10.1088/1748-9326/ab9389

- Khan, Z., Wild, T., Vernon, C., Miller, A., Hejazi, M., Clarke, L., Miralles-Wilhelm, F., Castillo, R.M., Moreda, F., Bereslawski, J.L., Suriano, M. and Casado, J., 2020. Metis – A Tool to Harmonize and Analyze Multi-Sectoral Data and Linkages at Variable Spatial Scales. Journal of Open Research Software, 8(1), p.10. DOI: http://doi.org/10.5334/jors.292

  
