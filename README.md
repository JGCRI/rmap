<!-- badges: start -->
[![codecov](https://codecov.io/gh/JGCRI/rmap/branch/main/graph/badge.svg?token=XQ913U4IYM)](https://codecov.io/gh/JGCRI/rmap) 
[![build](https://github.com/JGCRI/rmap/workflows/build/badge.svg)](https://github.com/JGCRI/rmap/workflows/build/badge.svg)
[![docs](https://github.com/JGCRI/rmap/actions/workflows/docs.yaml/badge.svg?branch=main)](https://github.com/JGCRI/rmap/actions/workflows/docs.yaml)
<!-- badges: end -->


<!-- ------------------------>
<!-- ------------------------>
# <a name="Introduction"></a>Introduction
<!-- ------------------------>
<!-- ------------------------>

`rmap` is comprehensive mapping package to create easily customizable maps and compare across scenarios, years and data classes. The package allows creation of difference maps as well as customizable legends, color palettes and styles.

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

Khan, Z., Zhao, M., Wild, T., Vernon, C., 2021. rmap - An R package to easily plot polygon and gridded data on highly customizable built-in and user maps. (In progress) Journal of Open Source Software, DOI: XXXX

<!-- ------------------------>
<!-- ------------------------>
# <a name="InstallGuide"></a>Installation Guide
<!-- ------------------------>
<!-- ------------------------>

1. Download and install:
    - R (https://www.r-project.org/)
    - R studio (https://www.rstudio.com/)  
    
    
2. Open R studio:

```r
install.packages("devtools")
devtools::install_github("JGCRI/jgcricolors")
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

<!-- ------------------------>
<!-- ------------------------>
# <a name="Publications"></a>Related Publications
<!-- ------------------------>
<!-- ------------------------>

- Khan, Z., Wild, T., Carrazzone, M.E.S., Gaudioso, R., Mascari, M.P., Bianchi, F., Weinstein, F., Pérez, F., Pérez, W., Miralles-Wilhelm, F. and Clarke, L., 2020. Integrated energy-water-land nexus planning to guide national policy: an example from Uruguay. Environmental Research Letters. DOI: https://doi.org/10.1088/1748-9326/ab9389

- Khan, Z., Wild, T., Vernon, C., Miller, A., Hejazi, M., Clarke, L., Miralles-Wilhelm, F., Castillo, R.M., Moreda, F., Bereslawski, J.L., Suriano, M. and Casado, J., 2020. Metis – A Tool to Harmonize and Analyze Multi-Sectoral Data and Linkages at Variable Spatial Scales. Journal of Open Research Software, 8(1), p.10. DOI: http://doi.org/10.5334/jors.292

  
