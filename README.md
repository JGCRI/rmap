<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/JGCRI/rmap.svg?branch=master)](https://travis-ci.org/JGCRI/rmap)
  [![codecov](https://codecov.io/gh/JGCRI/rmap/branch/master/graph/badge.svg?token=XQ913U4IYM)](https://codecov.io/gh/JGCRI/rmap)
  <!-- badges: end -->

<p align="center"> <img src="READMEfigs/rmapHeaderThick.PNG"></p>

- Github: https://github.com/JGCRI/rmap
- Webpage: https://jgcri.github.io/rmap/
- Cheatsheet: https://github.com/JGCRI/rmap/blob/master/rmapCheatsheet.pdf

<!-- ------------------------>
<!-- ------------------------>
# <a name="Contents"></a>Contents
<!-- ------------------------>
<!-- ------------------------>

- [Introduction](#Introduction)
- [Citation](#Citation)
- [Installation Guide](#InstallGuide)
- [How-to guide](#howto) 
- [Publications](#Publications)

  
<!-- ------------------------>
<!-- ------------------------>
# <a name="Introduction"></a>Introduction
<p align="center"> <img src="READMEfigs/rmapHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

`rmap` is comprehensive mapping package to create easily customizable maps and compare across scenarios, years and data classes. The package allows creation of difference maps as well as customizable legends, color palettes and styles.


<!-- ------------------------>
<!-- ------------------------>
# <a name="Citation"></a>Citation
<p align="center"> <img src="READMEfigs/rmapHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)


<!-- ------------------------>
<!-- ------------------------>
# <a name="InstallGuide"></a>Installation Guide
<p align="center"> <img src="READMEfigs/rmapHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

1. Download and install:
    - R (https://www.r-project.org/)
    - R studio (https://www.rstudio.com/)  
    
    
2. Open R studio:

```r
install.packages(“devtools”)
devtools::install_github(“JGCRI/rgcam”)
devtools::install_github(“JGCRI/rmap”)
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
# <a name="keyfunctions"></a> How to guides
<p align="center"> <img src="READMEfigs/rmapHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

A detailed step-by-step walkthrough and how-to guide is provided on the [How-to](https://jgcri.github.io/rmap/articles/vignette_map.html) page. 


<!-- ------------------------>
<!-- ------------------------>
# <a name="Publications"></a>Publications
<p align="center"> <img src="READMEfigs/rmapHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

- Khan, Z., Wild, T., Carrazzone, M.E.S., Gaudioso, R., Mascari, M.P., Bianchi, F., Weinstein, F., Pérez, F., Pérez, W., Miralles-Wilhelm, F. and Clarke, L., 2020. Integrated energy-water-land nexus planning to guide national policy: an example from Uruguay. Environmental Research Letters. DOI: https://doi.org/10.1088/1748-9326/ab9389

- Khan, Z., Wild, T., Vernon, C., Miller, A., Hejazi, M., Clarke, L., Miralles-Wilhelm, F., Castillo, R.M., Moreda, F., Bereslawski, J.L., Suriano, M. and Casado, J., 2020. Metis – A Tool to Harmonize and Analyze Multi-Sectoral Data and Linkages at Variable Spatial Scales. Journal of Open Research Software, 8(1), p.10. DOI: http://doi.org/10.5334/jors.292

  
