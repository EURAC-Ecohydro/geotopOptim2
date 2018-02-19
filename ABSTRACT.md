# geotopOptim2
============

TEMPLATE


This is an example abstract for a hands-on tutorial features a brief introduction to **foo** using *bar*, then provides several case studies focusing on performance improvements on **bar**.

R packages: knitr, pander, ggplot2

References:
- Foo et al (2018): Bars and bars. URL








BLA BLA ...
Nowadays distributed eco-hydrolsharepointogical models are increasingly used in the context of precision agriculture, since they allow to account for a detailed description of the water cycle at various scales: local scale, where the analysis is performed on a specific site, or spatially distributed scale, where the analysis is performed over a larger area, e.g. an
hillslope or a watershed. In these contexts, working in hydrological modeling one produces bigger and bigger amount of raw data. Therefore the need to develop flexible and user-oriented interfaces to visualize multiple outputs, perform sensitivity analyzes and compare
against observations emerges. The aim of this work is to focus the attention on the
coupled use of hydrological models and open-source tools for data analysis. An example
considering the GEOtop hydrological distributed model and the R programming language
and software environment is presented here. The GEOtop model solves water mass and
energy budget within soil and in a specific region or site, it handles meteorological
variables and produces spatio-temporal maps of soil water content, soil temperature, snow
depth, etc.. An R package called *geotopbricks* ([https://github.com/ecor/geotopbricks] and [] ) contains functions that are able to read the GEOtop configuration file and browse all input/output data of the model. Moreover, an R-based automatic calibration procedure
called *geotopOptim2* has been developed applied to GEOtop. The open-suorce tool
geotopOptim is published as an R package on https://github.com/ecor/geotopOptim . It is
based on the Particle Swarm Optimization approach included in the “hydroPSO” R
package). Further R tools based on Rmarkdown and Shiny allow a quick and interactive
way to visualize the results. Thus, all GEOtop variables are handled to extract useful
information to end-users and researchers, within the R platform for spatio-temporal
aggregation, analysis, reproducible research and web visualization, making use of actually
about 8000 R packages available on the official CRAN (Comprehensive R Archive
Network) plus the ones released as open-source software in the Github or other similar
repositories. Finally, an exercise to analyze in details modeled soil moisture and
evapotranspiration in some alpine agricultural sites with this interactive web visualization is
presented.

The R package geotopOtim2 is a plugin for the automatic calibration and sensitivity analisis of GEOtop 2.x hydrological model, based on the "Particle Swarm Optimisation" approach and the LHOAT "Latin-Hypercube One-factor-At-a-Time approach.

The package is a wrapper for GEOtop of the  R package:

[hydroPSO](https://cran.r-project.org/web/packages/hydroPSO/index.html) "Enhanced Particle Swarm Optimisation algorithm", wich has been updated to support MPI <https://gitlab.inf.unibz.it/Samuel.Senoner/hydroPSO>

To install this package on R fram R console:

## Installation

From R console:

```
>library(devtools)

>install_github("ecor/geotopbricks")
>install.packages("hydroGOF")
>install_git("https://gitlab.inf.unibz.it/Samuel.Senoner/hydroPSO")
>install_github("EURAC-Ecohydro/geotopOptim2")
```

### You might need before to intall also MPI under R and the R package snow

```
>install.packages("Rmpi") 
>install.packages("snow")
```

## [Package Documentetion: Vignette](https://github.com/EURAC-Ecohydro/geotopOptim2/blob/master/vignettes/Calibration.Rmd)


