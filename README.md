# geotopOptim2
============

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


