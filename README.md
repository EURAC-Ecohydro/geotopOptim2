# geotopOptim2
============

The R package geotopOtim2 is a plugin for the automatic calibration and sensitivity analisis of GEOtop 2.x hydrological model via the  R package hydroPSO, which has been modified in order to support MPI

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

## Package Documentetion: Vignette

<geotopOptim2/vignettes/Calibration.Rmd>
