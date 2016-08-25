
# geotopOptimPSO Calibration/Optimization Wrapper R Package for the hydrological model GEOtop.

## Installation note on GEOtop 

GEOtop (Endrizzi et al, 2014 and references therein) is a distributed model of the mass and energy balance of the hydrological cycle. GEOtop is applicable to simulations in continuum in small or relatively large montain catchments. GEOtop deals with the effects of topography on the interaction between energy balance and hydrological cycle (water, glacier and snow) with peculiar solutions. The source code of GEOtop 2.0 with detailed documentation is available for researchers and environmental/software engineers through the following links:

* https://code.google.com/p/geotop/
* https://github.com/se27xx/GEOtop/
* https://github.com/skyglobe/geotop

For  Unix-like OS users, a C source code can be rapidly downloaded and built with the following instrunctions (https://github.com/se27xx/GEOtop/):


1. Open a console and go to the drectory where to clone GEOtop source code;
2. Clone the source code typing: "git clone https://github.com/se27xx/GEOtop";
3. Enter GEOtop directory typing: "cd GEOtop";
4. Create the subdirectory for the executable file  "mkdir bin";
5. Build and create GEOtop executale file: "make -f geotop.make"

GEOtop executable will be created in the subdirectory "bin". 
For other versions of GEOtop, please see the related URLs. 

## GEOtop calibration with "geotopOptim"

THe package can be installed within R by following the instruction above: 
```R

> library(devtools)
>
> install_github("ecor/geotopbricks")
> install_github("ecor/geotopOptim")

```
For a "geotopOptim" within the R environment, see the package documentation. 
The package contains an R script whick you can get by typing 'system.file("examples/example.geotop.pso.R",package="geotopOptim")' from the R console.
This scripts calibrates soil parameters for soil moisture modeling with GEOtop hydrological model. THe calibration algorithm is based on Particle Swarm Optimization. 
Actually this script works for a 1D (point/local scale) usage of the Hydrological model GEOtop. 
To run the script, see the usage and the options which can be omitted and replaced by the related enviroment variables:

```
>
>Usage: ./example.geotop.pso.R  -wpath_out $GM_WPATH_OUT  -optim-soil-param $GM_OPTIM_PARAM_CSV_FILE -geotopbin $GM_GEOTOP_BIN -wpath_simpath $GM_GEOTOP_DATA 
>
>Options: 
>
>-wpath_out            directory for output files
>-geotopbin            GEOtop executable/binary file (full path)
>-wpath_simpath        path to directory containing GEOtop simulation  (i.e. 'geotop.inpts' file)
>					  See for example: 'system.file("Muntatschini_pnt_1_225_B2_004",package="geotopOptim") from an R console.
>
>-wpath_runpath        directory where to run GEOtop (optional). By default is the same directory given by '-wpath-out'
>-optim-soil-param     full name of the CSV file containing ranges of soil calibration parameter. 
>					  See for example: 'system.file("examples/param/param.csv",package="geotopOptim")' from an R console.
>				   
>--help                help with  'example.geotop.pso.R' options and flags
```
## How to set GEOtop calibration parameters with "geotopOptim"


The function "geotopPSO" calibrates 1D simulation profile mastering the soil property profile file, often referred as "soil0001.txt". 
An example for CSV file requested by the option "-optim-soil-param" or the environment variable "GM_OPTIM_PARAM_CSV_FILE" is the following: 



```
prefix__name,lower,upper
SOIL__N,1.45,1.89
SOIL__Alpha,0.00131,0.0131
SOIL__ThetaSat,0.41,0.53
SOIL__ThetaSat_bottomlayer,0.08,0.09
SOIL__ThetaRes,0.05,0.07
SOIL__LateralHydrConductivity,0.0923,0.1
SOIL__NormalHydrConductivity,0.0923,0.1
SOIL__LateralHydrConductivity_bottomlayer,0.00923,0.01
SOIL__NormalHydrConductivity_bottomlayer,0.00923,0.01
SOIL__SoilInitPresL0001,-10000,100
SOIL__SoilInitPresL0002,-10000,100
SOIL__SoilInitPresL0003,-10000,100
SOIL__SoilInitPresL0004,-10000,100
SOIL__SoilInitPresL0005,-10000,100
SOIL__SoilInitPresL0006,-10000,100
SOIL__SoilInitPresL0007,-10000,1000
SOIL__SoilInitPresL0008,-10000,1000
SOIL__SoilInitPresL0009,-10000,0
SOIL__PsiGamma,0.5,1
SOIL__SoilDepth,3000,30000
SOIL__NumberOfSoilLayers,9,20
VECTOR_1_LSAI,2,4
```

where "N", "Alpha", "ThetaSat", "LateralHydrConductivity", "NormalHydrConductivity", "ThetaRes",,.. are GEOtop keyword referred to the respective soil parameters. By default, "geotopPSO" considers soil parameters uniformly distributed within the soil profile unless they are repated in the CSV file with some suffixes, like "_bottomlayer"  or "_ALL". In "_bottomlayer" case, the parameter (upper and lower) values are referred to the first (near surface) layer and the last (bottom) layer and the values of internal layers are exponentially interpolated. In this case a decrease of hydraulic conductivity or soil porosity can be modeled. In "_ALL" case, soil parameter is taken as variables with soil layers. So the reported values refer to a range for so many soil parameter of the same type how many the soil layers are.
 The keyword "SoilInitPres" refers to the initial soil water pressure head. If the formatter "L%04d" is appended as a suffix, the value is referred to the indicated layer. The keyword "PsiGamma" refers to the soil water pressure gradient along the terrain-normal downward direction and is applied to calculate initial soil water pressure head in the above layers assuming a continuous profile. 
"SoilDeth" and "NumberOfSoilLayers" refer to the whole soil depth (which now corresponds to the whole soil column used as domain for balance equation integration) and the number of soil layers in which the soil column is divided. Soil layer thickness increase with depth following a geometric progression.









## References: 

* Endrizzi, S., Gruber, S., Dall'Amico, M., and Rigon, R. (2014): GEOtop 2.0: simulating the combined energy and water balance at and below the land surface accounting for soil freezing, snow cover and terrain effects, Geosci. Model Dev., 7, 2831-2857, doi:10.5194/gmd-7-2831-2014, 2014, http://www.geosci-model-dev.net/7/2831/2014/gmd-7-2831-2014.html

* Zambrano-Bigiarini, M.; R. Rojas (2013), A model-independent Particle Swarm Optimisation software for model
 calibration, Environmental Modelling & Software, 43, 5-25, doi:10.1016/j.envsoft.2013.01.004

*  Zambrano-Bigiarini, M., Rojas, R.(2014). hydroPSO: Particle Swarm Optimisation, with focus on Environmental Models. R
  package version 0.3-4.

* Cordano E.,  Andreis D. and Zottele F. (2015). geotopbricks: An R Plug-in for the Distributed
  Hydrological Model GEOtop. R package version 1.3.6. http://CRAN.R-project.org/package=geotopbricks


