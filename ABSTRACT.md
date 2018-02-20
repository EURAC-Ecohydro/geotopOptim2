R use in Hydrology: an example with R and the hydrological model GEOtop

Author: Emanuele Cordano, Giacomo, Bertoldi, Samuel Senoner

This work presents two R open-source packages *geotopbricks* and *geotopOptim2* with some R visualization tools in order to describe water cycle at the terrein level. Nowadays distributed eco-hydrological models are increasingly used in the context of precision agriculture, since they allow to account for a detailed description of the water cycle at various scales: local scale, where the analysis is performed on a specific site, or spatially distributed scale, where the analysis is performed over a larger area, e.g. an
hillslope or a watershed. In these contexts, working in hydrological modeling one produces bigger and bigger amount of raw data. Therefore the need to develop flexible and user-oriented interfaces to visualize multiple outputs, perform sensitivity analyzes and compare
against observations emerges. An example
considering the GEOtop hydrological distributed model and the R programming language
and software environment is presented here. The GEOtop model solves water mass and
energy budget within soil and in a specific region or site, it handles meteorological
variables and produces spatio-temporal maps of soil water content, soil temperature, snow
depth, etc..  *geotopbricks* ([https://github.com/ecor/geotopbricks] and [https://CRAN.R-project.org/package=geotopbricks] ) contains functions that are able to read the GEOtop configuration file and browse all input/output data of the model. Moreover, an R-based automatic calibration is implented in  *geotopOptim2* ([https://github.com/EURAC-Ecohydro/geotopOptim2])calling *hydroPSO* ([https://CRAN.R-project.org/package=hydroPSO]) package functions.  Further details and complete R package dependencies are listed in *geotopOtim2* description file.  Finally, an analysis of modeled  and observed soil moisture and
evapotranspiration time series in some alpine agricultural sites ([https://github.com/EURAC-Ecohydro/MonaLisa]) are 
presented.

R packages: geotopbricks,stringr,geotopOptim2(github),hydroPSO,hydroGOF,shiny and leaflet (for visualizion)

References:

- Endrizzi, S., Gruber, S., Dall'Amico, M., and Rigon, R. (2014): GEOtop 2.0: simulating the combined energy and water balance at and below the land surface accounting for soil freezing, snow cover and terrain effects, Geosci. Model Dev., 7, 2831-2857, doi:10.5194/gmd-7-2831-2014, 2014, http://www.geosci-model-dev.net/7/2831/2014/gmd-7-2831-2014.html

- Zambrano-Bigiarini, M.; R. Rojas (2013), A model-independent Particle Swarm Optimisation software for model
 calibration, Environmental Modelling & Software, 43, 5-25, doi:10.1016/j.envsoft.2013.01.004

-  Zambrano-Bigiarini, M., Rojas, R.(2014). hydroPSO: Particle Swarm Optimisation, with focus on Environmental Models. R
  package version 0.3-4.

- Cordano E.,  Andreis D. and Zottele F. (2015). geotopbricks: An R Plug-in for the Distributed
  Hydrological Model GEOtop. R package version 1.3.6. http://CRAN.R-project.org/package=geotopbricks




