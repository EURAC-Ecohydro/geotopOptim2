R and  Hydrology: an example with GEOtop Hydrological Model

Author: Emanuele Cordano, Giacomo, Bertoldi, Samuel Senoner

Nowadays distributed eco-hydrolsharepointogical models are increasingly used in the context of precision agriculture, since they allow to account for a detailed description of the water cycle at various scales: local scale, where the analysis is performed on a specific site, or spatially distributed scale, where the analysis is performed over a larger area, e.g. an
hillslope or a watershed. In these contexts, working in hydrological modeling one produces bigger and bigger amount of raw data. Therefore the need to develop flexible and user-oriented interfaces to visualize multiple outputs, perform sensitivity analyzes and compare
against observations emerges. The aim of this work is to focus the attention on the
coupled use of hydrological models and open-source tools for data analysis. An example
considering the GEOtop hydrological distributed model and the R programming language
and software environment is presented here. The GEOtop model solves water mass and
energy budget within soil and in a specific region or site, it handles meteorological
variables and produces spatio-temporal maps of soil water content, soil temperature, snow
depth, etc.. An R package called *geotopbricks* ([https://github.com/ecor/geotopbricks] and [https://CRAN.R-project.org/package=geotopbricks.it] ) contains functions that are able to read the GEOtop configuration file and browse all input/output data of the model. Moreover, an R-based automatic calibration procedure
called *geotopOptim2* has been developed applied to GEOtop. The open-suorce tool
geotopOptim is published as an R package on https://github.com/ecor/geotopOptim . It is
based on the Particle Swarm Optimization approach included in the *hydroPSO*  R
package). Further details and complete R package dendencies are listed in *geotopOtim2* description file. Further R tools based on Rmarkdown and Shiny are implented fwith this tools and  allow a quick and interactive
way to visualize the results. Finally, an exercise to analyze in details modeled soil moisture and
evapotranspiration in some alpine agricultural sites with this interactive web visualization is
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




