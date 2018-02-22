# R use in Hydrology: an example with R and the hydrological model GEOtop

## Authors: Emanuele Cordano (1), Giacomo, Bertoldi (2), Samuel Senoner (3)
(1) aff
(2) Eurac Research, Institute for Alpine Environment, Bolzano, Italy.
(3) aff

Eco-hydrological models are increasingly used in the contexts of hydrology, ecology, precision agriculture for a detailed description of the water cycle at various scales: local scale, an hillslope or a watershed. However, with increasing computing power and observations avalaible, bigger and bigger amount of raw data are produced. Therefore the need to develop flexible and user-oriented interfaces to visualize multiple outputs, perform sensitivity analyzes and compare against observations emerges.
This work presents two R open-source packages: **geotopbricks** and **geotopOptim2**. They offer an I/0 interface and R visualization tools the GEOtop hydrological distributed model (http://geotopmodel.github.io/geotop/), which  solves water mass and energy budget equations
to describe water cycle in the Earth´s critical zone.
The package  *geotopbricks* (https://github.com/ecor/geotopbricks and https://CRAN.R-project.org/package=geotopbricks ) is able to  to read the GEOtop I/O data of the model. The package  *geotopOptim2* (https://github.com/EURAC-Ecohydro/geotopOptim2) calling the **hydroPSO** (https://CRAN.R-project.org/package=hydroPSO) package can be used for model calibration against observations.  Further details and complete R package dependencies are listed in *geotopOtim2* description file.  
As a demonstration example, an analysis of modeled  and observed soil moisture and evapotranspiration time series in some alpine agricultural sites (https://github.com/EURAC-Ecohydro/MonaLisa) are presented.

R packages: *geotopbricks,stringr,geotopOptim2(github),hydroPSO,hydroGOF,shiny and leaflet (for visualizion)*


Reference:

- Endrizzi, S., Gruber, S., Dall'Amico, M., and Rigon, R. (2014): GEOtop 2.0: simulating the combined energy and water balance at and below the land surface accounting for soil freezing, snow cover and terrain effects, Geosci. Model Dev., 7, 2831-2857, doi:10.5194/gmd-7-2831-2014, 2014, http://www.geosci-model-dev.net/7/2831/2014/gmd-7-2831-2014.html
