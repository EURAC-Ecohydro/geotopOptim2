R use in Hydrology: an example with R and the hydrological model GEOtop

Author: Emanuele Cordano, Giacomo, Bertoldi, Samuel Senoner

This work presents two R open-source packages *geotopbricks* and *geotopOptim2* with some R visualization tools in order to describe water cycle at the terrein level. Eco-hydrological models are increasingly used in the context of precision agriculture for a detailed description of the water cycle at various scales: local scale, an hillslope or a watershed.  Bigger and bigger amount of raw data are produced. Therefore the need to develop flexible and user-oriented interfaces to visualize multiple outputs, perform sensitivity analyzes and compare against observations emerges. Here is an example
considering the GEOtop hydrological distributed model ([http://geotopmodel.github.io/geotop/]), which  solves water mass and energy budget eqquation within soil and in a specific region or site, and the specific packages  etc..  *geotopbricks* ([https://github.com/ecor/geotopbricks] and [https://CRAN.R-project.org/package=geotopbricks] ) able to  to read the GEOtop I/O data of the model; and   *geotopOptim2* ([https://github.com/EURAC-Ecohydro/geotopOptim2]) calling *hydroPSO* ([https://CRAN.R-project.org/package=hydroPSO]) package functions for model calibration against observations.  Further details and complete R package dependencies are listed in *geotopOtim2* description file.  Finally, an analysis of modeled  and observed soil moisture and
evapotranspiration time series in some alpine agricultural sites ([https://github.com/EURAC-Ecohydro/MonaLisa]) are 
presented.

R packages: geotopbricks,stringr,geotopOptim2(github),hydroPSO,hydroGOF,shiny and leaflet (for visualizion)


Reference:

- Endrizzi, S., Gruber, S., Dall'Amico, M., and Rigon, R. (2014): GEOtop 2.0: simulating the combined energy and water balance at and below the land surface accounting for soil freezing, snow cover and terrain effects, Geosci. Model Dev., 7, 2831-2857, doi:10.5194/gmd-7-2831-2014, 2014, [http://www.geosci-model-dev.net/7/2831/2014/gmd-7-2831-2014.html]