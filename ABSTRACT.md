
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

R packages: geotopbricks,stringr, ggplot2,shiny,geotopOptim2(github)

References:
- Foo et al (2018): Bars and bars. URL



