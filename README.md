
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fishcastr

<!-- badges: start -->

[![R-CMD-check](https://github.com/as-french/fishcastr/workflows/R-CMD-check/badge.svg)](https://github.com/as-french/fishcastr/actions)
<!-- badges: end -->

# What is fishcastr?

`fishcastr` is an R package that provides a collection of tools for
generating seasonal migration forecasts for the timing of seaward
movements of diadromous fishes.

# Why forecast diadromous fish movements?

Diadromous fish migration forecasts can inform management of in-river
infrastructures that impede fish movement. Globally, the seaward
movements of diadromous fishes are impeded by anthropogenic in-river
infrastructure (e.g., hydropower generation turbines, flood management
pump stations and dams). To facilitate seaward movement, infrastructure
operators employ measures to reduce direct mortality, such as trap and
transport or temporarily shutting down power generation. The success of
these measures depend on being able to predict the times during which
the largest cohorts of migrating fish will be captured. Advanced warning
of migration timing (e.g., onset and cessation of migration periods)
therefore has the potential to improve the survival of vulnerable
populations.

# What is a seasonal migration forecast?

Seasonal migration forecasts are communicated in terms of probabilistic
predictions of timing anomalies (e.g., early, late or average mean date
of migration). For consistency with other seasonal environmental
forecasts, seasonal migration forecast are produced by applying
multi-member ensemble *seasonal climate forecast* forcing data, such as
[ECMWF’s SEAS5](https://gmd.copernicus.org/articles/12/1087/2019/)
product, to a model chain. This application produces an ensemble of
plausible daily count simulations/scenarios within a target season of
interest (e.g., March through July). From these ensemble simulations,
migration timing statistics (e.g., mean date of migration) are
calculated for each member and ranked by tercile frequencies; i.e., 1/25
members predict lower tercile (early migration), 20/25 predict upper
tercile (late migration), and 4/25 predict middle tercile (average
migration).

# Why develop seasonal forecasts for diadromous fishes?

The potential of *seasonal climate forecasts* for informing
environmental and ecological management through operational forecasts is
receiving increasing attention (e.g., in marine and freshwater settings,
as well as fire risk).

Diadromous fish migrations occur seasonally (e.g., the seaward spring
Atlantic salmon (*Salmo salar*) juvenile (smolt) migration), and
variability in timing among catchments associates with latitude, but
also varies inter-annually within catchments. This inter-annual
variability within catchments is associated with seasonal climate
variability at short and medium time scales. In Europe, for example,
movements of migrating Atlantic salmon (*Salmo salar*), andadromous
brown trout (*Salmo trutta*) and European eel (*Anguilla anguilla*) are
recorded by a sparse network of in-river trapping facilities where fish
counts associate with daily environmental variability related to recent
weather (e.g., river discharge and temperature), and other factors
associated with predator avoidance (e.g., moonlight exposure). Moreover,
prior to sea entry, diadromous fishes undergo a physiological transition
that prepare them for marine endurance (e.g., the smoltification
transition from resident juvenile salmon parr to migratory salmon
smolt), and the initialisation and time it takes for this transition to
complete is associated with seasonal climate variability that can be
quantified (for example) using degree days. If modelled, interactions
among environmental and climate variables associated with environmental
tolerability for movement and physiological preparedness for sea entry
can be used to predict inter-annual variation in the timing of migration
of diadromous fishes. The added value to seasonal predictions is the
advanced warning they can provide for those who are responsible for the
logistics such as trap and transport and power generation contingency
plans.

# What does fishcastr do?

`fishcastr`’s functionality is demonstrated in the package vignettes.
The vignettes “manuscript” and “supplementary materials” describe the
process of accessing and pre-process publicly available fish count data,
environmental data and climate data and use open-source statistical and
process based modelling tools to build a forecast workflow and evaluate
the credibility of its predictions by retrospective skill assessment.

## A non-exhaustive list of functionality

-   Generate ensemble fish count simulations from seasonal climate
    forecast input data and a model chain (see below)
-   Convert ensemble simulations to probabilistic migration timing
    forecasts for a range of seasonal timing summary statistics (mean
    day of migration, percentiles etc.)
-   Visualise uncertainty in probabilistic predictions, building on some
    of the functionality in the
    [visualizeR](https://github.com/SantanderMetGroup/visualizeR)
    package for seasonal climate forecast visualization
-   Predict water temperature from air temperature
-   Calculate moonlight exposure
-   Stratify count census data into “forecast years” based on biological
    information (thermal/daylength thresholds)

***Model chain***

The example seasonal migration forecasts contained in the `fishcastr`
vignettes are derived from a model chain that includes a process based
rainfall run-off model (using [airGR](https://github.com/cran/airGR))
and correlative fish count models (using
[glmmTMB](https://github.com/glmmTMB/glmmTMB)). Additional elements of
the model chain, including a statistical water temperature model and
moonlight exposure estimates are implemented within `fishcastr`.

# How to install

Install Rtools: <https://cran.r-project.org/bin/windows/Rtools/>

Install R package:

``` r
install.packages("devtools")
devtools::install_github("as-french/fishcastr",
                         build_vignettes = FALSE)
```

## Accessing raw data

`fishcastr` was built to support the WATExR project which requires all
outputs to be open access. Therefore, we only use open-access data and
include all code required to reproduce the analysis described in the
“vignettes/Manuscript”. While we do not include published raw data
within the package itself, we maximise the length of our reproducibility
chain by facilitating access to the raw data according to two current
scenarios.

### Scenario 1 \| Data are fully open access

Where data are “fully open-access” (i.e., registration/login credentials
are not required at point of download), we have included several
functions within `fishcastr` that should be run immediately after
package installation to pull several data sources from web archives.
These data sources are as follows:

-   Met Éireann’s [manual and automatic meteorological station
    data](https://www.met.ie//climate/available-data/historical-data);

-   Foras na Mara - Marine Institute’s [diadromous fish
    counts](http://data.marine.ie/geonetwork/srv/eng/catalog.search#/metadata/ie.marine.data:dataset.4343);

-   Foras na Mara - Marine Institute’s [Lough Feeagh surface water
    temperature](http://data.marine.ie/geonetwork/srv/eng/catalog.search#/metadata/ie.marine.data:dataset.3757);

-   Irish Environmental Protection Agency’s [Lough Feeagh water
    levels](https://www.epa.ie/hydronet/#32070);

-   Potsdam Institute for Climate Impact Research’s [pseudo climate
    observations (EWEMBI) for bias correction of climate input
    data](https://dataservices.gfz-potsdam.de/pik/showshort.php?id=escidoc:3928916)

These sources are all licensed under [CC-BY
4.0](https://creativecommons.org/licenses/by/4.0/).

``` r
library(fishcastr)

# Following package installation, download the following sources (and publisher)

# Lough Feeagh 2m depth water temperature (2004 - 2019) (Foras na Mara - Marine
# Institute; MI)
fishcastr::download_Feeagh_water_temp()

# Lough Feeagh water level (1976 - 2019) (Irish Environmental Protection Agency;
# EPA)
fishcastr::download_Feeagh_wlevel()

# Seaward migrating diadromous fish counts in Burrishoole recored at the Mill
# Race and Salmon Leap traps (1970 - 2019) (Foras na Mara - Marine Institute)
fishcastr::download_fish_data()

# Manual and automatic meteorological station data at Furnace, Burrishoole (Met
# Éireann) (manual: 1959 - 2019; automatic: 2005 -2019)
fishcastr::download_Met_Eireann_station_data()

# River water temperature at Mill Race outflow from Lough Feeagh (1970 - 2019).
# This is a proxy for surface water temperature of Lough Feeagh (Foras na Mara -
# Marine Institute)
fishcastr::download_MR_temperature()

# EWEMBI climate pseudo-observations (1979 - 2016) at Burrishoole for infilling gaps in local meteorological station data (Potsdam Institute for Climate Impact Research)
fishcastr::download_EWEMBI()
```

### Scenario 2 \| Data access requires registration

Where website registration is necessary, we have provided R scripts in
the /raw-data/ folder that should facilitate data access if login
credentials are updated by the new user. These data sources are as
follows:

-   ECMWF’s climate reanalysis (ERA5);
-   ECMWF’s seasonal climate forecasts (SEAS5);

#### ISIMIP’s pseudo climate observations (EWEMBI) - accessed through University of Cantabria’s “User Data Gateway”

The raw netcdf files for EWEMBI are archived by [Potsdam Institute for
Climate Impact
Research](https://dataservices.gfz-potsdam.de/pik/showshort.php?id=escidoc:3928916).
However, an archived copy of ISIMIP’s pseudo climate observations
(EWEMBI) are maintained by the University of Cantabria Met Group, who
facilitate targeted data extraction (e.g., grid references, times,
variables), through their R based package bundle
[Climate4R](https://github.com/SantanderMetGroup/climate4R). While it is
necessary to register with the Met Group’s User Data Gateway for data
access (see: <https://meteo.unican.es/trac/wiki/udg/registration>),
`Climate4R` provides tools that facilitate reproducibility.

**NOTE** - when installing `Climate4R` packages, see instructions on
each package’s GitHub page, taking particular care with `loadeR`, owing
to its dependency on a *Java* installation and the R package `rJava`,
which has [known
issues](https://github.com/SantanderMetGroup/loadeR/wiki/Installation).

We recommend installing in following order (see code below for more
details):

-   [rJava](https://cran.r-project.org/web/packages/rJava/index.html)
-   [loadeR.java](https://github.com/SantanderMetGroup/loadeR.java)
-   [climate4R.UDG](https://github.com/SantanderMetGroup/climate4R.UDG)
-   [loadeR](https://github.com/SantanderMetGroup/loadeR)

``` r
# ----------------------------------------------------------------------------------- #
# SETTING UP R FOR fishcastr PACKAGE INSTALLATION (FOR WINDOWS 10)
# ----------------------------------------------------------------------------------- #

# NOTES ----
# Package built using R version 3.6.3 on windows 10
# Might need to disable some aspect of anti-virus software to allow packages to be installed (e.g., "Safe Files" for Bitdefender 2018v onwards)

# DOWNLOAD AND INSTALL Java ----
# Download java 12.0.2 jdk 64 bit (note version 14 is not compatible; date 19-09-2020)
# https://www.oracle.com/java/technologies/javase/jdk12-archive-downloads.html
# see https://www.oracle.com/java/technologies/javase-downloads.html for other versions

# SET NEW ENVIRONMENT VARIABLE "JAVA_HOME" ----
# either manually by adding to environment path JAVA_HOME: C:\Program Files\Java\jdk-12.0.2 
# or set in r using code
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-12.0.2/")

# ADD JDK bin PATH TO SYSTEM ENV PATH ----
# C:\Program Files\Java\jdk-12.0.2\bin to system Path variable

# INSTALL Rtools35 ----
# https://cran.r-project.org/bin/windows/Rtools/history.html

# INSTALL devtools R package ----
# install.packages("devtools")

# INSTALL rJava R package ----
# install.packages("rJava",type = "source")

# INSTALL UNICAN R packages ----
# NOTE - this is not necessary for fishcastr to install, or for vignette
# functions to work, but is necessary for developing forecasts for other regions
# following protocol contained in the Manuscript vignette
#### noting we have installed 64-bit java only; hence "--no-multiarch"
# devtools::install_github("SantanderMetGroup/loadeR.java", INSTALL_opts=c("--no-multiarch"))
# devtools::install_github("SantanderMetGroup/climate4R.UDG", INSTALL_opts=c("--no-multiarch"))
# devtools::install_github("SantanderMetGroup/loadeR", INSTALL_opts=c("--no-multiarch"))
# devtools::install_github(c("SantanderMetGroup/transformeR",
#                            "SantanderMetGroup/downscaleR",
#                            "SantanderMetGroup/visualizeR",
#                            "SantanderMetGroup/drought4R"))
```

Following registration, EWEMBI data can be accessed using the script
provided (/data-raw/01\_data\_grid\_ewembi.R); **NOTE** - this script
should be run immediately preceding running of script
(/data-raw/02\_data\_grid\_met\_obs\_1979\_2019.R).

#### ECMWF data - accessed though the Copernicus “Climate Data Store”

ECMWF’s ERA5 and SEAS5 data are available through the [Copernicus
Climate Data Store (CDS)](https://cds.climate.copernicus.eu/#!/home)
(CDS), which requires registration
(<https://cds.climate.copernicus.eu/user/register>) to obtain an access
key that is stored on the user’s computer. Following registration, data
is downloaded using the CDS Application programming Interface (API;
<https://confluence.ecmwf.int/display/CKB/How+to+download+ERA5>). While
building `fishcastr`, we used the `ecmwfr` to facilitate reproducibility
in the R environment and recommend users of `fishcastr` to familiarise
themselves with `ecmwfr` functionality (e.g., for converting
automatically generated python scripts to R;
<https://cran.r-project.org/web/packages/ecmwfr/vignettes/cds_vignette.html>).
These script conversions form the basis of several R scripts we provide
to download subsets of ERA5 and SEAS5 (e.g., see
/data-raw/grid\_ERA5\_Jan\_2020\_download.R for downloading February to
December 2019 and January 2020 ERA5).

**NOTE** - We accessed re-forecasts and archived operational forecasts
through the University of Cantabria’s User Data Gateway; however, this
access was
[restricted](http://meteo.unican.es/tds5/catalogs/Copernicus/CDS_CopernicusDatasets.html?dataset=Copernicus/SYSTEM5_ecmwf_Seasonal_25Members_SFC.ncml)
to WATExR project participants (amongst other European projects).
Therefore, users of `fishcastr` must access SEAS5 via the [Copernicus
Climate Data
Store](https://cds.climate.copernicus.eu/cdsapp#!/dataset/seasonal-original-single-levels?tab=overview).

# How to use

See vignettes.

# Issues

This package is maturing and has many areas that could be improved. If
you find an issue, please raise it at:
<https://github.com/as-french/fishcastr/issues>.

# Credits

`fishcastr` was built to support the ClimateJPI ERA4CS WATExR project
2017 - 2021 (<https://watexr.eu>).

# Literature cited
