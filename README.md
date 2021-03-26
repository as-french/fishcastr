
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fishcastr

<!-- badges: start -->

[![R build
status](https://github.com/as-french/fishcastr/workflows/R-CMD-check/badge.svg)](https://github.com/as-french/fishcastr/actions)
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
of migration). The first step in producing a Seasonal migration
forecasts are produced is to apply a *seasonal climate forecast*
multi-member ensemble, such as ECMWF’s SEAS5 forecast product, to a
model chain to produce an ensemble of plausible daily count
simulations/scenarios within a target season of interest (e.g., March
through July). From these ensemble simulations, migration timing
statistics (e.g., mean date of migration) are calculated for each member
and ranked by tercile frequencies; i.e., 1/25 members predict lower
tercile (early migration), 20/25 predict upper tercile (late migration),
and 4/25 predict middle tercile (average migration).

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
    of the functionality in the `visualizeR` package for seasonal
    climate forecast visualization
-   Predict water temperature from air temperature
-   Calculate moonlight exposure
-   Stratify count census data into “forecast years” based on biological
    information (thermal/daylength thresholds)

***Model chain***

The example seasonal migration forecasts contained in the `forecastr`
vignettes are derived from a model chain that includes a process based
rainfall run-off model (using `airGR`) and correlative fish count models
(using `glmmTMB`). Additional elements of the model chain, including a
statistical water temperature model and moonlight exposure estimates are
implemented within `forecastr`.

# How to install

Install Rtools: <https://cran.r-project.org/bin/windows/Rtools/>

Install R package:

``` r
install.packages("devtools")
devtools::install_github("as-french/fishcastr",
                         build_vignettes = FALSE)
```

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
