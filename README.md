
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ConFluxPro)](https://CRAN.R-project.org/package=ConFluxPro)
<!-- badges: end -->

# ConFluxPro <a><img src='inst/logo_hexagon.svg' align="right" height="139" /></a>

ConFluxPro is a free toolbox for modelling soil gas fluxes using the
Flux Gradient Method (FGM). It provides functions for data preparation,
a framework for model set-up and implements different FGM models,
including an inverse approach.

<!-- TODO: Insert text to link to papers  -->

## Basis

The Flux Gradient Method (FGM) calculates diffusive flux rates $F$ of
gases from vertical concentration gradients $dc/dz$ in the soil air and
the apparent diffusion coefficient coefficient $D_s$.

$$
F= -D_s\cdot \frac{dc}{dz}
$$

The FGM is an excellent alternative to other methods, such as
Eddy-Covariance or chamber measurements, that can be costly or work
intensive. By measuring the concentration gradients in the soil and
deriving the apparent diffusion coefficient from soil physical
parameters, a continuous and low-impact measurement of soil gas fluxes
and vertical production profiles is possible.

While the basic calculation of fluxes may be simple, FGM requires the
combination of different datasets of varying methods. This is where
ConFluxPro comes in. This package can help to easily process raw data,
combine datasets and set up different model variants in a
straightforward and reproducible manner.

## Workflow

### Data handling

A central idea in ConFluxPro is that each distinct profile, i.e. a
single time point at a given site for a given gas, can be uniquely
identified by a set of columns called `id_cols`.

Different classes help to set up and validate datasets:

- `cfp_gasdata()` A `data.frame` where for each profile there is
  concentration data in a column `x_ppm` at different depths in column
  `depth`.
- `cfp_soilphys()` A `data.frame` with soil physical information. Each
  profile is split into layers defined by their `upper` and `lower`
  boundary, without gaps or overlaps. Each layer has at least
  information of the density of the air `c_air` and the diffusion
  coefficient `DS` for a given gas.
- `cfp_layers_map()` A `data.frame` that is layered similarly and gives
  information for the model structure, i.e. for which layers a
  production rate should be calculated.

``` r
gasdata <- cfp_gasdata(ConFluxPro::gasdata,
                       id_cols = c("site", "Date"))

soilphys <- cfp_soilphys(ConFluxPro::soilphys,
                         id_cols = c("site", "Date"))

layers_map <- cfp_layers_map(ConFluxPro::layers_map,
                             gas = "CO2", lowlim = 0, highlim = 1000,
                             id_cols = "site")
```

These three datasets are then combined in the central data class
`cfp_dat()`, and automatically adjusted to correctly match each other.
This object contains then all necessary information.

``` r
my_dat <- cfp_dat(gasdata, soilphys, layers_map)
```

### Flux modeling

Once a `cfp_dat()` object is created succesfully, the modelling is very
easy:

``` r
# 'normal' forward model
FLUX <- fg_flux(my_dat)
# inverse model
PROFLUX <- pro_flux(my_dat)
```

Each modelling function can be adapted to different needs. For example,
we can provide a different `modes` argument to `fg_flux()` to calculate
the concentration gradient form an exponential fit instead of a linear
model.

``` r
FLUX <- fg_flux(my_dat, modes = "EF")
```

The result in both cases is an object that contains the original data
(`my_dat`) and th flux rates in different soil layers for each of the
profiles identified in `cfp_dat()`. From this, the soil/atmoshere efflux
rate and the specific production rate in each model soil layer can be
extracted.

``` r
# soil/atmosphere efflux
efflux(FLUX)
efflux(PROFLUX)

# per-layer production rate
production(FLUX)
production(PROFLUX)
```

In the case of the forward model (`FLUX`), this may require some
consideration for which method of extrapolation to be used (see the
manual `?efflux`), as different approaches are implemented.

### Extracting information

Most information stored in the objects can be easily extracted.
Extraction functions have the prefix `cfp_`.

``` r
# Get the id_cols that identify the unique profiles of an object:
cfp_id_cols(gasdata)
cfp_id_cols(FLUX)

# Get the layers_map from a combined dataset or model:
cfp_layers_map(my_dat)
cfp_layers_map(PROFLUX)
```

### Big datasets: Paralell processing and progress bars

For big datasets (1000+ profiles), some calculations may takes some
time. ConFluxPro uses the excellent
[`future`](https://github.com/HenrikBengtsson/future) and
[`progressr`](https://github.com/HenrikBengtsson/progressr) packages for
parallel processing and progress bars.

``` r
library(future)
library(progressr)

# enable paralell processing with future
plan(multisession())
# disable
plan(sequential())

# enable progress bars for one function call
with_progress({pro_flux(my_dat)})
# or for all function calls automatically
handlers(global = TRUE)
# and change layout
handlers(handler_progress(format = ":percent [:bar] :eta"))
```

### Post processing

### Conveniance

## Installation

Install the current development version from github:

``` r
# install.packages("remotes")
remotes::install_github("valentingar/ConFluxPro")
```

To get started, check out the provided vignette after installation:

``` r
vignette("overview", package = "ConFluxPro")
```

## Contact

This package is being developed by Valentin Gartiser: code \[at\]
valentingartiser.de

Please contact me if you experience any problems or have questions - I
will be glad to help out where I can.
