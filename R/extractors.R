#' @title Extract elements from an object
#'
#' @description These functions extract components from
#' different objects that can be created in ConFluxPro.
#'
#' @param x An object from which to extract the information.
#' @name extractors
#'
#' @returns The extracted component, e.g. a \code{data.frame()} or
#' \code{character()}.
#'
#' @examples
#' my_data <- ConFluxPro::base_dat |>
#'   filter(Date == "2021-01-01") # subset for example = faster runtime
#'
#' ### from cfp_dat objects (and derivatives)
#' cfp_id_cols(my_data)
#'
#' cfp_gasdata(my_data) |> head()
#' cfp_soilphys(my_data) |> head()
#' cfp_layers_map(my_data) |> head()
#' my_data$profiles |> head()
#'
#' ### from cfp_pfmod or cfp_pfres objects
#' PROFLUX <- my_data |> pro_flux()
#' cfp_zero_flux(PROFLUX)
#' cfp_zero_limits(PROFLUX)
#' cfp_DSD0_optim(PROFLUX) #deprecated
#' cfp_evenness_factor(PROFLUX)
#' cfp_known_flux_factor(PROFLUX)
#' PROFLUX$PROFLUX |> head()
#'
#' ### from cfp_fgmod or cfp_fgres objects
#' FLUX <- my_data |> fg_flux()
#'
#' cfp_gases(FLUX)
#' cfp_modes(FLUX)
#' cfp_param(FLUX)
#' cfp_funs(FLUX)
#' FLUX$FLUX |> head()
#'
#' ### from cfp_run_map
#' set.seed(42)
#' my_run_map <-
#' cfp_run_map(
#'   PROFLUX,
#'   list("TPS" = c(0.9, 1.1)),
#'   "factor",
#'   n_runs = 2)
#'
#' cfp_params_df(my_run_map)
#' cfp_n_runs(my_run_map)
#' cfp_layers_from(my_run_map)
#' cfp_layers_different(my_run_map)
#' cfp_runmap_type(my_run_map)
#' cfp_layers_altmap(my_run_map)
#'
#' ### from cfp_altres
#' my_altres <-
#' alternate(
#'   x = PROFLUX,
#'   f = \(x) complete_soilphys(x, "a+AFPS^b", quiet = TRUE),
#'   run_map = my_run_map)
#'
#' cfp_og_model(my_altres)
#' cfp_run_map(my_altres)
#'
#' @order 1
NULL
