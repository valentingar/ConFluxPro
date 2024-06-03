### globals --------------------------------------------------------------------

globals_base <-
  c("upper",
    "lower",
    "depth",
    "height",
    "layer",
    "gas",
    "pmap")

globals_gasdata <-
  c("conc",
    "x_ppm")

globals_soilphys <-
  c("c_air",
    "TPS",
    "SWC",
    "AFPS",
    "DSD0",
    "D0",
    "DS")

globals_layers_map <-
  c("lowlim",
    "highlim",
    "layer_couple")

globals_cfp_dat <-
  c("prof_id",
    "gd_id",
    "sp_id",
    "group_id",
    "id_cols")

globals_pro_flux <-
  c("step_id",
    "prod_abs"
    )


globals_alternate <-
  c("run_id",
    "topheight",
    "param_id",
    "param",
    "param_min",
    "param_max")


utils::globalVariables(
  c(globals_base,
    globals_gasdata,
    globals_soilphys,
    globals_layers_map,
    globals_cfp_dat,
    globals_pro_flux,
    globals_alternate))
