test_that("proflux_alternate works", {


  data("gasdata")
  data("soilphys")

  lmap <- soilphys %>%
    select(upper,site) %>%
    distinct() %>%
    group_by(site) %>%
    slice_max(upper) %>%
    summarise(upper = c(upper,0),
              lower = c(0,-100),
              lowlim = 0,
              highlim = 1000,
              layer_couple = 0,
              layer = c(1,1))

  PROFLUX <-
    pro_flux(gasdata,
             soilphys,
             lmap,
             c("site","Date"))

  EFFLUX <-
    PROFLUX %>%
    pf_efflux()

  error_funs <-
    list(gd = error_gasdata,
         ef = error_efflux)

  error_args <-
    list(gd = list(param_cols = c("site"),
                   normer = "sd"),
         ef = list(EFFLUX = EFFLUX,
                   param_cols = "site",
                   normer = "sd")
    )


  PF2 <-
  proflux_alternate(PROFLUX,
                    "TPS",
                    1.0,
                    params_map = lmap %>%
                      select(upper,lower,site),
                    no_confirm = TRUE,
                    DSD0_formula = "a*AFPS^b",
                    error_funs = error_funs,
                    error_args = error_args)

  error_funs2 <-
    list(MC = error_compare_models)

  error_args2 <-
    list(MC = list(PF_summary = proflux_summarise(PROFLUX),
                   param_cols = "site"))

  PF3 <-
    proflux_alternate(PROFLUX,
                      c("TPS"),
                      facs = c(1.5,2),
                      params_map = lmap %>%
                        select(upper,lower,site),
                      sensitivity_analysis = "OAT",
                      no_confirm = TRUE,
                      DSD0_formula = "a*AFPS^b",
                      error_funs = error_funs2,
                      error_args = error_args2)

  PF4 <-
    proflux_alternate(PROFLUX,
                      c("TPS"),
                      facs = c(1.5),
                      params_map = lmap %>%
                        select(upper,lower,site),
                      sensitivity_analysis = "OATL",
                      no_confirm = TRUE,
                      DSD0_formula = "a*AFPS^b",
                      error_funs = error_funs2,
                      error_args = error_args2)


  expect_equal(nrow(PF2$results),4)
  expect_equal(any(is.na(PF2$results$NRMSE)),FALSE)
  expect_equal(round(PF2$results$NRMSE,digits = 2),
               c(0.21,0.24,0,0))

  expect_equal(nrow(PF3$results),18)
  expect_equal(unique(PF3$results$run_id),c(1,2,3))

  expect_equal(unique(PF4$run_map$run_id),c(1,2,3))

})

test_that("proflux_alternate topheight_var",{



  data("gasdata")
  data("soilphys")

  lmap <- soilphys %>%
    select(upper,site) %>%
    distinct() %>%
    group_by(site) %>%
    slice_max(upper) %>%
    summarise(upper = c(upper,0),
              lower = c(0,-100),
              lowlim = 0,
              highlim = 1000,
              layer_couple = 0,
              layer = c(1,1))

  PROFLUX <-
    pro_flux(gasdata,
             soilphys,
             lmap,
             c("site","Date"))


expect_warning(
  PF2 <-
    proflux_alternate(PROFLUX,
                      "TPS",
                      1.0,
                      params_map = lmap %>%
                        select(upper,lower,site),
                      topheight_var = c(-10),
                      no_confirm = TRUE,
                      DSD0_formula = "a*AFPS^b",
                      return_raw = TRUE,
                      error_args = NULL,
                      error_funs = NULL)
 )

  expect_equal(class(PF2),"list")
}
)


test_that("oatl map maker works",{

  params_map <- data.frame(upper = c(5,0,7,0),
                           lower = c(0,-100,0,-100),
                           site = c("site_a","site_a","site_b","site_b"),
                           gr_id = c(1,1,2,2),
                           layer_alt = c("A","B","A","B"))

  facs_map <- data.frame(fac_TPS = 1,
                         perm_id = 1)

  facs_map_2 <- data.frame(fac_TPS = c(1,2),
                           perm_id = c(1,2))

  run_map <- make_map_oatl(params_map,
                           facs_map
                           ) %>%
    dplyr::arrange(gr_id,run_id,layer_alt)

  run_map_2 <- make_map_oatl(params_map,
                             facs_map_2) %>%
    ungroup() %>%
    dplyr::arrange(gr_id,run_id,layer_alt,perm_id) %>%
    as.data.frame()

  run_map_2_exp <-
    list({params_map %>%
        dplyr::mutate(fac_TPS =
                        ifelse(layer_alt == "A",
                                      2,
                                      1),
                      run_id = 1)},
        {params_map %>%
            dplyr::mutate(fac_TPS =
                            ifelse(layer_alt == "A",
                                          1,
                                          2),
                          run_id = 3)},
        {params_map %>%
            dplyr::mutate(fac_TPS = 1,
                          run_id = 2)

        }
        ) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(facs_map_2) %>%
    dplyr::arrange(gr_id,run_id,layer_alt,perm_id)

  run_map_2_exp <-
    run_map_2_exp[,c(4,1,2,3,5,6,8,7)]

  run_map_exp <-
    params_map %>%
    dplyr::left_join(facs_map,
                     by = character()) %>%
    dplyr::mutate(run_id = 1) %>%
    dplyr::arrange(gr_id,run_id,layer_alt)

  run_map_exp <-
    run_map_exp[,c(4,1,2,3,5,6,7,8)]


  expect_equal(run_map %>%
                 as.data.frame(),
               run_map_exp)

  expect_identical(nrow(run_map_2),
                   nrow(run_map_2_exp))

})


