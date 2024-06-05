test_that("no errors in plot_profile()", {

  PROFLUX <- readRDS(test_path("fixtures", "base_proflux.rds")) %>%
    filter(prof_id < 3)

  FLUX <- fg_flux(PROFLUX)


  expect_no_error(p <- plot_profile(PROFLUX))
  expect_true(ggplot2::is.ggplot(p))
  expect_no_error(p <- plot_profile(FLUX))
  expect_true(ggplot2::is.ggplot(p))
  expect_no_error(p <- plot_profile(get_soilphys(PROFLUX)))
  expect_true(ggplot2::is.ggplot(p))
  expect_no_error(p <- plot_profile(get_gasdata(PROFLUX)))
  expect_true(ggplot2::is.ggplot(p))
  expect_no_error(p <- plot_profile(get_layers_map(PROFLUX)))
  expect_true(ggplot2::is.ggplot(p))




})
