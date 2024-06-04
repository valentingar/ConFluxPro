test_that("error conc pfres works", {

  PROFLUX <- readRDS(test_path("fixtures", "base_proflux.rds"))

  expect_equal(error_concentration(PROFLUX, normer = "sd") %>%
                 dplyr::mutate(NRMSE = round(NRMSE, 3)) |>
                 dplyr::ungroup(),
               tibble::tibble(site = c("site_a", "site_b"),
                      gas = "CO2",
                      NRMSE = c(0.209, 0.237)))

})

test_that("error efflux pfres works", {

  PROFLUX <- readRDS(test_path("fixtures", "base_proflux.rds"))

  expect_equal(error_efflux(PROFLUX,
                            EFFLUX = efflux(PROFLUX),
                            normer = "sd",
                            param_cols = "site"),
               tibble::tibble(site = c("site_a", "site_b"),
                              NRMSE = c(0, 0)))

})
