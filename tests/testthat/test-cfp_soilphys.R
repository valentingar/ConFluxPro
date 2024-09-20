test_that("create cfp_soilphys object",{

  df <- data.frame(site = rep(c("site_a","site_b"),
                              each = 3),
                   upper = rep(c(10,0,-20),
                               times = 2),
                   lower = rep(c(0,-20,-100),
                               times = 2),
                   c_air = 1,
                   DS = 1,
                   gas = "CO2")

  x <- cfp_soilphys(df,
                    id_cols = "site")

  expect_message(cfp_soilphys(df,
                              id_cols = "site"))
  expect_error(cfp_soilphys(df))
  expect_true(inherits(x, "cfp_soilphys"))
  expect_equal(cfp_id_cols(x),c("site","gas"))

  # should result in error because of upper / lower consistency
  expect_error(cfp_soilphys(df,id_cols = c()))

})

test_that("cfp_soilphys works", {

  soilphys <-
    ConFluxPro::soilphys %>%
    cfp_soilphys(id_cols = c("site", "Date"))

  expect_equal(class(soilphys), c("cfp_soilphys","cfp_layered_profile","cfp_profile","data.frame"))

})

test_that("cfp_soilphys no negatives allowed", {
  library(dplyr)

  soilphys1 <- ConFluxPro::soilphys %>%
    mutate(c_air = -c_air)

  soilphys2 <- ConFluxPro::soilphys %>%
    mutate(DS = -DS)

  expect_error(soilphys1 %>%
                 cfp_soilphys(id_cols = c("site", "Date"))
  )
  expect_error(soilphys2 %>%
                 cfp_soilphys(id_cols = c("site", "Date"))
  )

})
