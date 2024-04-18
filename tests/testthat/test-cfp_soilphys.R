test_that("cfp_soilphys works", {

  soilphys <-
    ConFluxPro::soilphys %>%
    cfp_soilphys(id_cols = c("site", "Date"))

  expect_equal(class(soilphys), c("cfp_soilphys", "data.frame"))

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
