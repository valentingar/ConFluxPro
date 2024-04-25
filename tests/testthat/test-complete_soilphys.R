test_that("complete_soilphys works", {
  library(dplyr)

  soilphys_completed <-
    ConFluxPro::soilphys %>%
    complete_soilphys(overwrite = TRUE, "a*AFPS^b")


  expect_equal(soilphys_completed[names(ConFluxPro::soilphys)], ConFluxPro::soilphys)
  expect_message(ConFluxPro::soilphys %>%
                   mutate(a = -a) %>%
                   complete_soilphys(overwrite = TRUE, "a*AFPS^b"),
                 "Negative DSD0 calculated, setting NA!")

  })
