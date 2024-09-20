# Make basic data for faster testing

base_dat <- testhelp_make_dat()
base_proflux <- pro_flux(base_dat)


saveRDS(base_dat, testthat::test_path("fixtures", "base_dat.rds"))
saveRDS(base_proflux, testthat::test_path("fixtures", "base_proflux.rds"))
