test_that("efflux works", {

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
              layer_couple = 0)
  PROFLUX <-
    pro_flux(gasdata,
             soilphys,
             lmap,
             c("site","Date"))

  PF_EF <- PROFLUX %>%
    filter(upper %in% c(5,7)) %>%
    ungroup() %>%
    rename(efflux = flux) %>%
    select(site,Date,efflux) %>%
    tibble() %>%
    arrange(site,Date)

  EFFLUX.PF <-
    pf_efflux(PROFLUX)%>%
    arrange(site,Date)

  PROFLUX.df <- data.frame(PROFLUX)

  EFFLUX.df <-
    pf_efflux(PROFLUX.df,
              id_cols = c("site","Date"))%>%
    arrange(site,Date)


  expect_equal(EFFLUX.PF,
               PF_EF )
  expect_equal(EFFLUX.df,
               PF_EF )

})

test_that("prod works", {

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
              layer_couple = 0)
  PROFLUX <-
    pro_flux(gasdata,
             soilphys,
             lmap,
             c("site","Date"))

  PF_PROD <- PROFLUX %>%
    dplyr::mutate(dprod = prod * height) %>%
    dplyr::group_by(site,Date,layer) %>%
    dplyr::summarise(prod = mean(prod),
      dflux = sum(dprod))

  PROD.PF <-
    pf_prod(PROFLUX)%>%
    arrange(site,Date)

  PROFLUX.df <- data.frame(PROFLUX)

  PROD.df <-
    pf_prod(PROFLUX.df,
              id_cols = c("site","Date"))%>%
    arrange(site,Date)


  expect_equal(PROD.PF,
               PF_PROD )
  expect_equal(PROD.df,
               PF_PROD )

})
