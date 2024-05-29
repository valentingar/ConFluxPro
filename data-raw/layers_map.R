layers_map <-
  data.frame(
    site = rep(c("site_a", "site_b"), times = 2),
    upper = c(5,7,0,0),
    lower = c(0,0,-100,-100))

usethis::use_data(layers_map,
                  layers_map,
                  overwrite = T)
