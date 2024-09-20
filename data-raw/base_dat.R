### Unit all example datas into one cfp_dat object


# !! need to run all other data definitions first
# + devtools::load_all()
base_dat <- testhelp_make_dat()

usethis::use_data(base_dat,
                  overwrite = T)

