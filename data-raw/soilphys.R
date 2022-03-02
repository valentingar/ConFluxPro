#### create a completed soilphys dataframe
#### for testing and examples


source("data-raw/soilwater.R")
source("data-raw/soiltemp.R")
source("data-raw/soildiff.R")


library(tidyr)
library(dplyr)
dt <- soilwater %>%
  select(upper,lower,site) %>%
  distinct() %>%
  pivot_longer(cols = c(upper,lower),
               names_to = "tmp",
               values_to = "depth") %>%
  select(site,depth) %>%
  distinct()


soilphys <-
  soildiff %>%
  discretize_depth(param = c("TPS","a","b"),
                   method = "boundary",
                   depth_target = dt,
                   id_cols = "site"
  ) %>%
  left_join(soilwater %>%
              discretize_depth(param = c("SWC"),
                               method = "boundary",
                               depth_target = dt,
                               id_cols = c("site","Date")
              )) %>%
  left_join(soiltemp %>%
              discretize_depth(param = c("t"),
                               method = "linear",
                               depth_target = dt,
                               id_cols = c("site","Date")
              )) %>%
  dplyr::mutate(p = 1013) %>%
  complete_soilphys(gases = "CO2")

usethis::use_data(soilphys,
                  soilphys,
                  overwrite = T)

