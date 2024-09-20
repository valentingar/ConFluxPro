#' code to prepare the 'soiltemp' dataset.
#'

library(dplyr)

#Dates
dates <- seq.Date(as.Date("2021-01-01"),
                  by = "1 months",
                  length.out = 12)


#A factor to mimic a typical annual cycle beginning with january
date_temp <- c(-1,-0,2,3,7,12,14,16,13,10,7,4)
date_factor <- c(seq(1.4,0.95,length.out = 5),0.99,0.95,
                 seq(0.95,1.4,length.out = 5))


sites <- c("site_a","site_b")

depth_a <- c(5,0,-20,-30,-100)
depth_b <- c(7,0,-20,-30,-100)

depth_factors <- c(12,11,9,8,5)

set.seed(42)

soiltemp <- data.frame(site = rep(sites,each = 5),
                       depth = c(depth_a,depth_b),
                       depth_factor = rep(depth_factors,
                                          times =2)) %>%
  rowwise() %>%
  reframe(Date = !!dates,
            site = site,
            depth = depth,
            depth_factor = depth_factor) %>%
  mutate(t = sin((lubridate::month(Date)-4)/6*pi)*depth_factor+5,
         randomiser = rnorm(n(),1,0.1)) %>%
  mutate(randomiser = randomiser +
           ifelse(depth %in% c(5,7),
                  sin((lubridate::month(Date)-4)/6*pi)/6,
                  0)
         ) %>%
  mutate(t = t*randomiser) %>%
  select(site,
         Date,
         depth,
         t)

#soiltemp %>%
#  ggplot(aes(x = Date,
#             y = t,
#             col = factor(depth)))+
#  geom_line()+
#  facet_wrap(~site)




usethis::use_data(soiltemp,
                  soiltemp,
                  overwrite = T)


