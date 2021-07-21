## code to prepare `gasdata` dataset goes here


#Dates
dates <- seq.Date(as.Date("2021-01-01"),
                  by = "1 months",
                  length.out = 12)

#A factor to mimic a typical annual cycle beginning with january
date_factor <- c(0.1,
                 .15,
                 .2,
                 0.45,
                 0.6,
                 0.7,
                 1.0,
                 0.8,
                 0.6,
                 0.4,
                 0.2,
                 0.15)

#two example sites
sites <- c("site_a","site_b")

#depth of gasdata measurement
depths_a <- c(5,0,-10,-20,-100)
depths_b <- c(7,0,-10,-20,-100)

#replication per depth
reps_a <- c(1,3,3,3,3)
reps_b <- c(1,3,3,3,3)

#which gas this should represent
gas <- "CO2"

#mean concentration per depth relative to atmosphere
mean_conc_a <- c(1,1.1,1.5,2,4)
mean_conc_b <- c(1,1.1,1.2,1.3,2.8)

#a standard deviation to mimic natural data in ppm
sd_conc_a <- c(20,40,60,100,150)
sd_conc_b <- c(20,40,60,100,150)

#create dataframe
gasdata <- data.frame(site = rep(sites,each = 5),
                      depth = c(depths_a,depths_b),
                      reps = c(reps_a,reps_b),
                      mean_conc = c(mean_conc_a,mean_conc_b),
                      sd_conc = c(sd_conc_a,sd_conc_b))

gasdata <- data.frame(Date = rep(dates,each=10),
                      date_factor = rep(date_factor,each=10),
                      rep(gasdata,times=12))

# expand data per replication and mimic natural data by
# calling rnorm
gasdata <-
gasdata %>%
  dplyr::mutate(mean_conc = date_factor*(mean_conc-1)*420+420,
         sd_conc = date_factor*sd_conc) %>%
  dplyr::rowwise() %>%
  dplyr::summarise(site = rep(site,reps),
                   Date = rep(Date,reps),
                   depth = rep(depth,reps),
                   repetition = c(1:reps),
                   NRESULT_ppm = rnorm(reps,
                                       mean_conc,
                                       sd_conc)) %>%
  dplyr::mutate(gas = !!gas)

#gasdata %>%
#  ggplot(aes(y=NRESULT_ppm,
#             x=depth,
#             col = season(Date)))+
#  geom_point()+
#  stat_smooth(geom = "line",
#              method = "lm",
#              formula = y~bs(x,knots = c(0,-10,-20),degree = 1))+
#  facet_wrap(~site)+
#  coord_flip()
#
#gasdata %>%
#  ggplot(aes(x=Date,y=NRESULT_ppm,col = site))+
#  stat_summary(geom = "line")

usethis::use_data(gasdata, gasdata,
                  overwrite = T)
