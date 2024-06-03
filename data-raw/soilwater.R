#' code to prepare the 'soilwater' dataset.
#'

library(dplyr)

#Dates
dates <- seq.Date(as.Date("2021-01-01"),
                  by = "1 months",
                  length.out = 12)


#A factor to mimic a typical annual cycle beginning with january
date_factor <- c(1,
                 0.9,
                 0.85,
                 0.9,
                 0.7,
                 0.6,
                 0.5,
                 0.45,
                 0.4,
                 0.45,
                 0.6,
                 0.9)

sites <- c("site_a","site_b")


upper_a <- c(5,0,-7,-15,-23,-40,-60)
upper_b <- c(7,3,0,-5,-13,-20,-40,-60)

lower_a <- c(0,-7,-15,-23,-40,-60,-100)
lower_b <- c(3,0,-5,-13,-20,-40,-60,-100)

TPS_a <- c(84,52,45,45,38,38,38)/100
TPS_b <- c(80,80,61,61,20,20,35,35)/100

swc_a <- c(0.2,0.21,0.22,0.25,0.3,0.35,0.4)
swc_b <- c(0.18,0.21,0.22,0.24,0.25,0.28,0.36,0.43)


#a standard deviation to mimic natural data in ppm
sd_swc_a <- rep(0.01,7)*seq(1,1.5,length.out = 7)/TPS_a
sd_swc_b <- rep(0.01,8)*seq(1,1.5,length.out = 8)/TPS_b



#create dataframe
soilwater <- data.frame(site = c(rep("site_a",7),
                                  rep("site_b",8)),
                      upper = c(upper_a,upper_b),
                      lower = c(lower_a,lower_b),
                      swc_seed = c(swc_a,swc_b),
                      sd_swc = c(sd_swc_a,sd_swc_b),
                      TPS = c(TPS_a,TPS_b)
)

soilwater <-
soilwater %>%
  rowwise()%>%
  reframe(Date = dates,
            date_factor = date_factor,
            site = rep(site),
            upper = rep(upper),
            lower = rep(lower),
            swc_seed = rep(swc_seed),
            TPS = rep(TPS),
            sd_swc = rep(sd_swc))

set.seed(42)

soilwater <-
soilwater %>%
  rowwise() %>%
  mutate(SWC = date_factor * rnorm(1,swc_seed,sd_swc)*TPS) %>%
  select(site,
         Date,
         upper,
         lower,
         SWC)


#soilwater %>%
#  ggplot(aes(y=SWC,x=Date,col = factor(upper)))+
#  geom_line()+
#  facet_wrap(~site)


usethis::use_data(soilwater,
                  soilwater,
                  overwrite = T)

