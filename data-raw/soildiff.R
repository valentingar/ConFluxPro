


sites <- c("site_a","site_b")

upper_a <- c(5,0,-7,-23)
upper_b <- c(7,0,-13,-40)

lower_a <- c(0,-7,-23,-100)
lower_b <- c(0,-13,-40,-100)

TPS_a <- c(84,52,45,38)
TPS_b <- c(80,61,20,35)

a_a <- c(0.35,0.8,1.05,1.2)
a_b <- c(0.4,0.7,0.8,1.2)

b_a <- c(1.2,1.7,1.6,1.5)
b_b <- c(1.5,1.4,1.3,1.5)

soildiff <- data.frame(site = rep(sites,each= 4),
                       upper = c(upper_a,upper_b),
                       lower = c(lower_a,lower_b),
                       TPS = c(TPS_a,TPS_b)/100,
                       a = c(a_a,a_b),
                       b = c(b_a,b_b))

usethis::use_data(soildiff,
                  soildiff,
                  overwrite = T)

