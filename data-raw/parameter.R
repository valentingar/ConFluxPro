## code to prepare `parameter` dataset goes here


parameter <-
  data.frame(name = c("x_ppm",       #1
                      "gas",         #2
                      "depth",       #3
                      "upper",       #4
                      "lower",       #5
                      "TPS",         #6
                      "AFPS",        #7
                      "SWC",         #8
                      "t",           #9
                      "p",           #10
                      "DSD0",        #11
                      "DS",          #12
                      "D0",          #13
                      "c_air",       #14
                      "flux",        #15
                      "F0",          #16
                      "prod",        #17
                      "conc",        #18
                      "pmap",        #19
                      "sp_id",       #20
                      "step_id",     #21
                      "prof_id",     #22
                      "RMSE"         #23
                      ),
             description = c("mole fraction of a gas",               #1
                             "name of the gas",                      #2
                             "depth, high values on top",            #3
                             "upper border of layer",                #4
                             "lower border of layer",                #5
                             "total pore space",                     #6
                             "air-filled pore space",                #7
                             "soil water content",                   #8
                             "temperature",                          #9
                             "pressure",                             #10
                             "relative diffusivity",                 #11
                             "effective diffusion coefficient",      #12
                             "diffusion coefficient in air",         #13
                             "number density of air",                #14
                             "particle flux density",                #15
                             "incoming particle flux density",       #16
                             "production rate (source term)",        #17
                             "number concentration",                 #18
                             "integer, mapping unique production rates 1 = lowest", #19
                             "id of the soilphys profile in a cfp_dat object", #20
                             "id of each step within a soilphys profile 1 = lowest", #21
                             "id of distinct profiles within a cfp_dat object", #22
                             "real mean square error"                #23
                             ),
             unit = c("ppm",             #1
                      NA ,               #2
                      "cm",              #3
                      "cm",              #4
                      "cm",              #5
                      "vol/vol",         #6
                      "vol/vol",         #7
                      "vol/vol",         #8
                      "°C",              #9
                      "hPa",             #10
                      "1",               #11
                      "m^2 s^-1",        #12
                      "m^2 s^-1",        #13
                      "mol m^-3",        #14
                      "µmol m^-2 s^-1",  #15
                      "µmol m^-2 s^-1",  #16
                      "µmol m^-3 s^-1",  #17
                      "µmol m^-3",       #18
                      NA,                #19
                      NA,                #20
                      NA,                #21
                      NA,                #22
                      NA                 #23
                      ))

usethis::use_data(parameter, overwrite = TRUE, internal = TRUE)
