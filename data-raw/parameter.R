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
                      "RMSE",        #23
                      "lowlim",      #24
                      "highlim",     #25
                      "layer_couple",#26
                      "layer",       #27
                      "group_id",    #28
                      "efflux",      #29
                      "prod_abs",    #30
                      "prod_rel"     #31
                      ),
             description = c("mole fraction of a gas",               #1
                             "name of the gas",                      #2
                             "depth, higher values point up",        #3
                             "upper border of layer",                #4
                             "lower border of layer",                #5
                             "total pore space",                     #6
                             "air-filled pore space",                #7
                             "soil water content",                   #8
                             "temperature",                          #9
                             "pressure",                             #10
                             "relative diffusivity",                 #11
                             "apparent diffusion coefficient",       #12
                             "diffusion coefficient in air",         #13
                             "number density of air",                #14
                             "flux density",                         #15
                             "incoming flux density at model bottom",#16
                             "production rate (source term)",        #17
                             "number concentration",                 #18
                             "integer, mapping unique production rates 1 = lowest", #19
                             "id of the soilphys profile in a cfp_dat object", #20
                             "id of each step within a soilphys profile 1 = lowest", #21
                             "id of distinct profiles within a cfp_dat object", #22
                             "real mean square error",               #23
                             "lower limit for production optimisation", #24
                             "upper limit for production optimisation", #25
                             "coupling factor with layer below",     #26
                             "layer name",                           #27
                             "id for each combination of id_cols in layers_map", #28
                             "efflux at the soil/atmosphere interface", #29
                             "production rate (source term)",        #30
                             "ratio of production rate to efflux"    #31
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
                      NA,                #23
                      "µmol m^-3 s^-1",  #24
                      "µmol m^-3 s^-1",  #25
                      NA,                #26
                      NA,                #27
                      NA,                #28
                      "µmol m^-2 s^-1",  #29
                      "µmol m^-2 s^-1",  #30
                      "1"                #31
                      ))

usethis::use_data(parameter, overwrite = TRUE, internal = TRUE)
