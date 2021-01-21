
#Dependencies ----------------

load.libraries<-function(){
  library(lubridate)
  #library(plyr)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(ggpubr)
  library(openxlsx)
  library(splines)
}

load.libraries()


library(dplyr)
library(ConFluxPro)



#Directories ----------------

MainDir<-"C:/Users/valen/Documents/FVA/Gartiser_Valentin/LEVELII_gasflux/"
ScriptDir<-"Scripts/"
DatDir<-"Data/"
PlotDir<-"Plots/"
ResDir<-"Results/"

dir.create(paste0(MainDir,ScriptDir))
dir.create(paste0(MainDir,DatDir))
dir.create(paste0(MainDir,PlotDir))
dir.create(paste0(MainDir,ResDir))

#Load Data ------------------
Sys.setlocale (category = "LC_ALL", locale = "english")

#Loading gas concentration Data from file
gasdata<-read.csv(paste0(MainDir,DatDir,"gas_level_II.csv"),stringsAsFactors = F,header = T)

gasdata$NRESULT<-as.numeric(gasdata$NRESULT) #necessary because of false entries

gasdata$Date<-as.Date(gasdata$PN_DATUM)


#Select parameters of interest
gasdata <- gasdata %>% filter(PA_NAME %in% c("Ar","C2H4","CH4","CO2","N2","N2O","O2"))

#Generate Plot and SubPlot, WDH, depth_cat based on MST-ID
#Plot is the site e.g. ES = Esslingen
#Subplot is the variant e.g. Fi = Fichte
#WDH is the repeated measurement per depth per site and variant
#depth_cat is the categorised *discrete* depth, e.g. "AL","WL","-5cm"

gasdata$Plot <- unlist(lapply(strsplit(gasdata$MST_ID,split="_"),function(l){return(l[2])}))
gasdata <- gasdata[is.na(gasdata$Plot)==F,]

gasdata$WDH<-unlist(lapply(strsplit(gasdata$MST_ID,split="_"),function(l){return(substr(l[length(l)],3,4))}))

gasdata$depth_cat<-unlist(lapply(strsplit(gasdata$MST_ID,split="_"),function(l){return(substr(l[length(l)],nchar(l[length(l)])-1,nchar(l[length(l)])))}))


#Rename the Plot-values and creating the Subplot-variable
gasdata <-gasdata %>% mutate(Plot =recode(Plot,"och"="OC",
                                          "alt"="AS",
                                          "ess"="ES",
                                          "rot"="RO",
                                          "hd"="HD",
                                          "con"="CO"))
#mapping SubPlot to Var-ID
SubPlot_map <- gasdata %>%
  group_by(Plot, VAR_ID) %>%
  count() %>%
  select(-n)
SubPlot_map$SubPlot <- c("Bu","NA","Fi", #AS
                         "Bu","NA","Fi", #CO
                         "Bu","Fi", #ES
                         "Bu","Fi","Fi_DO", #HD
                         "Bu","Fi_DO","NA","NA","Fi","NA", #OC
                         "Fi" #RO
)
#adding SubPlot variable by Plot and Var_ID
gasdata<- gasdata %>%
  left_join(SubPlot_map)

## import of humusheights per Plot##-------------
humheights <- read.table(paste0(MainDir,DatDir,"humheights.txt"),stringsAsFactors = F,sep=",",header=T)


#  **ES/FI**
humheights <-humheights %>% #MANUAL CHANGE OF HUMHEIGHT TO 6 cm
  mutate(topheight = ifelse(Plot == "ES" &
                              SubPlot == "Fi",
                            6,
                            humheight))
# **ES/FI** end


##create new variable depth
gasdata$depth<- -as.numeric(gasdata$depth_cat)
gasdata$depth[grep("+",gasdata$depth_cat,fixed=T)]<- -gasdata$depth[grep("+",gasdata$depth_cat,fixed=T)]

##replace "AL" and "WL" with respective humus height
gasdata <-gasdata %>%
  left_join(humheights %>% select(Plot,SubPlot,topheight)) %>%
  mutate(depth = ifelse(depth_cat %in% c("AL","WL"),topheight,depth))

gasdata$MainPlot <- gasdata$Plot
gasdata$Plot <-paste0(gasdata$Plot,"_",gasdata$SubPlot)

gasdata$gas <-gasdata$PA_NAME

gasdata <- gasdata %>% mutate(NRESULT_ppm = ifelse(PARAM_UNITS=="%",10000,1)*NRESULT)

gasdata <-gasdata %>% select(MST_ID,SAMPLE_NO,NRESULT,NRESULT_ppm,Date,gas,Plot,depth_cat,MainPlot,SubPlot,depth,topheight)

soilphys_raw<-read.csv(paste0(MainDir,DatDir,"DSD0_LevelII_akt.csv"),
                       sep = ";",
                       stringsAsFactors = F)
#creating Plot and SubPlot variable
soilphys_raw <- soilphys_raw %>%
  mutate(Plot = unlist(strsplit(STD,split = "/"))[seq(1,by=2,length.out = nrow(soilphys_raw))],
         SubPlot = unlist(strsplit(STD,split = "/"))[seq(2,by=2,length.out = nrow(soilphys_raw))])

#DSD0 Berechnung:
#ggplot(soilphys_raw,
#       aes(x=AFPS, y=DSD0,col=SOURCE))+
#  geom_point()+
#  facet_wrap(~paste(Plot, SubPlot,DEPTH))+
#  stat_smooth(method = "nls",
#              formula = y ~ (a*(x)^b),
#              start = list(a = -10, b = -3),
#              se=F,
#              col="black",
#              fullrange = T)+
#  xlim(0,1)+
#  ylim(0,1)

#Calculating means of all samples & their parameters.
#soilphys will be the tibble for all further parameters.
soilphys<-soilphys_raw %>%
  group_by(Plot,SubPlot,DEPTH) %>%
  summarise_all(.funs = "mean") %>%
  mutate(TPS = TP) %>%
  select(Plot, SubPlot,DEPTH,TPS)

# Fitting exponential functions for DSD0<->AFPS
# and adding the fitting parameters to soilphys
soilphys <- soilphys %>% left_join(soilphys_raw %>%
                                     group_by(Plot, SubPlot, DEPTH) %>%
                                     group_modify(~{
                                       linmod<-coef(lm(log(DSD0)~log(AFPS),data =(.x %>% filter(DSD0 > 0))))
                                       expmod<-nls(formula =DSD0 ~ (a*(AFPS)^b),
                                                   start = list(a = exp(linmod[1]), b = linmod[2]),
                                                   data =.x)

                                       df<-cbind.data.frame(a=coef(expmod)[1],
                                                            b=coef(expmod)[2])
                                       return(df)
                                     }))

#adding humus heights and renaming DEPTH to depth_Cat
soilphys<-soilphys %>% left_join(humheights)
soilphys <- soilphys %>%
  mutate(depth_cat = DEPTH) %>%
  select(-DEPTH)

###############################-
## WHH-Model -------------------
###############################-

# Key for linking the "NL" variable of the
# Brook90 model to the depths
# RIGHT NOW ONLY **ES/FI** in txt file!!
# upper = upper bound of calculated layer [cm]
# lower = lower ~ ~ ~ ~ [cm]
# height = thickness of layer d[cm]
# depth = middle of the layer [cm]
brook90map <- read.table(paste0(MainDir,DatDir,"brook90map.txt"),header =T,stringsAsFactors = F)
brook90map <- brook90map %>% mutate(upper = upper*100,
                                    height = height / 10) %>%
  mutate(lower = upper-height,
         depth = upper - height/2)

#Loading Actual brook90 model values.
soilwater <- read.csv(paste0(MainDir,DatDir,"SWATDAY.csv"),stringsAsFactors = F)

# **ES/FI**
soilwater <- soilwater %>% filter(Plot == "ES",
                                  SubPlot == "Fi")
# **ES/FI** end

soilwater <- soilwater %>% left_join(brook90map) %>%
  left_join(humheights) %>%
  # **ES/FI**
  mutate(upper = ifelse(upper>0 & Plot == "ES" & SubPlot == "Fi",6/9,1)*upper,
         lower = ifelse(upper>0 & Plot == "ES" & SubPlot == "Fi",6/9,1)*lower,
         depth = ifelse(upper>0 & Plot == "ES" & SubPlot == "Fi",6/9,1)*depth) %>%
  # **ES/FI** end
  mutate(height = upper - lower) %>%
  select(-NL) %>%
  select(-X)  %>%
  mutate(Date = as.Date(Date)) %>%
  filter(depth > -10)



# Adding pressure variable based on DWD Data
p.list <-list.files(paste0(MainDir,DatDir,"pressure_DWD/"),pattern = "data_P_MN008.csv",recursive = T,full.names = T)
t.list <-list.files(paste0(MainDir,DatDir,"temperature_DWD/"),pattern = "data_TMK_MN004.csv",recursive = T,full.names = T)


#load data of availably stations
pressure_raw <- lapply(p.list,function(p){
  df <- read.table(p, header=T, sep = ",")
  return(df)
}) %>%
  bind_rows() %>%
  mutate(Date = as.Date(as.character(Zeitstempel),format = "%Y%m%d")) %>%
  group_by(SDO_ID,Date) %>%
  summarise(p0 = mean(Wert,na.rm=T)) %>%
  left_join(lapply(t.list,function(p){
    df <- read.table(p, header=T, sep = ",")
    return(df)
  }) %>%
    bind_rows() %>%
    mutate(Date = as.Date(as.character(Zeitstempel),format = "%Y%m%d")) %>%
    rename(temp = Wert) %>%
    select(SDO_ID,Date,temp)
  )


#altitudes
altitudes <-
  read.table(paste0(MainDir,DatDir,"altitude.csv"),sep = ";",header = T) %>%
  mutate(Plot = substr(plot,1,2),
         SubPlot = substr(plot,4,100)) %>%
  select(-plot)

pressure <- data.frame("Plot" = c("ES"),
                       "SDO_ID" =c(4928)) %>%
  left_join(altitudes) %>%
  left_join(pressure_raw) %>%
  group_by(Plot,SubPlot)%>%
  group_modify(~{t <- approxfun(.x$Date,.x$temp)(.x$Date)
  df <- .x %>% mutate(temp = !!t)
  return(df)}) %>%
  mutate(p = p0 * exp(-(0.02896 * 9.807) / (8.314 * 273.15+temp )*altitude))



#################################-
## Soil temperature --------------
#################################-

# loading soil temperature values based on a HYDRUS model
# Values present in 1 cm steps but ONLY FOR DATES THAT WERE GAS SAMPLED
soiltemp <- read.csv(paste0(MainDir,DatDir,"Hydrus_SoilT.csv"),stringsAsFactors = F)
soiltemp <- soiltemp %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(depth = NL) %>%
  select(-X) %>%
  select(-NL)









#ACTUAL TESTING ---------------------------------


gasdata_balcorr <- balance_correction(gasdata,
                                      limits = c(0.5,1.2),
                                      set_na = T)

library(ggplot2)
gasdata_balcorr %>%  filter(Plot == "ES_Fi") %>% ggplot(aes(x=Date,y=NRESULT_ppm,col=depth_cat))+
  geom_line()+facet_wrap(~gas,scales="free",ncol=1)

gasdata_co2corr <- gasdata_balcorr %>%
 dplyr::group_by(Plot,depth_cat,gas) %>%
dplyr::group_modify(~{
             if(.y$gas == "CO2"){
                 df <- series_cleaner(.x)
             } else {
                 df <- .x}
             return(df)
                        })
o2map<-offset_subsetting(df = gasdata_co2corr,
                  gas = "O2",
                  depth_cal = c("AL","WL"),
                  start = as.Date(c("2000-01-01",
                                    "2005-08-20",
                                            "2006-04-11",
                                            "2006-10-02",
                                            "2008-12-08",
                                            "2010-07-20",
                                            "2010-10-15",
                                            "2015-03-02",
                                            "2017-01-02")),
                  end =  as.Date(c("2005-08-19",
                                   "2006-04-10",
                                   "2006-10-01",
                                   "2008-12-07",
                                   "2010-07-19",
                                   "2010-10-14",
                                   "2015-03-01",
                                   "2017-01-01",
                                   "2020-08-19"

                  )),
                  mode = c(rep("const",7),"lin","const")
                  )

spec_o2<-gasdata %>% filter(Date > "2008-12-07",
                            Date < "2009-03-01",
                            depth_cat %in% c("AL","WL"),
                            gas == "O2",
                            NRESULT < 21) %>% pull(SAMPLE_NO)

o2map <- offset_override(o2map,
                 SAMPLE_NO = spec_o2,
                 section =3)

gasdata_o2offset <- offset_correction(gasdata_co2corr,o2map,"O2",0.21,depth_cal = c("AL","WL"))


gasdata_o2offset <-gasdata_o2offset %>% filter(!(gas == "O2" & NRESULT_ppm>21.5e4))

gasdata_o2offset %>% filter(gas == "O2") %>% ggplot(aes(x=Date, y=NRESULT_ppm,col = ifelse(NRESULT_ppm>21.5e4,T,F)))+geom_point()



o2remove <- gasdata_o2offset %>% filter(gas == "O2",Plot == "ES_Fi",
                                        depth_cat %in% c("AL","WL")) %>%
  group_by(Date,Plot) %>%
  summarise(m = mean(NRESULT_ppm,na.rm=T)) %>%
  filter((m>211000 | m<205000)) %>%
  mutate(gas = "O2") %>%
  select(!m) %>%
  mutate(t = paste(Plot,Date,gas)) %>%
  pull(t)

# replacing of these values with NA
gasdata_o2offset$NRESULT_ppm[with(gasdata_o2offset, paste(Plot,Date,gas)) %in% o2remove] <- NA




#soilphys zusammenführung:

soilphys <- soilphys %>% mutate(upper= ifelse(depth_cat == "HUMUS",humheight,ifelse(depth_cat == "0-5cm",0,-5)),
                    lower= ifelse(depth_cat == "HUMUS",0,ifelse(depth_cat == "0-5cm",-5,-10))) %>%
  mutate(MainPlot = Plot) %>%
  mutate(Plot = paste0(Plot,"_",SubPlot))

soilwater <- soilwater %>%
  mutate(SWC = THETA) %>%
  mutate(MainPlot = Plot) %>%
  mutate(Plot = paste0(Plot,"_",SubPlot))

soiltemp <- soiltemp%>%
  mutate(MainPlot = Plot) %>%
  mutate(Plot = paste0(Plot,"_",SubPlot))

pressure <- pressure %>%
  mutate(MainPlot = Plot) %>%
  mutate(Plot = paste0(Plot,"_",SubPlot))


soilphys <-soilphys %>% filter(!MainPlot == "V")

depth_target <-unique(c(soilwater$upper,soilwater$lower))

dt <- data.frame(Plot = rep(c("ES_Fi","ES_Bu"),each=8),
                 depth = rep(depth_target,times=2))

sphys_dd <- discretize_depth(df = soilphys %>% filter(Plot == "ES_Fi"),
                 param = c("TPS","a","b"),
                 method = "boundary",
                 depth_target = depth_target,
                 boundary_nearest =F,
                 id_cols = c("Plot"))

head(soiltemp)

stemp_dd <- discretize_depth(df =soiltemp,
                 param = c("Temp"),
                 method = "linear",
                 depth_target = depth_target,
                 int_depth =0.5,
                 id_cols = c("Plot","Date"))

soilphys_joined <- left_join(sphys_dd,soilwater %>% select(Plot,Date,depth,SWC)) %>%
  left_join(stemp_dd %>% select(Plot,Date,Temp,depth)) %>%
  left_join(pressure %>% select(Plot,Date,p))

check_soilphys(soilphys_joined)

gases <-unique(gasdata_o2offset$gas)
gases<-gases[!gases %in% c("Ar","C2H4","N2")]


soilphys_complete <- complete_soilphys(soilphys_joined,gases=gases,DSD0_formula ="a*AFPS^b" )

#sp_backup <-soilphys

#soilphys <- soilphys_complete

layers_map<-data.frame(Plot = rep(c("ES_Fi","AS_Fi"),each = 3),
                       upper = c(6,0,-5,8,0,-5),
                       lower = rep(c(0,-5,-10),2),
                       layer = rep(c("HU","MIN1","MIN2"),2))

FLUX <- calculate_flux(gasdata %>% filter(Plot == "ES_Fi"
                                          #,Date == "1998-10-26"
                                          ),
               soilphys_complete,
               layers_map = layers_map ,
               gases = c("CO2","CH4","O2"),
               modes =c("LL","LS","EF"),
               param = c("DSD0","DS","SWC","Temp","p"),
               funs = c("harm","harm","arith","arith","arith"))

FLUX %>% ggplot(aes(x=Date,y=flux,col=mode))+geom_line()+facet_wrap(~paste(Plot,layer,gas),ncol=1,scales = "free")

unique(gasdata$Date[gasdata$Plot == "ES_Fi"])[180]
df <-gasdata %>% filter(Date == "2012-08-27",Plot == "ES_Fi",gas == "CH4")
dcdz_layered(df,layers_map = layers_map[layers_map$Plot == "ES_Fi",] ,mode = "LL")
dcdz_ef(df,6,mode = "LR")



ggplot(df, aes(x=NRESULT_ppm,y=depth))+geom_point()

FLUX$topheight <- 6
FLUX$depth <- (FLUX$upper-FLUX$lower) / 2+FLUX$lower
EFFLUX <-efflux_extrap(FLUX,method = "linextrap",layers = c("MIN1","MIN2"),modename = "Hirano")
EFFLUX_t <-efflux_extrap(FLUX,method = "linextrap",layers = c("HU","MIN2"),modename = "Tang")
EFFLUX_lm <-efflux_extrap(FLUX,method = "lm",layers = c("HU","MIN2"),modename = "lm")
EFFLUX_ne <- efflux_extrap(FLUX,method = "nearest",layers = c("HU"),modename = "ne")

FLUX %>% filter(Date == "1998-10-26",Plot == "ES_Fi",gas == "CH4",mode == "LL")


EFFLUX %>%bind_rows(EFFLUX_t)%>%
  bind_rows(EFFLUX_lm) %>%
  filter(is.na(efflux)==F,gas == "CO2",mode == "LL") %>%
  ggplot(aes(x=Date, y=efflux,col=extrapmode))+
  geom_line()
EFFLUX %>% filter(Date == "2016-03-07")



PROFLUX <- pro_flux(gasdata_o2offset %>% filter(Plot == "ES_Fi",gas=="CO2"),
               soilphys_complete,
               prod_depth = c(-10,0,6),
               storage_term = 0,
               zero_flux = F,
               highlim = 1000,
               lowlim = -1000,
               id_cols = c("Plot","Date","gas"))

ggplot(PROFLUX %>% filter(upper == 6,gas == "CO2"), aes (x=Date,y=flux))+
  geom_line()+
  geom_line(data = EFFLUX %>% filter(mode == "LL",gas == "CO2"),aes(col = extrapmode, y=efflux))+
  facet_wrap(~gas,scales = "free_y")

gasdata_o2offset %>% filter(gas == "O2",Plot == "ES_Fi") %>% filter(!NRESULT_ppm > 2.5e5)%>% ggplot(aes(x=Date,y=NRESULT_ppm,col = depth_cat))+geom_line()

gasdata_o2offset %>% filter(gas == "O2",Plot == "ES_Fi") %>% filter(NRESULT_ppm > 2.5e5)
