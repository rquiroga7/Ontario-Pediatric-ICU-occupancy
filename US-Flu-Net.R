library(pacman)
p_load(tidyr,dplyr,aweek,lubridate,readr,ggborderline,ggrepel,forcats,gghighlight,viridis)
today<-Sys.Date()
tag=paste0("FluSurv-NET: Influenza Hospitalization Surveillance Network, CDC. Accessed on ",today, " - Graph by @rquiroga777")
ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}


#US CDC Flu HOSPITALIZATION DATA:
#https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html
flu_hosp<-read.csv("./Data/FluSurveillance_Custom_Download_Data.csv")
flu_hosp[rsv_hosp$Age.Category=="2-4",]$Age.Category<-"2-4 years"
flu_hosp[rsv_hosp$Age.Category=="5-17",]$Age.Category<-"5-17 years"

