library(pacman)
p_load(tidyr,dplyr,aweek,lubridate,readr,ggborderline,ggrepel,forcats,gghighlight,viridis)
today<-Sys.Date()
tag=paste0("RSV-NET: Respiratory Syncytial Virus Hospitalization Surveillance Network, CDC. Accessed on ",today, " - Graph by @rquiroga777")
ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}

#US CDC RSV HOSPITALIZATION DATA:
#https://data.cdc.gov/Case-Surveillance/Weekly-Rates-of-Laboratory-Confirmed-RSV-Hospitali/29hc-w46k
#rsv_hosp<-read.csv("Data/US_Weekly_Rates_of_Laboratory-Confirmed_RSV_Hospitalizations_from_the_RSV-NET_Surveillance_System.csv")
rsv_hosp<-read.csv("Data/2022_11_17_Weekly_Rates_of_Laboratory-Confirmed_RSV_Hospitalizations_from_the_RSV-NET_Surveillance_System.csv")
rsv_hosp[rsv_hosp$Age.Category=="2-4",]$Age.Category<-"2-4 years"
rsv_hosp[rsv_hosp$Age.Category=="5-17",]$Age.Category<-"5-17 years"
by_race<-rsv_hosp %>% filter(Sex=="Overall" & State=="Entire Network (RSV-NET)" ) %>% filter(MMWR.Year<2022 | (MMWR.Year==2022 & MMWR.Week<=43)) %>% mutate(date=week2date(paste0(MMWR.Year,"-W",formatC(as.integer(MMWR.Week),width=2,flag="0"),"-7"),week_start=7))  %>% group_by(Race) %>% arrange(Race,date)
                              
mindate=min(by_state$date)
maxdate=max(by_state$date)
by_state<-rsv_hosp %>% filter(Sex=="Overall" & Race=="Overall" & MMWR.Week!="Overall") %>% filter(MMWR.Year<2022 | (MMWR.Year==2022 & MMWR.Week<=43)) %>% mutate(date=week2date(paste0(MMWR.Year,"-W",formatC(as.integer(MMWR.Week),width=2,flag="0"),"-7"),week_start=7))
by_state<-by_state  %>% group_by(State,Age.Category) %>% arrange(date) %>% padr::pad(interval = "1 week", start_val = mindate, end_val = maxdate) 
by_state <-by_state %>% mutate(Rate=ifelse(is.na(Rate),0,Rate)) %>% filter(date>as.Date("2018-10-01") & (MMWR.Week<53 | is.na(MMWR.Week)))
#data_ends <- by_state %>% 
#  group_by(State) %>% 
#  top_n(1, age) 
#data_ends
ages=c("0-<6 months","6-<12 months","1-<2 years","2-4 years","5-17 years")

tag5=paste0("3 week trailing rolling average, per 100000 population for each age group.\nStates included: California, Colorado,Connecticut,Georgia, Maryland, Michigan, Minnesota, New Mexico, New York, Oregon, Tennessee, Utah.\n",tag)
by_state <- by_state %>% filter(Age.Category %in% ages)
by_state$Age.Category<-factor(by_state$Age.Category, levels = c("0-<6 months","6-<12 months","1-<2 years","2-4 years","5-17"))
ggplot(by_state %>% filter(Age.Category != "5-17" & State=="Entire Network (RSV-NET)") ,aes(x=date,y=Rate,color=Age.Category))+
  theme_light(base_size=14)+
  ggtitle(paste0("US Weekly Pediatric RSV hospitalization Rate by age group"))+
  ylab("Hospitalization Rate")+
  xlab("Date")+
  scale_x_date(limits=c(mindate,maxdate),date_breaks = "3 month", date_labels =  "%Y-%m",expand=c(0,15))+ 
  labs(caption=tag5)+
  geom_borderline(size=1.1)+
  gghighlight(use_direct_label = F) +
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,vjust=0.5),
         legend.position = "bottom",
         plot.title = element_text(hjust=0.5,vjust=0.5))+
  facet_wrap(~Age.Category,ncol = 2)
ggsave(filename = paste0("./Plots/RSV_Net_facet_weekly_rates.png"),width = 12, height=8, dpi= 150)

#ggplot(by_state %>% filter(Age.Category==age & (State=="Connecticut" | State=="Minnesota" | State=="Michigan" ))%>% group_by(State) %>% arrange(State,date)  %>% mutate(state_last = if_else(date == maxdate, State, NA_character_)) ,aes(x=date,y=ma(Rate,3),color=State))+
#  theme_light(base_size=16)+
#  ggtitle(paste0("US RSV hospitalization rate (per 100000)",age))+
#  #scale_x_date(limits=c(mindate,maxdate+300), date_breaks = "3 month", date_labels =  "%Y-%m",expand = c(0, 0))+
#  scale_y_continuous(minor_breaks=NULL, breaks=seq(0,300,50))+
#  scale_x_date(limits=c(mindate,maxdate+300), minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 0))+
#  labs(caption=tag)+
#  ylab("Hospitalization rate (3 week trailing moving average)")+
#  #geom_smooth(size=1.1)+
#  geom_borderline(size=1.1)+
#  geom_text_repel(
#    aes(color = State, label = state_last),
#    family = "Lato",
#    fontface = "bold",
#    size = 4,
#    direction = "y",
#    xlim = c(maxdate, NA),
#    hjust = 0,
#    segment.size = .7,
#    segment.alpha = .5,
#    segment.linetype = "dotted",
#    box.padding = .4,
#    segment.curvature = -0.1,
#    segment.ncp = 3,
#    segment.angle = 20
#  ) +
#  coord_cartesian(
#    clip = "off",
#  ) +
#  theme( axis.title.y = element_text(color = "black", size=13),
#         axis.text.x = element_text(angle=90,hjust=0.5,vjust=0.5),
#         legend.position = "none")
#ggsave(filename = paste0("./Plots/RSV_Net_by_state",age,".png"),width = 12, height=8, dpi= 150)
#
for (age in ages){
if (age=="0-<6 months"){birthdatemin=maxdate-180;birthdatemax=maxdate-0}; if (age=="6-<12 months"){birthdatemin=maxdate-365;birthdatemax=maxdate-180};if (age=="1-<2 years"){birthdatemin=maxdate-365*2;birthdatemax=maxdate-365};if (age=="2-4 years"){birthdatemin=maxdate-365*4;birthdatemax=maxdate-365*2};if (age=="5-17 years"){birthdatemin=maxdate-365*17;birthdatemax=maxdate-365*5}

#BY STATE
tag2=paste0("3 week trailing rolling average, per 100000 population for each age group and State.\nHospitalization ratesBirth dates for age group highlighed in green background\n",tag)
#plotdata<- by_state %>% filter(Age.Category==age & (State!="Entire Network (RSV-NET)" & State!="Tennessee" & State!="Oregon" & State!="New Mexico"& State!="California" & State!="Georgia" & State!="Utah" ))%>% group_by(State) %>% arrange(State,date)  %>% mutate(state_last = if_else(date == maxdate, State, NA_character_))
plotdata<- by_state %>% filter(Age.Category==age & (State!="Entire Network (RSV-NET)" & State!="California" & State!="Oregon" & State!="Utah" ))%>% group_by(State) %>% arrange(State,date)  %>% mutate(state_last = if_else(date == maxdate, State, NA_character_))
ggplot(plotdata ,aes(x=date,y=as.numeric(ma(Rate,3)),color=State))+
  theme_light(base_size=16)+
  ggtitle(paste0("US RSV Weekly Hospitalization rate ",age))+
  #scale_x_date(limits=c(mindate,maxdate+300), date_breaks = "3 month", date_labels =  "%Y-%m",expand = c(0, 0))+
  scale_y_continuous(minor_breaks=NULL, breaks=seq(0,300,50),expand=c(0,0))+
  scale_x_date(limits=c(mindate+7,maxdate+7), minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 15))+
  labs(caption=tag2)+
  ylab("Hospitalization rate")+
  #geom_smooth(size=1.1)+
  annotate(geom = "rect", xmin = birthdatemin,xmax = birthdatemax,ymin = 0, ymax = Inf, fill = "palegreen", colour = "palegreen", alpha = 0.2) +
  geom_borderline(size=1.25)+
  #stat_peaks(colour = "red",span=28,ignore_threshold = .25) +
  #stat_peaks(geom = "text", colour = "red", vjust = -0.5, y.label.fmt = "%.0f",span=25,ignore_threshold = .25) +
  #gghighlight(use_direct_label = F) +
  facet_wrap(~State,ncol = 3)+
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,hjust=0.5,vjust=0.5),
         legend.position = "none")
ggsave(filename = paste0("./Plots/Facet_RSV_Net_by_state",age,".png"),width = 9, height=9, dpi= 150)
}
#COVID incidence from IHME models

ihme_covid<-read.csv("Data/IHME.csv")
ihme_covid<-ihme_covid %>% dplyr::select(date,location_name,inf_mean,population) %>% mutate(incidence=inf_mean/population)
names(ihme_covid)[2]<-"State"
ihme_covid$date<-as.Date(ihme_covid$date)
tag3=paste0("Birth dates for age group ",age, " highlighed in green background\n",tag)
ggplot(ihme_covid ,aes(x=date,y=incidence*100,color=State))+
  theme_light(base_size=16)+
  ggtitle(paste0("US COVID-19 IHME model Incidence"))+
  #scale_x_date(limits=c(mindate,maxdate+300), date_breaks = "3 month", date_labels =  "%Y-%m",expand = c(0, 0))+
  scale_y_continuous(minor_breaks=NULL, breaks=seq(0,2,0.5),expand=c(0,0))+
  scale_x_date(limits=c(mindate+7,maxdate+7), minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 3))+
  labs(caption=tag3)+
  ylab("Incidence (% of population)")+
  #geom_smooth(size=1.1)+
  annotate(geom = "rect", xmin = birthdatemin,xmax = birthdatemax,ymin = 0, ymax = Inf, fill = "palegreen", colour = "palegreen", alpha = 0.2) +
  geom_borderline(size=1.25)+
  gghighlight(use_direct_label = F) +
  facet_wrap(~State,ncol = 3)+
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,hjust=0.5,vjust=0.5),
         legend.position = "none")
ggsave(filename = paste0("./Plots/COVID_incidence_by_state",age,".png"),width = 9, height=9, dpi= 150)



#BY RACE
plotdatar<-by_race %>% filter(MMWR.Year<2022 | (MMWR.Year==2022 & MMWR.Week<=43)) %>% filter(Sex=="Overall" & State=="Entire Network (RSV-NET)" & MMWR.Week!="Overall") %>% filter(Age.Category==age )%>% group_by(Race) %>% arrange(Race,date)  %>% mutate(state_last = if_else(date == maxdate, Race, NA_character_))
ggplot(plotdatar ,aes(x=date,y=ma(Rate,3),color=Race))+
  theme_light(base_size=16)+
  ggtitle(paste0("US RSV hospitalization rate (per 100000)",age))+
  #scale_x_date(limits=c(mindate,maxdate+300), date_breaks = "3 month", date_labels =  "%Y-%m",expand = c(0, 0))+
  scale_y_continuous(minor_breaks=NULL, breaks=seq(0,300,50))+
  scale_x_date(limits=c(mindate+7,maxdate+7), minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 0))+
  labs(caption=tag)+
  ylab("Hospitalization rate (3 week trailing moving average)")+
  #geom_smooth(size=1.1)+
  geom_borderline(size=1.1)+
  gghighlight(use_direct_label = F) +
  facet_wrap(~Race,ncol = 3)+
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,hjust=0.5,vjust=0.5),
         legend.position = "none")
ggsave(filename = paste0("./Plots/Facet_RSV_Net_by_race",age,".png"),width = 9, height=9, dpi= 150)



#CDC CASES AND POSITIVITY
counter=0
rm(final)
for (state in c("CO","CA","CT","GA","MD","MN","NM","NY","OR","TN")) {
  url<-paste0("https://www.cdc.gov/surveillance/nrevss/images/rsvstate/RSV1PPCent3AVG_State",state,".htm")
  tabla<- url %>%
  as.character() %>% 
  read_html() %>% 
  html_table()
  if (exists("tabla")){
    counter=counter+1
    tabla1<-tabla[[1]]
    if(counter==1){
        final<-tabla1} 
      if(counter>1){names(tabla1)<-names(final)
      final<-rbind(final,tabla1)
      }
    }
  }

#https://www.cdc.gov/surveillance/nrevss/images/rsvstate/RSV1PPCent3AVG_StateCA.htm

state_abb<-tibble(State = c("Georgia","Tennessee","Minnesota","Oregon"), Abb = c("GA","TN","MN","OR"))
rsv_cases<-merge(final,state_abb,by.x="StateID",by.y="Abb",all.x=FALSE)
rsv_cases<-rsv_cases %>% mutate(date=as.Date(RepWeekDate,format="%m/%d/%y"))
tag2=paste0("Data from The National Respiratory and Enteric Virus Surveillance System (NREVSS)\n",tag)
ggplot(rsv_cases ,aes(x=date,y=`Percent Positive`,color=State))+
  theme_light(base_size=16)+
  ggtitle(paste0("US RSV Test positivity"))+
  #scale_x_date(limits=c(mindate,maxdate+300), date_breaks = "3 month", date_labels =  "%Y-%m",expand = c(0, 0))+
  scale_y_continuous(minor_breaks=NULL, breaks=seq(0,100,10),expand=c(0,0))+
  scale_x_date(limits=c(mindate+7,maxdate+7), minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 3))+
  labs(caption=tag2)+
  ylab("RSV Test positivity")+
  #geom_smooth(size=1.1)+
  #annotate(geom = "rect", xmin = birthdatemin,xmax = birthdatemax,ymin = 0, ymax = Inf, fill = "palegreen", colour = "palegreen", alpha = 0.2) +
  geom_borderline(size=1.25)+
  gghighlight(use_direct_label = F) +
  facet_wrap(~State,ncol = 2)+
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,hjust=0.5,vjust=0.5),
         legend.position = "none")
ggsave(filename = paste0("./Plots/RSV_Positivity_by_state",age,".png"),width = 9, height=9, dpi= 150)



#ANALYZE PEAKS FOR EACH AGE GROUP
counter=0
#for (age in c("0-<6 months","6-<12 months","1-<2 years","2-4 years","5-17 years)) {
counter=counter+1
plotdata<- by_state %>% filter(( (Age.Category == "0-<6 months" |  Age.Category == "6-<12 months" |  Age.Category == "1-<2 years" |  Age.Category == "2-4 years" |  Age.Category == "5-17 years" ) & (MMWR.Week<53 | is.na(MMWR.Week)) & State!="Entire Network (RSV-NET)" & State!="Tennessee" & State!="Oregon" & State!="New Mexico"& State!="California" & State!="Georgia" & State!="Utah" ))%>% group_by(State) %>% arrange(State,date)  %>% mutate(state_last = if_else(date == maxdate, State, NA_character_))
#Create season data
plotdata<-plotdata %>% mutate(season=ifelse(date >=as.Date("2018-07-01") & date <as.Date("2019-07-01"),2018,ifelse(date >=as.Date("2019-07-01") & date <as.Date("2020-07-01"),2019,ifelse(date >=as.Date("2020-07-01") & date <as.Date("2021-07-01"),2020,ifelse(date >=as.Date("2021-07-01") & date <as.Date("2022-07-01"),2021,2022)))))

#CDC Wonder population data
#https://wonder.cdc.gov/single-race-single-year-v2021.html
pop_age<-read.csv("Data/Single-Race Population Estimates 2020-2021 by State and Single-Year Age.txt",sep = "\t") %>% dplyr::select(States,`Single.Year.Ages.Code`,Population) %>% rename(State=States,Age=`Single.Year.Ages.Code`,Population=Population)
age0<-pop_age %>% group_by(State) %>% filter(Age==0) %>% mutate(Age.Category="0-<6 months",Population=Population/2) %>% dplyr::select(State,Population,Age.Category)
age0.5<-pop_age %>% group_by(State) %>% filter(Age==0) %>% mutate(Age.Category="6-<12 months",Population=Population/2) %>% dplyr::select(State,Population,Age.Category)
age1<-pop_age %>% group_by(State) %>% filter(Age==1) %>% mutate(Age.Category="1-<2 years",Population=Population) %>% dplyr::select(State,Population,Age.Category)
age2<-pop_age %>% group_by(State) %>% filter(Age==2 | Age==3 | Age==4) %>% group_by(State) %>% mutate(Age.Category="2-4 years",Population=sum(Population)) %>% dplyr::select(State,Population,Age.Category) %>% unique()
age3<-pop_age %>% group_by(State) %>% filter(Age>=5 & Age<=17) %>% group_by(State) %>% mutate(Age.Category="5-17 years",Population=sum(Population)) %>% dplyr::select(State,Population,Age.Category) %>% unique()
age_groups_pop<-rbind(rbind(rbind(rbind(age0,age0.5),age1),age2),age3)

#Merge population data, calculate hospitalizations
plotdata<-merge(plotdata,age_groups_pop,by=c("Age.Category","State"),all.x=TRUE,all.y=FALSE) %>% mutate(Rate=as.numeric(Rate),Population=as.integer(Population)) %>% mutate(hospitalizations=as.integer(Rate/100000*Population)) %>% arrange(State,Age.Category,date)
#Choose peak for each season (for each state)
peaks<-plotdata %>% group_by(State,season,date) %>% summarise(hospitalizations=sum(hospitalizations)) %>% ungroup() %>% group_by(State,season) %>% filter(hospitalizations==max(hospitalizations)) %>% summarise(date=median(date),hospitalizations=hospitalizations) %>% unique()
counter=0
for (state1 in c("Colorado","Connecticut","Maryland","Michigan","Minnesota","New York")) {
  counter=counter+1
  peaks_state<-peaks %>% filter(State ==state1)
  if (counter==1){table<-plotdata %>% filter(State ==state1) %>% filter(date %in% peaks_state$date)}
  if (counter>1){ table<-rbind(table,plotdata %>% filter(State ==state1) %>% filter(date %in% peaks_state$date))}
}
table2<-table %>% group_by(season,Age.Category) %>% summarise(hospitalizations=sum(hospitalizations))
write.csv(table2,"peak_hosp_season.csv")
table$Age.Category<-as.factor(table$Age.Category)
table$Age.Category<-factor(table$Age.Category, levels = c("0-<6 months","6-<12 months","1-<2 years","2-4 years","5-17 years"))
tag4=paste0("Seasons defined as July to June next year.Population data estimated from CDC Wonder Data.\nStates included in data analysis: Colorado,Connecticut, Maryland, Michigan, Minnesota, New York.\n",tag)
ggplot(table, aes(fill=Age.Category,y=hospitalizations, x=season)) + 
  labs(caption = tag4)+
  geom_bar(position="stack", stat="identity",aes(order =  forcats::fct_rev(Age.Category))) + 
  ggtitle("Peak Weekly US RSV Hospitalizations by Age Group and season")+
  scale_fill_viridis(discrete=TRUE,begin=0,end=0.9, name="") +
  theme_light() +
  theme(legend.position = "bottom",plot.title = element_text(hjust=0.5,vjust=0.5))+
  ylab("Hospitalizations") + 
  xlab("Season")
ggsave(filename = paste0("./Plots/Stacked_bar_Hospitalizations_SEASONMAX.png"),width = 9, height=9, dpi= 600)


#CUMULATIVE STACKED BAR BY STATE AND SEASON, RELATIVE TO WORST
tag4=paste0("RSV Hospitalization rates by state and age group from RSV-NET.Population data estimated from CDC Wonder Data.\nStates included in data analysis: Colorado,Connecticut, Maryland, Michigan, Minnesota, New York.\n",tag)
plotdata2<-plotdata %>% ungroup() %>% arrange(State,Age.Category,date) %>% group_by(State,Age.Category,season) %>%   mutate(cumhosp=cumsum(replace_na(hospitalizations, 0)))%>% arrange(State,Age.Category,date)
#plotdata2<-plotdata2 %>% ungroup() %>% arrange(State,Age.Category,date) %>% group_by(State,Age.Category) %>% padr::pad(interval = "1 week", start_val = mindate, end_val = maxdate)%>% mutate(season=ifelse(date >=as.Date("2018-07-01") & date <as.Date("2019-07-01"),2018,ifelse(date >=as.Date("2019-07-01") & date <as.Date("2020-07-01"),2019,ifelse(date >=as.Date("2020-07-01") & date <as.Date("2021-07-01"),2020,ifelse(date >=as.Date("2021-07-01") & date <as.Date("2022-07-01"),2021,2022))))) %>% group_by(State,Age.Category,season) %>%   mutate(cumhosp=cumsum(replace_na(hospitalizations, 0)))%>% arrange(State,Age.Category,date)

cumseason<-plotdata2 %>% ungroup() %>% group_by(State,season,date) %>% summarise(hosp=sum(hospitalizations,na.rm=TRUE)) %>% ungroup() %>% group_by(State,season) %>% summarise(hosp=sum(hosp,na.rm=TRUE))# %>% summarise(hospitalizations=sum(hospitalizations)) %>% ungroup() %>% group_by(State,season) %>% filter(hospitalizations==max(hospitalizations)) %>% summarise(date=median(date),hospitalizations=hospitalizations) %>% unique()
maxseason<-plotdata2 %>% ungroup() %>% group_by(State,season,date) %>% summarise(hosp=sum(hospitalizations,na.rm=TRUE)) %>% ungroup() %>% group_by(State,season) %>% summarise(hosp=max(hosp,na.rm=TRUE))
maxh<-cumseason %>% group_by(State) %>% summarise(maxh=max(hosp,na.rm=TRUE))
maxwh<-maxseason %>% group_by(State) %>% summarise(maxwh=max(hosp,na.rm=TRUE))
plotdata2<-merge(plotdata2,maxh, by="State",all.x=TRUE) %>% group_by(State,Age.Category,season) %>%  mutate(cumhosp_norm=cumhosp/maxh)
plotdata2<-merge(plotdata2,maxwh, by="State",all.x=TRUE) %>% group_by(State,Age.Category,season) %>%  mutate(norm_hosp=hospitalizations/maxwh)
tag4=paste0("Seasons defined as July to June next year.Population data estimated from CDC Wonder Data.\nStates included in data analysis: Colorado,Connecticut, Maryland, Michigan, Minnesota, New York.\n",tag)
plotdata2$Age.Category<-factor(plotdata2$Age.Category, levels = c("0-<6 months","6-<12 months","1-<2 years","2-4 years","5-17 years"))
#min2018<-min(plotdata2 %>% filter(season==2018) %>% pull(date)); max2018 <-max(plotdata2 %>% filter(season==2018)  %>% pull(date)); min2019<-min(plotdata2 %>% filter(season==2019) %>% pull(date)); max2019 <-max(plotdata2 %>% filter(season==2019) %>% pull(date)); min2020<-min(plotdata2 %>% filter(season==2020) %>% pull(date)); max2020 <-max(plotdata2 %>% filter(season==2020) %>% pull(date)); min2021<-min(plotdata2 %>% filter(season==2021) %>% pull(date)); max2021 <-max(plotdata2 %>% filter(season==2021) %>% pull(date));min2022<-min(plotdata2 %>% filter(season==2022) %>% pull(date)); max2022 <-max(plotdata2 %>% filter(season==2022) %>% pull(date))
ggplot(plotdata2,aes(x=date,y=as.numeric(cumhosp_norm),fill=Age.Category)) +
  theme_light(base_size=20) +
  ggtitle(paste0("Cumulative US RSV weekly hospitalizations by age group and season, relative to max"))+
  #scale_x_date(limits=c(mindate,maxdate+300), date_breaks = "3 month", date_labels =  "%Y-%m",expand = c(0, 0))+
  scale_y_continuous(minor_breaks=NULL, breaks=seq(0,1,.25),expand=c(0,0),limits=c(0,1))+
  scale_x_date(limits=c(mindate+7,maxdate+7), minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 15))+
  labs(caption=tag4)+
  ylab("Hospitalizations (3 week trailing moving average)")+
  #geom_smooth(size=1.1)+
  scale_fill_viridis(discrete=TRUE,begin=0,end=0.9, name="") +
  #annotate(geom = "rect", xmin = min2018,xmax = max2018,ymin = -.1, ymax = 0, fill = "palegreen", alpha = 0.2) +
  #annotate(geom = "rect", xmin = min2019,xmax = max2019,ymin = -.1, ymax = 0, fill = "paleyellow", alpha = 0.2) +
  geom_bar(position="stack", stat="identity",aes(order =  forcats::fct_rev(Age.Category)))+
  #stat_peaks(colour = "red",span=28,ignore_threshold = .25) +
  #stat_peaks(geom = "text", colour = "red", vjust = -0.5, y.label.fmt = "%.0f",span=25,ignore_threshold = .25) +
  #gghighlight(use_direct_label = F) +
  facet_wrap(~State,ncol = 3)+
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,hjust=0.5,vjust=0.5),
         legend.position = "bottom",
         plot.title=element_text(hjust=0.5,vjust=0.5))
ggsave(filename = paste0("./Plots/Cum_stacked_bar_Facet_RSV_Net_by_state.png"),width = 18, height=18, dpi= 150)


#STACKED BAR BY STATE
tag4=paste0("RSV Hospitalization rates by state and age group from RSV-NET.Population data estimated from CDC Wonder Data.\nStates included in data analysis: Colorado,Connecticut, Maryland, Michigan, Minnesota, New York.\n",tag)
#plotdata2<-merge(by_state %>% filter(State!="Entire Network (RSV-NET)" & State!="California" & State!="Oregon" & State!="Utah" ),age_groups_pop,by=c("Age.Category","State"),all.x=TRUE,all.y=FALSE) %>% mutate(hospitalizations=as.integer(Rate/100000*Population)) %>% group_by(State) %>% mutate(norm_hosp=hospitalizations/max(hospitalizations,na.rm=TRUE))
#plotdata2<-plotdata2 %>% mutate(norm_hosp=hospitalizations/maxh)
#plotdata2$Age.Category<-factor(plotdata2$Age.Category, levels = c("0-<6 months","6-<12 months","1-<2 years","2-4 years","5-17 years"))
ggplot(plotdata2,aes(x=date,y=as.numeric(ma(norm_hosp,3)),fill=Age.Category)) +
  theme_light(base_size=20) +
  ggtitle(paste0("US RSV weekly hospitalizations by age group relative to max"))+
  #scale_x_date(limits=c(mindate,maxdate+300), date_breaks = "3 month", date_labels =  "%Y-%m",expand = c(0, 0))+
  scale_y_continuous(minor_breaks=NULL, breaks=seq(0,1,.25),expand=c(0,0),limits=c(0,1))+
  scale_x_date(limits=c(mindate+7,maxdate+7), minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 15))+
  labs(caption=tag4)+
  ylab("Hospitalizations (3 week trailing moving average)")+
  #geom_smooth(size=1.1)+
  scale_fill_viridis(discrete=TRUE,begin=0,end=0.9, name="") +
  annotate(geom = "rect", xmin = birthdatemin,xmax = birthdatemax,ymin = 0, ymax = Inf, fill = "palegreen", colour = "palegreen", alpha = 0.2) +
  geom_bar(position="stack", stat="identity",aes(order =  forcats::fct_rev(Age.Category)))+
  #stat_peaks(colour = "red",span=28,ignore_threshold = .25) +
  #stat_peaks(geom = "text", colour = "red", vjust = -0.5, y.label.fmt = "%.0f",span=25,ignore_threshold = .25) +
  #gghighlight(use_direct_label = F) +
  facet_wrap(~State,ncol = 3)+
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,hjust=0.5,vjust=0.5),
         legend.position = "bottom",
         plot.title=element_text(hjust=0.5,vjust=0.5))
ggsave(filename = paste0("./Plots/Stacked_bar_Facet_Hosps_by_state.png"),width = 16, height=9, dpi= 150)

#RSV-NET weekly hospitalizations
tag5=paste0("States included in data analysis: Colorado,Connecticut, Maryland, Michigan, Minnesota, New York.\n",tag)
plotdata4<-plotdata2 %>% filter(Age.Category != "5-17 years") %>% group_by(Age.Category,date) %>% summarise(hosps=sum(hospitalizations) )
ggplot(plotdata4  ,aes(x=date,y=hosps,color=Age.Category))+
  theme_light(base_size=16)+
  ggtitle(paste0("US Weekly Pediatric RSV hospitalizations by age group"))+
  ylab("Hospitalizations")+
  xlab("Date")+
  scale_x_date(limits=c(mindate,maxdate),date_breaks = "3 month", date_labels =  "%Y-%m")+ 
  scale_y_continuous(limits=c(0,max(plotdata4$hosps)*1.1),breaks=seq(0,800,100),minor_breaks = NULL)+
  labs(caption=tag5)+
  geom_borderline(size=1.1)+
  gghighlight(use_direct_label = F) +
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,vjust=0.5),
         legend.position = "none")+
  facet_wrap(~Age.Category,ncol = 2)+
  geom_vline(xintercept = as.Date(c("2018-07-01","2019-07-01","2020-07-01","2021-07-01","2022-07-01")))
ggsave(filename = paste0("./Plots/RSV_Net_facet_weekly_hospitalizations.png"),width = 12, height=8, dpi= 150)

#RSV-NET cumulative hospitalizations
tag5=paste0("States included in data analysis: Colorado,Connecticut, Maryland, Michigan, Minnesota, New York.\n",tag)
plotdata4<-plotdata2 %>% filter(Age.Category != "5-17 years") %>% group_by(Age.Category,season,date) %>% summarise(hosps=sum(cumhosp) )
ggplot(plotdata4, aes(x=date,y=hosps,color=Age.Category))+
  theme_light(base_size=16)+
  ggtitle(paste0("US Cumulative Pediatric RSV hospitalizations by age group and season"))+
  ylab("Hospitalizations by season")+
  xlab("Date")+
  scale_x_date(limits=c(mindate,maxdate),date_breaks = "3 month", date_labels =  "%Y-%m")+ 
  scale_y_continuous(limits=c(0,max(plotdata4$hosps)*1.1),breaks=seq(0,5000,1000),minor_breaks = NULL)+
  labs(caption=tag5)+
  geom_borderline(size=1.1)+
  gghighlight(use_direct_label = F) +
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,vjust=0.5),
         legend.position = "none")+
  facet_wrap(~Age.Category,ncol = 2)+
  geom_vline(xintercept = as.Date(c("2019-07-01","2020-07-01","2021-07-01","2022-07-01")))
ggsave(filename = paste0("./Plots/RSV_Net_facet_cumulative_hospitalizations.png"),width = 12, height=8, dpi= 150)

#RSV-NET cumulative hospitalizations with labels
tag5=paste0("States included in data analysis: Colorado,Connecticut, Maryland, Michigan, Minnesota, New York.\n",tag)
plotdata3<-plotdata2 %>% filter(Age.Category != "5-17 years") %>% group_by(Age.Category,season,date) %>% summarise(hosps=sum(cumhosp)) %>% mutate(seasonlabel=ifelse(date==max(date,na.rm=TRUE),paste0(season,": ",hosps),NA_character_)) 
ggplot(plotdata3 ,aes(x=date,y=hosps,color=Age.Category,group=Age.Category))+
  theme_light(base_size=16)+
  ggtitle(paste0("US Cumulative Pediatric RSV hospitalizations by age group and season"))+
  ylab("Hospitalizations by season")+
  xlab("Date")+
  scale_x_date(limits=c(mindate,maxdate+200),date_breaks = "3 month", date_labels =  "%Y-%m")+ 
  scale_y_continuous(limits=c(0,max(plotdata3$hosps)*1.1),breaks=seq(0,5000,1000))+
  labs(caption=tag5)+
  geom_borderline(size=1.1)+
  geom_label_repel(data=subset(plotdata3, season < 2022),aes(label = seasonlabel),force=0,size=4,nudge_x=-175,nudge_y=325,segment.size = .7, segment.alpha = .5,  segment.curvature = -0.1,segment.ncp = 3)+
  geom_label_repel(data=subset(plotdata3, season == 2022),aes(label = seasonlabel),force=0,size=4,nudge_x=75,nudge_y=325,segment.size = .7, segment.alpha = .5, segment.curvature = -0.1,segment.ncp = 3)+
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,vjust=0.5),
         legend.position = "none")+
  facet_wrap(~Age.Category,ncol = 2)+
  geom_vline(xintercept = as.Date(c("2019-07-01","2020-07-01","2021-07-01","2022-07-01")),alpha=0.5)
ggsave(filename = paste0("./Plots/label_RSV_Net_facet_cumulative_hospitalizations.png"),width = 12, height=8, dpi= 150)


