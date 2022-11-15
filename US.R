library(pacman)
p_load(rvest,dplyr,xml2,aweek,lubridate,readr,ggborderline,ggrepel,forcats)
tag="Data from https://www.cdc.gov/rsv/research/rsv-net/ - Graph by @rquiroga777"
ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}

#US CDC RSV HOSPITALIZATION DATA:
#https://www.cdc.gov/rsv/research/rsv-net/dashboard.html#:~:text=In%20the%202022%2D2023%20season,was%2013.0%20per%20100%2C000%20people
rsv_hosp<-read.csv("US_Weekly_Rates_of_Laboratory-Confirmed_RSV_Hospitalizations_from_the_RSV-NET_Surveillance_System.csv")
by_race<-rsv_hosp %>% filter(Sex=="Overall" & State=="Entire Network (RSV-NET)" ) %>% filter(MMWR.Year<2022 | (MMWR.Year==2022 & MMWR.Week<=43)) %>% mutate(date=week2date(paste0(MMWR.Year,"-W",formatC(as.integer(MMWR.Week),width=2,flag="0"),"-7"),week_start=7))  %>% group_by(Race) %>% arrange(Race,date)
                              

by_state<-rsv_hosp %>% filter(Sex=="Overall" & Race=="Overall" & MMWR.Week!="Overall") %>% filter(MMWR.Year<2022 | (MMWR.Year==2022 & MMWR.Week<=43)) %>% mutate(date=week2date(paste0(MMWR.Year,"-W",formatC(as.integer(MMWR.Week),width=2,flag="0"),"-7"),week_start=7))
by_state<-by_state  %>% group_by(State) %>% arrange(date)

mindate=min(by_state_0_6m$date)
maxdate=max(by_state_0_6m$date)
#data_ends <- by_state %>% 
#  group_by(State) %>% 
#  top_n(1, age) 
#data_ends
age="0-<6 months"
ggplot(by_state %>% filter( Age.Category==age & State=="Entire Network (RSV-NET)")) ,aes(x=date,y=Rate,color=State))+
  theme_light(base_size=16)+
  ggtitle(paste0("US RSV-NET RSV hospitalizations ",age))+
  scale_x_date(limits=c(mindate,maxdate),date_breaks = "3 month", date_labels =  "%Y-%m")+ 
  labs(caption=tag)+
  geom_line()+
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90),
         legend.position = "bottom")
ggsave(filename = paste0("RSV_Net_",age,".png"),width = 12, height=8, dpi= 150)

ggplot(by_state %>% filter(Age.Category==age & (State=="Connecticut" | State=="Minnesota" | State=="Michigan" ))%>% group_by(State) %>% arrange(State,date)  %>% mutate(state_last = if_else(date == maxdate, State, NA_character_)) ,aes(x=date,y=ma(Rate,3),color=State))+
  theme_light(base_size=16)+
  ggtitle(paste0("US RSV hospitalization rate (per 100000)",age))+
  #scale_x_date(limits=c(mindate,maxdate+300), date_breaks = "3 month", date_labels =  "%Y-%m",expand = c(0, 0))+
  scale_y_continuous(minor_breaks=NULL, breaks=seq(0,300,50))+
  scale_x_date(limits=c(mindate,maxdate+300), minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 0))+
  labs(caption=tag)+
  ylab("Hospitalization rate (3 week trailing moving average)")+
  #geom_smooth(size=1.1)+
  geom_borderline(size=1.1)+
  geom_text_repel(
    aes(color = State, label = state_last),
    family = "Lato",
    fontface = "bold",
    size = 4,
    direction = "y",
    xlim = c(maxdate, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  coord_cartesian(
    clip = "off",
  ) +
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,hjust=0.5,vjust=0.5),
         legend.position = "none")
ggsave(filename = paste0("RSV_Net_by_state",age,".png"),width = 12, height=8, dpi= 150)


#age="0-<6 months"
age="6-<12 months"
#age="1-<2 years"
#age="2-4"
#age="5-17"
if (age=="0-<6 months"){birthdatemin=maxdate-180;birthdatemax=maxdate-0}; if (age=="6-<12 months"){birthdatemin=maxdate-365;birthdatemax=maxdate-180};if (age=="1-<2 years"){birthdatemin=maxdate-365*2;birthdatemax=maxdate-365};if (age=="2-4"){birthdatemin=maxdate-365*4;birthdatemax=maxdate-365*2};if (age=="2-4"){birthdatemin=maxdate-365*17;birthdatemax=maxdate-365*5}

#BY STATE
tag2=paste0("Birth dates for age group highlighed in green background\n",tag)
#plotdata<- by_state %>% filter(Age.Category==age & (State!="Entire Network (RSV-NET)" & State!="Tennessee" & State!="Oregon" & State!="New Mexico"& State!="California" & State!="Georgia" & State!="Utah" ))%>% group_by(State) %>% arrange(State,date)  %>% mutate(state_last = if_else(date == maxdate, State, NA_character_))
plotdata<- by_state %>% filter(Age.Category==age & (State!="Entire Network (RSV-NET)" & State!="California" & State!="Oregon" & State!="Utah" ))%>% group_by(State) %>% arrange(State,date)  %>% mutate(state_last = if_else(date == maxdate, State, NA_character_))
ggplot(plotdata ,aes(x=date,y=as.numeric(ma(Rate,3)),color=State))+
  theme_light(base_size=16)+
  ggtitle(paste0("US RSV hospitalization rate (per 100000)",age))+
  #scale_x_date(limits=c(mindate,maxdate+300), date_breaks = "3 month", date_labels =  "%Y-%m",expand = c(0, 0))+
  scale_y_continuous(minor_breaks=NULL, breaks=seq(0,300,50),expand=c(0,0))+
  scale_x_date(limits=c(mindate+7,maxdate+7), minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 15))+
  labs(caption=tag2)+
  ylab("Hospitalization rate (3 week trailing moving average)")+
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
ggsave(filename = paste0("Facet_RSV_Net_by_state",age,".png"),width = 9, height=9, dpi= 150)

#STACKED BAR BY STATE
tag4=paste0("RSV Hospitalization rates by state and age group from RSV-NET.Population data estimated from CDC Wonder Data.\nStates included in data analysis: Colorado,Connecticut, Maryland, Michigan, Minnesota, New York.\n",tag)
plotdata2<-merge(by_state %>% filter(State!="Entire Network (RSV-NET)" & State!="California" & State!="Oregon" & State!="Utah" ),age_groups_pop,by=c("Age.Category","State"),all.x=TRUE,all.y=FALSE) %>% mutate(hospitalizations=as.integer(Rate/100000*Population)) %>% group_by(State) %>% mutate(norm_hosp=hospitalizations/max(hospitalizations,na.rm=TRUE))
plotdata2$Age.Category<-factor(plotdata2$Age.Category, levels = c("0-<6 months","6-<12 months","1-<2 years","2-4","5-17"))
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
ggsave(filename = paste0("Stacked_bar_Facet_RSV_Net_by_state.png"),width = 16, height=9, dpi= 150)

#COVID incidence from IHME models

ihme_covid<-read.csv("IHME.csv")
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
ggsave(filename = paste0("COVID_incidence_by_state",age,".png"),width = 9, height=9, dpi= 150)



#BY RACE
plotdata<-by_race %>% filter(MMWR.Year<2022 | (MMWR.Year==2022 & MMWR.Week<=43)) %>% filter(Sex=="Overall" & State=="Entire Network (RSV-NET)" & MMWR.Week!="Overall") %>% filter(Age.Category==age )%>% group_by(Race) %>% arrange(Race,date)  %>% mutate(state_last = if_else(date == maxdate, Race, NA_character_))
ggplot(plotdata ,aes(x=date,y=ma(Rate,3),color=Race))+
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
ggsave(filename = paste0("Facet_RSV_Net_by_race",age,".png"),width = 9, height=9, dpi= 150)



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
ggsave(filename = paste0("RSV_Positivity_by_state",age,".png"),width = 9, height=9, dpi= 150)



#ANALYZE PEAKS FOR EACH AGE GROUP
counter=0
for (age in c("0-<6 months","6-<12 months","1-<2 years","2-4","5-17")) {
counter=counter+1
plotdata<- by_state %>% filter(Age.Category==age & (State!="Entire Network (RSV-NET)" & State!="Tennessee" & State!="Oregon" & State!="New Mexico"& State!="California" & State!="Georgia" & State!="Utah" ))%>% group_by(State) %>% arrange(State,date)  %>% mutate(state_last = if_else(date == maxdate, State, NA_character_))
p<-ggplot(plotdata ,aes(x=date,y=as.numeric(ma(Rate,5)),color=State))+
  #scale_x_date(limits=c(mindate,maxdate+300), date_breaks = "3 month", date_labels =  "%Y-%m",expand = c(0, 0))+
  scale_y_continuous(minor_breaks=NULL, breaks=seq(0,300,50),expand=c(0,0))+
  scale_x_date(limits=c(mindate+7,maxdate+7), minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 15))+
  geom_borderline(size=1.25)+
  stat_peaks(colour = "red",span=25,ignore_threshold = .15) +
  stat_peaks(colour = "red",span=7,ignore_threshold = .35) +
  #stat_peaks(geom = "text", colour = "red", vjust = -0.5, y.label.fmt = "%.0f",span=28,ignore_threshold = .25) +
  #gghighlight(use_direct_label = F) +
  facet_wrap(~State,ncol = 3)
p  
# Get locations of peaks found by stat_peaks
pb = ggplot_build(p)
state_number<-pb$plot[[1]] %>% filter(MMWR.Week==50 & MMWR.Year==2020) %>% dplyr::select(State,.group,Age.Category) %>% rename(State=State,PANEL=.group,Age.Category=Age.Category)
peaks<-merge(pb$data[[2]] %>% arrange(label),state_number,x.all=TRUE,by="PANEL") %>% dplyr::select(State,x.label,y.label,Age.Category) %>% rename(State=State,date=x.label,Rate=y.label) %>% mutate(date=as.Date(date))
peaks<-peaks %>% mutate(season=ifelse(date >as.Date("2018-08-01") & date <as.Date("2019-08-01"),1,ifelse(date >as.Date("2019-08-01") & date <as.Date("2020-08-01"),2,ifelse(date >as.Date("2020-08-01") & date <as.Date("2021-08-01"),3,ifelse(date >as.Date("2021-08-01") & date <as.Date("2022-08-01"),4,0))))) %>% group_by(State) %>% arrange(State,Rate) %>% group_by(State,season,Age.Category) %>% filter(Rate==max(Rate)) %>% ungroup() %>% dplyr::select(State,Rate,date,Age.Category)
#peaks2<-by_state %>% filter(date>as.Date("2022-09-01") & State %in% state_number$State & Age.Category==age) %>% dplyr::select(State,date,Rate,Age.Category)
peaks2<-by_state %>% filter(date>as.Date("2022-09-01") & State %in% state_number$State & Age.Category==age) %>% dplyr::select(State,date,Rate,Age.Category) %>% mutate(season=ifelse(date >as.Date("2018-08-01") & date <as.Date("2019-08-01"),1,ifelse(date >as.Date("2019-08-01") & date <as.Date("2020-08-01"),2,ifelse(date >as.Date("2020-08-01") & date <as.Date("2021-08-01"),3,ifelse(date >as.Date("2021-08-01") & date <as.Date("2022-08-01"),4,0))))) %>% group_by(State) %>% arrange(State,Rate) %>% group_by(State,season,Age.Category) %>% filter(Rate==max(Rate)) %>% ungroup() %>% dplyr::select(State,Rate,date,Age.Category)
allpeaks<-rbind(peaks,peaks2) %>% arrange(State,date)

#CDC Wonder population data
#https://wonder.cdc.gov/single-race-single-year-v2021.html
pop_age<-read.csv("Data/Single-Race Population Estimates 2020-2021 by State and Single-Year Age.txt",sep = "\t") %>% dplyr::select(States,`Single.Year.Ages.Code`,Population) %>% rename(State=States,Age=`Single.Year.Ages.Code`,Population=Population)
age0<-pop_age %>% group_by(State) %>% filter(Age==0) %>% mutate(Age.Category="0-<6 months",Population=Population/2) %>% dplyr::select(State,Population,Age.Category)
age0.5<-pop_age %>% group_by(State) %>% filter(Age==0) %>% mutate(Age.Category="6-<12 months",Population=Population/2) %>% dplyr::select(State,Population,Age.Category)
age1<-pop_age %>% group_by(State) %>% filter(Age==1) %>% mutate(Age.Category="1-<2 years",Population=Population) %>% dplyr::select(State,Population,Age.Category)
age2<-pop_age %>% group_by(State) %>% filter(Age==2 | Age==3 | Age==4) %>% group_by(State) %>% mutate(Age.Category="2-4",Population=sum(Population)) %>% dplyr::select(State,Population,Age.Category) %>% unique()
age3<-pop_age %>% group_by(State) %>% filter(Age>=5 & Age<=17) %>% group_by(State) %>% mutate(Age.Category="5-17",Population=sum(Population)) %>% dplyr::select(State,Population,Age.Category) %>% unique()
age_groups_pop<-rbind(rbind(rbind(rbind(age0,age0.5),age1),age2),age3)

if (counter==1){ table<-merge(allpeaks,age_groups_pop,by=c("Age.Category","State"),all.x=TRUE,all.y=FALSE) %>% mutate(Rate=as.numeric(Rate),Population=as.integer(Population)) %>% mutate(hospitalizations=as.integer(Rate/100000*Population))}
if (counter>1){ table<-rbind(table,merge(allpeaks,age_groups_pop,by=c("Age.Category","State"),all.x=TRUE,all.y=FALSE) %>% mutate(Rate=as.numeric(Rate),Population=as.integer(Population)) %>% mutate(hospitalizations=as.integer(Rate/100000*Population)))}
}

table2<-table %>% mutate(season=ifelse(date >as.Date("2018-08-01") & date <as.Date("2019-08-01"),2018,ifelse(date >as.Date("2019-08-01") & date <as.Date("2020-08-01"),2019,ifelse(date >as.Date("2020-08-01") & date <as.Date("2021-08-01"),2020,ifelse(date >as.Date("2021-08-01") & date <as.Date("2022-08-01"),2021,2022))))) 
table2$Age.Category<-as.factor(table2$Age.Category)
table2$Age.Category<-factor(table2$Age.Category, levels = c("0-<6 months","6-<12 months","1-<2 years","2-4","5-17"))
tag4=paste0("RSV Hospitalization rates by state and age group from RSV-NET.Population data estimated from CDC Wonder Data.\nStates included in data analysis: Colorado,Connecticut, Maryland, Michigan, Minnesota, New York.\n",tag)
ggplot(table2, aes(fill=Age.Category,color=Age.Category y=hospitalizations, x=season)) + 
  labs(caption = tag4)+
  geom_bar(position="stack", stat="identity",aes(order =  forcats::fct_rev(Age.Category))) + 
  ggtitle("Peak Weekly US RSV Hospitalizations by Age Group and season")+
  scale_fill_viridis(discrete=TRUE,begin=0,end=0.9, name="") +
  theme_light() +
  theme(legend.position = "bottom",plot.title = element_text(hjust=0.5,vjust=0.5))+
  ylab("Hospitalizations") + 
  xlab("Year")
ggsave(filename = paste0("Stacked_bar_Hospitalizations_SEASONMAX.png"),width = 9, height=9, dpi= 600)

