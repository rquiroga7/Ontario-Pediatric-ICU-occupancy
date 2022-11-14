library(pacman)
p_load(rvest,dplyr,xml2,aweek,lubridate,readr,ggborderline,ggrepel)
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


age="0-<6 months"
#age="6-<12 months"
#age="1-<2 years"
#age="2-4"
if (age=="0-<6 months"){birthdatemin=maxdate-180;birthdatemax=maxdate-0}; if (age=="6-<12 months"){birthdatemin=maxdate-365;birthdatemax=maxdate-180};if (age=="1-<2 years"){birthdatemin=maxdate-365*2;birthdatemax=maxdate-365};if (age=="2-4"){birthdatemin=maxdate-365*4;birthdatemax=maxdate-365*2}

#BY STATE
tag2=paste0("Birth dates for age group highlighed in green background\n",tag)
plotdata<- by_state %>% filter(Age.Category==age & (State!="Entire Network (RSV-NET)" & State!="Tennessee" & State!="Oregon" & State!="New Mexico"& State!="California" & State!="Georgia" & State!="Utah" ))%>% group_by(State) %>% arrange(State,date)  %>% mutate(state_last = if_else(date == maxdate, State, NA_character_))
ggplot(plotdata ,aes(x=date,y=ma(Rate,3),color=State))+
  theme_light(base_size=16)+
  ggtitle(paste0("US RSV hospitalization rate (per 100000)",age))+
  #scale_x_date(limits=c(mindate,maxdate+300), date_breaks = "3 month", date_labels =  "%Y-%m",expand = c(0, 0))+
  scale_y_continuous(minor_breaks=NULL, breaks=seq(0,300,50),expand=c(0,0))+
  scale_x_date(limits=c(mindate+7,maxdate+7), minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 3))+
  labs(caption=tag2)+
  ylab("Hospitalization rate (3 week trailing moving average)")+
  #geom_smooth(size=1.1)+
  annotate(geom = "rect", xmin = birthdatemin,xmax = birthdatemax,ymin = 0, ymax = Inf, fill = "palegreen", colour = "palegreen", alpha = 0.2) +
  geom_borderline(size=1.25)+
  gghighlight(use_direct_label = F) +
  facet_wrap(~State,ncol = 3)+
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,hjust=0.5,vjust=0.5),
         legend.position = "none")
ggsave(filename = paste0("Facet_RSV_Net_by_state",age,".png"),width = 9, height=9, dpi= 150)


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

