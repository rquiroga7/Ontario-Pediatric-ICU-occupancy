library(pacman)
p_load(readr,tidyr,dplyr,ggplot2,zoo,ggborderline)

tag="Data from Ontario Public Health.\nCOVID cases expressed in hundreds. Graph by @rquiroga777"
ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}
ms <- function(x, n = 7){stats::filter(x, rep(1, n), sides = 1)}

#Analyze Canadian viral cases
for (year in 2018:2022){
  assign(paste0("a",year), read_csv(paste0("./Data/",year,"_test_data.csv")))
}
names(a2018)<-names(a2022);names(a2019)<-names(a2022);names(a2020)<-names(a2022);names(a2021)<-names(a2022)
todo<-a2018; todo<-rbind(todo,a2019);todo<-rbind(todo,a2020);todo<-rbind(todo,a2021);todo<-rbind(todo,a2022)
todo<-todo %>% mutate(`Total Flu Positive` =  `Total Flu A Positive`+ `Total Flu B Positive`)
todo2<-todo %>% dplyr::select(date,`Flu Tested`, `Total Flu Positive`,`RSV Tested`,`RSV Positive`,`ADV Tested`,`ADV Positive`,`EV/RV Tested`,`EV/RV Positive`)

#Get COVID data
url<-"https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/ed270bb8-340b-41f9-a7c6-e8ef587e6d11/download/covidtesting.csv"
download.file(destfile="covid_data.csv",url)
tablacovid<-read_csv("covid_data.csv")
covid<-tablacovid %>% dplyr::select(`Reported Date`, `Total Cases`,`Total tests completed in the last day`,`Percent positive tests in last day`)
covid<-covid %>% mutate(casos_new=`Total Cases`-lag(`Total Cases`,n=1))
covid<-covid %>% mutate(cases_week=ms(casos_new)/100)
covid<-covid %>% mutate(pos_week=ms(casos_new)/ms(`Total tests completed in the last day`))
names(covid)[1]<-"date"
covid<-covid %>% dplyr::select(date,'COVID Positive'=cases_week,'COVID Positivity'=pos_week)
todo3<-merge(todo2,covid,by="date",all.x=TRUE)
todo3<-todo3 %>% mutate(`Flu Positivity`=`Total Flu Positive`/`Flu Tested`,`RSV Positivity`=`RSV Positive`/`RSV Tested`,,`ADV Positivity`=`ADV Positive`/`ADV Tested`,`EV/RV Positivity`=`EV/RV Positive`/`EV/RV Tested`,`EV/RV Positivity`=`EV/RV Positive`/`EV/RV Tested`)

#WIDE TO LONG
data_long <- gather(todo3, Data,Count, 'Flu Tested':'EV/RV Positivity', factor_key=TRUE)

increase_cases <- function(observed_cases, pos_rate, m, k){
  y <- observed_cases * pos_rate ^ k * m
  return(y)
}
todo3<-todo3 %>% mutate('Adjusted Flu'=increase_cases(`Total Flu Positive`, `Flu Positivity`, m = 5, k = 0.5))
todo3<-todo3 %>% mutate('Adjusted RSV'=increase_cases(`RSV Positive`, `RSV Positivity`, m = 5, k = 0.5))
todo3<-todo3 %>% mutate('Adjusted ADV'=increase_cases(`ADV Positive`, `ADV Positivity`, m = 5, k = 0.5))
todo3<-todo3 %>% mutate('Adjusted EV/RV'=increase_cases(`EV/RV Positive`, `EV/RV Positivity`, m = 5, k = 0.5))
todo3<-todo3 %>% mutate('Adjusted COVID'=increase_cases(`COVID Positive`, `COVID Positivity`, m = 5, k = 0.5))
data_long3 <- gather(todo3, Data,Count, 'Flu Tested':'Adjusted COVID', factor_key=TRUE)



mindate=min(data_long$date)
maxdate=max(data_long$date)
#Plot cases
ggplot(data_long %>% filter(Data=="Total Flu Positive" | Data=="RSV Positive"| Data=="ADV Positive"| Data=="EV/RV Positive"| Data=="COVID Positive"),aes(x=date,y=Count,color=Data))+
  theme_light(base_size=16)+
  ggtitle("Ontario - Weekly positives for different viruses")+
  theme(legend.position = "bottom")+
  scale_y_continuous(limits = c(0,1250),minor_breaks=NULL, breaks=seq(0,1250,250),expand=c(0,0))+
  scale_x_date(minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 15))+
  labs(caption=tag)+
  geom_borderline(size=1.1)+
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,hjust=0.5,vjust=0.5),
         legend.position = "bottom")
ggsave(filename = paste0("./Plots/CANADA_viruses_weekly_cases.png"),width = 12, height=8, dpi= 150)

ggplot(data_long %>% filter(Data=="Total Flu Positive" | Data=="RSV Positive" | Data=="COVID Positive"),aes(x=date,y=Count,color=Data))+
  theme_light(base_size=16)+
  ggtitle("Ontario - Weekly positives for different viruses")+
  scale_y_continuous(limits = c(0,1250),minor_breaks=NULL, breaks=seq(0,1250,250),expand=c(0,0))+
  scale_x_date(minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 15))+
  theme(legend.position = "bottom")+
  labs(caption=tag)+
  geom_borderline(size=1.1)+
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,hjust=0.5,vjust=0.5),
         legend.position = "bottom")
ggsave(filename = paste0("./Plots/CANADA_rsv_flu_covid__weekly_cases.png"),width = 12, height=8, dpi= 150)

#Plot positivity
ggplot(data_long %>% filter(Data=="Flu Positivity" | Data=="RSV Positivity"| Data=="ADV Positivity"| Data=="EV/RV Positivity"),aes(x=date,y=Count,color=Data))+
  theme_light(base_size=16)+
  geom_line()

#Plot "Adjusted EV/RV" 
ggplot(data_long3 %>% filter(Data=="Adjusted Flu" | Data=="Adjusted RSV"| Data=="Adjusted ADV"| Data=="Adjusted EV/RV"| Data=="Adjusted COVID"),aes(x=date,y=Count,color=Data))+
  theme_light(base_size=16)+
  geom_line()



#Get Pediatric ICU data
#url<-"https://data.ontario.ca/dataset/1b5ff63f-48a1-4db6-965f-ab6acbab9f29/resource/c7f2590f-362a-498f-a06c-da127ec41a33/download/icu_beds.csv"
#download.file(destfile="icu_data.csv",url)
tablaicu<-read_csv("./Data/2022-11-18_icu_data.csv")
tablaicu$date<-as.Date(tablaicu$date)
fechas_sel<-c(todo3$date,max(todo3$date)+5)
tablaicu<- tablaicu %>% mutate(maxicu= rollmax(total_ped_icu_patients,k = 7,na.pad = TRUE,align = "right"))
icu<-tablaicu %>% dplyr::select(date,total_ped_icu_patients,maxicu) %>% filter(date %in% as.Date(fechas_sel))
todo4<-merge(todo3,icu,by="date",all.x=TRUE,all.y=TRUE)
#todo4<-todo4 %>% mutate(change=ped_icu_non_crci_patients- lag(ped_icu_non_crci_patients,1)) %>% mutate(change_smooth=ma(change,7))
data_long4 <- gather(todo4, Data,Count, 'Flu Tested':maxicu, factor_key=TRUE)
#acf(icu$ped_icu_non_crci_patients,lag.max = 14,plot=FALSE)

summary(m1 <- glm(data = todo4, ~ `Total Flu Positive` + `RSV Positive` + `ADV Positive` + `EV/RV Positive`))
summary(m1 <- glm(data = todo4,change ~ `Total Flu Positive` + `RSV Positive` + `ADV Positive` + `EV/RV Positive`))
summary(m1 <- glm(data = todo4,change_smooth ~ `Total Flu Positive` + `RSV Positive` + `ADV Positive` + `EV/RV Positive`))

#Plot cases + ICU
coeff=10
ggplot(data_long4 %>% filter()%>% filter(Data=="Total Flu Positive" | Data=="RSV Positive" | Data=="COVID Positive" ),aes(x=date,y=Count,color=Data))+
  theme_light(base_size=16)+
  ggtitle("Ontario - Weekly positives for different viruses")+
  theme(legend.position = "bottom")+
  scale_y_continuous(sec.axis = sec_axis(~./coeff, name="Max pediatric ICU beds occupied over last week"),    name = "Cases",limits = c(0,1250),minor_breaks=NULL, breaks=seq(0,1250,250),expand=c(0,0))+
  scale_x_date(minor_breaks = NULL,breaks = seq(mindate,maxdate,by="3 months"), date_labels =  "%Y-%m",expand = c(0, 15))+
  labs(caption=tag)+
  geom_borderline(size=1.1)+
  geom_borderline(size=1.1,data=data_long4 %>% filter( Data=="maxicu"),aes(x=date,y=Count*10,color="ICU beds Occupied"))+
  theme( axis.title.y = element_text(color = "black", size=13),
         axis.text.x = element_text(angle=90,hjust=0.5,vjust=0.5),
         legend.position = "bottom",
         axis.title.y.right = element_text(color = "#7CAE00", size=13))
  ggsave(filename = "./Data/CANADA_virus_ICU_ped_Ontario.png",width = 12, height=8, dpi= 150)

#Calculate severity (hospitalization rate
#Develop another graph, see here: https://www.r-bloggers.com/2022/06/another-case-for-redesigning-dual-axis-charts/

#Plot HSR for RSV