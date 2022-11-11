

library(pacman)
p_load(rvest,dplyr,xml2,aweek,lubridate,readr)

download_table<- function(url="") {suppressWarnings(url %>%
                                                 as.character() %>% 
                                                 read_html() %>% 
                                                 html_table()) }

#Scrape RSV positive and test data from html:
counter=0
suppressWarnings(try({rm(tabla);rm(tabla1);rm(ontario);rm(final)},silent=TRUE,))
#for (year in 2018:2022){
todayweek=date2week(Sys.Date(),week_start = 7,numeric = TRUE)
for (year in 2022){
#for (year in c(2018,2020) ){
  #for (week in c(1,52)){
  #if (year ==2020){ weeks=1:53} else{weeks=1:52}
  #if (year ==2020){ weeks=1:53} else{weeks=1:52}
  if (year ==2020){ weeks=1:53} else if (year==2022){weeks=1:todayweek} else{weeks=1:52}
  for (week in weeks ){
    Sys.setlocale("LC_ALL","en_US.UTF-8")
    week0=formatC(week,width=2,flag="0")
    #year=2018
    day=week2date(paste0(year,"-W",week0,"-7"),week_start=7)
    first=week2date(paste0(year,"-W",week0,"-1"),week_start=7)
    last<-(date2week(paste0(year,"-12-31"),week_start = 7))
    dia<-tolower(format(day, "%B-%-d-%Y"))
    suppressWarnings(try({rm(tabla)},silent=TRUE,))
    #https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2017-2018/respiratory-virus-detections-isolations-week-1-ending-january-6-2018.html
    #url <- "https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2020-2021/week-43-ending-october-24-2020.html" 
    try({
      if(year < 2020 ){
        url<-paste0("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/",year-1,"-",year,"/respiratory-virus-detections-isolations-week-",week,"-ending-",dia,".html")
      } 
      if(year >= 2020 ) {
        url<-paste0("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/",year-1,"-",year,"/week-",week,"-ending-",dia,".html")
      }
      suppressWarnings(tabla<- url %>%
        as.character() %>% 
        read_html() %>% 
        html_table())
      
      if(exists("tabla")){
      counter=counter+1
      print(paste0(year,"-W",week," ",counter, "- funcó primera -",dia))
      Sys.sleep(2)
      }
    },silent=TRUE)
    if (!exists("tabla")){
      try({
        if(year < 2020 ){
          url<-paste0("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/",year,"-",year+1,"/respiratory-virus-detections-isolations-week-",week,"-ending-",dia,".html")
        } 
        if(year >= 2020 ) {
          url<-paste0("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/",year,"-",year+1,"/week-",week,"-ending-",dia,".html")
        }
        suppressWarnings(tabla<- url %>%
          as.character() %>% 
          read_html() %>% 
          html_table())
        if(exists("tabla")){
          counter=counter+1
          print(paste0(year,"-W",week," ",counter, "- funcó segunda -",dia))
          Sys.sleep(2)
        }
      },silent=TRUE)
    }
    if (!exists("tabla")){
      try({
        if(year >=2020 ){
          url<-gsub("august","auguat",paste0("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/",year,"-",year+1,"/week-",week,"-ending-",dia,".html"))
        } 
        suppressWarnings(tabla<- url %>%
                           as.character() %>% 
                           read_html() %>% 
                           html_table())
        
        if(exists("tabla")){
          counter=counter+1
          print(paste0(year,"-W",week," ",counter, "- funcó primera -",dia))
          Sys.sleep(2)
        }
      },silent=TRUE)
    }
    if (!exists("tabla")){
      try({
        if(year < 2020 ){
          url<-paste0("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/",year,"-",year+1,"/espiratory-virus-detections-isolations-week-",week,"-ending-",dia,".html")
        } 
        if(year >= 2020 ) {
          url<-paste0("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/",year,"-",year+1,"/week-",week,"-ending-",dia,".html")
        }
        suppressWarnings(tabla<- url %>%
                           as.character() %>% 
                           read_html() %>% 
                           html_table())
        if(exists("tabla")){
          counter=counter+1
          print(paste0(year,"-W",week," ",counter, "- funcó segunda -",dia))
          Sys.sleep(2)
        }
      },silent=TRUE)
    }
    if (!exists("tabla")){
      try({
        if(year < 2020 ){
          url<-paste0("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/",year-1,"-",year,"/respiratory-virus-detections-isolations-week-",week0,"-ending-",dia,".html")
        } 
        if(year >= 2020 ) {
          url<-paste0("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/",year-1,"-",year,"/week-",week0,"-ending-",dia,".html")
        }
        suppressWarnings(tabla<- url %>%
                          as.character() %>% 
                          read_html() %>% 
                          html_table())
        
        if(exists("tabla")){
          counter=counter+1
          print(paste0(year,"-W",week," ",counter, "- funcó primera -",dia))
          Sys.sleep(2)
        }
      },silent=TRUE)
    }
    if (!exists("tabla")){
      try({
        if(year < 2020 ){
          url<-paste0("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/",year,"-",year+1,"/respiratory-virus-detections-isolations-week-",week0,"-ending-",dia,".html")
        } 
        if(year >= 2020 ) {
          url<-paste0("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/",year,"-",year+1,"/week-",week0,"-ending-",dia,".html")
        }
        suppressWarnings(tabla<- url %>%
                           as.character() %>% 
                           read_html() %>% 
                           html_table())
        if(exists("tabla")){
          counter=counter+1
          print(paste0(year,"-W",week," ",counter, "- funcó segunda -",dia))
          Sys.sleep(2)
        }
      },silent=TRUE)
    }
    if (exists("tabla")){
      tabla1<-tabla[[1]]
      tabla1[which(tabla1$`Reporting Laboratory`=="Province of    Ontario"),]$`Reporting Laboratory`<-"Province of Ontario"
      ontario<-tabla1[which(tabla1$`Reporting Laboratory`=="Province of Ontario"),]
      nrow(ontario)
      ontario$date<-day
      if(nrow(ontario)==0){
        print(paste0("Error procesando:",year,"-W",week," ",counter, "- funcó segunda -",dia))
      }
      else {
        if(counter==1){
          final<-ontario} 
        if(counter>1){names(ontario)<-names(final)
                      final<-rbind(final,ontario)
        }
      }
    }
  }
  write.csv(file=paste0(year,"_test_data.csv"),final %>% arrange(date) %>% unique() %>% filter(year(date)==year),quote=FALSE )
}







#Fill in problems 2019
url<-"https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2019-2020/espiratory-virus-detections-isolations-week-50-ending-december-14-2019.html"
tabla1<-download_table(url)[[1]]
week=50;year=2019;Sys.setlocale("LC_ALL","en_US.UTF-8"); week0=formatC(week,width=2,flag="0"); day=week2date(paste0(year,"-W",week0,"-7"),week_start=7);dia<-tolower(format(day, "%B-%-d-%Y"))
tabla1$date<-day
ontario1<-tabla1[which(tabla1$`Reporting Laboratory`=="Province of Ontario"),]

url<-"https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2019-2020/respiratory-virus-detections-isolations-week-51-ending-december-21-2019.html"
tabla1<-download_table(url)[[1]]
week=51;year=2019;Sys.setlocale("LC_ALL","en_US.UTF-8"); week0=formatC(week,width=2,flag="0"); day=week2date(paste0(year,"-W",week0,"-7"),week_start=7);dia<-tolower(format(day, "%B-%-d-%Y"))
tabla1$date<-day
ontario2<-tabla1[which(tabla1$`Reporting Laboratory`=="Province of Ontario"),]

url<-"https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2019-2020/respiratory-virus-detections-isolations-week-52-ending-december-28-2019.html"
tabla1<-download_table(url)[[1]]
week=52;year=2019;Sys.setlocale("LC_ALL","en_US.UTF-8"); week0=formatC(week,width=2,flag="0"); day=week2date(paste0(year,"-W",week0,"-7"),week_start=7);dia<-tolower(format(day, "%B-%-d-%Y"))
tabla1$date<-day
ontario3<-tabla1[which(tabla1$`Reporting Laboratory`=="Province of Ontario"),]

ontario<-read_csv("2019_test_data.csv")
all<-bind_rows(ontario,ontario1,ontario2,ontario3)
write.csv(all,"2019_test_data_2.csv")

