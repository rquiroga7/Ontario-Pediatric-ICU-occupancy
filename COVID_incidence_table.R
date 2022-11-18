#Make table for estimated incidence by age group and RSV wave
library(pacman)
p_load(flextable)
dt<-read.csv("covid_incidence_age.csv")
names(dt)<-c("Age group","Born before 2021 RSV wave","Born before lockdowns","Mother pregnant during lockdowns","Nov 2021 COVID incidence prev 12 months","Nov 2022 COVID incidence prev 12 months")
dt1<-autofit(flextable(dt))
save_as_image(dt1,path="./Plots/table_age.png")
