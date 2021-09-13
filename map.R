
library(ggplot2) # a for making pretty plots
library(dplyr)
library(raster)
library(tmap)


library(sp)
library(RColorBrewer)

View(data)

View(in_s@data)

in_s <- getData('GADM', country='IND', level=1)
str(in_s, max.level=2)



data <- read.csv("C:/Users/DELL/Desktop/all.csv")
plot(in_s)


str(in_s@data)
library(dplyr)#to summarize data
data_sum<-data%>%group_by(State)%>%summarise(Population=sum(Persons))


states_cens<-unique(data_sum$State)
states_map<-unique(in_s$NAME_1)

data_sum$State<-ifelse(data_sum$State=="AN","Andaman and Nicobar",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="Andhra","Andhra Pradesh",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="ArunachalPradesh","Arunachal Pradesh",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="CG","Chhattisgarh",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="D_D","Daman and Diu",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="D_N_H","Dadra and Nagar Haveli",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="Delhi","NCT of Delhi",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="HP","Himachal Pradesh",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="JK","Jammu and Kashmir",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="MP","Madhya Pradesh",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="Orrisa","Odisha",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="TN","Tamil Nadu",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="UP","Uttar Pradesh",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="Uttranchal","Uttarakhand",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="WB","West Bengal",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="Lakshdweep","Lakshadweep",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="Meghalya","Meghalaya",data_sum$State)
data_sum$State<-ifelse(data_sum$State=="Pondicherry","Puducherry",data_sum$State)

in_s@data<-merge(in_s@data,data_sum,by.x="NAME_1",by.y="State",all.x=TRUE)

tm_shape(in_s) +
  tm_fill(col = "Population",style = "quantile")+
  tm_borders(col = "burlywood4")+
  tm_legend(position = c("right", "bottom"))

breaks<-unique(sort(sort(in_s@data$Population, decreasing = TRUE)[1:8]+1))

blups <- brewer.pal(9, "Blues")
tm_shape(in_s) +
  tm_fill(col = "Population",style = "fixed", 
          breaks=breaks,
          palette=blups) +
  tm_borders(col = "burlywood4")+
  tm_legend(position = c("right", "bottom")) 










in_d <- getData('GADM', country='IND', level=2)# Level=2 for district level map
plot(in_d)


pun_subset<- in_d[in_d@data$NAME_1=="Punjab",]
pun_subset@data$NAME_2<- tolower(pun_subset@data$NAME_2)

pun_subset@data<-merge(pun_subset@data, punjab, by.x="NAME_2", by.y="NAME_2", all.x=TRUE)

tm_shape(pun_subset) +  tm_fill(col = "mean")

library(leaflet)

leaflet(pun_subset) %>% addPolygons() %>% addTiles()




breaks<-quantile(in_d@data$Sex.ratio..0.6.years.,seq(0,1,0.1),na.rm=TRUE)
colschm <- brewer.pal(9,"Greens")

tm_shape(pun_subset) +
  tm_fill(col = "mean",
          style = "fixed", 
          breaks=breaks,
          palette=colschm)+
  tm_legend(position = c("right", "bottom"))+
  tm_shape(pun)+
  tm_borders(col="black")
