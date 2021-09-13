kt <- read.csv("G:/ifpri work/karnataka/kt.csv")


are<- kt%>% filter(crop=="Arecant") %>% 
  group_by(NAME_2) %>% 
  summarize(Arecant=sum(value))

red<- kt%>% filter(crop=="Redgram") %>% 
  group_by(NAME_2) %>% 
  summarize(redgram=sum(value))


maiz<- kt%>% filter(crop=="Maize") %>% 
  group_by(NAME_2) %>% 
  summarize(Maize=sum(value))

baj<- kt%>% filter(crop=="Bajra") %>% 
  group_by(NAME_2) %>% 
  summarize(Bajra=sum(value))


ben<- kt%>% filter(crop=="benGram") %>% 
  group_by(NAME_2) %>% 
  summarize(BenGram=sum(value))

green<- kt%>% filter(crop=="Greengram") %>% 
  group_by(NAME_2) %>% 
  summarize(Greengram=sum(value))


Sgrcn<- kt%>% filter(crop=="Sgrcn") %>% 
  group_by(NAME_2) %>% 
  summarize(sgrcn=sum(value))

sunflr<- kt%>% filter(crop=="Snflr") %>% 
  group_by(NAME_2) %>% 
  summarize(snflr=sum(value))


oni<- kt%>% filter(crop=="onion") %>% 
  group_by(NAME_2) %>% 
  summarize(onion=sum(value))

grnt<- kt%>% filter(crop=="Grnut") %>% 
  group_by(NAME_2) %>% 
  summarize(groundnut=sum(value))


cot<- kt%>% filter(crop=="Cotton") %>% 
  group_by(NAME_2) %>% 
  summarize(cotton=sum(value))

pad<- kt%>% filter(crop=="paddy") %>% 
  group_by(NAME_2) %>% 
  summarize(pady=sum(value))


rag<- kt%>% filter(crop=="Ragi") %>% 
  group_by(NAME_2) %>% 
  summarize(ragi=sum(value))

wht<- kt%>% filter(crop=="wheat") %>% 
  group_by(NAME_2) %>% 
  summarize(wheat=sum(value))


chill<- kt%>% filter(crop=="chilli") %>% 
  group_by(NAME_2) %>% 
  summarize(chili=sum(value))

tom<- kt%>% filter(crop=="tomato") %>% 
  group_by(NAME_2) %>% 
  summarize(tomatoo=sum(value))


pom<- kt%>% filter(crop=="pomgrnt") %>% 
  group_by(NAME_2) %>% 
  summarize(pomgt=sum(value))

flwr<- kt%>% filter(crop=="flower") %>% 
  group_by(NAME_2) %>% 
  summarize(flowers=sum(value))


cart<- kt%>% filter(crop=="carrot") %>% 
  group_by(NAME_2) %>% 
  summarize(carot=sum(value))

cbg<- kt%>% filter(crop=="cabbg") %>% 
  group_by(NAME_2) %>% 
  summarize(cabg=sum(value))

mng<- kt%>% filter(crop=="mango") %>% 
  group_by(NAME_2) %>% 
  summarize(mangoo=sum(value))


hrsgm<- kt%>% filter(crop=="horsgrm") %>% 
  group_by(NAME_2) %>% 
  summarize(hrgrm=sum(value))

tur<- kt%>% filter(crop=="turmeric") %>% 
  group_by(NAME_2) %>% 
  summarize(turmer=sum(value))


cap<- kt%>% filter(crop=="capscm") %>% 
  group_by(NAME_2) %>% 
  summarize(caps=sum(value))


coc<- kt%>% filter(crop=="cocnt") %>% 
  group_by(NAME_2) %>% 
  summarize(cocnt=sum(value))


brin<- kt%>% filter(crop=="brinjal") %>% 
  group_by(NAME_2) %>% 
  summarize(brinj=sum(value))


cof<- kt%>% filter(crop=="coffee") %>% 
  group_by(NAME_2) %>% 
  summarize(coffe=sum(value))


pot<- kt%>% filter(crop=="potato") %>% 
  group_by(NAME_2) %>% 
  summarize(pota=sum(value))


jow<- kt%>% filter(crop=="Jowar") %>% 
  group_by(NAME_2) %>% 
  summarize(jowar=sum(value))

ban<- kt%>% filter(crop=="banana") %>% 
  group_by(NAME_2) %>% 
  summarize(bana=sum(value))






store<-Reduce(function(x, y) merge(x, y, all=TRUE),
              list(red, baj, maiz, are,
                   ben,green,Sgrcn,sunflr,
                   oni,grnt,cot,pad,
                   rag,wht,chill,tom,pom,
                   flwr, cart,cbg,mng,
                   hrsgm,tur,cap,coc,
                   brin,cof, pot, jow,
                   ban))

View(store)
 
write.csv(store, "notech.csv")


####MAP section#########
library(dplyr)
library(ggplot2)
library(dplyr)
library(raster)
library(tmap)
library(sp)
library(RColorBrewer)
library(rgeos)
library(dplyr)
library(tidyverse)

in_d <- getData('GADM', country='IND', level=2)
str(in_d, max.level=2)

data <- read.csv("E:/ifpri work/karnataka/map2/kt.csv")
View(data)

kar_subset<-in_d[in_d@data$NAME_1=="Karnataka",]

kar_subset@data$NAME_2<- tolower(kar_subset@data$NAME_2)

kar_subset@data<- merge(kar_subset@data,data, by.x="NAME_2", by.y="NAME_2", all.x=TRUE)
View(kar_map)


pun_subset<- in_d[in_d@data$NAME_1=="Karnataka",]
pun_subset@data$NAME_2<- tolower(pun_subset@data$NAME_2)

pun_subset@data<-merge(pun_subset@data, data, by.x="NAME_2", by.y="NAME_2", all.x=TRUE)

View(pun_subset@data)
tm_shape(pun_subset) +  tm_fill(col = "Redgram")

map1<-tm_shape(pun_subset) +
  tm_fill(col = "Redgram")+
  tm_shape(in_d)+
  tm_borders(col="black")+
  tm_legend(outside = TRUE)

map2<-tm_shape(pun_subset) +
  tm_fill(col = "Bajra")+
  tm_shape(in_d)+
  tm_borders(col="black")+
  tm_legend(outside = TRUE)

map3<-tm_shape(pun_subset) +
  tm_fill(col = "Maize")+
  tm_shape(in_d)+
  tm_borders(col="black")+
  tm_legend(outside = TRUE)

map4<-tm_shape(pun_subset) +
  tm_fill(col = "Arecant")+
  tm_shape(in_d)+
  tm_borders(col="black")+
  tm_legend(outside = TRUE)


map5<-tm_shape(pun_subset) +
  tm_fill(col = "BenGram")+
  tm_shape(in_d)+
  tm_borders(col="black")+
  tm_legend(outside = TRUE)


map6<-tm_shape(pun_subset) +
  tm_fill(col = "sgrcn")+
  tm_shape(in_d)+
  tm_borders(col="black")+
  tm_legend(outside = TRUE)

map7<-tm_shape(pun_subset) +
  tm_fill(col = "snflr")+
  tm_shape(in_d)+
  tm_borders(col="black")+
  tm_legend(outside = TRUE)

map8<-tm_shape(pun_subset) +
  tm_fill(col = "onion")+
  tm_shape(in_d)+
  tm_borders(col="black")+
  tm_legend(outside = TRUE)

map9<-tm_shape(pun_subset) +
  tm_fill(col = "Groundnut")+
  tm_shape(in_d)+
  tm_borders(col="black")+
  tm_legend(outside = TRUE)


map10<-tm_shape(pun_subset) +
  tm_fill(col = "Cotton")+
  tm_shape(in_d)+
  tm_borders(col="black")+
  tm_legend(outside = TRUE)


tmap_arrange(map1,map2,map3,map4,map5,map6,map7,map8,map9,map10)

######################## under CSA @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
kt <- read.csv("E:/ifpri work/karnataka/map2/kt.csv")


notech<-kt %>% group_by(NAME_2) %>% summarise(sum(value)) 

write.csv(notech,"notech.csv")

CSA_Districtwise<- kt %>% filter(tech=="t1" | tech=="t2" | tech=="t3" |tech=="t5" | tech=="t6" | tech=="t7") %>% 
  group_by(NAME_2) %>% summarise(CSA=sum(value))

Improved<- kt %>% filter(tech=="t4" | tech=="t8" | tech=="t9" |
                                      tech=="t10" | tech=="t11" | tech=="t12" 
                                    | tech=="t13" | tech=="t14" | tech=="t15") %>% 
  group_by(NAME_2) %>% summarise(improve_CSA_Districtwise=sum(value))


no_CSA<- kt %>% filter(tech=="t0") %>% group_by(NAME_2) %>% summarise(no_csa=sum(value))
write.csv(no_CSA,"t0tech.csv")


csaimprocved<- merge(CSA_Districtwise, Improved)

write.csv(csaimprocved, "csa&improve.csv")

write.csv(no_CSA, "no_tech.csv")

kt <- read.csv("E:/ifpri work/karnataka/map2/kt.csv")

notech<-kt %>% group_by(NAME_2) %>% summarise(notech=sum(value)) 

withtech<-kt %>% group_by(NAME_2) %>% summarise(withtech=sum(value)) 

withtech<-kt %>% group_by(NAME_2) %>% summarise(withtech=sum(value)) 

write.csv(notech,"notech.csv")
write.csv(withtech,"withtech.csv")
write.csv(notech,"income.csv")

write.csv(withtech,"excluding t0 withtech.csv")
################## CSA and Improve Income @@@@@@@@@@@@

kt <- read.csv("E:/ifpri work/karnataka/map2/kt.csv")
in_d <- getData('GADM', country='IND', level=2)

kar_subset<-in_d[in_d@data$NAME_1=="Karnataka",]


kar_subset@data<- merge(kar_subset@data,kt, by.x="NAME_2", by.y="NAME_2", all.x=TRUE)

#############

kt <- read.csv("E:/ifpri work/karnataka/map2/kt.csv")

csa<-tm_shape(kar_subset) +
  tm_fill(c("csa"), style="kmeans", 
          title=c(""))+ 
  tm_borders(col = "white")+
  tm_text("NAME_2", size = 1/2,col = "black")+
  tm_borders()+
  tm_layout("CSA, Scenario 3(b)",legend.title.size = .2,
            legend.text.size = .5,
            legend.position = c("right","centre"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

im<-tm_shape(kar_subset) +
  tm_fill(c("improve"), style="kmeans", 
          title=c(""))+ 
  tm_borders(col = "white")+
  tm_text("NAME_2", size = 1/2,col = "black")+
  tm_borders()+
  tm_layout("Other Improved Technologies, 
            Scenario 3(b)",legend.title.size = .5,
            legend.text.size = .5,
            legend.position = c("right","centre"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

notech<-tm_shape(kar_subset) +
  tm_fill(c("notech"), style="kmeans", 
          title=c(""))+ 
  tm_borders(col = "white")+
  tm_text("NAME_2", size = 1/2,col = "black")+
  tm_borders()+
  tm_layout("Base Technology",legend.title.size = .5,
            legend.text.size = .5,
            legend.position = c("right","centre"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

  
tmap_arrange(csa, im, ncol =2)
##########################income 

kt <- read.csv("E:/ifpri work/karnataka/map2/kt.csv")
 
###########################income
income<-tm_shape(kar_subset) +
  tm_fill(c("notech_income"), style="kmeans", 
          title=c(""),palette = "BuGn")+ 
  tm_borders(col = "white")+
  tm_text("NAME_2", size = 1/2,col = "black")+
  tm_borders()+
  tm_layout("Farm Income, Scenario 3(a)",legend.title.size = .5,
            legend.text.size = .5,
            legend.position = c("right","centre"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

tech_income<-tm_shape(kar_subset) +
  tm_fill(c("withtech_income"), style="kmeans", 
          title=c(""),palette = "BuGn")+ 
  tm_borders(col = "white")+
  tm_text("NAME_2", size = 1/2,col = "black")+
  tm_borders()+
  tm_layout("Farm Income, Scenario 3(b)",legend.title.size = .5,
            legend.text.size = .5,
            legend.position = c("right","centre"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

tmap_arrange(income,tech_income, ncol =2)

###########################dis_income


dis_income<-tm_shape(kar_subset) +
  tm_fill(c("dis_income"), style="kmeans", 
          title=c(""),palette = "BuGn")+ 
  tm_borders(col = "white")+
  tm_text("NAME_2", size = 1/2,col = "black")+
  tm_borders()+
  tm_layout("Profit Income, Scenario 3(a)",legend.title.size = .5,
            legend.text.size = .5,
            legend.position = c("right","centre"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

tech_income<-tm_shape(kar_subset) +
  tm_fill(c("dis_incometech"), style="kmeans", 
          title=c(""),palette = "BuGn")+ 
  tm_borders(col = "white")+
  tm_text("NAME_2", size = 1/2,col = "black")+
  tm_borders()+
  tm_layout("Profit Income, Scenario 3(b)",legend.title.size = .5,
            legend.text.size = .5,
            legend.position = c("right","centre"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

tmap_arrange(dis_income,tech_income, ncol =2)


##############################tech adopted 

tech_adopt<-tm_shape(kar_subset) +
  tm_fill(c("csa_techadopt"), style="kmeans", 
          title=c(""),palette = "BuGn")+ 
  tm_borders(col = "white")+
  tm_text("NAME_2", size = 1/2,col = "black")+
  tm_borders()+
  tm_layout("CSA, Scenario 3(b)",legend.title.size = .5,
            legend.text.size = .5,
            legend.position = c("right","centre"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

imp_techadopt<-tm_shape(kar_subset) +
  tm_fill(c("imp_techadopt"), style="kmeans", 
          title=c(""),palette = "BuGn")+ 
  tm_borders(col = "white")+
  tm_text("NAME_2", size = 1/2,col = "black")+
  tm_borders()+
  tm_layout("Other Improved Technologies, 
            Scenario 3(b)",legend.title.size = .5,
            legend.text.size = .5,
            legend.position = c("right","centre"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

tmap_arrange(tech_adopt,imp_techadopt, ncol =2)



