#### Preamble ####
# Purpose: Replicates graphs from targeted project
# Author: Cheng Yang
# Date: 28 March 2024
# Contact: yvonneyang.cheng@mail.utoronto.ca
# License: MIT

# Portions of the code used to create visualizations were adapted from methodologies shared in the 
# original paper and previous example (https://figshare.com/articles/dataset/A_global_dataset_for_the_projected_impacts_of_climate_change_on_four_major_crops/14691579/4?file=31603022).

rm(list=ls())
library(openxlsx)
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(rworldmap)
library(RColorBrewer)

TAB<-read.xlsx("Projected_impacts_datasheet_11242021.xlsx", sheet=1)
head(TAB)

#Extraction of columns
Species<-as.factor(TAB$Crop)
Ref<-TAB$Ref.No
Country<-as.factor(TAB$Country)
Region<-as.factor(TAB$Region)
Sample<-TAB$Sample.No.
Delta_temp<-as.numeric(TAB$Global.delta.T.from.2005)
Effect<-as.numeric(TAB$Climate.impacts.relative.to.2005)
CO2<-TAB$CO2
CO2[CO2=="Yes" | CO2=="yes"]<-"Yes"
CO2[CO2=="No"]<-"No"
CO2<-as.factor(CO2)
Adaptation<-as.factor(TAB$Adaptation)
RCP<-as.factor(TAB$Climate.scenario)
TempAvg<-as.numeric(TAB$`Current.Average.Temperature.(dC)_area_weighted`)
Latitude<-TAB$latitude
Longitude<-TAB$longitude
Time<-as.factor(TAB$Time.slice)
DATA<-data.frame(Effect,Country,Species,Delta_temp,Time,RCP,CO2,Adaptation,TempAvg, Latitude, Longitude,Region)

#selecting CO2 fertilization = "Yes" & "Without Adaptation" 
DATA = filter(DATA, CO2 == "Yes")

#RCP 8.5 End Century 
EC8.5 = filter(DATA,RCP == "RCP8.5"& Time == "EC"& Adaptation == "No")
EC8.5plus = filter(EC8.5, Effect >0)
EC8.5minus = filter(EC8.5, Effect < 0)

#RCP 2.6 End Century 
EC2.6 = filter(DATA,RCP == "RCP2.6"& Time == "EC"& Adaptation == "No")
EC2.6plus = filter(EC2.6, Effect >0)
EC2.6minus = filter(EC2.6, Effect < 0)

#RCP 4.5 End Century 
EC4.5 = filter(DATA,RCP == "RCP4.5"& Time == "EC"& Adaptation == "No")
EC4.5plus = filter(EC4.5, Effect >0)
EC4.5minus = filter(EC4.5, Effect < 0)

#RCP 8.5 Mid Century 
MC8.5 = filter(DATA,RCP == "RCP8.5"& Time == "MC"& Adaptation == "No")
MC8.5plus = filter(MC8.5, Effect >0)
MC8.5minus = filter(MC8.5, Effect < 0)

#RCP 4.5 Mid Century 
MC4.5 = filter(DATA,RCP == "RCP4.5"& Time == "MC"& Adaptation == "No")
MC4.5plus = filter(MC4.5, Effect >0)
MC4.5minus = filter(MC4.5, Effect < 0)

#RCP 2.6 Mid Century 
MC2.6 = filter(DATA,RCP == "RCP2.6"& Time == "MC"& Adaptation == "No")
MC2.6plus = filter(MC2.6, Effect >0)
MC2.6minus = filter(MC2.6, Effect < 0)

world = map_data("world")

pal <- c("#99FFFF","#66FFCC","#99CCFF","#33CCFF","#3399CC","#006699","#0000FF","#0000FF","#0000FF","#000099", "#000066") 

# RCP8.5 End Century Negative Impact 
dev.new(1)
world %>% 
  ggplot()+
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = "#CCCCCC", colour = "black", size = 0.1)+
  theme_bw()+
  geom_point(aes(x = Longitude, y = Latitude, color = Effect),size = 2,data = EC8.5minus)+
  scale_color_distiller(name = "Impact (%)", palette = "YlOrRd", direction = -1)+
  xlab("Longitude") +
  ylab("Latitude")+
  scale_x_continuous(breaks=seq(-180,180, by = 45),limits = c(-180,180))+
  scale_y_continuous(breaks=seq(-90,90, by = 30),limits = c(-90,90))+  
  ggtitle("RCP8.5 End-century")+
  theme(axis.ticks.length = unit(-2, "mm"),
        axis.text.x = element_text(margin =unit(rep(8,4),"mm")),
        axis.text.y = element_text(margin =unit(rep(8,4),"mm"))) +
  theme(legend.text = element_text(size = 13)) +
  theme(axis.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 13))+
  facet_grid(.~Species) +
  theme(strip.text.y = element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14, face ="bold"))

ggsave(file = "Fig4_1.tiff",dpi= 300, width = 12, height = 3.5 )

# RCP8.5 End-Century Positive impacts
dev.new(2)
world %>% 
  ggplot()+
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = "#CCCCCC", colour = "black", size = 0.1)+
  theme_bw()+
  geom_point(aes(x = Longitude, y = Latitude, color = Effect),size = 2,data = EC8.5plus)+
  scale_color_gradientn(name = "Impact (%)",colours = pal)+
  xlab("Longitude") +
  ylab("Latitude")+
  scale_x_continuous(breaks=seq(-180,180, by = 45),limits = c(-180,180))+
  scale_y_continuous(breaks=seq(-90,90, by = 30),limits = c(-90,90))+
  ggtitle("RCP8.5 End-Century ")+
  theme(axis.ticks.length = unit(-2, "mm"),
        axis.text.x = element_text(margin =unit(rep(8,4),"mm")),
        axis.text.y = element_text(margin =unit(rep(8,4),"mm"))) +
  theme(legend.text = element_text(size = 13)) +
  theme(axis.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 13))+
  facet_grid(.~Species) +
  theme(strip.text.y = element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14, face ="bold"))

ggsave(file = "Fig4_2.tiff",dpi= 300, width = 12, height = 3.5 )

# RCP8.5 Mid Century Negative Impact 
dev.new(3)
world %>% 
  ggplot()+
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = "#CCCCCC", colour = "black", size = 0.1)+
  theme_bw()+
  geom_point(aes(x = Longitude, y = Latitude, color = Effect),size = 2,data = MC8.5minus)+
  scale_color_distiller(name = "Impact (%)", palette = "YlOrRd", direction = -1)+
  xlab("Longitude") +
  ylab("Latitude")+
  scale_x_continuous(breaks=seq(-180,180, by = 45),limits = c(-180,180))+
  scale_y_continuous(breaks=seq(-90,90, by = 30),limits = c(-90,90))+  
  ggtitle("RCP8.5 Mid-century")+
  theme(axis.ticks.length = unit(-2, "mm"),
        axis.text.x = element_text(margin =unit(rep(8,4),"mm")),
        axis.text.y = element_text(margin =unit(rep(8,4),"mm"))) +
  theme(legend.text = element_text(size = 13)) +
  theme(axis.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 13))+
  facet_grid(.~Species) +
  theme(strip.text.y = element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14, face ="bold"))

ggsave(file = "Fig4_3.tiff",dpi= 300, width = 12, height = 3.5 )

# RCP8.5 Mid-Century Positive impacts
dev.new(4)
world %>% 
  ggplot()+
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = "#CCCCCC", colour = "black", size = 0.1)+
  theme_bw()+
  geom_point(aes(x = Longitude, y = Latitude, color = Effect),size = 2,data = MC8.5plus)+
  scale_color_gradientn(name = "Impact (%)",colours = pal)+
  xlab("Longitude") +
  ylab("Latitude")+
  scale_x_continuous(breaks=seq(-180,180, by = 45),limits = c(-180,180))+
  scale_y_continuous(breaks=seq(-90,90, by = 30),limits = c(-90,90))+
  ggtitle("RCP8.5 Mid-Century ")+
  theme(axis.ticks.length = unit(-2, "mm"),
        axis.text.x = element_text(margin =unit(rep(8,4),"mm")),
        axis.text.y = element_text(margin =unit(rep(8,4),"mm"))) +
  theme(legend.text = element_text(size = 13)) +
  theme(axis.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 13))+
  facet_grid(.~Species) +
  theme(strip.text.y = element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14, face ="bold"))

ggsave(file = "Fig4_4.tiff",dpi= 300, width = 12, height = 3.5 )

