library(visNetwork)
library(igraph)
library(igraphdata)
library(visNetwork)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(magrittr)
library(ggraph)
library(network)
library(sna)
library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
#Add more geographical attribute
continent<-read.csv("../Input/country-and-continent-codes-list.csv")
#1---1995 to 1999####
BACI_Grav_95_99 <- read.csv("../Output/Combined/BACI_Grav_95_99.csv")
str(BACI_Grav_95_99)
BACI_Grav_95_99$X<- NULL
#we have one country which is labled as N/A remove it. 
BACI_Grav_95_99<-BACI_Grav_95_99[(BACI_Grav_95_99$Exporter!='N/A' & BACI_Grav_95_99$Importer!='N/A'),]



edges_p1<-BACI_Grav_95_99[,which(names(BACI_Grav_95_99)%in%c('Exporter','Importer'))]
edges_p1$Source<-edges_p1$Exporter
edges_p1$Exporter<-NULL
edges_p1$Target<-edges_p1$Importer
edges_p1$Importer<-NULL

#**Adding Edge attributes ####
edges_p1$VoT  <- c(BACI_Grav_95_99$VoT)
edges_p1$Quantity  <- c(BACI_Grav_95_99$Quantity)
edges_p1$distance  <- c(BACI_Grav_95_99$distance)
edges_p1$contiguity  <- c(BACI_Grav_95_99$contiguity)
edges_p1$agree_pta_goods  <- c(BACI_Grav_95_99$agree_pta_goods)
edges_p1$agree_pta_services  <- c(BACI_Grav_95_99$agree_pta_services)
edges_p1$agree_cu  <- c(BACI_Grav_95_99$agree_cu)
edges_p1$agree_eia  <- c(BACI_Grav_95_99$agree_eia)
edges_p1$agree_fta  <- c(BACI_Grav_95_99$agree_fta)
edges_p1$agree_psa  <- c(BACI_Grav_95_99$agree_psa)
edges_p1$agree_pta  <- c(BACI_Grav_95_99$agree_pta)
edges_p1$VoT_Proportion  <- c(BACI_Grav_95_99$VoT_Proportion)

write.csv(edges_p1, file = "../Output/Network Data/edges_p1.csv", row.names = FALSE) 


#Adding Export Attributes
detach("package:plyr")
Exporter_attributes_p1<-BACI_Grav_95_99%>%group_by(BACI_Grav_95_99$Exporter)%>%
  summarise(landlocked_o=ifelse(sum(landlocked_o,na.rm=TRUE)>=1, 1, 0),
            island_o=ifelse(sum(island_o,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_o=mean(gdp_wdi_const_o,na.rm=TRUE),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o,na.rm=TRUE),
            pop_o=mean(pop_o,na.rm=TRUE))
Exporter_attributes_p1$country<-Exporter_attributes_p1$'BACI_Grav_95_99$Exporter'
Exporter_attributes_p1$'BACI_Grav_95_99$Exporter'<-NULL
Exporter_attributes_p1<-Exporter_attributes_p1[,c(6, 1:5)]#reorder

Importer_attributes_p1<-BACI_Grav_95_99%>%group_by(BACI_Grav_95_99$Importer)%>%
  summarise(landlocked_d=ifelse(sum(landlocked_d,na.rm=TRUE)>=1, 1, 0),
            island_d=ifelse(sum(island_d,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d,na.rm=TRUE),
            gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d,na.rm=TRUE),
            pop_d=mean(pop_d,na.rm=TRUE))
Importer_attributes_p1$country<-Importer_attributes_p1$'BACI_Grav_95_99$Importer'
Importer_attributes_p1$'BACI_Grav_95_99$Importer'<-NULL
Importer_attributes_p1<-Importer_attributes_p1[,c(6, 1:5)]#reorder

#the node attribute Dataframe####
node_attribute_p1<-merge(x=Exporter_attributes_p1, y=Importer_attributes_p1, by= 'country')

node_attribute_p1$landlocked<- apply(node_attribute_p1[,names(node_attribute_p1) %in% c("landlocked_o", "landlocked_d")], 1, max, na.rm=TRUE)
node_attribute_p1[,names(node_attribute_p1) %in% c("landlocked_o", "landlocked_d")]<- NULL 

node_attribute_p1$island<- apply(node_attribute_p1[,names(node_attribute_p1) %in% c("island_o", "island_d")], 1, max, na.rm=TRUE)
node_attribute_p1[,names(node_attribute_p1) %in% c("island_o", "island_d")]<- NULL  


node_attribute_p1$gdp_wdi_const<- apply(node_attribute_p1[,names(node_attribute_p1) %in% c("gdp_wdi_const_o", "gdp_wdi_const_d")], 1, mean, na.rm=TRUE)
node_attribute_p1[,names(node_attribute_p1) %in% c("gdp_wdi_const_o", "gdp_wdi_const_d")]<- NULL  


node_attribute_p1$gdp_wdi_cap_const<- apply(node_attribute_p1[,names(node_attribute_p1) %in% c("gdp_wdi_cap_const_o", "gdp_wdi_cap_const_d")], 1, mean, na.rm=TRUE)
node_attribute_p1[,names(node_attribute_p1) %in% c("gdp_wdi_cap_const_o", "gdp_wdi_cap_const_d")]<- NULL  

node_attribute_p1$pop<- apply(node_attribute_p1[,names(node_attribute_p1) %in% c("pop_o", "pop_d")], 1, mean, na.rm=TRUE)
node_attribute_p1[,names(node_attribute_p1) %in% c("pop_o", "pop_d")]<- NULL  

node_attribute_p1<- merge(x=node_attribute_p1, y=continent, by.x= 'country', by.y = 'Three_Letter_Country_Code', all.x = TRUE)
node_attribute_p1$ID<-node_attribute_p1$country
node_attribute_p1<-node_attribute_p1[,c(9, 1:8)]

write.csv(node_attribute_p1, file = "../Output/Network Data/node_attribute_p1.csv", row.names = FALSE)




#2---2000 to 2004####
BACI_Grav_00_04 <- read.csv("../Output/Combined/BACI_Grav_00_04.csv")
str(BACI_Grav_00_04)
BACI_Grav_00_04$X<- NULL
#we have one country which is labled as N/A remove it. 
BACI_Grav_00_04<-BACI_Grav_00_04[(BACI_Grav_00_04$Exporter!='N/A' & BACI_Grav_00_04$Importer!='N/A'),]


edges_p2<-BACI_Grav_00_04[,which(names(BACI_Grav_00_04)%in%c('Exporter','Importer'))]
edges_p2$Source<-edges_p2$Exporter
edges_p2$Exporter<-NULL
edges_p2$Target<-edges_p2$Importer
edges_p2$Importer<-NULL



#**Adding Edge attributes ####
edges_p2$VoT  <- c(BACI_Grav_00_04$VoT)
edges_p2$Quantity  <- c(BACI_Grav_00_04$Quantity)
edges_p2$distance  <- c(BACI_Grav_00_04$distance)
edges_p2$contiguity  <- c(BACI_Grav_00_04$contiguity)
edges_p2$agree_pta_goods  <- c(BACI_Grav_00_04$agree_pta_goods)
edges_p2$agree_pta_services  <- c(BACI_Grav_00_04$agree_pta_services)
edges_p2$agree_cu  <- c(BACI_Grav_00_04$agree_cu)
edges_p2$agree_eia  <- c(BACI_Grav_00_04$agree_eia)
edges_p2$agree_fta  <- c(BACI_Grav_00_04$agree_fta)
edges_p2$agree_psa  <- c(BACI_Grav_00_04$agree_psa)
edges_p2$agree_pta  <- c(BACI_Grav_00_04$agree_pta)
edges_p2$VoT_Proportion  <- c(BACI_Grav_00_04$VoT_Proportion)

write.csv(edges_p2, file = "../Output/Network Data/edges_p2.csv", row.names = FALSE)


#Adding Export Attributes
Exporter_attributes_p2<-BACI_Grav_00_04%>%group_by(BACI_Grav_00_04$Exporter)%>%
  summarise(landlocked_o=ifelse(sum(landlocked_o,na.rm=TRUE)>=1, 1, 0),
            island_o=ifelse(sum(island_o,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_o=mean(gdp_wdi_const_o,na.rm=TRUE),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o,na.rm=TRUE),
            pop_o=mean(pop_o,na.rm=TRUE))
Exporter_attributes_p2$country<-Exporter_attributes_p2$'BACI_Grav_00_04$Exporter'
Exporter_attributes_p2$'BACI_Grav_00_04$Exporter'<-NULL
Exporter_attributes_p2<-Exporter_attributes_p2[,c(6, 1:5)]#reorder

Importer_attributes_p2<-BACI_Grav_00_04%>%group_by(BACI_Grav_00_04$Importer)%>%
  summarise(landlocked_d=ifelse(sum(landlocked_d,na.rm=TRUE)>=1, 1, 0),
            island_d=ifelse(sum(island_d,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d,na.rm=TRUE),
            gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d,na.rm=TRUE),
            pop_d=mean(pop_d,na.rm=TRUE))
Importer_attributes_p2$country<-Importer_attributes_p2$'BACI_Grav_00_04$Importer'
Importer_attributes_p2$'BACI_Grav_00_04$Importer'<-NULL
Importer_attributes_p2<-Importer_attributes_p2[,c(6, 1:5)]#reorder

#the node attribute Dataframe####
node_attribute_p2<-merge(x=Exporter_attributes_p2, y=Importer_attributes_p2, by= 'country')

node_attribute_p2$landlocked<- apply(node_attribute_p2[,names(node_attribute_p2) %in% c("landlocked_o", "landlocked_d")], 1, max, na.rm=TRUE)
node_attribute_p2[,names(node_attribute_p2) %in% c("landlocked_o", "landlocked_d")]<- NULL 

node_attribute_p2$island<- apply(node_attribute_p2[,names(node_attribute_p2) %in% c("island_o", "island_d")], 1, max, na.rm=TRUE)
node_attribute_p2[,names(node_attribute_p2) %in% c("island_o", "island_d")]<- NULL  


node_attribute_p2$gdp_wdi_const<- apply(node_attribute_p2[,names(node_attribute_p2) %in% c("gdp_wdi_const_o", "gdp_wdi_const_d")], 1, mean, na.rm=TRUE)
node_attribute_p2[,names(node_attribute_p2) %in% c("gdp_wdi_const_o", "gdp_wdi_const_d")]<- NULL  


node_attribute_p2$gdp_wdi_cap_const<- apply(node_attribute_p2[,names(node_attribute_p2) %in% c("gdp_wdi_cap_const_o", "gdp_wdi_cap_const_d")], 1, mean, na.rm=TRUE)
node_attribute_p2[,names(node_attribute_p2) %in% c("gdp_wdi_cap_const_o", "gdp_wdi_cap_const_d")]<- NULL  

node_attribute_p2$pop<- apply(node_attribute_p2[,names(node_attribute_p2) %in% c("pop_o", "pop_d")], 1, mean, na.rm=TRUE)
node_attribute_p2[,names(node_attribute_p2) %in% c("pop_o", "pop_d")]<- NULL  

node_attribute_p2<- merge(x=node_attribute_p2, y=continent, by.x= 'country', by.y = 'Three_Letter_Country_Code', all.x = TRUE)

node_attribute_p2$ID<-node_attribute_p2$country
node_attribute_p2<-node_attribute_p2[,c(9, 1:8)]

write.csv(node_attribute_p2, file = "../Output/Network Data/node_attribute_p2.csv", row.names = FALSE)




#3---2005 to 2009####
BACI_Grav_05_09 <- read.csv("../Output/Combined/BACI_Grav_05_09.csv")
str(BACI_Grav_05_09)
BACI_Grav_05_09$X<- NULL
#we have one country which is labled as N/A remove it. 
BACI_Grav_05_09<-BACI_Grav_05_09[(BACI_Grav_05_09$Exporter!='N/A' & BACI_Grav_05_09$Importer!='N/A'),]


edges_p3<-BACI_Grav_05_09[,which(names(BACI_Grav_05_09)%in%c('Exporter','Importer'))]
edges_p3$Source<-edges_p3$Exporter
edges_p3$Exporter<-NULL
edges_p3$Target<-edges_p3$Importer
edges_p3$Importer<-NULL

#**Adding Edge attributes ####
edges_p3$VoT  <- c(BACI_Grav_05_09$VoT)
edges_p3$Quantity  <- c(BACI_Grav_05_09$Quantity)
edges_p3$distance  <- c(BACI_Grav_05_09$distance)
edges_p3$contiguity  <- c(BACI_Grav_05_09$contiguity)
edges_p3$agree_pta_goods  <- c(BACI_Grav_05_09$agree_pta_goods)
edges_p3$agree_pta_services  <- c(BACI_Grav_05_09$agree_pta_services)
edges_p3$agree_cu  <- c(BACI_Grav_05_09$agree_cu)
edges_p3$agree_eia  <- c(BACI_Grav_05_09$agree_eia)
edges_p3$agree_fta  <- c(BACI_Grav_05_09$agree_fta)
edges_p3$agree_psa  <- c(BACI_Grav_05_09$agree_psa)
edges_p3$agree_pta  <- c(BACI_Grav_05_09$agree_pta)
edges_p3$VoT_Proportion  <- c(BACI_Grav_05_09$VoT_Proportion)

write.csv(edges_p3, file = "../Output/Network Data/edges_p3.csv", row.names = FALSE) 

#Adding Export Attributes
Exporter_attributes_p3<-BACI_Grav_05_09%>%group_by(BACI_Grav_05_09$Exporter)%>%
  summarise(landlocked_o=ifelse(sum(landlocked_o,na.rm=TRUE)>=1, 1, 0),
            island_o=ifelse(sum(island_o,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_o=mean(gdp_wdi_const_o,na.rm=TRUE),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o,na.rm=TRUE),
            pop_o=mean(pop_o,na.rm=TRUE))
Exporter_attributes_p3$country<-Exporter_attributes_p3$'BACI_Grav_05_09$Exporter'
Exporter_attributes_p3$'BACI_Grav_05_09$Exporter'<-NULL
Exporter_attributes_p3<-Exporter_attributes_p3[,c(6, 1:5)]#reorder

Importer_attributes_p3<-BACI_Grav_05_09%>%group_by(BACI_Grav_05_09$Importer)%>%
  summarise(landlocked_d=ifelse(sum(landlocked_d,na.rm=TRUE)>=1, 1, 0),
            island_d=ifelse(sum(island_d,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d,na.rm=TRUE),
            gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d,na.rm=TRUE),
            pop_d=mean(pop_d,na.rm=TRUE))
Importer_attributes_p3$country<-Importer_attributes_p3$'BACI_Grav_05_09$Importer'
Importer_attributes_p3$'BACI_Grav_05_09$Importer'<-NULL
Importer_attributes_p3<-Importer_attributes_p3[,c(9, 1:8)]#reorder


#the node attribute Dataframe####
node_attribute_p3<-merge(x=Exporter_attributes_p3, y=Importer_attributes_p3, by= 'country')

node_attribute_p3$landlocked<- apply(node_attribute_p3[,names(node_attribute_p3) %in% c("landlocked_o", "landlocked_d")], 1, max, na.rm=TRUE)
node_attribute_p3[,names(node_attribute_p3) %in% c("landlocked_o", "landlocked_d")]<- NULL 

node_attribute_p3$island<- apply(node_attribute_p3[,names(node_attribute_p3) %in% c("island_o", "island_d")], 1, max, na.rm=TRUE)
node_attribute_p3[,names(node_attribute_p3) %in% c("island_o", "island_d")]<- NULL  


node_attribute_p3$gdp_wdi_const<- apply(node_attribute_p3[,names(node_attribute_p3) %in% c("gdp_wdi_const_o", "gdp_wdi_const_d")], 1, mean, na.rm=TRUE)
node_attribute_p3[,names(node_attribute_p3) %in% c("gdp_wdi_const_o", "gdp_wdi_const_d")]<- NULL  


node_attribute_p3$gdp_wdi_cap_const<- apply(node_attribute_p3[,names(node_attribute_p3) %in% c("gdp_wdi_cap_const_o", "gdp_wdi_cap_const_d")], 1, mean, na.rm=TRUE)
node_attribute_p3[,names(node_attribute_p3) %in% c("gdp_wdi_cap_const_o", "gdp_wdi_cap_const_d")]<- NULL  

node_attribute_p3$pop<- apply(node_attribute_p3[,names(node_attribute_p3) %in% c("pop_o", "pop_d")], 1, mean, na.rm=TRUE)
node_attribute_p3[,names(node_attribute_p3) %in% c("pop_o", "pop_d")]<- NULL  

node_attribute_p3<- merge(x=node_attribute_p3, y=continent, by.x= 'country', by.y = 'Three_Letter_Country_Code', all.x = TRUE)
node_attribute_p3$ID<-node_attribute_p3$country
node_attribute_p3<-node_attribute_p3[,c(9, 1:8)]

write.csv(node_attribute_p3, file = "../Output/Network Data/node_attribute_p3.csv", row.names = FALSE)






#4---2010 to 2014####
BACI_Grav_10_14 <- read.csv("../Output/Combined/BACI_Grav_10_14.csv")
str(BACI_Grav_10_14)
BACI_Grav_10_14$X<- NULL
#we have one country which is labled as N/A remove it. 
BACI_Grav_10_14<-BACI_Grav_10_14[(BACI_Grav_10_14$Exporter!='N/A' & BACI_Grav_10_14$Importer!='N/A'),]


edges_p4<-BACI_Grav_10_14[,which(names(BACI_Grav_10_14)%in%c('Exporter','Importer'))]
edges_p4$Source<-edges_p4$Exporter
edges_p4$Exporter<-NULL
edges_p4$Target<-edges_p4$Importer
edges_p4$Importer<-NULL

#Adding Edge attributes ####
edges_p4$VoT  <- c(BACI_Grav_10_14$VoT)
edges_p4$Quantity  <- c(BACI_Grav_10_14$Quantity)
edges_p4$distance  <- c(BACI_Grav_10_14$distance)
edges_p4$contiguity  <- c(BACI_Grav_10_14$contiguity)
edges_p4$agree_pta_goods  <- c(BACI_Grav_10_14$agree_pta_goods)
edges_p4$agree_pta_services  <- c(BACI_Grav_10_14$agree_pta_services)
edges_p4$agree_cu  <- c(BACI_Grav_10_14$agree_cu)
edges_p4$agree_eia  <- c(BACI_Grav_10_14$agree_eia)
edges_p4$agree_fta  <- c(BACI_Grav_10_14$agree_fta)
edges_p4$agree_psa  <- c(BACI_Grav_10_14$agree_psa)
edges_p4$agree_pta  <- c(BACI_Grav_10_14$agree_pta)
edges_p4$VoT_Proportion  <- c(BACI_Grav_10_14$VoT_Proportion)

write.csv(edges_p4, file = "../Output/Network Data/edges_p4.csv", row.names = FALSE) 

#Adding Export Attributes
Exporter_attributes_p4<-BACI_Grav_10_14%>%group_by(BACI_Grav_10_14$Exporter)%>%
  summarise(landlocked_o=ifelse(sum(landlocked_o,na.rm=TRUE)>=1, 1, 0),
            island_o=ifelse(sum(island_o,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_o=mean(gdp_wdi_const_o,na.rm=TRUE),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o,na.rm=TRUE),
            pop_o=mean(pop_o,na.rm=TRUE))
Exporter_attributes_p4$country<-Exporter_attributes_p4$'BACI_Grav_10_14$Exporter'
Exporter_attributes_p4$'BACI_Grav_10_14$Exporter'<-NULL
Exporter_attributes_p4<-Exporter_attributes_p4[,c(6, 1:5)]#reorder

Importer_attributes_p4<-BACI_Grav_10_14%>%group_by(BACI_Grav_10_14$Importer)%>%
  summarise(landlocked_d=ifelse(sum(landlocked_d,na.rm=TRUE)>=1, 1, 0),
            island_d=ifelse(sum(island_d,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d,na.rm=TRUE),
            gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d,na.rm=TRUE),
            pop_d=mean(pop_d,na.rm=TRUE))
Importer_attributes_p4$country<-Importer_attributes_p4$'BACI_Grav_10_14$Importer'
Importer_attributes_p4$'BACI_Grav_10_14$Importer'<-NULL
Importer_attributes_p4<-Importer_attributes_p4[,c(6, 1:5)]#reorder


#the node attribute Dataframe####
node_attribute_p4<-merge(x=Exporter_attributes_p4, y=Importer_attributes_p4, by= 'country')

node_attribute_p4$landlocked<- apply(node_attribute_p4[,names(node_attribute_p4) %in% c("landlocked_o", "landlocked_d")], 1, max, na.rm=TRUE)
node_attribute_p4[,names(node_attribute_p4) %in% c("landlocked_o", "landlocked_d")]<- NULL 

node_attribute_p4$island<- apply(node_attribute_p4[,names(node_attribute_p4) %in% c("island_o", "island_d")], 1, max, na.rm=TRUE)
node_attribute_p4[,names(node_attribute_p4) %in% c("island_o", "island_d")]<- NULL  


node_attribute_p4$gdp_wdi_const<- apply(node_attribute_p4[,names(node_attribute_p4) %in% c("gdp_wdi_const_o", "gdp_wdi_const_d")], 1, mean, na.rm=TRUE)
node_attribute_p4[,names(node_attribute_p4) %in% c("gdp_wdi_const_o", "gdp_wdi_const_d")]<- NULL  


node_attribute_p4$gdp_wdi_cap_const<- apply(node_attribute_p4[,names(node_attribute_p4) %in% c("gdp_wdi_cap_const_o", "gdp_wdi_cap_const_d")], 1, mean, na.rm=TRUE)
node_attribute_p4[,names(node_attribute_p4) %in% c("gdp_wdi_cap_const_o", "gdp_wdi_cap_const_d")]<- NULL  

node_attribute_p4$pop<- apply(node_attribute_p4[,names(node_attribute_p4) %in% c("pop_o", "pop_d")], 1, mean, na.rm=TRUE)
node_attribute_p4[,names(node_attribute_p4) %in% c("pop_o", "pop_d")]<- NULL  

node_attribute_p4<- merge(x=node_attribute_p4, y=continent, by.x= 'country', by.y = 'Three_Letter_Country_Code', all.x = TRUE)

node_attribute_p4$ID<-node_attribute_p4$country
node_attribute_p4<-node_attribute_p4[,c(9, 1:8)]

write.csv(node_attribute_p4, file = "../Output/Network Data/node_attribute_p4.csv", row.names = FALSE) 



#5---2015 to 2017####
BACI_Grav_15_17 <- read.csv("../Output/Combined/BACI_Grav_15_17.csv")
str(BACI_Grav_15_17)
BACI_Grav_15_17$X<- NULL
#we have one country which is labled as N/A remove it. 
BACI_Grav_15_17<-BACI_Grav_15_17[(BACI_Grav_15_17$Exporter!='N/A' & BACI_Grav_15_17$Importer!='N/A'),]


edges_p5<-BACI_Grav_15_17[,which(names(BACI_Grav_15_17)%in%c('Exporter','Importer'))]
edges_p5$Source<-edges_p5$Exporter
edges_p5$Exporter<-NULL
edges_p5$Target<-edges_p5$Importer
edges_p5$Importer<-NULL

#**Adding Edge attributes ####
edges_p5$VoT  <- c(BACI_Grav_15_17$VoT)
edges_p5$Quantity  <- c(BACI_Grav_15_17$Quantity)
edges_p5$distance  <- c(BACI_Grav_15_17$distance)
edges_p5$contiguity  <- c(BACI_Grav_15_17$contiguity)
edges_p5$agree_pta_goods  <- c(BACI_Grav_15_17$agree_pta_goods)
edges_p5$agree_pta_services  <- c(BACI_Grav_15_17$agree_pta_services)
edges_p5$agree_cu  <- c(BACI_Grav_15_17$agree_cu)
edges_p5$agree_eia  <- c(BACI_Grav_15_17$agree_eia)
edges_p5$agree_fta  <- c(BACI_Grav_15_17$agree_fta)
edges_p5$agree_psa  <- c(BACI_Grav_15_17$agree_psa)
edges_p5$agree_pta  <- c(BACI_Grav_15_17$agree_pta)
edges_p5$VoT_Proportion  <- c(BACI_Grav_15_17$VoT_Proportion)


write.csv(edges_p5, file = "../Output/Network Data/edges_p5.csv", row.names = FALSE) 

#Adding Export Attributes
Exporter_attributes_p5<-BACI_Grav_15_17%>%group_by(BACI_Grav_15_17$Exporter)%>%
  summarise(landlocked_o=ifelse(sum(landlocked_o,na.rm=TRUE)>=1, 1, 0),
            island_o=ifelse(sum(island_o,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_o=mean(gdp_wdi_const_o,na.rm=TRUE),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o,na.rm=TRUE),
            pop_o=mean(pop_o,na.rm=TRUE))
Exporter_attributes_p5$country<-Exporter_attributes_p5$'BACI_Grav_15_17$Exporter'
Exporter_attributes_p5$'BACI_Grav_15_17$Exporter'<-NULL
Exporter_attributes_p5<-Exporter_attributes_p5[,c(6, 1:5)]#reorder

Importer_attributes_p5<-BACI_Grav_15_17%>%group_by(BACI_Grav_15_17$Importer)%>%
  summarise(landlocked_d=ifelse(sum(landlocked_d,na.rm=TRUE)>=1, 1, 0),
            island_d=ifelse(sum(island_d,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d,na.rm=TRUE),
            gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d,na.rm=TRUE),
            pop_d=mean(pop_d,na.rm=TRUE))
Importer_attributes_p5$country<-Importer_attributes_p5$'BACI_Grav_15_17$Importer'
Importer_attributes_p5$'BACI_Grav_15_17$Importer'<-NULL
Importer_attributes_p5<-Importer_attributes_p5[,c(6, 1:5)]#reorder

#the node attribute Dataframe####
node_attribute_p5<-merge(x=Exporter_attributes_p5, y=Importer_attributes_p5, by= 'country')

node_attribute_p5$landlocked<- apply(node_attribute_p5[,names(node_attribute_p5) %in% c("landlocked_o", "landlocked_d")], 1, max, na.rm=TRUE)
node_attribute_p5[,names(node_attribute_p5) %in% c("landlocked_o", "landlocked_d")]<- NULL 

node_attribute_p5$island<- apply(node_attribute_p5[,names(node_attribute_p5) %in% c("island_o", "island_d")], 1, max, na.rm=TRUE)
node_attribute_p5[,names(node_attribute_p5) %in% c("island_o", "island_d")]<- NULL  


node_attribute_p5$gdp_wdi_const<- apply(node_attribute_p5[,names(node_attribute_p5) %in% c("gdp_wdi_const_o", "gdp_wdi_const_d")], 1, mean, na.rm=TRUE)
node_attribute_p5[,names(node_attribute_p5) %in% c("gdp_wdi_const_o", "gdp_wdi_const_d")]<- NULL  


node_attribute_p5$gdp_wdi_cap_const<- apply(node_attribute_p5[,names(node_attribute_p5) %in% c("gdp_wdi_cap_const_o", "gdp_wdi_cap_const_d")], 1, mean, na.rm=TRUE)
node_attribute_p5[,names(node_attribute_p5) %in% c("gdp_wdi_cap_const_o", "gdp_wdi_cap_const_d")]<- NULL  

node_attribute_p5$pop<- apply(node_attribute_p5[,names(node_attribute_p5) %in% c("pop_o", "pop_d")], 1, mean, na.rm=TRUE)
node_attribute_p5[,names(node_attribute_p5) %in% c("pop_o", "pop_d")]<- NULL  

node_attribute_p5<- merge(x=node_attribute_p5, y=continent, by.x= 'country', by.y = 'Three_Letter_Country_Code', all.x = TRUE)


node_attribute_p5$ID<-node_attribute_p5$country
node_attribute_p5<-node_attribute_p5[,c(9, 1:8)]

write.csv(node_attribute_p5, file = "../Output/Network Data/node_attribute_p5.csv", row.names = FALSE) 

