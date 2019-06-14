

#install the following packages

library(dplyr)
library(geojsonio)
library(broom)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)


#Set theme
theme_set(theme_classic())

###GRAPH 2
#Import CNA_3, CNA_4, CNA_5, CNA_6
##MEGADATA
m1<-merge(CNA_3,CNA_4)
m2<-merge(m1,CNA_5)
Megadata2<-merge(m2,CNA_6)

#Labeling
names(Megadata2)[6]<-"Coffee"
names(Megadata2)[8]<-"Palm_oil"
names(Megadata2)[10]<-"Sugar_raw"
names(Megadata2)[14]<-"Cocoa"
names(Megadata2)[18]<-"Tobacco_raw"
names(Megadata2)[26]<-"Livestock"
names(Megadata2)[28]<-"fruit"
names(Megadata2)[30]<-"Bananas"
names(Megadata2)[46]<-"Plantains"

MEGADATA<-mutate(Megadata2,Fruits=fruit-Bananas)
MEGADATA<-MEGADATA %>% gather("Fruits","Plantains","Coffee",
                              "Palm_oil","Sugar_raw","Cocoa",
                              "Tobacco_raw","Livestock","Bananas",
                              key = "Product", value = "Land")

#Barchart national total
National1<-MEGADATA %>% subset(Departamento=="Total Nacional")

p3<-National1 %>%
  mutate(Product = fct_reorder(Product, -(as.numeric(Land)), fun=median)) %>%
  ggplot( aes(x=reorder(Product, -(as.numeric(Land))), y=(Land/10000), fill=Product)) +
  geom_bar(stat="identity")+
  xlab("Export Product") + ylab("Use of Land (per 10,000 Hectares)")+
  theme(legend.position="none")+
  scale_y_log10()
p3

p3<-National1 %>%
  mutate(Product = fct_reorder(Product, -(as.numeric(Land)), fun=median)) %>%
  ggplot(aes(x=reorder(Product, -(as.numeric(Land))), y=(Land), fill=Product)) +
  geom_bar(stat="identity")+
  xlab("Product") + ylab("Use of Land (Hectares)",caption = "Sources: DANE(2016) & FAO(2016)")+
  theme(legend.position="none")+
  scale_y_log10()
p3

##EXPORT GRAPH
#Import Exports

p2<-Exports %>% 
  mutate(Item = fct_relevel(Item,"Livestock","Fruits","Plantains","Coffee",
                            "Palm_oil","Sugar_raw","Cocoa","Bananas","Tobacco_raw")) %>%
  ggplot( aes(x=Item, y=Value)) +
  geom_bar(stat="identity")+
  xlab("")+ylab("Exports Value (1000 US dolars)")+
  theme(legend.position="none")+
  labs(title = "Land usage and export value", subtitle = "")+
  geom_text(aes(label=Ranking), vjust=-0.25, size=3.5)+
  scale_y_log10()
p2

library(gridExtra)
grid.arrange(p2,p3, nrow = 2)
ggsave("g6.png", width = 7, height =7)


###GRAPH 1
#import Suitable_use.csv
g1<-Suitable_use %>% gather("SUITABLE USE", "CURRENT USE", key = "type_use", value = "Land") %>%
  ggplot(mapping = aes(x = Type, y = Land, fill = type_use)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(fill="",title ="Suitable and current use of land",
       x = "Agricultural activity", y = "Land (Million hectares)",
       caption = "Sources: UPRA (2013) and DANE(2016, Tomo II)")+
  scale_color_brewer(palette = "Set3")
g1
ggsave("g1.png", width = 5, height =3)

###GRAPH 2
#import Assistance.csv
g2<-Assistance %>%
  ggplot(aes(x=reorder(Type, UPA_dist), y=UPA_dist, fill=reorder(Type, UPA_dist))) +
  geom_bar(stat="identity")+
  coord_flip()+
  geom_text(aes(label=percentage), vjust=0.5, hjust=-0.025, size=3) +
  labs(fill="",title ="UPA distribution (%), according to type of assistance", 
       x = "UPA distribution (%)", y = "Type of Assistance",
       caption = "Sources: DANE(2016, Tomo II)")+
  theme(legend.position="none")+
  scale_color_brewer(palette = "Set3")
g2
ggsave("g2.png", width = 7, height =5)

###GRAPH 5
#import Credit.csv
g3<-Credit %>%
  ggplot(aes(x=reorder(Approved_credit_destination, UPA_dist), y=UPA_dist
             , fill=reorder(Approved_credit_destination, UPA_dist))) +
  geom_bar(stat="identity")+
  coord_flip()+
  geom_text(aes(label=percentage), vjust=0.5, hjust=-0.025, size=3.5) +
  labs(fill="",title ="UPA distribution (%), according to approved credit destination", 
       x = "UPA distribution (%)", y = "Approved credit destination",
       caption = "Sources: DANE(2016, Tomo II)")+
  theme(legend.position="none")+
  scale_color_brewer(palette = "Set2")
g3
ggsave("g3.png", width = 8, height =5)

###GRAPH 6 
#import Land_use.csv
g4<-Land_use %>% gather("Livestock", "Crop_farming",
                            key = "type_use", value = "percentage") %>%
  mutate(Prop_dist = fct_relevel(Prop_dist,"National Total","< 5 ha","5 to 10 ha","10 to 50 ha"
                                 ,"50 to 100 ha","100 to 500 ha","500 to 1000 ha","> 1000 ha")) %>%
  ggplot(aes(x=Prop_dist,y=percentage,fill=type_use))+
  geom_bar(stat="identity")+
  coord_flip()+
  geom_text(aes(label=percentage), size = 3, position = position_stack(vjust = 0.5))+
  labs(fill="",title ="SHARE (%) OF LAND USED FOR LIVESTOCK AND CROP FARMING, BY UPA SIZE", 
       x = "", y = "Share of land %",caption = "Sources: DANE(2016,Tomo II)")+
  scale_color_brewer(palette = "Set3") 
g4
ggsave("g4.png", width = 8, height =4)

###GRAPH 7
g5<-Gini_index %>%
  ggplot(aes(x=YEAR,y=GINI))+
  geom_line(size=2,colour="red")+
  labs(fill="",title ="CHANGE IN GINI  COEFFICIENT FOR RURAL PROPERTY DISTRIBUTION (1960 TO 2014)", 
       x = "", y = "",caption = "Sources: IGAC(2014)")+
  ylim(0.810,0.910)+
  xlim(1960,2014)+
  geom_text(aes(label=GINI), size=3.5, vjust=-1.5, hjust=0)+
  theme_light()
g5
ggsave("g5.png", width = 7, height =5)
geom_text(aes(label=percentage), vjust=0.5, hjust=-0.025, size=3)

###INDEX GRAPHS

#IPM and Machines
#Import and merge
try1<-merge(IPM,IPM_people)
names(Machiney)[4]<-"Machines_total"
names(Machiney)[7]<-"eT"
DATA<-merge(try1,Machiney)
Datapoor<-DATA%>%filter(!is.na(Codigo))
names(Datapoor)[7]<-"IPM"
names(Datapoor)[10]<-"Poor"
names(Datapoor)[12]<-"UPA_Machines"
names(Datapoor)[13]<-"UPA_No_Machines"
names(Datapoor)[11]<-"Total_UPA"
Datapoor<-mutate(Datapoor,Tech_lack=(UPA_No_Machines/UPA_Machines))
p <- Datapoor %>% filter(Tech_lack<=50) %>%
  ggplot(mapping = aes(x = IPM, y = Tech_lack)) +
  geom_point()
p


p <- ggplot(data = DATA,
            mapping = aes(x = landsh, y = crop))
p + geom_point()


##INDEX

###GRAPH 1
#DATA MANIPULATION
#IMPORT CNA1 AND CNA2
CNA_DEP<-merge(CNA1,CNA2,by=c("Departamento","Codigo"))#MERGE CNA1 AND CNA2
#ADJUST FOR LINK VARIABLE
colnames(CNA_DEP)[2]<-"DPTO"

#VARIABLES CREATION
CNA_DEP<-mutate(CNA_DEP, Total_area =`Área agropecuaria`+`Área en otros usos`+`Área no agropecuaria` ) #FORESTS EXCLUDED
CNA_DEP<-mutate(CNA_DEP, productivity = (`Área agropecuaria`-`Uso predominantemente pecuario`)/Total_area)
CNA_DEP<-mutate(CNA_DEP, raw_productivity = (`Área agropecuaria`)/Total_area)
CNA_DEP$productivity[CNA_DEP$productivity<0]<-0.01

#COLOMBIA MAP
colombia <- geojson_read("https://gist.githubusercontent.com/john-guerra/43c7656821069d00dcbc/raw/3aadedf47badbdac823b00dbe259f6bc6d9e1899/colombia.geo.json",  what = "sp")

col_fortified<- tidy(colombia, region = "DPTO")
col_fortified = col_fortified %>%
  left_join(.,CNA_DEP,by=c("id"="DPTO"))

#Graph raw prod.:
q1<-ggplot() + 
  geom_polygon(data = col_fortified, aes(fill = raw_productivity, x = long, y = lat, group = group)) +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  labs(title = "Share of productive agricultural lands", subtitle = "Including livestock") +
  theme_light() +
  labs(fill="")
q1
ggsave("map1.png", width = 5, height = 5)
#Graph prod. final:

q2<-ggplot() + 
  geom_polygon(data = col_fortified, aes(fill = productivity, x = long, y = lat, group = group)) +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  labs(title = "Share of productive agricultural lands", subtitle = "Excluding livestock") +
  theme_light() + 
  labs(fill="")
q2
ggsave("map2.png", width = 5, height = 5)


##Boxplot
#import Megadata
#Labeling
names(Megadata)[7]<-"Coffee"
names(Megadata)[9]<-"Palm_oil"
names(Megadata)[11]<-"Sugar_raw"
names(Megadata)[15]<-"Cocoa"
names(Megadata)[19]<-"Tobacco_raw"
names(Megadata)[28]<-"Livestock"
names(Megadata)[29]<-"fruit"
names(Megadata)[31]<-"Bananas"
names(Megadata)[47]<-"Plantains"

Megadata<-mutate(Megadata,Fruits=fruit-Bananas)

library(tidyverse)
Megadatanew<-Megadata %>% gather("Fruits","Plantains","Coffee","Palm_oil","Sugar_raw","Cocoa","Tobacco_raw","Livestock","Bananas", key = "Product", value = "Land")

#Barchart national total
National<-Megadatanew %>% subset(Departamento=="Total Nacional")

p3<-ggplot(National, mapping=aes(x=Product, y=(Land/1000000))) +
  geom_bar(stat="identity")
p3

Megadatanew2<-Megadatanew %>% filter(as.numeric(Land)<=2000000,as.numeric(Land)>=100)
p1 <- Megadatanew2 %>%
  mutate(Product = fct_reorder(Product, -(as.numeric(Land)), fun=median)) %>%
  ggplot( aes(x=reorder(Product, -(as.numeric(Land))), y=as.numeric(Land), fill=Product)) + 
  geom_boxplot() +
  xlab("Product") + ylab("Land (Hectare)")+
  theme(legend.position="none")
p1
ggsave("plot8.png", width = 5, height = 5)


