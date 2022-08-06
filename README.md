# Exploratory-Data-Analysis-and-Data-Visualisation-in-R
---
title: "MA_304_COURSEWORK"
author: "AHMAD_CHEEMA_2111168"
date: "15/04/2022"
output: html_document
---
## Introduction:

Data visualization plays an important part in Data science and big data. Human mind get things more easily when they are visualized. It makes easy to analyze and interpret data. Sometimes it plots graph by using every point of dataset like scatter plot, and sometimes uses statistical summary of the data for graph plotting like histograms. It helps to explore trends in data, to group data into clusters, for data cleaning or to evaluate output models. It makes easy to see and understands the patterns in the data in less time which is very hard to get from theoretical analysis or from raw data.

Crimes exists in every country, city, district and street of the world, but effective policing can control its extent. It is very important to control the crime rate so that people can live without fear and feel save to carry out their daily routine activities. To make policing more effective and efficient, to reduce crime rate, some strategies need to be implemented. One of the way is to analyze the history of the crime reported, the location, time, age group of subjects, weapon used etc. this will help police to deal with future cases that at what location, what day and what time of the day more security is required, which weapon are used mostly at crime scenes so that police will make strategies accordingly.

In this project, we have the dataset of crimes reported in 2016 in city of Dallas. It have information regarding the location, time, race, gender of the subject, weapons used in the crime. We will visualize these variables to see the relation between them that how they are related to each other at what time and location crimes are highest, what is the most common reason behind these crimes. This information helps to reduce the crime rate in future.


## Preliminary Analysis:

## Dataset:

The given dataset is of the crimes reported in Dallas in 2016. It have information of location which include city, area, division, district, sector, street name , street number and even complete address of the street where the incident is happened. It also mentioned the date and time of the occurrence of the incident.

At the same time it also provide information of the officers of dealt with the cases like his/her id, gender, race, hire date, number of years of his/her services. It also maintained details of the injuries if any officer is injured, like what type of injury he/she had, is he/she hospitalized.
It includes the information of the subject of the crime incident, his sex, race, his description, detail of the injury if he/she had any and whether he/she is arrested or not.






```{r setup, include=FALSE, warning=FALSE,message=FALSE}
knitr::opts_chunk$set(echo = TRUE)

###Load data
policing_data <- read.csv("C:/Users/cahma/Downloads/37-00049_UOF-P_2016_processed_policing 2016.csv", 
                      header = TRUE, colClasses = "factor", skip = 1)
##Import libraries
library(plotly)
library(ggplot2)
library(htmlwidgets)
library(plyr)
library(leaflet)
library(devtools)
library(lubridate)
library(forcats)
library(data.table)
library(dplyr)
library(scales)

##summary(policing_data)
##class(policing_data$Longitude)
policing_data$Latitude<-as.numeric(levels(policing_data$Latitude))[policing_data$Latitude]
policing_data$Longitude<-as.numeric(levels(policing_data$Longitude))[policing_data$Longitude]
```

## Analysis:

Firstly, we plot the incident on the map of Dallas city. It gives an overview that how incidents are divided into division, district and street of Dallas. More we zoom more we get the detailed overview from division to the street of the crime incident. It shows that how different regions of the city are affected by the crimes which region is highly affected which one is least. By visualizing the data on the crimes at different location, at different time helps to analyze the pattern in crime incidents 





```{r, echo=FALSE, warning=FALSE, message=FALSE}
#Plott_Map 1
##Clusters of crime in different locations of Dallas
map_division<-policing_data%>%leaflet(width = "100%")%>% addTiles() %>% 
addProviderTiles(providers$Esri.WorldStreetMap,group = "WorldStreetMap") %>%
  addMiniMap(tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE) %>%
  addMarkers(
  clusterOptions = markerClusterOptions(),clusterId = "quakesCluster")%>%addLayersControl(
    baseGroups = c("World StreetMap"),
    options = layersControlOptions(collapsed = FALSE)
  )
map_division
  
```

**Number of Incident per Subject's race**

We plot the graph to see the races of the subjects who are involved in the reported crimes.

In figure 1, we can see that Black people have high ratio who are suspected in reported crimes which is round about 1200, then there is Hispanics followed by white people and American Indian, Asian are reported least.


```{r, echo=FALSE,warning=FALSE,message=FALSE, fig.width=8}

##Plott_Figure 1
##No of Incidents per subject's race
policing_data3 <- policing_data[policing_data$CitRace != "NULL", ]

policing_data3 <- policing_data3[policing_data3$CIT_ARREST != "No", ]

count_race<-policing_data3%>%ggplot(aes(x = CitRace)) + 
  geom_bar(fill="#778899") + 
  labs(title="No of Incidents per subject's race",x = "Race",
       y = "No of Subjects")+   
  theme_minimal()+
  coord_flip()
  
ggplotly(count_race)
```

**Subject Races’ per division**

To see how subject of different races are spotted in different divisions, so that we draw the graph between subject’s race and divisions.

In the graph, figure 2, we can see that Black have high ratio in all divisions expect Northwest and North-central. In Northwest Hispanics are more reported for the crimes than Black, and in North-central white people are reported highest in crimes i.e. 40%. Asians are reported lowest in almost divisions.




```{r, echo=FALSE,warning=FALSE,message=FALSE, fig.width=8}
##Plott_Figure 2
##Ratio of subject's race per division
division_race <- setDT(policing_data, )[,list(count = .N), by = .(DIVISION, CitRace)][,list(CitRace = CitRace, 
count = count,
percent = paste0(formatC(count*100/sum(count), digits = 2), "%"),
percent_num = count/sum(count)), by = DIVISION]

division_race<-ggplot(division_race, aes(x = DIVISION, y = percent_num, fill = CitRace))+
geom_bar(position = position_fill(),stat = "identity",width = 0.4)+
labs(title = "Ratio of subject's race per division",x = "Division", y = "Ratio", fill="Race")+
geom_text(aes(label = percent), position = position_fill(vjust = 0.5), size = 2)+
theme_minimal(base_size=7.5)
ggplotly(division_race)
```

In the map below we can see that how subjects with different races are spread in different divisions of Dallas.


```{r, echo=FALSE, warning=FALSE, message=FALSE}
##Plott_Map 2
##Subjects with different races per division
map <- leaflet(policing_data3)%>%

  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain")
map_race <- map%>% addCircles(data =  policing_data3[policing_data3$CitRace=="White",], 
                                           group = "White",col="#d73027")%>%
  addCircles(data = policing_data3[policing_data3$CitRace=="Other",],
             group = "Other",col="#FF0000")%>%
  addCircles(data = policing_data3[policing_data3$CitRace=="Hispanic",],
             group = "Hispanic",col="#CD853F")%>%
  addCircles(data = policing_data3[policing_data3$CitRace=="Black",],
             group = "Black",col="#4169E1")%>%
  addCircles(data = policing_data3[policing_data3$CitRace=="Asian",],
             group = "Asian",col="#32CD32")%>%
  addCircles(data = policing_data3[policing_data3$CitRace=="American Ind",],
             group = "American Ind",col="#008080")
#layers control 
map_race <- map_race%>%addLayersControl(
  baseGroups = c("Terrain", "OSM (default)"),
  overlayGroups = c("White","Other","Hispanic","Black",
                    "Asian","American Ind"),
  options = layersControlOptions(collapsed = FALSE))
map_race
```



**Number of Incident per Officer's race**

We also plot the graph to see the races of the Officers who dealt with these crimes.

In figure 3, we can see that white officers have high ratio who dealt with reported crimes which is round about 1200, then there is Hispanics followed by black people, whereas there is very short percentage of officers with Asian, American Indian and other races.

This shows that we cannot do racism that specific race is involved in crimes. Good and bad people are present everywhere, like as shown in the graphs Hispanics have noticeable percentage in both subjects as well as officers graphs.


```{r, echo=FALSE,warning=FALSE,message=FALSE, fig.width=8}
##Plott_Figure 3
##No of Incidents per officer's race
policing_data3 <- policing_data[policing_data$OffRace != "NULL", ]

policing_data3 <- policing_data3[policing_data3$CIT_ARREST != "No", ]

count_race<-policing_data3%>%ggplot(aes(x = OffRace)) + 
  geom_bar(fill="#778899") + 
  labs(title="No of Incidents per officer's race",x = "Race",
       y = "No of officers")+   
  theme_minimal()+
  coord_flip()
  
ggplotly(count_race)
```

**Officer's Races per division**

To see how Officers of different races are providing services in different divisions, we draw the graph between officers’ race and divisions.

In the figure 4, we can see that White have highest ratio in all divisions, followed by Hispanics then black.



```{r, echo=FALSE, warning=FALSE, message=FALSE, fig.width=8}
##Plott_Figure 4
##Ratio of officer's race per division
division_race <- setDT(policing_data, )[,list(count = .N), by = .(DIVISION, OffRace)][,list(OffRace= OffRace
, 
count = count,
percent = paste0(formatC(count*100/sum(count), digits = 2), "%"),
percent_num = count/sum(count)), by = DIVISION]

division_race<-ggplot(division_race, aes(x = percent_num, y = DIVISION, fill = OffRace))+
geom_bar(position = position_fill(),stat = "identity",width = 0.4)+
labs(title = "Ratio of officer's race per division",x = "Ratio", y = "Division", fill="Race")+
geom_text(aes(label = percent), position = position_fill(vjust = 0.5), size = 1.5)+
theme_minimal(base_size=7.5)+
coord_flip()

ggplotly(division_race)
```


**Number of incidents per Division**

For more clear overview we divide the number on incidents per division, we have 7 divisions namely Central, Southeast, Southwest, South central, Northeast, Northwest, North central.

In the figure 5, we can see that highest number of crimes are reported in Central division which is more than 400, then we have southeast followed by Northeast. Southwest, South-central, North-Central almost have same number of crimes. Lowest number of crimes are reported in Northwest.



```{r, echo=FALSE,warning=FALSE, message=FALSE, fig.width= 8, fig.height=4}
##Plott_Figure 5
##Count of Incicdents per Division
count_division<-policing_data%>%ggplot(aes(x = DIVISION, fill = DIVISION)) + 
 geom_bar(fill="#778899") +
  theme_minimal()+
  labs(title="No of incidents per division",x = "Division", y = "Number of Incidents")+ 
  theme_minimal(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(count_division)
```


We also show number of incidents in different division on the map of Dallas city in the graph below.

Here Brown color represents Central division, North central is represented by Maroon color, Cyan color is used for Northwest, and Dark-green shows incidents in Northwest. South-Central is represented by Grey color, whereas Southeast and Southwest are represented by colors Fuchsia and Khaki respectively. 

In the map 3, we can see that how number of incidents are happened in different areas of the divisions. 


```{r, echo=FALSE,warning=FALSE, message=FALSE}
##Plott_Map 3
##Map showing incidents in different divisions of Dallas
map_division <- leaflet(policing_data)%>%
 
  addTiles() %>%
  addProviderTiles(providers$Stamen.TopOSMFeatures, group = "TopOSMFeatures") %>%addProviderTiles(providers$Esri.WorldStreetMap, group = "WorldStreetMap") %>% addLayersControl(
    baseGroups = c("TopOSMFeatures", "WorldStreetMap"),overlayGroups = c("NORTH CENTRAL","NORTHEAST","NORTHWEST","SOUTH CENTRAL","SOUTHEAST","SOUTHWEST"),
  
    options = layersControlOptions(collapsed = FALSE)
  )
 
 
map_division <- map_division%>% addCircles(data = policing_data[policing_data$DIVISION=="CENTRAL",], 
                         group = "CENTRAL",col="#d73027")%>%
  addCircles(data = policing_data[policing_data$DIVISION=="NORTH CENTRAL",],
             group = "NORTH CENTRAL",col="#A52A2A")%>%
  addCircles(data = policing_data[policing_data$DIVISION=="NORTHEAST",],
             group = "NORTHEAST",col="#00ffff")%>%
  addCircles(data = policing_data[policing_data$DIVISION=="NORTHWEST",],
             group = "NORTHWEST",col="#006400")%>%
  addCircles(data = policing_data[policing_data$DIVISION=="SOUTH CENTRAL",],
             group = "SOUTH CENTRAL",col="#696969")%>%
  addCircles(data = policing_data[policing_data$DIVISION=="SOUTHEAST",],
             group = "SOUTHEAST",col="#ff00ff")%>%
  addCircles(data = policing_data[policing_data$DIVISION=="SOUTHWEST",],
             group = "SOUTHWEST",col="#f0e68c")

map_division
```


**Number of incidents per Street type**

For more clear and detailed overview we divide the number on incidents per street type to see the crime ratio at smaller level, we have a different type of streets like avenue (Ave), boulevard (Blvd), street (St), Parkview (pkwy), freeway( Frwy), highway(Hwy), Expressway (Expwy.) etc.

In the graph we can see that highest number of crimes are reported in streets (St) which is more than 400. About 400 cases are reported on roads (Rd.), followed by avenue (Ave). Lowest number of crimes are reported in street type of circle (Cir.), Ct., row and intersects. 


```{r, echo=FALSE,warning=FALSE, message=FALSE, fig.width= 8, fig.height=4}
##Plott_Figure 6
##The count of incidents per Street
count_division <-policing_data%>%ggplot(aes(x = street_t,fill = street_t))+  
  geom_bar(fill="#778899") +
  theme_minimal()+
  labs(title="No of incidents per Street",x = "street Name", y = "No of Incidents")+ 
  theme_minimal(base_size = 7)+
  theme(plot.title = element_text(hjust = 0.5))


ggplotly(count_division)


```


**Number of Incidents per Day**

After visualizing and analyzing the number of incidents in different divisions, we also analyze that how the ratio of the number of incidents varies in different days of the week.

The graph below shows that Sunday have the highest rate of crime while Monday have the least. Tuesday and Thursday have almost same rate. Whereas crime rate gradually starts increasing from Wednesday, mid of the week, followed by Thursday, Friday, Saturday and have highest rate on Sunday. 
It shows that crimes incidents are usually high during weekends as compare to week days. 



```{r, echo=FALSE,warning=FALSE,message=FALSE, fig.width=8, fig.height=4}
##Plott_Figure 7
##Incidents on different days of the week

#policing_data$OCCURRED_D <- as.Date(policing_data$OCCURRED_D, "%m/%d/%y")
policing_data$OCCURRED_D <- parse_date_time(x = policing_data$OCCURRED_D,
                                        orders = c("d m y", "d B Y", "m/d/y"))

##Extracting weekdays from dates
policing_data$weekday<-weekdays(policing_data$OCCURRED_D)

count_day<-policing_data%>%
 mutate(weekday = fct_relevel(weekday, 
                            "Sunday", "Monday", "Tuesday", 
                            "Wednesday", "Thursday", "Friday", "Saturday"))%>%
  ggplot( aes(x=weekday,fill = weekday)) + 
  geom_bar(fill="#778899")+ theme_minimal()+
  labs(title="No of incidents per day",x = "Weekday", y = "No of Incidents")+ 
  theme_minimal(base_size = 10)+theme(plot.title = element_text(hjust = 0.5))+ 
  coord_flip()

ggplotly(count_day)


```

**Incidents on different days per Division**

We plot the graph to show the proportion of incidents in different divisions on different days of the week.

As show in the graph below, figure 8, Southeast and Northwest have lowest crime rate on Wednesday and highest on Sunday. South-central highest crime rate is recorded 17% on Thursday and lowest 12% that is on Monday.  In Northeast highest crime rates are recorded on Friday (24%) and lowest on Tuesday (9.1%). North-central have lowest and highest rate on Saturday and Sunday respectively. Whereas Central division have least crime rate on Thursday i.e. 11% and highest on Saturday i.e. 20%.



```{r,echo=FALSE,warning=FALSE,message=FALSE, fig.width=8}
##Plott_Figure 8
##Incidents on days of the week in each division
division_week <- setDT(policing_data, )[,list(count = .N), by = .(DIVISION, weekday)][,list(weekday = weekday, 
count = count,
percent = paste0(formatC(count*100/sum(count), digits = 2), "%"),
percent_total = count/sum(count)*100), by = DIVISION]

division_week<-ggplot(division_week, aes(x = DIVISION, y = weekday, fill=percent_total))+
geom_tile(stat = "identity")+
 scale_fill_gradient(low = "white", high = "red")+
labs(title="Ratio of incidents on different days per division",x = "Division", y = "Day", fill="percent_total")+
geom_text(aes(label = percent),position = position_dodge(), size = 2)+
theme_minimal()+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplotly(division_week)
```


**Number of Incident at different time of the day**

Incident happens on different time of the day. So we divide the 24 hours of the day into groups namely morning, evening, afternoon, night, where 5am to 12pm is grouped as morning, 12pm to 5pm is denoted as afternoon, 5pm to 9pm is in evening group and 9pm to 4am represent night.

In the figure 9, we can see that most of the crimes happened at night time and lowest at morning. As the time passes, it is noticed that crime rate increases gradually from morning (lowest) to night where crime rate is highest.



```{r, echo=FALSE,warning=FALSE,message=FALSE,fig.width=8, fig.height=4}
##Plott_Figure 9
##Incidents in different times of the day 
policing_data$time <- format(strptime(policing_data$OCCURRED_T, "%I:%M:%S %p"), "%H:%M:%S")


time <- as.POSIXct(strptime((policing_data$time),"%H:%M:%S"),"UTC")
x <- as.POSIXct(strptime(c("050000","115959","120000","165959", "170000","205959","210000","045959"),"%H%M%S"),"UTC")
policing_data$Parts_of_day <- case_when(
  between(time,x[1],x[2]) ~"Morning",
  between(time,x[3],x[4]) ~"Afternoon",
  between(time,x[5],x[6]) ~"Evening",
  TRUE ~"Night")

count_parts_of_day<-policing_data %>%
  mutate(Parts_of_day = fct_relevel(Parts_of_day, 
                            "Morning", "Afternoon", "Evening", 
                            "Night"))%>%
  ggplot( aes(x=Parts_of_day))+ 
  geom_bar(fill="#778899")+
  labs(title="Incidents in different times of the day ",x = "Parts of the day", y = "No of Incidents")+
  theme_minimal(base_size=10)

ggplotly(count_parts_of_day)
```

**Incident At different time of the day per division**

To analyze the pattern how crime rate increase or decrease during different time of the day in different division we plot the graph between the different time of the day and divisions.

It is shown in graph, figure 10, that in all divisions except South-central crime rate is highest at night, in South-central more crimes are reported in the evening than night. While Southwest, South-central, Northeast and Central have lowest crime rate in the morning, whereas Southeast, Northwest and North-central have least crime rate in the afternoon.




```{r, echo=FALSE,warning=FALSE,message=FALSE, fig.width=8}
##Plott_Figure 10
##Ratio of crime on different times of the day
division_parts_of_day <- setDT(policing_data, )[,list(count = .N), by = .(DIVISION,Parts_of_day)][,list(Parts_of_day = Parts_of_day, 
count = count,
percent = paste0(formatC(count*100/sum(count), digits = 2), "%"),
percent_num = count/sum(count)), by = DIVISION]

division_parts_of_day<-ggplot(division_parts_of_day, aes(x = DIVISION, y = percent_num, fill = Parts_of_day))+
geom_histogram(binwidth = 5, position = "dodge",stat = "identity",width = 0.4)+
labs(title="Ratio of incidents on different time of the day",x = "Division", y = "Ratio")+
geom_text(aes(label = percent), position = position_dodge(), size = 2.5)+
theme_minimal(base_size=8)

ggplotly(division_parts_of_day)
```

**Number of Incident per Subject Description**

Subject description can also play an important part to analyze the trends in crime incidents. It helps to reach at the root cause of the crime incident, it also give information about the reason to charge the subject, in return it helps the police and government to properly deal with the cases by taking necessary actions to avoid these type of incidents in future.

There are number of description that are recorded in the incidents like different drugs, mentally unstable suspect with any kind of weapon.

In figure 11, we can see that huge part of the subjects who are charged are mentally unstable. After mentally unstable most subjects are held charged for alcohol or other type of drugs. Some of the subjects are spotted with gun or other kind of weapons. It is also noticed that about 300 subjects are not detected with any of these description.


```{r, echo=FALSE,warning=FALSE, message=FALSE,fig.width=8,fig.height=4}
##Plott_Figure 11
##Number of Incident per Subject Description

policing_data2 <- policing_data[policing_data$CIT_INFL_A != "NULL", ]
policing_data2<- policing_data2[policing_data2$CIT_INFL_A != "Unknown"]

count_description<-ggplot(policing_data2,aes(x = CIT_INFL_A))+
  geom_bar(fill="#778899")+
  labs(title="No of incident per subject description",x = "Subject Description", y = "Count")+
  theme_minimal(base_size=8)+ 
  coord_flip()
ggplotly(count_description)
```



```{r, echo=FALSE,warning=FALSE,message=FALSE, fig.width=8}

policing_data<- policing_data[policing_data$CIT_INFL_A != "None detected"]
policing_data<- policing_data[policing_data$CIT_INFL_A != "Animal"]
policing_data<-policing_data[policing_data$CIT_INFL_A != "FD-Unknown if Armed"]
policing_data<- policing_data[policing_data$CIT_INFL_A != "FD-Animal"]
policing_data<- policing_data[policing_data$CIT_INFL_A != "FD-Suspect Unarmed"]
policing_data<- policing_data[policing_data$CIT_INFL_A != "FD-Motor Vehicle"]
#proportion of subject description on different days of the week
description_week <- setDT(policing_data, )[,list(count = .N), by = .(CIT_INFL_A,weekday)][,list(weekday = weekday, 
count = count,
percent = paste0(formatC(count*100/sum(count), digits = 2), "%"),
percent_num = count/sum(count)), by = CIT_INFL_A]

description_week<-ggplot(description_week, aes(x = CIT_INFL_A, y = percent_num, fill = weekday))+
geom_bar(position = position_fill(),stat = "identity",width = 0.4)+
labs(title ="Fig.7. Proportion of subject descriptions on different days", x = "Subject Description", y = "Proportion", fill="Day")+
geom_text(aes(label = percent), position = position_fill(vjust = 0.5), size = 2)+
theme_minimal(base_size=10)+
theme(plot.title = element_text(hjust = 0.5))+
theme(plot.caption = element_text(hjust=0.5, size=rel(1)))+coord_flip()
#ggplotly(description_week)
```


**Incident with different Subject description per division**

We plot the graph to see how the subjects are charged with different subject description in different division.

In the figure 11, we can see that subject with subject description mentally unstable, suspect with alcohol and other unknown drugs are highly reported in Central division. Marijuana is reported highest in incidents in Northeast. Subject with subject description suspected with gun are mostly belong to Southeast, and suspected with other weapons than gun have high ratio in North-central.


```{r,echo=FALSE,warning=FALSE,message=FALSE, fig.width=8}
##Plott_Figure 12
##Incident with different Subject description per division
description_division <- setDT(policing_data, )[,list(count = .N), by = .(CIT_INFL_A,DIVISION)][,list(DIVISION = DIVISION, 
count = count,
percent = paste0(formatC(count*100/sum(count), digits = 2), "%"),
percent_num = count/sum(count)), by = CIT_INFL_A]

description_division<-ggplot(description_division, aes(x = CIT_INFL_A, y = percent_num, fill = DIVISION))+
geom_bar(position = position_fill(),stat = "identity",width = 0.4)+
labs(title ="Ratio of subject descriptions", x = "Sub Description", y = "Ratio", fill="Division")+
geom_text(aes(label = percent), position = position_fill(vjust = 0.5), size = 2)+
theme_minimal(base_size=10)+
theme(plot.title = element_text(hjust = 0.5))+
theme(plot.caption = element_text(hjust=0.5, size=rel(1)))+coord_flip()

ggplotly(description_division)
```

## Results and Discussion

On the provided dataset we do some data analysis to get some specific patterns in the crime incidents.
From the analysis we get that mostly subjects who are charged for the crime incident belongs to black race in all divisions except Northwest and North-central where subjects belong to Hispanics and white race respectively. We also do analysis of race by using officers’ race data, which shows that mostly officers belong to white race in all divisions.

By analyzing and visualizing we get very interesting patterns in the location and time of the crime incidents. Central division have the highest crime rate whereas lowest crime rate is recorded in northwest. Streets (St.), roads (Rd.), avenues (Ave.) and drive (Dr.) street types have noticeable crime rate. Streets have the highest crime rate among all of them. We also see crime rate in high on weekends as compare to week days, highest crime ratio is recorded on Sunday while lowest on Monday. In almost all divisions crime rate is highest on weekends (Saturday and Sunday) or near to weakened (Friday) except South-central which have highest rate on Thursday. Crime rate varies during different times of the day, highest number of incidents are recorded in night and lowest in morning. All divisions have high crime rate during night time expect South-central, it has highest rate during evening.

From the analysis we get results that mostly subjects are mentally unstable and have alcohol or other type of drugs and they mostly belong to Central, while marijuana rate is high in Northwest and subjects with guns and other type of weapons are mostly spotted in southeast and North-central.



## Conclusion 

In world crime is a very big issue, and it is very important to deal with it to keep the peace of the world. To deal with crime, police needs to be more efficient. For efficient and effective policing there are certain necessary steps and actions are needed. By getting the trends and patterns from the past incidents, police will understand what necessary and important steps should take to avoid or at least overcome these type of incidents in future. Like Central division and streets have highest number of crime incidents, and on weekends crime rate is high as compare to week days and crime rate gradually increase from morning to night so police should increase the security in these circumstances or stay more alert during such situations. From the analysis we get that most subjects who are charged mostly black and mostly officers in all divisions are belong to white race. In Central mostly subjects are mentally unstable or have drugs, whereas in Southeast and North-central mostly subjects are suspected with gun and other weapons so police should use supplies and do arrangements accordingly.
