#1 
#subset the data frame to get long and lat 
#use sapply() to get the mean range of long and lat 
#use get_map() and ggmap() tp plot the location of 5 hawks using geom_points() and labs()
mydata = read.csv("the_hawks.csv", header = T)
names(mydata)
head(mydata)
head(mydata$time)
install.packages("ggmap")
library(ggmap)
df = subset(mydata, select = c(long, lat))
loc = sapply(df[c("long","lat")], function(x) mean(range(x)))
m= get_map(loc, zoom = 10, maptype = "satellite")
ggmap(m)+ geom_point(aes(x= long , y = lat, 
                         color = factor(mydata$tag)), data = mydata, alpha = 0.5) + labs(
                           title ="GGmap Visualizing Location of 5 Hawks", x ="Long", y= "Lat")
#2.
#subset the data to get the arrival
#using unique() to know the tags of which hawks has arrivals sequences
#using saaply () to get mean range of long and lat and using ggplot to plot the each hawks
#using grid.arrange() to combine 2 plots
install.packages("stringr") #already installed
library(stringr)
arrival = subset(mydata,mydata$stage == "arrival")
unique(arrival$tag)
arr1 = subset(arrival,arrival$tag == unique(arrival$tag)[1])
arr2 = subset(arrival,arrival$tag == unique(arrival$tag)[2])
loc1 = sapply(arr1[c("long", "lat")], function(x) mean(range(x)))
m1 = get_map(loc1, zoom = 15)
data.frame(arr1)
a= ggmap(m1)+
  geom_point(aes(x = long, y = lat,color = factor(arr1$speed), size =factor(arr1$height )),data = arr1, alpha =0.5)+
  geom_line(aes(x = long, y=lat),data = arr1) + 
  labs(title ="GGmap Visualizing Location of Hawk named 105936", x ="Long", y= "Lat")+
  geom_text(aes(x = long, y = lat,color = factor(arr1$speed),label = factor(arr1$height)),
            data = arr1, alpha = 0.2)
a
loc2 = sapply(arr2[c("long", "lat")], function(x) mean(range(x)))
m2 = get_map(loc2, zoom = 12)
data.frame(arr2)
b = ggmap(m2)+geom_point(aes(x= long, y = lat),
                           data =arr1)+geom_point(aes(x = long, y = lat,color = factor(arr2$speed),size = factor(arr2$height)),data = arr2, alpha =0.5)+geom_line(aes(x = long, y=lat),data = arr2) + 
  labs(title ="GGmap Visualizing Location of Hawk named 105928", x ="Long", y= "Lat")
b
#install.packages("gridExtra")
library(gridExtra)
grid.arrange(a,b, newpage = TRUE)

#3
#using aggregate to get median of long, lat based on the tag of each hawks
#distGeo to get the distance from 1 point to others
#ggplot to plot 5 of them based on the distance and time with geom_points and lines
#subset the data wiht dis > 3 and time to get time interval of each hawk
#install.packages("geophere") # already installed
library(geosphere)
unique(mydata$tag)
hawk_meds<-aggregate(cbind(long,lat)~tag,mydata,median)
timea = format(as.Date(mydata$time), format ="%Y-%m-%d")
mydata$distance = distGeo(cbind(mydata$long,mydata$lat),hawk_meds[match(mydata$tag,hawk_meds$tag),-1])/1609
ggplot(mydata) + geom_point(aes(timea,distance,color = factor(mydata$tag))) + geom_line(aes(x = timea, y=distance, color = factor(tag)),data = mydata) + 
  labs(title ="GGmap Visualizing Distance of 5 Hawks ", x ="time", y= "distance")
hawk1 =subset(mydata, mydata$tag == unique(mydata$tag)[1])
hawk1_distance = subset(hawk1, hawk1$distance>3)
hawk1_timeinterval = sort(format(as.Date(hawk1_distance$time),"%Y-%m-%d"), decreasing = T)[1:6]
levels(as.factor(hawk1_timeinterval))

hawk2 =subset(mydata, mydata$tag == unique(mydata$tag)[2])
hawk2_distance = subset(hawk2, hawk2$distance>3)
hawk2_timeinterval =sort(format(as.Date(hawk2_distance$time),"%Y-%m-%d"), decreasing = TRUE)[1:11]
levels(as.factor(hawk2_timeinterval))

hawk3 =subset(mydata, mydata$tag == unique(mydata$tag)[3])
hawk3_distance = subset(hawk3, hawk3$distance>3)
hawk3_timeinterval =sort(format(as.Date(hawk3_distance$time),"%Y-%m-%d"), decreasing = TRUE)[1:5]
levels(as.factor(hawk3_timeinterval))

hawk4 =subset(mydata, mydata$tag == unique(mydata$tag)[4])
hawk4_distance = subset(hawk4, hawk4$distance>3)
hawk4_timeinterval = sort(format(as.Date(hawk4_distance$time),"%Y-%m-%d"), decreasing = TRUE)[1:7]
levels(as.factor(hawk4_timeinterval))

hawk5 =subset(mydata, mydata$tag == unique(mydata$tag)[5])
hawk5_distance = subset(hawk5, hawk5$distance>3)
hawk5_timeinterval = sort(format(as.Date(hawk5_distance$time),"%Y-%m-%d"), decreasing = TRUE)[1:3]
levels(as.factor(hawk5_timeinterval))

#4 Using the subset to get each hawk's long, lat, time, distance, speed... in order to know the migration of leaving the nest for good based on the time interval of problem 3
#plot ggplot() of each of hawk's migration based on long and lat with speed, height and time
library(lubridate)
dates_1 = levels(as.factor(hawk1_timeinterval))
hawk1_table = subset(hawk1_distance, format(as.Date(hawk1_distance$time),format= "%Y-%m-%d") %in% dates_1)
hour1 = hour(hawk1_table$time)
loc_hawk1 = sapply(hawk1_table[c("long", "lat")],function(x) mean(range(x)))
m1_hawk1 = get_map(loc_hawk1, zoom =10)
ggmap(m1_hawk1)+
  geom_point(aes(x = long, y = lat,color = factor(hawk1_table$speed),shape = factor(hour1), size =factor(hawk1_table$height )),data = hawk1_table, alpha =0.5)+
  geom_line(aes(x = long, y=lat),data = hawk1_table) + 
  labs(title ="Hawk 1 named [105936] Leaves The Nest ", x ="Long", y= "Lat")+
  geom_text(aes(x = long, y = lat,color = factor(hawk1_table$speed),label = factor(hawk1_table$height)),
            data = hawk1_table, alpha = 0.2)

dates_2 = levels(as.factor(hawk2_timeinterval))
hawk2_table = subset(hawk2_distance, format(as.Date(hawk2_distance$time),format= "%Y-%m-%d") %in% dates_2)
hour2 = hour(hawk2_table$time)
loc_hawk2 = sapply(hawk2_table[c("long", "lat")],function(x) mean(range(x)))
m2_hawk2 = get_map(loc_hawk2, zoom = 10)
ggmap(m2_hawk2)+
  geom_point(aes(x = long, y = lat,color = factor(hawk2_table$speed),shape = factor(hour2), size =factor(hawk2_table$height )),data = hawk2_table, alpha =0.5)+
  geom_line(aes(x = long, y=lat),data = hawk2_table) + 
  labs(title ="Hawk 2 named [105930] Leaves The Nest ", x ="Long", y= "Lat")+
  geom_text(aes(x = long, y = lat,color = factor(hawk2_table$speed),label = factor(hawk2_table$height)),
            data = hawk2_table, alpha = 0.2)

dates_3 = levels(as.factor(hawk3_timeinterval))
hawk3_table = subset(hawk3_distance, format(as.Date(hawk3_distance$time),format= "%Y-%m-%d") %in% dates_3)
hour3 = hour(hawk3_table$time)
loc_hawk3 = sapply(hawk3_table[c("long", "lat")],function(x) mean(range(x)))
m3_hawk3 = get_map(loc_hawk3, zoom = 12)
ggmap(m3_hawk3)+
  geom_point(aes(x = long, y = lat,color = factor(hawk3_table$speed),shape = factor(hour3), size =factor(hawk3_table$height )),data = hawk3_table, alpha =0.5)+
  geom_line(aes(x = long, y=lat),data = hawk3_table) + 
  labs(title ="Hawk 3 named [105923] Leaves The Nest ", x ="Long", y= "Lat")+
  geom_text(aes(x = long, y = lat,color = factor(hawk3_table$speed),label = factor(hawk3_table$height)),
            data = hawk3_table, alpha = 0.2)

dates_4 = levels(as.factor(hawk4_timeinterval))
hawk4_table = subset(hawk4_distance, format(as.Date(hawk4_distance$time),format= "%Y-%m-%d") %in% dates_4)
hour4 = hour(hawk4_table$time)
loc_hawk4 = sapply(hawk4_table[c("long", "lat")],function(x) mean(range(x)))
m4_hawk4 = get_map(loc_hawk4, zoom = 10)
ggmap(m4_hawk4)+
  geom_point(aes(x = long, y = lat,color = factor(hawk4_table$speed),shape = factor(hour4), size =factor(hawk4_table$height )),data = hawk4_table, alpha =0.5)+
  geom_line(aes(x = long, y=lat),data = hawk4_table) + 
  labs(title ="Hawk 4 named [105928] Leaves The Nest ", x ="Long", y= "Lat")+
  geom_text(aes(x = long, y = lat,color = factor(hawk4_table$speed),label = factor(hawk4_table$height)),
            data = hawk4_table, alpha = 0.2)


dates_5 = levels(as.factor(hawk5_timeinterval))
hawk5_table = subset(hawk5_distance, format(as.Date(hawk5_distance$time),format= "%Y-%m-%d") %in% dates_5)
hour5 = hour(hawk5_table$time)
loc_hawk5 = sapply(hawk5_table[c("long", "lat")],function(x) mean(range(x)))
m5_hawk5 = get_map(loc_hawk5, zoom = 17)
ggmap(m5_hawk5)+
  geom_point(aes(x = long, y = lat,color = factor(hawk5_table$speed),shape = factor(hour5), size =factor(hawk5_table$height )),data = hawk5_table, alpha =0.5)+
  geom_line(aes(x = long, y=lat),data = hawk5_table) + 
  labs(title ="Hawk 5 named [117527] Leaves The Nest ", x ="Long", y= "Lat")+
  geom_text(aes(x = long, y = lat,color = factor(hawk5_table$speed),label = factor(hawk5_table$height)),
            data = hawk5_table, alpha = 0.2)

