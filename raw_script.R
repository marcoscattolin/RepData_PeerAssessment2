library(reshape2)
library(plyr)
library(ggplot2)
library(maps)
#load data
bigdata <- read.csv(bzfile("./data/repdata-data-StormData.csv.bz2"))

#select relevant columns
data <- bigdata[,c("STATE","COUNTYNAME","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP","LATITUDE","LONGITUDE")]

#columns to lower case
colnames(data) <- tolower(colnames(data))

#damage exp to upper case
data$propdmgexp <- toupper(data$propdmgexp)
data$cropdmgexp <- toupper(data$cropdmgexp)


#evttype to upper case
data$evtype <- factor(toupper(data$evtype))


#check exp values
table(data$cropdmgexp)
table(data$propdmgexp)


#look for not valid values for cropdmgexp
validexp <- c("","H","K","M","B")
invalid.exp <- !(data$cropdmgexp %in% validexp)
mean(invalid.exp)
data[invalid.exp,]

#look for not valid for cropdmgexp
invalid.exp <- !(data$propdmgexp %in% validexp)
mean(invalid.exp)
data[invalid.exp,]


#define valid multipliers
exponents <- data.frame(c("","H","K","M","B"),c(1,10^2,10^3,10^6,10^9))
colnames(exponents) <- c("validexp","multiplier")

#subset over valid exps
data <- subset(data, (cropdmgexp %in% exponents$validexp) & (propdmgexp %in% exponents$validexp))


#convert damage values in number
colnames(exponents) <- c("validexp","propdmgmultiplier")
data <- merge(data, exponents, by.x="propdmgexp", by.y="validexp")
data$propdmg <- (data$propdmg*data$propdmgmultiplier)

colnames(exponents) <- c("validexp","cropdmgmultiplier")
data <- merge(data, exponents, by.x="cropdmgexp", by.y="validexp")
data$cropdmg <- (data$cropdmg*data$cropdmgmultiplier)

#clean latitudes and longitudes
data$longitude <- -data$longitude/100
data$latitude <- data$latitude/100


#drop unnecessary columns
data <- data[,c("state","evtype","fatalities","injuries","propdmg","cropdmg","latitude","longitude")]


#Define economicDmgScore using PCA
pca <- princomp(data[,c("propdmg","cropdmg")], scale=TRUE)
summary(pca)
plot(pca,type="lines", main = "Screeplot for Economic damage")
data$economicDmgScore <- -pca$scores[,1]
data[head(order(data$economicDmgScore,decreasing=T)),]


#Define healthDmgScore using PCA
pca <- princomp(data[,c("fatalities","injuries")], scale=TRUE)
summary(pca)
plot(pca,type="lines", main = "Screeplot for Economic damage")
data$healthDmgScore <- pca$scores[,1]        
data[head(order(data$healthDmgScore,decreasing=T)),]


#find worstEconomicEvents
worstEconomicEvents <- data[,c("state","evtype","economicDmgScore")]
worstEconomicEvents <- tapply(worstEconomicEvents$economicDmgScore,worstEconomicEvents$evtype,sum)
worstEconomicEvents <- data.frame(head(worstEconomicEvents[order(worstEconomicEvents,decreasing=T)],5))
economicDisasters <- subset(data, evtype %in% row.names(worstEconomicEvents))
economicDisasters <- arrange(economicDisasters, desc(economicDmgScore))
economicDisasters <- subset(economicDisasters,(longitude != 0 & latitude != 0))
economicDisasters <- head(economicDisasters,100)
map("usa")
radius <- economicDisasters$economicDmgScore/(100000000)
symbols(economicDisasters$longitude,economicDisasters$latitude, circles=radius)



#find worstHealthEvents
worstHealthEvents <- data[,c("state","evtype","healthDmgScore")]
worstHealthEvents <- tapply(worstHealthEvents$healthDmgScore,worstHealthEvents$evtype,sum)
worstHealthEvents <- data.frame(head(worstHealthEvents[order(worstHealthEvents,decreasing=T)],5))
healthDisasters <- subset(data, evtype %in% row.names(worstHealthEvents))
healthDisasters <- arrange(healthDisasters, desc(healthDmgScore))
healthDisasters <- subset(healthDisasters,longitude != 0 & latitude != 0)
healthDisasters <- head(healthDisasters,100)





#-------------------ECONOMIC

#Get world map info
map <- map_data("usa")

#Creat a base plot
p <- ggplot() + coord_fixed()

#Add map to base plot
base_world <- p + geom_polygon(data=map,
                               aes(x=long,
                                   y=lat,
                                   group=group))

#Creat an example plot
map_with_jitter <- base_world+geom_point(data=economicDisasters,
                                         aes(x=economicDisasters$longitude,
                                             y=economicDisasters$latitude,
                                             size=log(economicDisasters$economicDmgScore)),
                                         colour= "red",
                                         alpha=I(0.5))
print(map_with_jitter)


#-------------------HEALTH

#Get world map info
map <- map_data("usa")

#Creat a base plot
p <- ggplot() + coord_fixed()

#Add map to base plot
base_world <- p + geom_polygon(data=map,
                               aes(x=long,
                                   y=lat,
                                   group=group))

#Creat an example plot
map_with_jitter <- base_world+geom_point(data=healthDisasters,
                                         aes(x=healthDisasters$longitude,
                                             y=healthDisasters$latitude,
                                             size=healthDisasters$healthDmgScore),
                                         colour= "red",
                                         alpha=I(0.5))
print(map_with_jitter)
