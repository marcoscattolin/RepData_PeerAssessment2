library(reshape2)
library(plyr)
library(ggplot2)
library(maps)


calculateCosts <- function(data){
        #define valid multipliers
        exponents <- data.frame(c("","0","H","K","M","B"),c(1,1,10^2,10^3,10^6,10^9))
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
        data
}



#load data
bigdata <- read.csv(bzfile("./data/repdata-data-StormData.csv.bz2"))

#select relevant columns
data <- bigdata[,c("STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP","LATITUDE","LONGITUDE")]

#columns to lower case
colnames(data) <- tolower(colnames(data))

#damage and event exp to upper case
data$propdmgexp <- factor(toupper(data$propdmgexp))
data$cropdmgexp <- factor(toupper(data$cropdmgexp))
data$evtype <- factor(toupper(data$evtype))


#check exp values
table(data$cropdmgexp)
table(data$propdmgexp)


#look for not valid values for cropdmgexp
mean(!(data$cropdmgexp %in% c("","0","H","K","M","B")))
data[!(data$cropdmgexp %in% c("","0","H","K","M","B")),]

#look for not valid for propdmgexp
mean(!(data$propdmgexp %in% c("","0","H","K","M","B")))
data[!(data$propdmgexp %in% c("","0","H","K","M","B")),]



data <- calculateCosts(data)


#clean latitudes and longitudes
data$longitude <- -data$longitude/100
data$latitude <- data$latitude/100


#drop unnecessary columns
data <- data[,c("state","evtype","fatalities","injuries","propdmg","cropdmg","latitude","longitude")]


#----------economic section
data$totalCost <- data$propdmg+data$cropdmg     #define total cost
economicData <- subset(data, totalCost > 0)     #select only events with cost > 0

#polish dataframe
economicData <- economicData[,c("state","evtype","totalCost","propdmg","cropdmg","latitude","longitude")]
economicData$evtype <- factor(economicData$evtype)
economicData$state <- factor(economicData$state)

#sum over states by type of event and arrange for decreasing costs
economicData <- dcast(economicData, state~evtype,fun.aggregate=sum,value.var="totalCost")
economicData <- melt(economicData, id="state")
economicData <- arrange(economicData, state, desc(value))

#select only max cost event
economicData <- split(economicData,economicData$state)
economicData <- lapply(economicData, function(x) x[1,])
economicData <- melt(economicData, id="state", id.vars="variable", measure.vars="value")
colnames(economicData) <- c("evtype","totalCost","state")
economicData$evtype <- factor(economicData$evtype)
economicData$state <- factor(economicData$state)


#----------health section

#define total health cost through pca
pca <- data[,c("fatalities","injuries")]
pca <- princomp(pca)
summary(pca)
data$totalHealthCost <- pca$scores[,1]     

healthData <- subset(data, totalHealthCost > 0)     #select only events with cost > 0


#polish dataframe
healthData <- healthData[,c("state","evtype","totalHealthCost","fatalities","injuries","latitude","longitude")]
healthData$evtype <- factor(healthData$evtype)
healthData$state <- factor(healthData$state)

#sum over states by type of event and arrange for decreasing costs
healthData <- dcast(healthData, state~evtype,fun.aggregate=sum,value.var="totalHealthCost")
healthData <- melt(healthData, id="state")
healthData <- arrange(healthData, state, desc(value))

#select only max cost event
healthData <- split(healthData,healthData$state)
healthData <- lapply(healthData, function(x) x[1,])
healthData <- melt(healthData, id="state", id.vars="variable", measure.vars="value")
colnames(healthData) <- c("evtype","totalHealthCost","state")
healthData$evtype <- factor(healthData$evtype)
healthData$state <- factor(healthData$state)
