library(reshape2)
library(plyr)
library(ggplot2)
#load data
bigdata <- read.csv(bzfile("./data/repdata-data-StormData.csv.bz2"))

#select relevant columns
data <- bigdata[,c("STATE","COUNTYNAME","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]

#columns to lower case
colnames(data) <- tolower(colnames(data))

#damage exp to upper case
data$propdmgexp <- toupper(data$propdmgexp)
data$cropdmgexp <- toupper(data$cropdmgexp)

#verify values
table(data$cropdmgexp)
table(data$propdmgexp)
summary(data$cropdmg)
summary(data$propdmg)


#define valid multipliers
validexp <- c("","H","K","M","B")
exponents$validexp <- validexp
exponents$propdmgmultiplier <- c(1,10^2,10^3,10^6,10^9)
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

#medltadata for cost damage plot
melted <- melt(data, id="evtype",measure.vars=c("propdmg","cropdmg"),value.name="costdamage")
costdamage <- dcast(melted,evtype~variable,fun.aggregate=sum)
costdamage <- costdamage[((costdamage$propdmg+costdamage$cropdmg) > 0),]
costdamage$propdmg <- ifelse(costdamage$propdmg == 0,1,costdamage$propdmg)
costdamage$cropdmg <- ifelse(costdamage$cropdmg == 0,1,costdamage$cropdmg)

#rank by total cost
costdamage$totalcost <- costdamage$propdmg+costdamage$cropdmg
costdamage <- arrange(costdamage,desc(totalcost))
qplot(data=costdamage,log(propdmg),log(cropdmg))
head(costdamage,10)

#rank by PCA
a <- princomp(costdamage[,2:3], scale=TRUE)
summary(a)
screeplot(a)
costdamage$score <- -a$scores[,1]
costdamage <- arrange(costdamage,desc(score))
head(costdamage,10)


#medltadata for cost health plot
melted <- melt(data, id="evtype",measure.vars=c("fatalities","injuries"),value.name="healthdamage")
healthdamage <- dcast(melted,evtype~variable,fun.aggregate=sum)
healthdamage <- healthdamage[((healthdamage$fatalities+healthdamage$injuries) > 0),]
healthdamage$fatalities <- ifelse(healthdamage$fatalities == 0,1,healthdamage$fatalities)
healthdamage$injuries <- ifelse(healthdamage$injuries == 0,1,healthdamage$injuries)
qplot(log(healthdamage$fatalities),log(healthdamage$injuries)) + geom_smooth(method="lm")


#rank by PCA
a <- princomp(healthdamage[,2:3], scale=TRUE)
summary(a)
screeplot(a)
healthdamage$score <- a$scores[,1]
healthdamage <- arrange(healthdamage,desc(score))
head(healthdamage,10)









