---
title: "assignment 1 course 5"
author: "abhinav palaparthy"
date: "28 September 2017"
output: html_document
---

##Assignment Instructions
1.Code for reading in the dataset and/or processing the data
2.Histogram of the total number of steps taken each day
3.Mean and median number of steps taken each day
4.Time series plot of the average number of steps taken
5.The 5-minute interval that, on average, contains the maximum number of steps
6.Code to describe and show a strategy for imputing missing data
7.Histogram of the total number of steps taken each day after missing values are imputed
8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report


##Step 1
##Code for reading in the dataset and/or processing the data
```{r, echo = TRUE}
table<-read.csv("activity.csv")
```

Exploring the basics of this data
```{r}
dim(table)
names(table)
head(table)
str(table)
#total number of missing data
sum(is.na(table$steps))/dim(table)[[1]]
#transforming the date column into date format using lubridate
library(lubridate)
table$date<-ymd(table$date)
length(unique(table$date))
```


##Step 2
##Histogram of the total number of steps taken each day
```{r, echo = TRUE}
library(ggplot2)
p1<-data.frame(tapply(table$steps,table$date,sum,na.rm=TRUE))
p1$date<-rownames(p1)
rownames(p1)<-NULL
names(p1)[[1]]<-"Total Steps"
png("plot1.png")
#Total Steps by date bar chart
ggplot(p1,aes(y=p1$`Total Steps`,x=p1$date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")
dev.off()
ggplot(p1,aes(y=p1$`Total Steps`,x=p1$date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")
#Histogram of total steps
qplot(p1$`Total Steps`,geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Historgram")
png("plot1.1.png")
qplot(p1$`Total Steps`,geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Historgram")
dev.off()
```


##Step 3
##Mean and median number of steps taken each day

```{r, echo = TRUE}
library(dplyr)
mnm<-data.frame(round(tapply(table$steps,table$date,mean,na.rm=TRUE),2))
mnm$date<-rownames(mnm)
rownames(mnm)<-NULL
names(mnm)[[1]]<-"Mean Steps"
temp<-table%>%select(date,steps) %>% group_by(date) %>% summarise(median(steps))
names(temp)[[2]]<-"Median Steps"
mnm$median<-temp$`Median Steps`
mnm<-mnm %>% select(date,`Mean Steps`,median)
```

##Step 4
##Time series plot of the average number of steps taken
```{r, echo = TRUE}
plot2<-mnm
plot2$date<-as.Date(plot2$date,format="%Y-%m-%d")
ggplot(plot2,aes(x=plot2$date,y=plot2$`Mean Steps`))+geom_bar(stat="identity")+scale_x_date()+ylab("Mean Steps Every day")+xlab("Date")+ggtitle("Mean Steps by Date")
png("plot2.png")
ggplot(plot2,aes(x=plot2$date,y=plot2$`Mean Steps`))+geom_bar(stat="identity")+scale_x_date()+ylab("Mean Steps Every day")+xlab("Date")+ggtitle("Mean Steps according to Date")
dev.off()
```


##Step 5
##The 5-minute interval that, on average, contains the maximum number of steps

```{r, echo = TRUE}
#This is assuming that the words on average means averaging steps by date and interval
table$interval<-factor(table$interval)
am<-aggregate(data=table,steps~date+interval,FUN="mean")
am<-aggregate(data=am,steps~interval,FUN="max")
```


##Step 6
Code to describe and show a strategy for imputing missing data

```{r, echo = TRUE}
impute<-table
impute$Missing<-is.na(impute$steps)
impute<-aggregate(data=impute,Missing~date+interval,FUN="sum")
impute.1<-data.frame(tapply(impute$Missing,impute$date,sum))
impute.1$date<-rownames(impute.1)
rownames(impute.1)<-NULL
names(impute.1)<-c("Missing","date")
impute.1$date<-as.Date(impute.1$date,format="%Y-%m-%d")

impute.2<-data.frame(tapply(impute$Missing,impute$interval,sum))
impute.2$date<-rownames(impute.2)
rownames(impute.2)<-NULL
names(impute.2)<-c("Missing","Interval")

par(mfrow=c(1,2))
plot(y=impute.1$Missing,x=impute.1$date,main="Missing Value Distribution by Date")
plot(y=impute.2$Missing,x=impute.2$Interval,main="Missing Value Distribution by Interval")
table(table$date)
```

By this point, from the plot, that the missing values have a very disctinct pattern. For every interval, there are consistantly 8 missing values. For the date, there are consistantly 288 missing values. And in total, there are 8 dates that have missing value. We don't exactly know the cause for these missing values but there's a pattern. For that matter, we can see that the mean value imputation is appropriate.

We can see that every date has 288 data points. It means that the 8 dates have no data points at all what so ever. We can refine the analysis by looking at these missing values depending on their Weekday and interval parameters to matach with the average 

```{r, echo = TRUE}
#Dates that have missing values 
library(lubridate)
impute.3<-as.data.frame(impute.1) %>% select(date,Missing) %>% arrange(desc(Missing))
impute.3<-impute.3[which(impute.3$Missing!=0),]
impute.3$Weekday<-wday(impute.3$date,label=TRUE)
impute.4<-table
impute.4$weekday<-wday(impute.4$date,label=TRUE)
#Finding the mean of steps every monday, and every interval
impute.5<-aggregate(data=impute.4,steps~interval+weekday,FUN="mean",na.rm=TRUE)
#Merge the pre-imputation table impute.4 table with the average table impute.5
impute.6<-merge(x=impute.4,y=impute.5,by.x=c("interval","weekday"),by.y=c("interval","weekday"),all.x=TRUE)
#Conditionally replacing the steps.x column NA value with the values from steps.y column value 
impute.6$Steps.Updated<-0
for (i in 1:dim(impute.6)[[1]]){
if(is.na(impute.6[i,3])){impute.6[i,6]=impute.6[i,5]}
else {impute.6[i,6]=impute.6[i,3]}
}
#Now simplify the imputed analytical data frame
impute.6 <-impute.6  %>% select(date,weekday,interval,Steps.Updated)
names(impute.6)[[4]]<-"Steps"

```


## Step 7
Histogram of the total number of steps taken each day after missing values are imputed

```{r, echo = TRUE}
png("plot7.png")
qplot(impute.6$Steps,geom="histogram",main="Total steps taken histogram post imputation",xlab="Steps",ylab="Count")
dev.off()
qplot(impute.6$Steps,geom="histogram",main="Total steps taken histogram post imputation",xlab="Steps",ylab="Count")

```




## Step 8
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r, echo = TRUE}
Q8<-impute.6
levels(Q8$weekday)<-c(1,2,3,4,5,6,7)
Q8$WDWE<-Q8$weekday %in% c(1,2,3,4,5)
Q8.1<-aggregate(data=Q8,Steps~interval+WDWE,mean,na.rm=TRUE)
Q8.1$WDWE<-as.factor(Q8.1$WDWE)
levels(Q8.1$WDWE)<-c("Weekend","Weekday")
png("plot8.png")
ggplot(data=Q8.1,aes(y=Steps,x=interval,group=1,color=WDWE))+geom_line() +scale_x_discrete(breaks = seq(0, 2500, by = 300))+ylab("Mean Steps")+xlab("Intervals")+ggtitle("Mean steps across intervals by Weekend and Weekday")
dev.off()
ggplot(data=Q8.1,aes(y=Steps,x=interval,group=1,color=WDWE))+geom_line() +scale_x_discrete(breaks = seq(0, 2500, by = 300))+ylab("Mean Steps")+xlab("Intervals")+ggtitle("Mean steps across intervals by Weekend and Weekday")

#Producing the panel plot
Q8.1$interval<-as.numeric(as.character(Q8.1$interval))
library(lattice)
xyplot(data=Q8.1,Steps~interval|WDWE, grid = TRUE, type = c("p", "smooth"), lwd = 4,panel = panel.smoothScatter)
library(hexbin)
hexbinplot(data=Q8.1,Steps~interval|WDWE, aspect = 1, bins=50)
png("plott8.1.png")
xyplot(data=Q8.1,Steps~interval|WDWE, grid = TRUE, type = c("p", "smooth"), lwd = 4,panel = panel.smoothScatter)
dev.off()

png("plot8.2.png")
hexbinplot(data=Q8.1,Steps~interval|WDWE, aspect = 1, bins=50)
dev.off()
```
