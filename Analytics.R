# It is a analytics docs. When the analytic is stable, code and result would be migratied into Rmd, 
# because of Rmd performance.

unzip("activity.zip")
data.total <- read.csv2("activity.csv", sep=",")

# Remove unexisting data
data.exist <- data.total[!is.na(data.total$steps),]

# Section 01
############
# Median and Mean Data of total daily steps
library(dplyr)
daily.summary.frame <- data.exist%>%
  group_by(date)%>% 
  summarise(Mean=mean(steps), Median=as.numeric(median(steps)), Max=max(steps), Min = min(steps), Sum=sum(steps))

# Total number of steps taken per day
hist(x=daily.summary.frame$Sum)

# Mean and Median
daily.summary <- summary(daily.summary.frame$Sum)

# Section 02
#############
# Median and Mean Data of total daily steps
daily.interval.mean <- aggregate(data.exist$steps, list(as.factor(data.exist$interval)), mean)

# Max Steps
daily.interval.max.fram <- daily.interval.mean[order(-daily.interval.mean$x),]
daily.interval.max.step <- daily.interval.max.fram$x[1]

plot(x=daily.interval.mean$Group.1, y=daily.interval.mean$x, type="l")

# Section 03
############

data.total.na.sum <- sum(is.na(data.total$steps))

daily.step.mean <- mean(daily.interval.mean$x)
data.total.clean <- data.total
data.total.clean$steps[is.na(data.total.clean$steps)]<-daily.step.mean

daily.clean.summary.frame <- data.total.clean%>%
  group_by(date)%>% 
  summarise(Mean=mean(steps), Median=as.numeric(median(steps)), Max=max(steps), Min = min(steps), Sum=sum(steps))

# Total number of steps taken per day
hist(x=daily.clean.summary.frame$Sum)

# Mean and Median
daily.clean.summary <- summary(daily.clean.summary.frame$Sum)

# Section 04

weekday.frame <- data.frame(Weekday=weekdays(as.Date(data.total$date)))
weekday.weekend <- as.factor(ifelse((weekday.frame$Weekday %in% c("Saturday","Sunday")), "Weekend", "Weekday"))
data.total.clean <- cbind(data.total.clean, Day = weekday.frame)
data.total.clean <- cbind(data.total.clean, Weekend = weekday.weekend)

data.total.clean.weekday <- data.total.clean[which(data.total.clean$Weekend == "Weekday"),]
daily.interval.clean.weekday <- aggregate(data.total.clean.weekday$steps, list(as.factor(data.total.clean.weekday$interval)), mean)
daily.interval.clean.weekday <- cbind(daily.interval.clean.weekday, Weekend = "Weekday")

data.total.clean.weekend <- data.total.clean[which(data.total.clean$Weekend != "Weekday"),]
daily.interval.clean.weekend <- aggregate(data.total.clean.weekend$steps, list(as.factor(data.total.clean.weekend$interval)), mean)
daily.interval.clean.weekend <- cbind(daily.interval.clean.weekend, Weekend = "Weekend")

daily.interval.weekend <- rbind(daily.interval.clean.weekend, daily.interval.clean.weekday)
names(daily.interval.weekend) <- c("interval", "steps", "Weekend")

library(lattice)
xyplot(steps ~ interval | Weekend, data=daily.interval.weekend, type="l", layout=(c(1,3)))
