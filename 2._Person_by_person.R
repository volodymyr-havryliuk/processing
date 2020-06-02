options(max.print=4000000)
library(dplyr)
library(data.table)
library(lubridate)
set.seed(2017)
options(digits=4)



cwd <- getwd()
setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\DataProcessing")



selected_2 <- features %>% select(Id,Author,DateTime,CS70_1,CS70_2,CS70_3,CS70_5,CS70_6,CS70_7,CS70_8,CS70_9,CS71_1,CS71_2,CS71_3,CS72_1,CS73_1,CS73_2,CS80_2,CS80_3,CS80_4,CS80_6,CS80_7,CS80_8,CS80_9)
fwrite(selected_2, "selected_2.csv")


features <- read.csv("selected_2.csv", header=T)
features$Author <- as.character(features$Author)
features$DateTime <- as.Date(features$DateTime, format = "%Y-%m-%d")


features <- features %>% group_by(Id, Author, DateTime) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
features <- features <- selected_features %>% group_by(month=floor_date(DateTime, "month"),Author) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
features <- subset(features, DateTime > as.Date("2016-01-01"))


res = aov(CS70_1 ~ CS70_2, features)
summary(res)

features_win <- subset(features, Author == "win")
features_win <- features_win  %>% arrange(month)

attach(features_win)

feature = CS70_2
dev.new()
opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence")

#opar <- par(no.readonly=TRUE)
#par(pch=19, xpd = F)
#abline(v=cs_70_rd_x, col="blue")
#par(opar)

#opar <- par(no.readonly=TRUE)
#par(pch=19, xpd = F)
#abline(lm(feature ~ month),col="#00CE08")
#par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS70_2. Pattern matching. Grouped by month.")
par(opar_start)
#dev.off()

detach(features_win)



features_JustArchi <- subset(features, Author == "JustArchi")
attach(features_JustArchi)

feature = CS70_1
dev.new()
opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence")

#opar <- par(no.readonly=TRUE)
#par(pch=19, xpd = F)
#abline(v=cs_70_rd_x, col="blue")
#par(opar)

#opar <- par(no.readonly=TRUE)
#par(pch=19, xpd = F)
#abline(lm(feature ~ month),col="#00CE08")
#par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS70_2. Pattern matching. Grouped by month.")
par(opar_start)
#dev.off()

detach(features_JustArchi)
