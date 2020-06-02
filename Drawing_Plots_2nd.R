options(max.print=4000000)
library(dplyr)
library(data.table)
library(lubridate)
set.seed(2017)
options(digits=4)

features <- read.csv("usage_results.csv", header=T)
features$DateTime <- as.Date(features$DateTime, format = "%m/%d/%Y %H:%M:%S")
features$Repository <- as.character(features$Repository)
features$FileName <- as.character(features$FileName)

selected_2 <- features %>% select(Id,Author,DateTime,CS70_1,CS70_2,CS70_3,CS70_5,CS70_6,CS70_7,CS70_8,CS70_9,CS71_1,CS71_2,CS71_3,CS72_1,CS73_1,CS73_2,CS80_2,CS80_3,CS80_4,CS80_6,CS80_7,CS80_8,CS80_9)
fwrite(selected_2, "selected_2.csv")

setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\DataProcessing")
getwd()

selected_features <- read.csv("selected_2.csv", header=T)
selected_features$Author <- as.character(selected_features$Author)
selected_features$DateTime <- as.Date(selected_features$DateTime, format = "%Y-%m-%d")

features_groupped_by_month <- selected_features %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
features_groupped_by_month_after_2016 <- subset(features_groupped_by_month, month > as.Date("2016-01-01"))

fwrite(features_groupped_by_month_after_2016, "features_groupped_by_month_after_2016.csv")

attach(features_groupped_by_month_after_2016)

cs_70_rd_x <- c("2017-03-01")
cs_70_rd_x <- as.Date(cs_70_rd_x)
cs_70_rd_y <- c(0)

cs_71_rd_x <- c("2017-08-01")
cs_71_rd_x <- as.Date(cs_71_rd_x)
cs_71_rd_y <- c(0)

cs_72_rd_x <- c("2017-11-01")
cs_72_rd_x <- as.Date(cs_72_rd_x)
cs_72_rd_y <- c(0)

cs_73_rd_x <- c("2018-05-01")
cs_73_rd_x <- as.Date(cs_73_rd_x)
cs_73_rd_y <- c(0)

cs_80_rd_x <- c("2019-09-01")
cs_80_rd_x <- as.Date(cs_80_rd_x)
cs_80_rd_y <- c(0)

lim <- as.Date(c("2016-01-01", "2020-06-01"))

setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\DataProcessing")
getwd()



pdf(width=12, "CS70_1_month.pdf")
#dev.new()
opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, CS70_1, type = "l",xlab="Date", ylab="Number of occurrences, times", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_70_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(CS70_1 ~ month),col="#00CE08")
par(opar)

#par(xpd=TRUE)
legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS70_1. out variables grouped by date.")
par(opar_start)
dev.off()



feature = CS70_2
pdf(width=12, "CS70_2_month.pdf")
#dev.new()
opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Number of occurrences, times", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_70_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS70_2. Pattern matching. Grouped by month.")
par(opar_start)
dev.off()



feature = CS70_3
pdf(width=12, "CS70_3_month.pdf")
#dev.new()
opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Number of occurrences, times", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_70_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS70_3. Ref locals and returns. Grouped by month.")
par(opar_start)
dev.off()



feature = CS70_5
pdf(width=12, "CS70_5_month.pdf")
opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_70_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS70_5. More expression-bodied members. Grouped by month.")
par(opar_start)
dev.off()



feature = CS70_6
pdf(width=12, "CS70_6_month.pdf")
opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_70_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS70_6. Throw expressions. Grouped by month.")
par(opar_start)
dev.off()



feature = CS70_7
pdf(width=12, "CS70_7_month.pdf")
opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_70_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS70_7. Generalized async return types. Grouped by month.")
par(opar_start)
dev.off()



feature = CS70_8
pdf(width=12, "CS70_8_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_70_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS70_8. Numeric literal syntax improvements. Grouped by month.")
par(opar_start)
dev.off()



feature = CS70_9
pdf(width=12, "CS70_9_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_70_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS70_9. Discards. Grouped by month.")
par(opar_start)
dev.off()



feature = CS71_1
pdf(width=12, "CS71_1_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_71_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS71_1. Async main method. Grouped by month.")
par(opar_start)
dev.off()



feature = CS71_2
pdf(width=12, "CS71_2_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_71_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS71_2. Default literal expressions. Grouped by month.")
par(opar_start)
dev.off()



feature = CS71_3
pdf(width=12, "CS71_3_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_71_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS71_3. Inferred tuple element names. Grouped by month.")
par(opar_start)
dev.off()



feature = CS72_1
pdf(width=12, "CS72_1_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_72_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS72_1. Leading underscores in numeric literals. Grouped by month.")
par(opar_start)
dev.off()



feature = CS73_1
pdf(width=12, "CS73_1_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_73_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS73_1. Attaching attributes to the backing fields for auto-implemented properties. Grouped by month.")
par(opar_start)
dev.off()



feature = CS73_2
pdf(width=12, "CS73_2_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_73_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS73_2. Enhanced generic constraints. Grouped by month.")
par(opar_start)
dev.off()



feature = CS80_2
pdf(width=12, "CS80_2_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_80_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS80_2. Switch expressions. Grouped by month.")
par(opar_start)
dev.off()



feature = CS80_3
pdf(width=12, "CS80_3_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_80_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS80_3. Using declarations. Grouped by month.")
par(opar_start)
dev.off()



feature = CS80_4
pdf(width=12, "CS80_4_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_80_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS80_4. Property patterns. Grouped by month.")
par(opar_start)
dev.off()



feature = CS80_6
pdf(width=12, "CS80_6_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_80_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS80_6. Index from end operator. Grouped by month.")
par(opar_start)
dev.off()



feature = CS80_7
pdf(width=12, "CS80_7_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_80_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS80_7. Ranges. Grouped by month.")
par(opar_start)
dev.off()



feature = CS80_8
pdf(width=12, "CS80_8_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_80_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS80_8. Null-coalescing assignment. Grouped by month.")
par(opar_start)
dev.off()



feature = CS80_9
pdf(width=12, "CS80_9_month.pdf")

opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(v=cs_80_rd_x, col="blue")
par(opar)

opar <- par(no.readonly=TRUE)
par(pch=19, xpd = F)
abline(lm(feature ~ month),col="#00CE08")
par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date", "Regression Line"), col=c("black", "blue", "#00CE08"), lwd = 1, lty = 1)
title("CS80_9. Struct Readonly Members. Grouped by month.")
par(opar_start)
dev.off()



