#features <- read.table("../../usage_results.csv", header=TRUE, sep=",", fill = TRUE)
options(max.print=4000000)
library(dplyr)
library(data.table)
library(lubridate)
set.seed(2017)
options(digits=4)

#features <- read.csv("../../usage_results.csv", header=T)
#features <- read.table("../../usage_results.csv", header=TRUE, sep=",", nrows = 1000)
#features$DateTime <- as.Date(features$DateTime, format = "%m/%d/%Y %H:%M:%S")
#features$Repository <- as.character(features$Repository)
#features$FileName <- as.character(features$FileName)
#str(features$DateTime)
#str(features)

#selected <- features %>% select(Id,Athor,DateTime,CS70_1,CS70_2,CS70_3,CS70_5,CS70_6,CS70_7,CS70_8,CS70_9,CS71_1,CS71_2,CS71_3,CS72_1,CS73_1,CS73_2,CS80_2,CS80_3,CS80_4,CS80_6,CS80_7,CS80_8,CS80_9)
#str(selected)
#write.csv(selected,"..\\..\\selected.csv", row.names = TRUE)
#fwrite(selected, "..\\..\\selected.csv")

#features %>% 
  #filter(DateTime <= as.Date("2019-03-28")) %>% 
  #  group_by(Item) %.%                       #you can add this line if you need to group by Item (it will appear in the output then)
  #summarize(DateTime = as.Date("2019-03-28"), QtyCS70_5 = sum(CS70_5))

#sorted <- features %>% arrange(desc(CS70_5))
setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\DataProcessing")
getwd()

features <- read.csv("selected_2.csv", header=T)

#features <- read.table("../../selected.csv", header=TRUE, sep=",", nrows = 200)
features$Author <- as.character(features$Author)
features$DateTime <- as.Date(features$DateTime, format = "%Y-%m-%d")
#str(features)
features_grouped_by_date <- features %>% group_by(DateTime) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
#fwrite(features_grouped_by_date, "..\\..\\features_grouped_by_date.csv")
#str(features$DateTime)

features_groupped_by_month <- features %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))



features_groupped_by_month$CS70_2



attach(features_grouped_by_date)

#attach(features_groupped_by_month)



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

lim <- as.Date(c("2015-01-01", "2021-01-01")) 

plot(DateTime, CS70_2, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
#lines(cs_70_rd_x, cs_70_rd_y, col="red", type = "p")
abline(v=cs_70_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS70_2. Default literal expressions.")
dev.off()




pdf("CS70_1.pdf")
#dev.new()
plot(DateTime, CS70_1, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_70_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS70_1. out variables grouped by date.")
dev.off()


pdf("CS70_2.pdf")
dev.new()
plot(DateTime, CS70_2, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_70_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS70_2. Pattern matching.")
dev.off()


pdf("CS70_3.pdf")
plot(DateTime, CS70_3, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_70_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS70_3. Ref locals and returns.")
dev.off()


pdf("CS70_5.pdf")
plot(DateTime, CS70_5, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_70_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS70_5. More expression-bodied members.")
dev.off()


pdf("CS70_6.pdf")
plot(DateTime, CS70_6, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_70_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS70_6. Throw expressions.")
dev.off()


pdf("CS70_7.pdf")
plot(DateTime, CS70_7, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_70_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS70_7. Generalized async return types.")
dev.off()


pdf("CS70_8.pdf")
plot(DateTime, CS70_8, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_70_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS70_8. Numeric literal syntax improvements.")
dev.off()


pdf("CS70_9.pdf")
plot(DateTime, CS70_9, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_70_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS70_9. Discards.")
dev.off()




pdf("CS71_1.pdf")
plot(DateTime, CS71_1, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_71_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.1 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS71_1. Async main method.")
dev.off()


pdf("CS71_2.pdf")
plot(DateTime, CS71_2, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_71_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.1 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS71_2. Default literal expressions.")
dev.off()


pdf("CS71_3.pdf")
plot(DateTime, CS71_3, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_71_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.1 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS71_3. Inferred tuple element names.")
dev.off()




pdf("CS72_1.pdf")
plot(DateTime, CS72_1, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_72_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.2 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS72_1. Leading underscores in numeric literals.")
dev.off()





pdf("CS73_1.pdf")
plot(DateTime, CS73_1, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_73_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.3 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS73_1. Attach attributes to the backing fields for auto-implemented properties.")
dev.off()


pdf("CS73_2.pdf")
plot(DateTime, CS73_2, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_73_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.3 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS73_2. Enhanced generic constraints.")
dev.off()






pdf("CS80_2.pdf")
plot(DateTime, CS80_2, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_80_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 8.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS80_2. Switch expressions.")
dev.off()


pdf("CS80_3.pdf")
plot(DateTime, CS80_3, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_80_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 8.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS80_3. Using declarations.")
dev.off()


pdf("CS80_4.pdf")
plot(DateTime, CS80_4, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_80_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 8.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS80_4. Property patterns.")
dev.off()


pdf("CS80_6.pdf")
plot(DateTime, CS80_6, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_80_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 8.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS80_6. Index from end operator.")
dev.off()


pdf("CS80_7.pdf")
plot(DateTime, CS80_7, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_80_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 8.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS80_7. Ranges.")
dev.off()


pdf("CS80_8.pdf")
plot(DateTime, CS80_8, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_80_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 8.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS80_8. Null-coalescing assignment.")
dev.off()


pdf("CS80_9.pdf")
plot(DateTime, CS80_9, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_80_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 8.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS80_9. Struct Readonly Members.")
dev.off()


detach(features_grouped_by_date)




attach(features_groupped_by_month)


#pdf("CS70_1.pdf")
dev.new()
plot(DateTime, CS70_1, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_70_rd_x, col="blue")
par(opar)
legend("topleft", inset=.05, title="Legend", c("Occurrences","CS 7.0 Release Date"), lty=c(1, 1), col=c("black", "blue"))
title("CS70_1. out variables grouped by date.")
dev.off()


pdf("CS70_2.pdf")
#dev.new()
plot(month, CS70_2, type = "l",xlab="", ylab="Feature Occurrence",xlim=lim,frame.plot=TRUE, xaxt="n")
axis(1, at=month, labels=month, las=2, pos = 0)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_70_rd_x, col="blue")
par(opar)
legend("topleft", inset=c(-.07, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date"), lty=c(1, 1), col=c("black", "blue"), cex = 0.75)
title("CS70_2. Pattern matching.")
title(xlab="Month", line=5, cex.lab=1.2)
dev.off()


pdf("CS70_3.pdf")
dev.new()
plot(month, CS70_3, type = "l",xlab="Date", ylab="Feature Occurrence", xlim=lim, frame.plot=TRUE, xaxt="n")
axis(1, at=month, labels=month, las=2, pos = 0)
opar <- par(no.readonly=TRUE)
par(pin=c(6,8))
par(pch=19)
abline(v=cs_70_rd_x, col="blue")
par(opar)
legend("topleft", inset=c(-.07, 0.0), title="Legend", c("Occurrences","CS 7.0 Release Date"), lty=c(1, 1), col=c("black", "blue"), cex = 0.75)
title("CS70_3. Ref locals and returns.")
title(xlab="Month", line=5, cex.lab=1.2)
dev.off()



detach(features_groupped_by_month)

length(month)
length(CS70_2)


#dev.off()





#save.image("myfile") Save the workspace to myfile (default = .RData).
#save(features_grouped_by_date,
#     file="features_grouped_by_date")
#load("features_grouped_by_date")
