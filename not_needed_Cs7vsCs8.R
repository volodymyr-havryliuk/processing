library(tidyverse)
library(data.table)

cwd=getwd()
setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\DataProcessing")
getwd()
Sys.setlocale(locale="es_ES.UTF-8")


features_groupped_by_month_after_2016 <- read.csv("features_groupped_by_month_after_2016.csv", header=T)


totals_cs_7_and_8 <- features_groupped_by_month_after_2016 %>%  mutate(Total_CS7 = select(., CS70_1:CS73_2) %>% rowSums(na.rm = TRUE))
totals_cs_7_and_8 <- totals_cs_7_and_8  %>%  mutate(Total_CS8 = select(., CS80_2:CS80_9) %>% rowSums(na.rm = TRUE))
totals_cs_7_and_8 <- totals_cs_7_and_8 %>% select(month, Total_CS7, Total_CS8)
totals_cs_7_and_8$month <- as.Date(totals_cs_7_and_8$month)



total_cs7 <- totals_cs_7_and_8 %>% select(month, Total_CS7)
total_cs7 <- subset(total_cs7, month >= as.Date("2017-03-01") & month < as.Date("2017-12-01"))
fwrite(total_cs7, "total_cs7_for_first_months.csv")


total_cs8 <- totals_cs_7_and_8 %>% select(month, Total_CS8)
total_cs8 <- subset(total_cs8, month >= as.Date("2019-09-01"))
fwrite(total_cs8, "total_cs8_for_first_months.csv")



attach(total_cs7)

feature = Total_CS7
pdf(width=12, "CS7_9_months.pdf")
#dev.new()
opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Features Occurrence")

#opar <- par(no.readonly=TRUE)
#par(pch=19, xpd = F)
#abline(v=cs_70_rd_x, col="blue")
#par(opar)

#opar <- par(no.readonly=TRUE)
#par(pch=19, xpd = F)
#abline(lm(feature ~ month),col="#00CE08")
#par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences"), col=c("black"), lwd = 1, lty = 1)
title("CS 7 features adoption during 9 month after release")
par(opar_start)
dev.off()

detach(total_cs7)



attach(total_cs8)

feature = Total_CS8
pdf(width=12, "CS8_9_months.pdf")
#dev.new()
opar_start <- par(no.readonly=TRUE)
par(xpd = T, mar = par()$mar + c(0,0,0,10))
plot(month, feature, type = "l",xlab="Date", ylab="Features Occurrence")

#opar <- par(no.readonly=TRUE)
#par(pch=19, xpd = F)
#abline(v=cs_70_rd_x, col="blue")
#par(opar)

#opar <- par(no.readonly=TRUE)
#par(pch=19, xpd = F)
#abline(lm(feature ~ month),col="#00CE08")
#par(opar)

legend("topright",inset=c(-.27, 0.0), title="Legend", c("Occurrences"), col=c("black"), lwd = 1, lty = 1)
title("CS 8 features adoption during 9 month after release")
par(opar_start)
dev.off()

detach(total_cs8)


