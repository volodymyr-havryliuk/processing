options(max.print=4000000)
library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)

set.seed(2017)
options(digits=4)
Sys.setlocale("LC_TIME", "English")


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
features_groupped_by_month_after_2016 <- subset(features_groupped_by_month, month >= as.Date("2016-01-01"))

fwrite(features_groupped_by_month_after_2016, "features_groupped_by_month_after_2016.csv")
features_groupped_by_month_after_2016 <- read.csv("features_groupped_by_month_after_2016.csv", header=T)
features_groupped_by_month_after_2016$month <- as.Date(features_groupped_by_month_after_2016$month)

cs_70_rd_x <- as.Date(c("2017-03-01"))

dev.new()
CS70_1_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_1)) +
  ggtitle("CS70_1. Out variables") +
  geom_line(size = 1.2, color ="#00ace6") +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE, color="#116315", size = 0.5)+
  xlab("Time") +
  ylab("Occurrence, times") +
  geom_vline(xintercept=cs_70_rd_x , linetype="dashed", color = "blue")

CS70_1_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")

