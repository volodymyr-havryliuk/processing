options(max.print=4000000)
library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)
library(hrbrthemes)


set.seed(2017)
options(digits=4)
Sys.setlocale("LC_TIME", "English")

setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\DataProcessing")
getwd()


features_1000 <- read.csv("usage_results.csv", header=T, nrows=1000)
features_1000$DateTime <- as.Date(features_1000$DateTime, format = "%m/%d/%Y %H:%M:%S")
features_1000$Author <- as.character(features_1000$Author)
features_1000$Repository <-as.character(features_1000$Repository)


selected_features_1000_for_conference <- features_1000 %>% select(Id,Author,DateTime,Repository,FileName,CS70_1,CS70_6,CS70_9,CS73_2,CS80_2,CS80_3,CS80_8)