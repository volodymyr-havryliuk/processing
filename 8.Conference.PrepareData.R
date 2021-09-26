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


features <- read.csv("usage_results.csv", header=T)
features$DateTime <- as.Date(features$DateTime, format = "%m/%d/%Y %H:%M:%S")
features$Author <- as.character(features$Author)
features$Repository <-as.character(features$Repository)

selected_features_for_conference <- features %>% select(Id,Author,DateTime,Repository,FileName,CS70_1,CS70_6,CS70_9,CS73_2,CS80_2,CS80_3,CS80_8)
#selected_features_for_conference$Repository <- gsub('\\s+', '', selected_features_for_conference$Repository)

fwrite(selected_features_for_conference, "1_selected_features_for_conference.csv")

#Manually removed unneeded spaces in file and load updated data
selected_features_for_conference <- read.csv("1_selected_features_for_conference.csv", header=T)
selected_features_for_conference$DateTime <- as.Date(selected_features_for_conference$DateTime, format = "%Y-%m-%d")
selected_features_for_conference$Author <- as.character(selected_features_for_conference$Author)
selected_features_for_conference$Repository <-as.character(selected_features_for_conference$Repository)


classic_features <- read.csv("usage_results_classic.csv", header=T)
classic_features$DateTime <- as.Date(classic_features$DateTime, format = "%m/%d/%Y %H:%M:%S")
classic_features$Author <- as.character(classic_features$Author)
classic_features$Repository <-as.character(classic_features$Repository)

fwrite(classic_features, "2_classic_features.csv")

merged_data <- merge(selected_features_for_conference,classic_features,by=c('Id','Author', 'DateTime', 'Repository', 'FileName'),all=F)

fwrite(merged_data, "3_merged_data.csv")
