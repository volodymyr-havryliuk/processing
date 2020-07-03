options(max.print=4000000)
library(dplyr)
library(data.table)
library(lubridate)
set.seed(2017)
options(digits=4)

cwd=getwd()
setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\DataProcessing")
getwd()

#1. Read and convert to needed formats all collected data
features <- read.csv("usage_results.csv", header=T)
features$DateTime <- as.Date(features$DateTime, format = "%m/%d/%Y %H:%M:%S")
features$Author <- as.character(features$Author)

#2. Select only features on which we are focusing and write them to file
selected_features <- features %>% select(Id,Author,DateTime,CS70_1,CS70_2,CS70_3,CS70_5,CS70_6,CS70_7,CS70_8,CS70_9,CS71_1,CS71_2,CS71_3,CS72_1,CS73_1,CS73_2,CS80_2,CS80_3,CS80_4,CS80_6,CS80_7,CS80_8,CS80_9)
#We can write selected features to file.
#fwrite(selected_features, "selected_features.csv")
#Read and transform data for features we are foucsed on
#selected_features <- read.csv("selected_2.csv", header=T)
#selected_features$DateTime <- as.Date(selected_features$DateTime, format = "%Y-%m-%d")
#selected_features$Author <- as.character(features$Author)

#3. Select 20 developers with largest number of commits
commits_groupped_by_id <- selected_features %>% group_by(Id, Author, DateTime) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
commits_sorted_by_developer_name <- commits_groupped_by_id %>% arrange(Author)

devs_with_max_commits<-sort(table(commits_sorted_by_developer_name$Author),decreasing=TRUE)[1:20]
devs_with_max_commits <- as.data.frame(devs_with_max_commits)

#4. Select data about feature occurrence for 10 most experienced developers. 10 most experienced developers have been identified in excel based on their contributions. Aggregate data by month.
most_experienced_devs = c("win",
                          "JustArchi",
                          "Swan",
                          "James Jackson-South",
                          "Martijn Laarman",
                          "Rico Suter",
                          "Mogens Heller Grabe",
                          "Kurt",
                          "Jaex",
                          "Chris Patterson"
)

selected_features_for_most_experienced_devs <- subset(selected_features, Author %in% most_experienced_devs)
selected_features_for_most_experienced_devs <- selected_features %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
#fwrite(selected_features_for_most_experienced_devs, "selected_features_for_most_experienced_devs.csv")
#features_most_exp_devs <- read.csv("selected_features_for_most_experienced_devs_by_month.csv", header=T)
#selected_features_for_most_experienced_devs$month <- as.Date(selected_features_for_most_experienced_devs$month)

#5. Select subset of data after 2016-01-01 and befor 2020-05-01 and calculate avarege occurrence of features for most experienced developers per month
selected_features_for_most_experienced_devs <- subset(selected_features_for_most_experienced_devs, month >= as.Date("2016-01-01"))
selected_features_for_most_experienced_devs <- subset(selected_features_for_most_experienced_devs, month < as.Date("2020-05-01"))

me_rowSums <- rowSums(selected_features_for_most_experienced_devs[-1])
me_number_of_months = length(me_rowSums)
me_devs_average = sum(me_rowSums)/me_number_of_months # 
me_devs_average # ~ 12286 per month     (10 most experienced developers used all 21 features 12286 times per month)







#6. Select 20 dvelopers with the least number of commits
devs_with_least_commits <- sort(table(commits_sorted_by_developer_name$Author),decreasing=TRUE)
devs_with_least_commits <- as.data.frame(devs_with_least_commits, responseName="NumberOfCommits")
#summary(devs_with_least_commits)
#hist(devs_with_least_commits$NumberOfCommits, breaks=100, ylim = c(0, 200), xlim = c(0,2000))
devs_with_least_commits <- subset(devs_with_least_commits, NumberOfCommits <= 500)
#one develeper was under different usernames, thats why here we take 21
devs_with_least_commits <- head(devs_with_least_commits, 21)

#7. We identified less eperienced devs in Excel. Now select data about feature occurrence for 10 less experienced developers and aggregate data by year
less_experienced_devs = c("Shawn Kendrot",
                          "Svyatoslav Danyliv",
                          "Pandu",
                          "Dr.Rx",
                          "Okeanij",
                          "Maurycy Markowski",
                          "Martin Zikmund",
                          "Netshroud",
                          "David Piepgrass",
                          "Matt Klingensmith"
)

selected_features_for_less_experienced_devs <- subset(selected_features, Author %in% less_experienced_devs)
selected_features_for_less_experienced_devs <- selected_features_for_less_experienced_devs %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
#fwrite(selected_features_for_less_experienced_devs, "selected_features_for_less_experienced_devs.csv")
#selected_features_for_less_experienced_devs <- read.csv("selected_features_for_less_experienced_devs.csv", header=T)
#selected_features_for_less_experienced_devs$month <- as.Date(selected_features_for_less_experienced_devs$month)

#8. Select subset of data after 2016 and calculate avarege occurrence of features for less experienced developers by month
selected_features_for_less_experienced_devs <- subset(selected_features_for_less_experienced_devs, month >= as.Date("2016-01-01"))
selected_features_for_less_experienced_devs <- subset(selected_features_for_less_experienced_devs, month < as.Date("2020-05-01"))

le_rowSums <- rowSums(selected_features_for_less_experienced_devs[-1])
le_number_of_months = length(le_rowSums)
le_devs_average = sum(le_rowSums)/le_number_of_months # ~ 350 per month    (10 less experienced developers used all 21 features 350 times per month)
le_devs_average

