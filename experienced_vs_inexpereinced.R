options(max.print=4000000)
library(dplyr)
library(data.table)
library(lubridate)
set.seed(2017)
options(digits=4)

#1. Read and convert to needed formats all collected data
features <- read.csv("usage_results.csv", header=T)
features$DateTime <- as.Date(features$DateTime, format = "%m/%d/%Y %H:%M:%S")
features$Repository <- as.character(features$Repository)
features$FileName <- as.character(features$FileName)


#2. Select only features on which we are focusing and write them to file
selected_2 <- features %>% select(Id,Author,DateTime,CS70_1,CS70_2,CS70_3,CS70_5,CS70_6,CS70_7,CS70_8,CS70_9,CS71_1,CS71_2,CS71_3,CS72_1,CS73_1,CS73_2,CS80_2,CS80_3,CS80_4,CS80_6,CS80_7,CS80_8,CS80_9)
#fwrite(selected_2, "selected_2.csv")

cwd=getwd()
setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\DataProcessing")
getwd()

#3. Read and transform data for features we are foucsed on
#selected_features <- read.csv("selected_2.csv", header=T)
selected_features <- selected_2
selected_features$Author <- as.character(selected_features$Author)
selected_features$DateTime <- as.Date(selected_features$DateTime, format = "%Y-%m-%d")


#Group focused features usage by month, take date only after 2016 and write them to file
features_groupped_by_month <- selected_features %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
features_groupped_by_month_after_2016 <- subset(features_groupped_by_month, month > as.Date("2016-01-01"))
fwrite(features_groupped_by_month_after_2016, "features_groupped_by_month_after_2016.csv")


#4. Select 20 dvelopers with largest number of commits
commits_groupped_by_id <- selected_features %>% group_by(Id, Author, DateTime) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
commits_sorted_by_developer_name <- commits_groupped_by_id %>% arrange(Author)
#sort(table(commits_sorted_by_developer_name$Author),decreasing=TRUE)[1:20]
devs_with_max_commits<-sort(table(commits_sorted_by_developer_name$Author),decreasing=TRUE)[1:20]
devs_with_max_commits <- as.data.frame(devs_with_max_commits)

#Select data about features occurrence only for those 20 developers. This step can be skipped, as later we select data separately for 10 most experienced developers
selected_features_for_experienced_devs <- subset(selected_features, Author %in% devs_with_max_commits$Var1)
selected_features_for_experienced_devs_by_month <- selected_features_for_experienced_devs %>% group_by(month=floor_date(DateTime, "month"), Author) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
selected_features_for_experienced_devs_by_month_sorted = selected_features_for_experienced_devs_by_month %>% arrange(Author)

#min(subset(selected_features_for_experienced_devs_by_month_sorted, Author == "Adam")$month)

#5. Select data about feature occurrence for 10 most experienced developers and aggregate data by year
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
selected_features_for_most_experienced_devs_by_month <- selected_features_for_most_experienced_devs %>% group_by(month=floor_date(DateTime, "month"), Author) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
#fwrite(selected_features_for_most_experienced_devs_by_month, "selected_features_for_most_experienced_devs_by_month.csv")

#features_most_exp_devs <- read.csv("selected_features_for_most_experienced_devs_by_month.csv", header=T)
features_most_exp_devs <- selected_features_for_most_experienced_devs_by_month
features_most_exp_devs$month <- as.Date(features_most_exp_devs$month)
features_groupped_by_month <- selected_features %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
features_most_exp_devs_groupped_by_year <- features_most_exp_devs %>% group_by(year=floor_date(month, "year")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))


save(features_most_exp_devs_groupped_by_year,
     file="features_most_exp_devs_groupped_by_year")
#load("features_grouped_by_date")

#6. Select subset of data after 2016 and calculate avarege occurrence of features for most experienced developers by year
features_most_exp_devs_groupped_by_year <- subset(features_most_exp_devs_groupped_by_year, year >= as.Date("2016-01-01"))
rowSums <- rowSums(features_most_exp_devs_groupped_by_year[-1])
exp_devs_average = sum(rowSums)/length(rowSums)








#7. Select only features on which we are focusing and write them to file
selected_2_with_repository <- features %>% select(Id,Author,DateTime,Repository,CS70_1,CS70_2,CS70_3,CS70_5,CS70_6,CS70_7,CS70_8,CS70_9,CS71_1,CS71_2,CS71_3,CS72_1,CS73_1,CS73_2,CS80_2,CS80_3,CS80_4,CS80_6,CS80_7,CS80_8,CS80_9)
#fwrite(selected_2_with_repository, "selected_2_with_repository.csv")


#Read and transform data for features we are foucsed on
#selected_features <- read.csv("selected_2_with_repository.csv", header=T)
#selected_features$Author <- as.character(selected_features$Author)
#selected_features$DateTime <- as.Date(selected_features$DateTime, format = "%Y-%m-%d")
#selected_features$Repository <- as.character(selected_features$Repository)
selected_features <-selected_2_with_repository


#8. Select 20 dvelopers with the least number of commits
commits_groupped_by_id <- selected_2_with_repository %>% group_by(Id, Author, DateTime) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
commits_sorted_by_developer_name <- commits_groupped_by_id %>% arrange(Author)


devs_with_least_commits <- sort(table(commits_sorted_by_developer_name$Author),decreasing=TRUE)
devs_with_least_commits <- as.data.frame(devs_with_least_commits, responseName="NumberOfCommits")
#summary(devs_with_least_commits)
#hist(devs_with_least_commits$NumberOfCommits, breaks=100, ylim = c(0, 200), xlim = c(0,2000))
devs_with_least_commits <- subset(devs_with_least_commits, NumberOfCommits <= 500)
#one develeper was under different usernames, thats why here we take 21
devs_with_least_commits <- head(devs_with_least_commits, 21)

#Select data about features occurrence only for those 20 developers. It's done in order to faster manual search of repository, where author commited
#selected_features_for_less_experienced_devs <- subset(selected_features, Author %in% devs_with_least_commits$Var1)



#9. We identified less eperienced devs in Excel. Now select data about feature occurrence for 10 less experienced developers and aggregate data by year
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

selected_features_with_less_experienced_devs <- subset(selected_features, Author %in% less_experienced_devs)
selected_features_with_less_experienced_devs_by_month <- selected_features_with_less_experienced_devs %>% group_by(month=floor_date(DateTime, "month"), Author) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
fwrite(selected_features_with_less_experienced_devs_by_month, "selected_features_with_less_experienced_devs_by_month.csv")

features_less_exp_devs <- read.csv("selected_features_with_less_experienced_devs_by_month.csv", header=T)
features_less_exp_devs$month <- as.Date(features_less_exp_devs$month)
#features_groupped_by_month <- selected_features %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
features_less_exp_devs_groupped_by_year <- features_less_exp_devs %>% group_by(year=floor_date(month, "year")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))


save(features_less_exp_devs_groupped_by_year,
     file="features_less_exp_devs_groupped_by_year")


#10. Select subset of data after 2016 and calculate avarege occurrence of features for less experienced developers by year
features_less_exp_devs_groupped_by_year <- subset(features_less_exp_devs_groupped_by_year, year >= as.Date("2016-01-01"))
rowSums <- rowSums(features_less_exp_devs_groupped_by_year[-1])
less_exp_devs_average = sum(rowSums)/length(rowSums)

