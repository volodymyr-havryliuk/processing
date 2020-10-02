options(max.print=4000000)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(DataCombine)
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
selected_features_for_most_experienced_devs_groupped_by_month <- selected_features_for_most_experienced_devs %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
#fwrite(selected_features_for_most_experienced_devs_groupped_by_month, "selected_features_for_most_experienced_devs_groupped_by_month.csv")
#features_most_exp_devs <- read.csv("selected_features_for_most_experienced_devs_by_month_groupped_by_month.csv", header=T)
#selected_features_for_most_experienced_dev_groupped_by_months$month <- as.Date(selected_features_for_most_experienced_devs_groupped_by_month$month)

#5. Select subset of data after 2016-01-01 and before 2020-05-01 and calculate avarege occurrence of features for most experienced developers per month
selected_features_for_most_experienced_devs_groupped_by_month <- subset(selected_features_for_most_experienced_devs_groupped_by_month, month >= as.Date("2016-01-01"))
selected_features_for_most_experienced_devs_groupped_by_month <- subset(selected_features_for_most_experienced_devs_groupped_by_month, month < as.Date("2020-05-01"))
fwrite(selected_features_for_most_experienced_devs_groupped_by_month, "selected_features_for_most_experienced_devs_groupped_by_month.csv")

#selected_features_for_most_experienced_devs <- subset(selected_features_for_most_experienced_devs, month >= as.Date("2016-01-01"))
#selected_features_for_most_experienced_devs <- subset(selected_features_for_most_experienced_devs, month < as.Date("2020-05-01"))
#fwrite(selected_features_for_most_experienced_devs, "selected_features_for_most_experienced_devs.csv")

me_rowSums <- rowSums(selected_features_for_most_experienced_devs_groupped_by_month[-1])
me_number_of_months = length(me_rowSums)
me_devs_average = sum(me_rowSums)/me_number_of_months # 
me_devs_average # ~ 1412 per mont1h     (10 most experienced developers used all 21 features 12286 times per month)


# Draw plot
cs70_release_date <- as.Date("2017-03-01")
cs71_release_date <- as.Date("2017-08-01")
cs72_release_date <- as.Date("2017-11-01")
cs73_release_date <- as.Date("2018-05-01")
cs80_release_date <- as.Date("2019-09-01")

selected_features_for_most_exp_plot_cs70 <- subset (selected_features_for_most_experienced_devs_groupped_by_month, month >= cs70_release_date)
selected_features_for_most_exp_plot_cs71 <- subset (selected_features_for_most_experienced_devs_groupped_by_month, month >= cs71_release_date)
selected_features_for_most_exp_plot_cs72 <- subset (selected_features_for_most_experienced_devs_groupped_by_month, month >= cs72_release_date)
selected_features_for_most_exp_plot_cs73 <- subset (selected_features_for_most_experienced_devs_groupped_by_month, month >= cs73_release_date)
selected_features_for_most_exp_plot_cs80 <- subset (selected_features_for_most_experienced_devs_groupped_by_month, month >= cs80_release_date)

selected_features_for_most_exp_plot_cs70 <- head(selected_features_for_most_exp_plot_cs70, 8)
selected_features_for_most_exp_plot_cs71 <- head(selected_features_for_most_exp_plot_cs71, 8)
selected_features_for_most_exp_plot_cs72 <- head(selected_features_for_most_exp_plot_cs72, 8)
selected_features_for_most_exp_plot_cs73 <- head(selected_features_for_most_exp_plot_cs73, 8)
selected_features_for_most_exp_plot_cs80 <- head(selected_features_for_most_exp_plot_cs80, 8)


selected_features_for_most_exp_plot <- selected_features_for_most_exp_plot_cs70[,c(2,3,4,5,6,7,8,9)]
selected_features_for_most_exp_plot[, c("CS71_1", "CS71_2", "CS71_3")] <- NA
selected_features_for_most_exp_plot[, c("CS71_1", "CS71_2", "CS71_3")] <- selected_features_for_most_exp_plot_cs71[10:12]
selected_features_for_most_exp_plot[, c("CS72_1")] <- NA
selected_features_for_most_exp_plot[, c("CS72_1")] <- selected_features_for_most_exp_plot_cs72[13]
selected_features_for_most_exp_plot[, c("CS73_1","CS73_2")] <- NA
selected_features_for_most_exp_plot[, c("CS73_1","CS73_2")] <- selected_features_for_most_exp_plot_cs73[14:15]
selected_features_for_most_exp_plot[,c("CS80_2","CS80_3","CS80_4","CS80_6","CS80_7","CS80_8","CS80_9")]<- NA
selected_features_for_most_exp_plot[,c("CS80_2","CS80_3","CS80_4","CS80_6","CS80_7","CS80_8","CS80_9")]<- selected_features_for_most_exp_plot_cs80[16:22]
selected_features_for_most_exp_plot[,c("month")] <- NA
selected_features_for_most_exp_plot[,c("month")] <- c(1:8)

# Create new row
empty_row <- as.list (rep(0, times=22))
# Insert into 4th row
selected_features_for_most_exp_plot <- InsertRow(selected_features_for_most_exp_plot, NewRow = empty_row, RowNum = 1)

columns=c("CS70_1", "CS70_2", "CS70_3", "CS70_5", "CS70_6", "CS70_7", "CS70_8", "CS70_9", "CS71_1", "CS71_2", "CS71_3", "CS72_1", "CS73_1", "CS73_2", "CS80_2", "CS80_3", "CS80_4","CS80_6", "CS80_7","CS80_8","CS80_9")
selected_features_for_most_exp_plot$all_features <- rowSums(selected_features_for_most_exp_plot[,columns])

png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200, filename="me_all_month_ggplot2.png")
me_all_plot <- ggplot(selected_features_for_most_exp_plot, aes(x=month, y=all_features)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=1,color = "blue"),linetype="dashed")+
  xlab("Month") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","Release "))
me_all_plot+
  theme(axis.text.x = element_text(angle = 0, size = 8))+
  theme(legend.position="bottom")
dev.off()

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
#fwrite(selected_features_for_less_experienced_devs, "selected_features_for_less_experienced_devs_not_grouped_by_month.csv")
selected_features_for_less_experienced_devs_groupped_by_month <- selected_features_for_less_experienced_devs %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
#fwrite(selected_features_for_less_experienced_devs_groupped_by_month, "selected_features_for_less_experienced_devs_groupped_by_month.csv")
#selected_features_for_less_experienced_devs_groupped_by_month <- read.csv("selected_features_for_less_experienced_devs_groupped_by_month.csv", header=T)
#selected_features_for_less_experienced_devs_groupped_by_month$month <- as.Date(selected_features_for_less_experienced_devs_groupped_by_month$month)

#8. Select subset of data after 2016 and calculate avarege occurrence of features for less experienced developers by month
selected_features_for_less_experienced_devs_groupped_by_month <- subset(selected_features_for_less_experienced_devs_groupped_by_month, month >= as.Date("2016-01-01"))
selected_features_for_less_experienced_devs_groupped_by_month <- subset(selected_features_for_less_experienced_devs_groupped_by_month, month < as.Date("2020-05-01"))
#fwrite(selected_features_for_less_experienced_devs, "selected_features_for_less_experienced_devs.csv")

#selected_features_for_less_experienced_devs <- subset(selected_features_for_less_experienced_devs, month >= as.Date("2016-01-01"))
#selected_features_for_less_experienced_devs <- subset(selected_features_for_less_experienced_devs, month < as.Date("2020-05-01"))
#fwrite(selected_features_for_less_experienced_devs, "selected_features_for_less_experienced_devs.csv")

le_rowSums <- rowSums(selected_features_for_less_experienced_devs_groupped_by_month[-1])
le_number_of_months = length(le_rowSums)
le_devs_average = sum(le_rowSums)/le_number_of_months # ~ 350 per month    (10 less experienced developers used all 21 features 350 times per month)
le_devs_average

# Draw plot
selected_features_for_less_exp_plot_cs70 <- subset (selected_features_for_less_experienced_devs_groupped_by_month, month >= cs70_release_date)
selected_features_for_less_exp_plot_cs71 <- subset (selected_features_for_less_experienced_devs_groupped_by_month, month >= cs71_release_date)
selected_features_for_less_exp_plot_cs72 <- subset (selected_features_for_less_experienced_devs_groupped_by_month, month >= cs72_release_date)
selected_features_for_less_exp_plot_cs73 <- subset (selected_features_for_less_experienced_devs_groupped_by_month, month >= cs73_release_date)
selected_features_for_less_exp_plot_cs80 <- subset (selected_features_for_less_experienced_devs_groupped_by_month, month >= cs80_release_date)

selected_features_for_less_exp_plot_cs70 <- head(selected_features_for_less_exp_plot_cs70, 8)
selected_features_for_less_exp_plot_cs71 <- head(selected_features_for_less_exp_plot_cs71, 8)
selected_features_for_less_exp_plot_cs72 <- head(selected_features_for_less_exp_plot_cs72, 8)
selected_features_for_less_exp_plot_cs73 <- head(selected_features_for_less_exp_plot_cs73, 8)
selected_features_for_less_exp_plot_cs80 <- head(selected_features_for_less_exp_plot_cs80, 8)


selected_features_for_less_exp_plot <- selected_features_for_less_exp_plot_cs70[,c(2,3,4,5,6,7,8,9)]
selected_features_for_less_exp_plot[, c("CS71_1", "CS71_2", "CS71_3")] <- NA
selected_features_for_less_exp_plot[, c("CS71_1", "CS71_2", "CS71_3")] <- selected_features_for_less_exp_plot_cs71[10:12]
selected_features_for_less_exp_plot[, c("CS72_1")] <- NA
selected_features_for_less_exp_plot[, c("CS72_1")] <- selected_features_for_less_exp_plot_cs72[13]
selected_features_for_less_exp_plot[, c("CS73_1","CS73_2")] <- NA
selected_features_for_less_exp_plot[, c("CS73_1","CS73_2")] <- selected_features_for_less_exp_plot_cs73[14:15]
selected_features_for_less_exp_plot[,c("CS80_2","CS80_3","CS80_4","CS80_6","CS80_7","CS80_8","CS80_9")]<- NA
selected_features_for_less_exp_plot[,c("CS80_2","CS80_3","CS80_4","CS80_6","CS80_7","CS80_8","CS80_9")]<- selected_features_for_less_exp_plot_cs80[16:22]
selected_features_for_less_exp_plot[,c("month")] <- NA
selected_features_for_less_exp_plot[,c("month")] <- c(1:8)

# Create new row
empty_row <- as.list (rep(0, times=22))
# Insert into 4th row
selected_features_for_less_exp_plot <- InsertRow(selected_features_for_less_exp_plot, NewRow = empty_row, RowNum = 1)

columns=c("CS70_1", "CS70_2", "CS70_3", "CS70_5", "CS70_6", "CS70_7", "CS70_8", "CS70_9", "CS71_1", "CS71_2", "CS71_3", "CS72_1", "CS73_1", "CS73_2", "CS80_2", "CS80_3", "CS80_4","CS80_6", "CS80_7","CS80_8","CS80_9")
selected_features_for_less_exp_plot$all_features <- rowSums(selected_features_for_less_exp_plot[,columns])

png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200, filename="le_all_month_ggplot2.png")
me_all_plot <- ggplot(selected_features_for_less_exp_plot, aes(x=month, y=all_features)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=1,color = "blue"),linetype="dashed")+
  xlab("Month") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","Release "))
me_all_plot+
  theme(axis.text.x = element_text(angle = 0, size = 8))+
  theme(legend.position="bottom")
dev.off()




le_me_plot <-tibble(`me_all` = c(0,0,0,0,0,0,0,0,0), `le_all`= c(0,0,0,0,0,0,0,0,0), `month`=c(0,1,2,3,4,5,6,7,8))
le_me_plot$me_all <- selected_features_for_most_exp_plot$all_features
le_me_plot$le_all <- selected_features_for_less_exp_plot$all_features

colors <- c("Most experienced" = "#009933", "Regression m.e."="darkgreen", "Less experienced" = "red", "Regression l.e."="darkred", "Release"="blue")

png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200, filename="le_me_all_month_ggplot2.png")
me_all_plot <- ggplot(le_me_plot, aes(x=month)) +
  geom_line(aes(y=le_me_plot$le_all, color="Less experienced"), size = 1)+
  geom_line(aes(y=le_me_plot$me_all, color="Most experienced"), size = 1)+
  
  geom_point(size = 1,aes(y=le_me_plot$le_all, color="Less experienced")) +
  geom_point(size = 1, aes(y=le_me_plot$me_all, color="Most experienced")) +
  
  geom_smooth(method = "lm",se=FALSE,aes(color="Regression l.e.", y=le_me_plot$le_all),size = 0.5)+
  geom_smooth(method = "lm",se=FALSE,aes(color="Regression m.e.", y=le_me_plot$me_all),size = 0.5)+
  
  geom_vline(aes(xintercept=1,color = "Release"),linetype="dashed")+
  xlab("Month") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =colors)
me_all_plot+
  theme(axis.text.x = element_text(angle = 0, size = 8))+
  theme(legend.position="bottom")
dev.off()

