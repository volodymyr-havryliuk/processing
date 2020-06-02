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





anova_df <- data.frame(
  Occurrences = c(features_groupped_by_month_after_2016[,"CS70_1"],
    features_groupped_by_month_after_2016[,"CS70_2"],
    features_groupped_by_month_after_2016[,"CS70_3"],
    features_groupped_by_month_after_2016[,"CS70_5"],
    features_groupped_by_month_after_2016[,"CS70_6"],
    features_groupped_by_month_after_2016[,"CS70_7"],
    features_groupped_by_month_after_2016[,"CS70_8"],
    features_groupped_by_month_after_2016[,"CS70_9"],
    features_groupped_by_month_after_2016[,"CS71_1"],
    features_groupped_by_month_after_2016[,"CS71_2"],
    features_groupped_by_month_after_2016[,"CS71_3"],
    features_groupped_by_month_after_2016[,"CS72_1"],
    features_groupped_by_month_after_2016[,"CS73_1"],
    features_groupped_by_month_after_2016[,"CS73_2"],
    features_groupped_by_month_after_2016[,"CS80_2"],
    features_groupped_by_month_after_2016[,"CS80_3"],
    features_groupped_by_month_after_2016[,"CS80_4"],
    features_groupped_by_month_after_2016[,"CS80_6"],
    features_groupped_by_month_after_2016[,"CS80_7"],
    features_groupped_by_month_after_2016[,"CS80_8"],
    features_groupped_by_month_after_2016[,"CS80_9"]
    )
  )

FeatureId <- rep(c("CS70_1"), times = 53)
FeatureId <- append (FeatureId,  rep(c("CS70_2"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS70_3"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS70_5"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS70_6"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS70_7"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS70_8"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS70_9"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS71_1"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS71_2"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS71_3"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS72_1"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS73_1"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS73_2"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_2"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_3"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_4"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_6"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_7"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_8"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_9"), times = 53))

anova_df$FeatureId = FeatureId
anova_df$FeatureId <- as.factor(anova_df$FeatureId)

fwrite(anova_df, "anova_df.csv")

levels(anova_df$FeatureId)

group_by(anova_df, FeatureId) %>%
  summarise(
    count = n(),
    mean = mean(Occurrences, na.rm = TRUE),
    sd = sd(Occurrences, na.rm = TRUE)
  )


library("ggpubr")
ggboxplot(anova_df, x = "FeatureId", y = "Occurrences", 
          color = "FeatureId", 
          ylab = "Occurrences", xlab = "FeatureId")

ggline(anova_df, x = "FeatureId", y = "Occurrences", 
       add = c("mean_se", "jitter"), 
       ylab = "Occurrences", xlab = "FeatureId")

res.aov <- aov(Occurrences ~ FeatureId, data = anova_df)
summary(res.aov)

TukeyHSD(res.aov)
plot(res.aov, 1)

library(car)
leveneTest(Occurrences ~ FeatureId, data = anova_df)

#https://www.statisticssolutions.com/the-assumption-of-homogeneity-of-variance/



