# Title     : Kruskal-Wallis test
# Objective : Perform Kruskal-Wallis test for conference. Based on http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
# Created by: Volodymyr Havryliuk
# Created on: 10.01.2021

# options(max.print = 4000000)
# library(ggplot2)
# library(data.table)
# library(lubridate)
# library(gridExtra)
# library(sqldf)
library(dplyr)
library("ggpubr")

#set.seed(2017)
#options(digits = 4)
Sys.setlocale("LC_TIME", "English")

setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\DataProcessing")
getwd()

# We use data that were prepared for ANOVA. These data contains new features occurences for 8 months after the release date.
all_features_data <- read.csv("anova_df_all.csv", header = T)

# Use subset of features, selected for a conference
features_for_conference <- subset(all_features_data, FeatureId %in% c("CS70_6", "CS70_9", "CS80_2", "CS80_3", "CS80_8", "CS70_1", "CS73_2"))
features_for_conference$FeatureId <- factor(features_for_conference$FeatureId)

head(features_for_conference)
levels(features_for_conference$FeatureId)


group_by(features_for_conference, FeatureId) %>%
  summarise(
    count = n(),
    mean = mean(Occurrences, na.rm = TRUE),
    sd = sd(Occurrences, na.rm = TRUE),
    median = median(Occurrences, na.rm = TRUE),
    IQR = IQR(Occurrences, na.rm = TRUE)
  )

ggboxplot(features_for_conference, x = "FeatureId", y = "Occurrences",
          color = "Occurrences",
          order = c("CS70_1", "CS70_6", "CS70_9", "CS73_2", "CS80_2", "CS80_3", "CS80_8"),
          ylab = "Occurrences", xlab = "Feature Id")

ggline(features_for_conference, x = "FeatureId", y = "Occurrences",
       add = c("mean_se", "jitter"),
       order = c("CS70_1", "CS70_6", "CS70_9", "CS73_2", "CS80_2", "CS80_3", "CS80_8"),
       ylab = "Occurrences", xlab = "Feature Id")

kruskal.test(Occurrences ~ FeatureId, data = features_for_conference)
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.
pairwise.wilcox.test(features_for_conference$Occurrences, features_for_conference$FeatureId,
                     p.adjust.method = "BH")
