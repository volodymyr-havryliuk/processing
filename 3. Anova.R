options(max.print=4000000)
library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)
library(gridExtra)
library("ggpubr")


set.seed(2017)
options(digits=4)
Sys.setlocale("LC_TIME", "English")

setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\DataProcessing")
getwd()

# 1. Load data, select needed features and keep only data after 2016-01-01
features <- read.csv("usage_results.csv", header=T)
features$DateTime <- as.Date(features$DateTime, format = "%m/%d/%Y %H:%M:%S")
features$Repository <- as.character(features$Repository)
features$FileName <- as.character(features$FileName)

selected_features <- features %>% select(Id,Author,DateTime,CS70_1,CS70_2,CS70_3,CS70_5,CS70_6,CS70_7,CS70_8,CS70_9,CS71_1,CS71_2,CS71_3,CS72_1,CS73_1,CS73_2,CS80_2,CS80_3,CS80_4,CS80_6,CS80_7,CS80_8,CS80_9)

#fwrite(selected_features, "selected_features.csv")
#selected_features <- read.csv("selected_features.csv", header=T)
#selected_features$Author <- as.character(selected_features$Author)
#selected_features$DateTime <- as.Date(selected_features$DateTime, format = "%Y-%m-%d")

features_groupped_by_month <- selected_features %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))


#######ANOVA FOR ALL DATA######
###NOT USED###SEE LATER###


features_groupped_by_month_after_2016 <- subset(features_groupped_by_month, month >= as.Date("2016-01-01"))
features_groupped_by_month_after_2016 <-data.frame(features_groupped_by_month_after_2016)
#fwrite(features_groupped_by_month_after_2016, "features_groupped_by_month_after_2016.csv")
#features_groupped_by_month_after_2016 <- read.csv("features_groupped_by_month_after_2016.csv", header=T)
#features_groupped_by_month_after_2016$month <- as.Date(features_groupped_by_month_after_2016$month)

# 2. Prepare C#7.0-7.3 data for ANOVA

anova_df_cs7 <- data.frame(
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
      features_groupped_by_month_after_2016[,"CS73_2"]
    )
  )

FeatureId <-c()
FeatureId <- append (FeatureId,  rep(c("CS70_1"), times = 53))
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

anova_df_cs7$FeatureId=FeatureId
anova_df_cs7$FeatureId <- as.factor(anova_df_cs7$FeatureId)

fwrite(anova_df_cs7, "anova_df_cs7.csv")
levels(anova_df_cs7$FeatureId)


# 3. Compute summary statistics by groups - count, mean, sd. Draw plots and apply ANOVA for C#7.0-7.3 features.
group_by(anova_df_cs7, FeatureId) %>%
  summarise(
    count = n(),
    mean = mean(Occurrences, na.rm = TRUE),
    sd = sd(Occurrences, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(anova_df_cs7, x = "FeatureId", y = "Occurrences", 
          color = "FeatureId", 
          ylab = "Occurrences", xlab = "FeatureId")

ggline(anova_df_cs7, x = "FeatureId", y = "Occurrences", 
       add = c("mean_se", "jitter"), 
       ylab = "Occurrences", xlab = "FeatureId")

res.aov <- aov(Occurrences ~ FeatureId, data = anova_df_cs7)
summary(res.aov)

TukeyHSD(res.aov)
plot(res.aov, 1)

#https://www.statisticssolutions.com/the-assumption-of-homogeneity-of-variance/


# 4. Prepare C#8.0 data for ANOVA

anova_df_cs8 <- data.frame(
  Occurrences = c(features_groupped_by_month_after_2016[,"CS80_2"],
                  features_groupped_by_month_after_2016[,"CS80_3"],
                  features_groupped_by_month_after_2016[,"CS80_4"],
                  features_groupped_by_month_after_2016[,"CS80_6"],
                  features_groupped_by_month_after_2016[,"CS80_7"],
                  features_groupped_by_month_after_2016[,"CS80_8"],
                  features_groupped_by_month_after_2016[,"CS80_9"]
  )
)

FeatureId <-c()
FeatureId <- append (FeatureId,  rep(c("CS80_2"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_3"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_4"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_6"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_7"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_8"), times = 53))
FeatureId <- append (FeatureId,  rep(c("CS80_9"), times = 53))

anova_df_cs8$FeatureId = FeatureId
anova_df_cs8$FeatureId <- as.factor(anova_df_cs8$FeatureId)

fwrite(anova_df_cs8, "anova_df_cs8.csv")

levels(anova_df_cs8$FeatureId)

# 5. Compute summary statistics by groups - count, mean, sd. Draw plots and apply ANOVA for C#8.0 features.

group_by(anova_df_cs8, FeatureId) %>%
  summarise(
    count = n(),
    mean = mean(Occurrences, na.rm = TRUE),
    sd = sd(Occurrences, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(anova_df_cs8, x = "FeatureId", y = "Occurrences", 
          color = "FeatureId", 
          ylab = "Occurrences", xlab = "FeatureId")

ggline(anova_df_cs8, x = "FeatureId", y = "Occurrences", 
       add = c("mean_se", "jitter"), 
       ylab = "Occurrences", xlab = "FeatureId")

res.aov <- aov(Occurrences ~ FeatureId, data = anova_df_cs8)
summary(res.aov)

TukeyHSD(res.aov)
plot(res.aov, 1)

#######ANOVA FOR FIRST EIGHT MONTHS AFTER RELEASE######
###USED IN THESIS###


cs_70_rd_x <- as.Date(c("2017-03-01"))
cs_70_8_months_after_rd <- as.Date(c("2017-11-01"))

cs_71_rd_x <- as.Date(c("2017-08-01"))
cs_71_8_months_after_rd <- as.Date(c("2018-04-01"))

cs_72_rd_x <- as.Date(c("2017-11-01"))
cs_72_8_months_after_rd <- as.Date(c("2018-07-01"))

cs_73_rd_x <- as.Date(c("2018-05-01"))
cs_73_8_months_after_rd <- as.Date(c("2019-01-01"))

cs_80_rd_x <- as.Date(c("2019-09-01"))
cs_80_8_months_after_rd <- as.Date(c("2020-05-01"))


cs70_data_for_8_months_after_rd_by_month <- subset(features_groupped_by_month, month >= cs_70_rd_x & month < cs_70_8_months_after_rd)
cs70_data_for_8_months_after_rd_by_month <- data.frame(cs70_data_for_8_months_after_rd_by_month)
cs71_data_for_8_months_after_rd_by_month <- subset(features_groupped_by_month, month >= cs_71_rd_x & month < cs_71_8_months_after_rd)
cs71_data_for_8_months_after_rd_by_month <- data.frame(cs71_data_for_8_months_after_rd_by_month)
cs72_data_for_8_months_after_rd_by_month <- subset(features_groupped_by_month, month >= cs_72_rd_x & month < cs_72_8_months_after_rd)
cs72_data_for_8_months_after_rd_by_month <- data.frame(cs72_data_for_8_months_after_rd_by_month)
cs73_data_for_8_months_after_rd_by_month <- subset(features_groupped_by_month, month >= cs_73_rd_x & month < cs_73_8_months_after_rd)
cs73_data_for_8_months_after_rd_by_month <- data.frame(cs73_data_for_8_months_after_rd_by_month)
cs80_data_for_8_months_after_rd_by_month <- subset(features_groupped_by_month, month >= cs_80_rd_x & month < cs_80_8_months_after_rd)
cs80_data_for_8_months_after_rd_by_month <- data.frame(cs80_data_for_8_months_after_rd_by_month)


anova_df_all <- data.frame(
  Occurrences = c(cs70_data_for_8_months_after_rd_by_month[,"CS70_1"],
                  cs70_data_for_8_months_after_rd_by_month[,"CS70_2"],
                  cs70_data_for_8_months_after_rd_by_month[,"CS70_3"],
                  cs70_data_for_8_months_after_rd_by_month[,"CS70_5"],
                  cs70_data_for_8_months_after_rd_by_month[,"CS70_6"],
                  cs70_data_for_8_months_after_rd_by_month[,"CS70_7"],
                  cs70_data_for_8_months_after_rd_by_month[,"CS70_8"],
                  cs70_data_for_8_months_after_rd_by_month[,"CS70_9"],
                  cs71_data_for_8_months_after_rd_by_month[,"CS71_1"],
                  cs71_data_for_8_months_after_rd_by_month[,"CS71_2"],
                  cs71_data_for_8_months_after_rd_by_month[,"CS71_3"],
                  cs72_data_for_8_months_after_rd_by_month[,"CS72_1"],
                  cs73_data_for_8_months_after_rd_by_month[,"CS73_1"],
                  cs73_data_for_8_months_after_rd_by_month[,"CS73_2"],
                  cs80_data_for_8_months_after_rd_by_month[,"CS80_2"],
                  cs80_data_for_8_months_after_rd_by_month[,"CS80_3"],
                  cs80_data_for_8_months_after_rd_by_month[,"CS80_4"],
                  cs80_data_for_8_months_after_rd_by_month[,"CS80_6"],
                  cs80_data_for_8_months_after_rd_by_month[,"CS80_7"],
                  cs80_data_for_8_months_after_rd_by_month[,"CS80_8"],
                  cs80_data_for_8_months_after_rd_by_month[,"CS80_9"]
  )
)


FeatureId <-c()
FeatureId <- append (FeatureId,  rep(c("CS70_1"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS70_2"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS70_3"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS70_5"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS70_6"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS70_7"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS70_8"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS70_9"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS71_1"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS71_2"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS71_3"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS72_1"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS73_1"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS73_2"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS80_2"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS80_3"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS80_4"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS80_6"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS80_7"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS80_8"), times = 8))
FeatureId <- append (FeatureId,  rep(c("CS80_9"), times = 8))


anova_df_all$FeatureId=FeatureId
anova_df_all$FeatureId <- as.factor(anova_df_all$FeatureId)


fwrite(anova_df_all, "anova_df_all.csv")
levels(anova_df_all$FeatureId)

# Show a random sample
set.seed(1234)
dplyr::sample_n(anova_df_all, 10)



statistics <- group_by(anova_df_all, FeatureId) %>%
  summarise(
    months_count = n(),
    occurrences_means = mean(Occurrences, na.rm = TRUE),
    standard_deviation = sd(Occurrences, na.rm = TRUE)
  )

statistics <- statistics %>% arrange(desc(occurrences_means))

##FeatureName <-c()

##FeatureName <- append (FeatureName,  c("Out variables"))
##FeatureName <- append (FeatureName,  c("Pattern matching"))
##FeatureName <- append (FeatureName,  c("Ref locals and returns"))
##FeatureName <- append (FeatureName,  c("More expression-bodied members"))
##FeatureName <- append (FeatureName,  c("Throw expressions"))
##FeatureName <- append (FeatureName,  c("Generalized async return types"))
##FeatureName <- append (FeatureName,  c("Numeric literal syntax improvements"))
##FeatureName <- append (FeatureName,  c("Discards"))

##FeatureName <- append (FeatureName,  c("Async main method"))
##FeatureName <- append (FeatureName,  c("Default literal expressions"))
##FeatureName <- append (FeatureName,  c("Inferred tuple element names"))

##FeatureName <- append (FeatureName,  c("Leading underscores in numeric literals"))

##FeatureName <- append (FeatureName,  c("Attributes to the backing fields for auto-implemented properties"))
##FeatureName <- append (FeatureName,  c("Enhanced generic constraints"))

##FeatureName <- append (FeatureName,  c("Switch expressions"))
##FeatureName <- append (FeatureName,  c("Using declarations"))
##FeatureName <- append (FeatureName,  c("Property patterns"))
##FeatureName <- append (FeatureName,  c("Index from end operator"))
##FeatureName <- append (FeatureName,  c("Ranges"))
##FeatureName <- append (FeatureName,  c("Null-coalescing assignment"))
##FeatureName <- append (FeatureName,  c("Struct readonly members"))

##statistics <- as.data.frame(statistics)

##statistics$FeatureName = FeatureName
##statistics <-statistics[c(1,5,2,3,4)]

png(width     = 19,
    height    = 16,
    units     = "cm",
    res       = 1200,
    filename="OccurrencesStatistics.png")
p<-tableGrob(statistics)
grid.arrange(p)
dev.off()


png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename="OccurrencesMean.png")
ggplot(statistics, aes(FeatureId, mean))+
  labs(x = "Feature Id", y = "Occurrences, mean") +
  theme(axis.text.x = element_text(angle = 90, size = 10))+
  geom_col()
dev.off()

png(width     = 18,
    height    = 21,
    units     = "cm",
    res       = 1200,
    filename="OccurrencesBoxPlotBW.png")
ggboxplot(anova_df_all,
          x = "FeatureId",
          y = "Occurrences", 
          color = "black", 
          ylab = "Occurrences",
          xlab = "Feature Id")+
theme(axis.text.x = element_text(angle = 90, size = 10), legend.position = "none")
dev.off()

png(width     = 18,
    height    = 12,
    units     = "cm",
    res       = 1200,
    filename="MeanPlot.png")
ggline(anova_df_all, x = "FeatureId", y = "Occurrences", 
       add = c("mean_se", "jitter"), 
       ylab = "Occurrences", xlab = "Feature Id")+
theme(axis.text.x = element_text(angle = 90, size = 10))
dev.off()

res.aov <- aov(Occurrences ~ FeatureId, data = anova_df_all)
summary(res.aov)


TukeyHSD(res.aov)
plot(res.aov, 1)