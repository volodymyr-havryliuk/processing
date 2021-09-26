options(max.print=4000000)
library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)

set.seed(2017)
options(digits=4)
Sys.setlocale("LC_TIME", "English")

setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\DataProcessing")
getwd()


features <- read.csv("usage_results_classic.csv", header=T)
features$DateTime <- as.Date(features$DateTime, format = "%m/%d/%Y %H:%M:%S")
features_groupped_by_month <- features %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS_C1 = sum(CS_C1),CS_C2 = sum(CS_C2))
fwrite(features_groupped_by_month, "classic_features_groupped_by_month.csv")

#define C#8.0 release date
cs_80_rd_x <- as.Date(c("2019-09-01"))



feature_id="CS_C1"
feature_name="Switch statements."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month, aes(x=month, y=CS_C1)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_80_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 8.0 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()

feature_id="CS_C2"
feature_name="Using statements."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month, aes(x=month, y=CS_C2)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_80_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 8.0 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()