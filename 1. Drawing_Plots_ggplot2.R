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


features <- read.csv("usage_results.csv", header=T)
features$DateTime <- as.Date(features$DateTime, format = "%m/%d/%Y %H:%M:%S")

#select important columns and columns with data for features we are interested in
selected_features <- features %>% select(Id,Author,DateTime,CS70_1,CS70_2,CS70_3,CS70_5,CS70_6,CS70_7,CS70_8,CS70_9,CS71_1,CS71_2,CS71_3,CS72_1,CS73_1,CS73_2,CS80_2,CS80_3,CS80_4,CS80_6,CS80_7,CS80_8,CS80_9)

#we can write data to file
#fwrite(selected_features, "selected_features.csv")
#selected_features <- read.csv("selected_2.csv", header=T)
#selected_features$DateTime <- as.Date(selected_features$DateTime, format = "%Y-%m-%d")

features_groupped_by_month <- selected_features %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
features_groupped_by_month_after_2016 <- subset(features_groupped_by_month, month >= as.Date("2016-01-01"))

#we can write data to file
fwrite(features_groupped_by_month_after_2016, "features_groupped_by_month_after_2016.csv")
#features_groupped_by_month_after_2016 <- read.csv("features_groupped_by_month_after_2016.csv", header=T)
#features_groupped_by_month_after_2016$month <- as.Date(features_groupped_by_month_after_2016$month)

#define C#7.0 release date
cs_70_rd_x <- as.Date(c("2017-03-01"))


#draw all plots

pdf(width=12, "CS70_1_month_ggplot2.pdf")
#dev.new()
CS70_1_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_1)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  ggtitle("CS70_1. Out variables.") +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
CS70_1_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()

feature_id="CS70_2"
feature_name="Pattern matching."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf"))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_2)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()

feature_id="CS70_3"
feature_name="Ref locals and returns."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_3)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()

feature_id="CS70_5"
feature_name="More expression-bodied members."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_5)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()

feature_id="CS70_6"
feature_name="Throw expressions."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_6)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()

feature_id="CS70_7"
feature_name="Generalized async return types."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_7)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()

feature_id="CS70_8"
feature_name="Numeric literal syntax improvements."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_8)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()

feature_id="CS70_9"
feature_name="Discards."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_9)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()




#define C#7.1 release date
cs_71_rd_x <- as.Date(c("2017-08-01"))

feature_id="CS71_1"
feature_name="Async main method."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS71_1)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_71_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.1 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()

feature_id="CS71_2"
feature_name="Default literal expressions."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS71_2)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_71_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.1 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()

feature_id="CS71_3"
feature_name="Inferred tuple element names."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS71_3)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_71_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.1 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()




#define C#7.2 release date
cs_72_rd_x <- as.Date(c("2017-11-01"))

feature_id="CS72_1"
feature_name="Leading underscores in numeric literals."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS72_1)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_72_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.2 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()




#define C#7.3 release date
cs_73_rd_x <- as.Date(c("2018-05-01"))

feature_id="CS73_1"
feature_name="Attaching attributes to the backing fields for auto-implemented properties."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS73_1)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_73_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.3 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()

feature_id="CS73_2"
feature_name="Enhanced generic constraints."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS73_2)) +
  geom_line(aes(color="#00ace6"),size = 1.1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_73_rd_x,color = "blue"),linetype="dashed")+
  ggtitle(paste(feature_id, feature_name, sep = ". ")) +
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.3 Release date"))
feature_plot+scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")
dev.off()




#define C#8.0 release date
cs_80_rd_x <- as.Date(c("2019-09-01"))

feature_id="CS80_2"
feature_name="Switch expressions."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_2)) +
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

feature_id="CS80_3"
feature_name="Using declarations."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_3)) +
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

feature_id="CS80_4"
feature_name="Property patterns."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_4)) +
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

feature_id="CS80_6"
feature_name="Index from end operator."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_6)) +
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

feature_id="CS80_7"
feature_name="Ranges."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_7)) +
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

feature_id="CS80_8"
feature_name="Null-coalescing assignment."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_8)) +
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

feature_id="CS80_9"
feature_name="Struct Readonly Members."
pdf(width=12, paste(feature_id, "_month_ggplot2.pdf",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_9)) +
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



#############
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200, filename="CS70_1_month_ggplot2.png")
CS70_1_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_1)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
CS70_1_plot+
  scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()

############
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200, filename="CS70_2_month_ggplot2.png")
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_2)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))

feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()


############
feature_id="CS70_3"
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_3)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()


############

feature_id="CS70_5"
feature_name="More expression-bodied members."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_5)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()


############

feature_id="CS70_6"
feature_name="Throw expressions."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_6)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()

############

feature_id="CS70_7"
feature_name="Generalized async return types."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_7)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()


############

feature_id="CS70_8"
feature_name="Numeric literal syntax improvements."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_8)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()


############

feature_id="CS70_9"
feature_name="Discards."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS70_9)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()


############
############

#define C#7.1 release date
cs_71_rd_x <- as.Date(c("2017-08-01"))

feature_id="CS71_1"
feature_name="Async main method."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS71_1)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_71_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.1 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()



############

feature_id="CS71_2"
feature_name="Default literal expressions."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS71_2)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_71_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.1 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()



############

feature_id="CS71_3"
feature_name="Inferred tuple element names."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS71_3)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_71_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.1 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()


#############
#############

#define C#7.2 release date
cs_72_rd_x <- as.Date(c("2017-11-01"))
feature_id="CS72_1"
feature_name="Leading underscores in numeric literals."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS72_1)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_72_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.2 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()



#############
#############

#define C#7.3 release date
cs_73_rd_x <- as.Date(c("2018-05-01"))

feature_id="CS73_1"
feature_name="Attaching attributes to the backing fields for auto-implemented properties."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS73_1)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_73_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.3 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()

#############

feature_id="CS73_2"
feature_name="Enhanced generic constraints."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS73_2)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_73_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 7.3 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()




#############
#############


#define C#8.0 release date
cs_80_rd_x <- as.Date(c("2019-09-01"))

feature_id="CS80_2"
feature_name="Switch expressions."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_2)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_80_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 8.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()




#############

feature_id="CS80_3"
feature_name="Using declarations."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_3)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_80_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 8.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()


#############

feature_id="CS80_4"
feature_name="Property patterns."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_4)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_80_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 8.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()



#############

feature_id="CS80_6"
feature_name="Index from end operator."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_6)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_80_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 8.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()



#############

feature_id="CS80_7"
feature_name="Ranges."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_7)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_80_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 8.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()



#############

feature_id="CS80_8"
feature_name="Null-coalescing assignment."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_8)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_80_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 8.0 Release date"))
feature_plot+scale_x_date(date_breaks = "months", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()





#############

feature_id="CS80_9"
feature_name="Struct Readonly Members."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2.png",sep = ""))
#dev.new()
feature_plot <- ggplot(features_groupped_by_month_after_2016, aes(x=month, y=CS80_9)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm",se=FALSE,aes(color="#116315"),size = 0.5)+
  geom_vline(aes(xintercept=cs_80_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Occurrence, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#116315"="#116315","blue"="blue"), labels = c("Occurrence","Regression","C# 8.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()

