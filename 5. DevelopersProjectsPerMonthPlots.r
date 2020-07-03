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


selected_features_with_repo <- features %>% select(Id,Author,DateTime,Repository,FileName,CS70_1,CS70_2,CS70_3,CS70_5,CS70_6,CS70_7,CS70_8,CS70_9,CS71_1,CS71_2,CS71_3,CS72_1,CS73_1,CS73_2,CS80_2,CS80_3,CS80_4,CS80_6,CS80_7,CS80_8,CS80_9)
feature = "CS70_1"
cs70_1_devs_and_projects <- selected_features_with_repo %>% select(Id,Author,DateTime,Repository,FileName,feature)
cs70_1_devs_and_projects <- subset(cs70_1_devs_and_projects, cs70_1_devs_and_projects$CS70_1 > 0)
cs70_1_devs_and_projects_data <- cs70_1_devs_and_projects %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(developers = n_distinct(Author), projects = n_distinct(Repository))
cs70_1_devs_and_projects_data <- subset(cs70_1_devs_and_projects_data, month >= as.Date("2016-01-01"))

cs_70_rd_x <- as.Date(c("2017-03-01"))

feature_id="CS70_1"
feature_name="Out variables."
png(width     = 18,
    height    = 10,
    units     = "cm",
    res       = 1200,
    filename=paste(feature_id, "_month_ggplot2_devs_projects.png",sep = ""))
#dev.new()
feature_plot <- ggplot(cs70_1_devs_and_projects_data, aes(x=month, y=developers)) +
  geom_line(aes(color="#00ace6"),size = 1) +
  geom_point(aes(y=developers), size = 1.5, color="#00ace6") +
  geom_line(aes(colour = "#ffb3ff", x=month, y = projects),size = 1) +
  geom_point(aes(y=projects), size = 1.5, color="#ffb3ff") +
  geom_vline(aes(xintercept=cs_70_rd_x,color = "blue"),linetype="dashed")+
  xlab("Time") +
  ylab("Developers, persons / Projects, times") +
  scale_colour_manual(name = 'Legend', 
                      values =c("#00ace6"="#00ace6","#ffb3ff"="#ffb3ff","blue"="blue"), labels = c("Developers","Projects","C# 7.0 Release date"))
feature_plot+scale_x_date(date_breaks = "month", date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(legend.position="bottom")
dev.off()
