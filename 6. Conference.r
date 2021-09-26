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


#select important columns and columns with data for features we are interested in
selected_features <- features %>% select(Id,Author,DateTime,Repository,CS70_1,CS70_2,CS70_3,CS70_5,CS70_6,CS70_7,CS70_8,CS70_9,CS71_1,CS71_2,CS71_3,CS72_1,CS73_1,CS73_2,CS80_2,CS80_3,CS80_4,CS80_6,CS80_7,CS80_8,CS80_9)
#features_by_repository_author_date <- selected_features %>% group_by(Repository, Author, month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_6 = sum(CS70_6),CS70_9 = sum(CS70_9),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_8 = sum(CS80_8))

#arranged <- arrange(features_by_repository_author_datetime, Author,  CS70_1, .by_group = TRUE)
#arranged <- arrange(features_by_repository_author_date, Author,  CS70_1, .by_group = TRUE)

#singleRepo <- features_by_repository_author_date %>% 
  #filter(Repository %in% c("D:\\Education\\PUT\\3d semester\\Master Thesis\\software\\ForAnalysis\\clonedRepositories\\windows-toolkit-WindowsCommunityToolkit\\.git\\"))

# singleRepoWithActiveAuthors <- singleRepo %>% group_by(Author) %>% filter(n()==10)

# singleRepoWithActiveAuthors %>%
#   ggplot( aes(x=month, y=CS70_1, group=Author, color=Author)) +
#   geom_line()


#devs <- singleRepo %>% filter(Author %in% c("Alexandre Zollinger Chohfi", "AVK"))
#devs <- singleRepo %>% filter(Author %in% c("Alexandre Zollinger Chohfi", "AVK"))

# pdf("CS70_1_windows-toolkit-WindowsCommunityToolkit.pdf")
# singleRepo %>% ggplot(aes(x=month, y=CS70_1, group=Author, color=Author)) +
#   geom_line() + theme(legend.position = "none")
# dev.off()
# 
# pdf("CS70_6_windows-toolkit-WindowsCommunityToolkit.pdf")
# singleRepo %>% ggplot(aes(x=month, y=CS70_6, group=Author, color=Author)) +
#   geom_line() + theme(legend.position = "none")
# dev.off()
# 
# pdf("CS70_9_windows-toolkit-WindowsCommunityToolkit.pdf")
# singleRepo %>% ggplot(aes(x=month, y=CS70_9, group=Author, color=Author)) +
#   geom_line() + theme(legend.position = "none")
# dev.off()
# 
# pdf("CS73_2_windows-toolkit-WindowsCommunityToolkit.pdf")
# singleRepo %>% ggplot(aes(x=month, y=CS73_2, group=Author, color=Author)) +
#   geom_line() + theme(legend.position = "none")
# dev.off()
# 
# pdf("CS80_2_windows-toolkit-WindowsCommunityToolkit.pdf")
# singleRepo %>% ggplot(aes(x=month, y=CS80_2, group=Author, color=Author)) +
#   geom_line() + theme(legend.position = "none")
# dev.off()
# 
# pdf("CS80_3_windows-toolkit-WindowsCommunityToolkit.pdf")
# singleRepo %>% ggplot(aes(x=month, y=CS80_3, group=Author, color=Author)) +
#   geom_line() + theme(legend.position = "none")
# dev.off()
# 
# pdf("CS80_8_windows-toolkit-WindowsCommunityToolkit.pdf")
# singleRepo %>% ggplot(aes(x=month, y=CS80_8, group=Author, color=Author)) +
#   geom_line() + theme(legend.position = "none")
# dev.off()


features_by_repository_author_datetime <- selected_features %>% group_by(Repository, Author, DateTime) %>% summarize(CS70_1 = sum(CS70_1),CS70_6 = sum(CS70_6),CS70_9 = sum(CS70_9),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_8 = sum(CS80_8))


setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\conference")
getwd()


singleRepo_datetime <- features_by_repository_author_datetime %>% 
  filter(Repository %in% c("D:\\Education\\PUT\\3d semester\\Master Thesis\\software\\ForAnalysis\\clonedRepositories\\windows-toolkit-WindowsCommunityToolkit\\.git\\"))
repoName="windows-toolkit-WindowsCommunityToolkit_datetime"

fwrite(singleRepo_datetime, paste(repoName,".csv"))


featureId = "CS70_1"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS70_1, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS70_6"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS70_6, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS70_9"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS70_9, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS73_2"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS73_2, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS80_2"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS80_2, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS80_3"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS80_3, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS80_8"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS80_8, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()



singleRepo_datetime <- features_by_repository_author_datetime %>% 
  filter(Repository %in% c("D:\\Education\\PUT\\3d semester\\Master Thesis\\software\\ForAnalysis\\clonedRepositories\\JosefNemec-Playnite\\.git\\"))
repoName="JosefNemec-Playnite_datetime"
fwrite(singleRepo_datetime, paste(repoName,".csv"))


featureId = "CS70_1"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS70_1, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS70_6"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS70_6, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS70_9"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS70_9, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS73_2"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS73_2, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS80_2"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS80_2, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS80_3"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS80_3, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS80_8"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS80_8, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()



setwd("D:\\Education\\PUT\\3d semester\\Master Thesis\\conference")
getwd()

singleRepo_datetime <- features_by_repository_author_datetime %>% 
  filter(Repository %in% c("D:\\Education\\PUT\\3d semester\\Master Thesis\\software\\ForAnalysis\\clonedRepositories\\kwsch-PKHeX\\.git\\"))
repoName="kwsch-PKHeX_datetime"

fwrite(singleRepo_datetime, paste(repoName,".csv"))


featureId = "CS70_1"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS70_1, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS70_6"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS70_6, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS70_9"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS70_9, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS73_2"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS73_2, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS80_2"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS80_2, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS80_3"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS80_3, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS80_8"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS80_8, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()





singleRepo_datetime <- features_by_repository_author_datetime %>% 
  filter(Repository %in% c("D:\\Education\\PUT\\3d semester\\Master Thesis\\software\\ForAnalysis\\clonedRepositories\\windows-toolkit-WindowsCommunityToolkit\\.git\\"))

repoName="windows-toolkit-WindowsCommunityToolkit_datetime"

featureId = "CS70_1"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS70_1, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS70_6"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS70_6, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS70_9"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS70_9, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS73_2"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS73_2, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS80_2"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS80_2, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS80_3"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS80_3, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()

featureId = "CS80_8"
pdf(paste(featureId, "_", repoName, ".pdf"))
singleRepo_datetime %>% ggplot(aes(x=DateTime, y=CS80_8, group=Author, color=Author)) +
  geom_line() + theme(legend.position = "none")
dev.off()




# p <- singleRepo %>%
#   ggplot( aes(x=month, fill=Author)) +
#   geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
#   scale_fill_manual(values=c("#69b3a2", "#404080")) +
#   theme_ipsum() +
#   labs(fill="")
# print(p)
# 
# 
# p <- singleRepo %>%
#   mutate(text = fct_reorder(text, value)) %>%
#   ggplot( aes(x=value, color=text, fill=text)) +
#   geom_histogram(alpha=0.6, binwidth = 5) +
#   scale_fill_viridis(discrete=TRUE) +
#   scale_color_viridis(discrete=TRUE) +
#   theme_ipsum() +
#   theme(
#     legend.position="none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   ) +
#   xlab("") +
#   ylab("Assigned Probability (%)") +
#   facet_wrap(~text)


#pdf(width=12, "temp.pdf")

singleRepo %>%  ggplot(aes(x=month, y=CS70_1, group=Author, color=Author))+geom_line()+theme(legend.position="none")
dev.off()
singleRepo %>%  ggplot(aes(x=month, y=CS70_6, group=Author, color=Author))+geom_line()+theme(legend.position="none")
dev.off()
#features_groupped_by_month <- selected_features %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
#features_groupped_by_month <- selected_features %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(CS70_1 = sum(CS70_1),CS70_6 = sum(CS70_6),CS70_9 = sum(CS70_9),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_8 = sum(CS80_8))



#we can write data to file
#fwrite(selected_features, "selected_features.csv")
#selected_features <- read.csv("selected_2.csv", header=T)
#selected_features$DateTime <- as.Date(selected_features$DateTime, format = "%Y-%m-%d")

#features_groupped_by_month <- selected_features %>% group_by(month=floor_date(DateTime, "month"))%>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))

commits_groupped_by_repository_and_author <- selected_features %>% group_by(Repository, Author) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
fwrite(commits_groupped_by_repository_and_author, "commits_groupped_by_repository_and_author.csv")

commits_groupped_by_repository <- selected_features %>% group_by(Repository) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
fwrite(commits_groupped_by_repository, "commits_groupped_by_repository.csv")

commits_groupped_by_author <- selected_features %>% group_by(Author) %>% summarize(CS70_1 = sum(CS70_1),CS70_2 = sum(CS70_2),CS70_3 = sum(CS70_3),CS70_5 = sum(CS70_5),CS70_6 = sum(CS70_6),CS70_7 = sum(CS70_7),CS70_8 = sum(CS70_8),CS70_9 = sum(CS70_9),CS71_1 = sum(CS71_1),CS71_2 = sum(CS71_2),CS71_3 = sum(CS71_3),CS72_1 = sum(CS72_1),CS73_1 = sum(CS73_1 ),CS73_2 = sum(CS73_2),CS80_2 = sum(CS80_2),CS80_3 = sum(CS80_3),CS80_4 = sum(CS80_4),CS80_6 = sum(CS80_6),CS80_7 = sum(CS80_7),CS80_8 = sum(CS80_8),CS80_9 = sum(CS80_9))
fwrite(commits_groupped_by_author, "commits_groupped_by_author.csv")

CS70_6_occurences<-sum(commits_groupped_by_repository$CS70_6)
CS70_6_occurences

CS70_9_occurences<-sum(commits_groupped_by_repository$CS70_9)
CS70_9_occurences

CS80_2_occurences<-sum(commits_groupped_by_repository$CS80_2)
CS80_2_occurences

CS80_3_occurences<-sum(commits_groupped_by_repository$CS80_3)
CS80_3_occurences

CS80_8_occurences<-sum(commits_groupped_by_repository$CS80_8)
CS80_8_occurences

CS70_1_occurences<-sum(commits_groupped_by_repository$CS70_1)
CS70_1_occurences

CS73_2_occurences<-sum(commits_groupped_by_repository$CS73_2)
CS73_2_occurences
