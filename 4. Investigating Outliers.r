selected_features_with_repo <- features %>% select(Id,Author,DateTime,Repository,FileName,CS70_1,CS70_2,CS70_3,CS70_5,CS70_6,CS70_7,CS70_8,CS70_9,CS71_1,CS71_2,CS71_3,CS72_1,CS73_1,CS73_2,CS80_2,CS80_3,CS80_4,CS80_6,CS80_7,CS80_8,CS80_9)


selected_features_with_repo_cs70_3 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2018-10-01") & selected_features_with_repo$DateTime < as.Date("2018-11-01"))
selected_features_with_repo_cs70_6 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2017-02-01") & selected_features_with_repo$DateTime < as.Date("2017-03-01"))
selected_features_with_repo_cs70_7 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2017-01-01") & selected_features_with_repo$DateTime < as.Date("2017-02-01"))
selected_features_with_repo_cs70_8 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2016-05-01") & selected_features_with_repo$DateTime < as.Date("2016-06-01"))
selected_features_with_repo_cs70_9 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2016-05-01") & selected_features_with_repo$DateTime < as.Date("2016-06-01"))





selected_features_with_repo_cs71_3_1 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2017-01-01") & selected_features_with_repo$DateTime < as.Date("2017-02-01"))
selected_features_with_repo_cs71_3_2 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2017-03-01") & selected_features_with_repo$DateTime < as.Date("2017-04-01"))



selected_features_with_repo_cs73_3 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2020-01-01") & selected_features_with_repo$DateTime < as.Date("2020-02-01"))




selected_features_with_repo_cs8_2_1 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2019-05-01") & selected_features_with_repo$DateTime < as.Date("2019-06-01"))
selected_features_with_repo_cs8_2_2 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2019-06-01") & selected_features_with_repo$DateTime < as.Date("2019-07-01"))
selected_features_with_repo_cs8_2_3 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2019-07-01") & selected_features_with_repo$DateTime < as.Date("2019-08-01"))

selected_features_with_repo_cs8_2 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2019-05-01") & selected_features_with_repo$DateTime < as.Date("2019-09-01"))

devs_and_projects <- subset(selected_features_with_repo_cs8_2, selected_features_with_repo_cs8_2$CS80_2 > 0)
devs_and_projects <- devs_and_projects %>% select(Author, Repository, CS80_2)

devs <- distinct(devs_and_projects,devs_and_projects$Author)
projects <- distinct(devs_and_projects,devs_and_projects$Repository)



selected_features_with_repo_cs8_3_1 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2020-03-01") & selected_features_with_repo$DateTime < as.Date("2020-04-01"))
selected_features_with_repo_cs8_3_2 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2020-04-01") & selected_features_with_repo$DateTime < as.Date("2020-05-01"))





selected_features_with_repo_cs8_4_1 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2019-05-01") & selected_features_with_repo$DateTime < as.Date("2019-06-01"))
selected_features_with_repo_cs8_4_2 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2019-06-01") & selected_features_with_repo$DateTime < as.Date("2019-07-01"))



selected_features_with_repo_cs8_6 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2019-05-01") & selected_features_with_repo$DateTime < as.Date("2019-06-01"))


selected_features_with_repo_cs8_7 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2016-03-01") & selected_features_with_repo$DateTime < as.Date("2016-04-01"))

selected_features_with_repo_cs8_9 <- subset(selected_features_with_repo, selected_features_with_repo$DateTime >= as.Date("2019-12-01") & selected_features_with_repo$DateTime < as.Date("2020-01-01"))


