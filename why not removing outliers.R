train$cd <- cooks.distance(fit9)
train$outlier <- train$cd > 4/(3010 - 10 -1)
train_new <- train[train$outlier == F,]



fit99 <- glm(Attrition ~ travel_freq + Age + NumCompaniesWorked + MaritalStatus + JobRole +
               YearsWithCurrManager + WorkLifeBalance + JobSatisfaction + EducationField + TotalWorkingYears
             ,data = train_new ,
             family = binomial())
summary(fit99)
##AIC : 921.78
train_new$pred99 <- predict(fit99,type = "response")
perf99 <- prediction(train_new$pred99,train_new$Attrition)
roc <- performance(perf99,measure = "tpr",x.measure = "fpr")
plot(roc,colorize = T)
auc <- performance(perf99,measure = "auc")
auc
##AUC : 0.9378998