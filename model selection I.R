set.seed(2500)  ##telling random variable to set it 
ix <- sample(1 : nrow(df),floor(0.7*nrow(df)))
train <- df[ix,]
test <- df[-ix,]



install.packages("ROCR")
library(ROCR)
 








                    #    CALCULATION OF THRESHOLD FOR CONFUSION MATRIX

x1 = 0
y1 = 1
mindist = 1
for (i in 1 : 4300) {
  x2 = roc@x.values[[1]][i]
  y2 = roc@y.values[[1]][i]
  
  dst = sqrt((x1-x2)^2 + (y1-y2)^2)
  print(dst)
  if(dst<mindist)
  {
    mindist = dst  #finds the min distance
    min_obs = i  #finds the observation at which we get the min distance
  }
  
}



print(min_obs)
roc@x.values[[1]][min_obs]
roc@y.values[[1]][min_obs]
roc@alpha.values[[1]][min_obs]


df$class <- ifelse(df$pred7 >  0.1714827 ,1,0)
table(p=df$class,df$Attrition)

                     






                        ##COMPARING MODELS       fit#$deviance

fit1 <- glm(Attrition ~ Age + travel_rarely + travel_freq + no_travel +
                Department + JobRole + MaritalStatus , data = train,family = binomial())
summary(fit1)
## AIC : 2502
train$pred1 <- predict(fit1,type = "response")
perf1 <- prediction(train$pred1,train$Attrition)
roc <- performance(perf1,measure = "tpr",x.measure = "fpr")
plot(roc,colorize = T)
auc <- performance(perf1,measure = "auc")
auc
##AUC : 0.6934894


fit2 <- glm(Attrition ~ Age + travel_freq + Department + MonthlyIncome + NumCompaniesWorked
               + PercentSalaryHike + TotalWorkingYears , data = train, family = binomial())
summary(fit2)
##AIC : 2505.3
train$pred2 <- predict(fit2,type = "response")
perf2 <- prediction(train$pred2,train$Attrition)
roc <- performance(perf2,measure = "tpr",x.measure = "fpr")
plot(roc,colorize = T)
auc <- performance(perf2,measure = "auc")
auc
##AUC : 0.6912595



fit3 <- glm(Attrition ~ Age + travel_freq + Department + NumCompaniesWorked
            +  TotalWorkingYears , data = train, family = binomial())
summary(fit3)
##AIC : 2511.1
train$pred3 <- predict(fit3,type = "response")
perf3 <- prediction(train$pred3,train$Attrition)
roc <- performance(perf3,measure = "tpr",x.measure = "fpr")
plot(roc,colorize = T)
auc <- performance(perf3,measure = "auc")
auc
##AUC : 0.6873466


fit4 <- glm(Attrition ~ TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion +
               YearsWithCurrManager, data = train, family = binomial())
summary(fit4)
##AIC : 2585.4
train$pred4 <- predict(fit4,type = "response")
perf4 <- prediction(train$pred4,train$Attrition)
roc <- performance(perf4,measure = "tpr",x.measure = "fpr")
plot(roc,colorize = T)
auc <- performance(perf4,measure = "auc")
auc
##AUC : 0.6499738



fit5 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance
            , data = train, family = binomial())
summary(fit5)
##AIC : 2609.3
train$pred5 <- predict(fit5,type = "response")
perf5 <- prediction(train$pred5,train$Attrition)
roc <- performance(perf5,measure = "tpr",x.measure = "fpr")
plot(roc,colorize = T)
auc <- performance(perf5,measure = "auc")
auc
##AUC : 0.6222341


fit6 <- glm(Attrition ~ Age + travel_freq + Department + NumCompaniesWorked + TotalWorkingYears
            + YearsWithCurrManager + WorkLifeBalance + JobSatisfaction ,data = train ,
                family = binomial())
summary(fit6)
##AIC : 2458.3
train$pred6 <- predict(fit6,type = "response")
perf6 <- prediction(train$pred6,train$Attrition)
roc <- performance(perf6,measure = "tpr",x.measure = "fpr")
plot(roc,colorize = T)
auc <- performance(perf6,measure = "auc")
auc
##AUC : 0.7194597



fit7 <- glm(Attrition ~ travel_freq + Department  + Age + MaritalStatus +NumCompaniesWorked +
        TotalWorkingYears + YearsWithCurrManager + JobSatisfaction + WorkLifeBalance ,data = train ,
            family = binomial())
summary(fit7)
##AIC : 2390.3
train$pred7 <- predict(fit7,type = "response")
perf7 <- prediction(train$pred7,train$Attrition)
roc <- performance(perf7,measure = "tpr",x.measure = "fpr")
plot(roc,colorize = T)
auc <- performance(perf7,measure = "auc")
auc
##AUC : 0.7418812




fit8 <- glm(Attrition ~ travel_freq + Age + NumCompaniesWorked + MaritalStatus +
    YearsWithCurrManager + WorkLifeBalance + JobSatisfaction + EducationField + TotalWorkingYears
            ,data = train,
            family = binomial())
summary(fit8)
##AIC : 2393.3
train$pred8 <- predict(fit8,type = "response")
perf8 <- prediction(train$pred8,train$Attrition)
roc <- performance(perf8,measure = "tpr",x.measure = "fpr")
plot(roc,colorize = T)
auc <- performance(perf8,measure = "auc")
auc
##AUC : 0.7402765





fit9 <- glm(Attrition ~ travel_freq + Age + NumCompaniesWorked + MaritalStatus + JobRole +
  YearsWithCurrManager + WorkLifeBalance + JobSatisfaction + EducationField + TotalWorkingYears
            ,data = train ,
            family = binomial())
summary(fit9)
##AIC : 2389.8
train$pred9 <- predict(fit9,type = "response")
perf9 <- prediction(train$pred9,train$Attrition)
roc <- performance(perf9,measure = "tpr",x.measure = "fpr")
plot(roc,colorize = T)
auc <- performance(perf9,measure = "auc")
auc
##AUC : 0.7503382




fit10 <- glm(Attrition ~ travel_freq + Age + NumCompaniesWorked + MaritalStatus + JobRole + EnvironmentSatisfaction + 
              YearsWithCurrManager + WorkLifeBalance + JobSatisfaction + EducationField + TotalWorkingYears
            ,data = train ,
            family = binomial())
summary(fit10)
##AIC : 2344.2
train$pred10 <- predict(fit10,type = "response")
perf10 <- prediction(train$pred10,train$Attrition)
roc <- performance(perf10,measure = "tpr",x.measure = "fpr")
plot(roc,colorize = T)
auc <- performance(perf10,measure = "auc")
auc
##AUC : 0.7612741





fit11 <- glm(Attrition ~ travel_freq + Age + NumCompaniesWorked + MaritalStatus + JobRole  + EnvironmentSatisfaction + 
    YearsWithCurrManager + WorkLifeBalance + JobSatisfaction + EducationField + TotalWorkingYears +
    PerformanceRating + StockOptionLevel ,data = train ,family = binomial())
summary(fit11)
##AIC : 2339.3
train$pred11 <- predict(fit11,type = "response")
perf11 <- prediction(train$pred11,train$Attrition)
roc <- performance(perf11,measure = "tpr",x.measure = "fpr")
plot(roc,colorize = T)
auc <- performance(perf11,measure = "auc")
auc
##AUC : 0.7639201





