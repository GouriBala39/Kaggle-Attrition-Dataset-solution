getwd()
setwd("C:/Users/GOURI/Desktop/dataset int")
list.files()
employee_info <- read.csv("general_data.csv")
employee_survey <- read.csv("employee_survey_data.csv")
manager_survey <- read.csv("manager_survey_data.csv")
options(max.print=5000)

intime <- read.csv("in_time.csv")
outtime <- read.csv("out_time.csv")


class(intime)
View(outtime)
outtime <- na.omit(outtime) ##omitting NA values
intime <- na.omit(intime)  ##omitting NA values


##files <- list.files(pattern = ".csv")
##files <- NULL

##require(purrr)
##install.packages("purrr")
##install.packages("dplyr")
##library(dplyr)
##df <- files %>% map_df(read.csv)

df <- cbind.data.frame(employee_info,employee_survey,manager_survey) ##merging excel files into 1
indx <- which(duplicated(names(df))) ##integer vector storing duplicate columns 
df <- df[,-indx]

df <- na.omit(df)  ##removing NA from the data frame
df <- df[,c(7,1:6,8:28)]  ##rearranging data frame

df$single <- ifelse(df$MaritalStatus == "Single",1,0)  ##creating a dummy variable

                ## HYPOTHESIS TESTING

t.test(employee_info$Age ~ employee_info$Attrition)

employee_info$travel_rarely <- 
      ifelse(employee_info$BusinessTravel== "Travel_Rarely",1,0)  ## Dummy variable

employee_info$travel_freq <- 
  ifelse(employee_info$BusinessTravel== "Travel_Frequently",2,0)  ## Dummy variable

employee_info$no_travel <- 
  ifelse(employee_info$BusinessTravel== "Non-Travel",3,0)        ## Dummy variable

employee_info$BusinessTravel <- NULL
employee_info$EmployeeCount <- NULL
employee_info$Over18 <- NULL

chisq.test(employee_info$Attrition,employee_info$Department)

t.test(employee_info$DistanceFromHome ~ employee_info$Attrition)

boxplot(employee_info$DistanceFromHome ~ employee_info$Attrition)

chisq.test(employee_info$Attrition,employee_info$Education)


chisq.test(employee_info$Attrition,employee_info$EducationField)


chisq.test(employee_info$Attrition,employee_info$Gender)


chisq.test(employee_info$Attrition,employee_info$JobLevel)

chisq.test(employee_info$Attrition,employee_info$JobRole)


chisq.test(employee_info$Attrition,employee_info$MaritalStatus)

t.test(employee_info$MonthlyIncome ~ employee_info$Attrition)

t.test(employee_info$NumCompaniesWorked ~ employee_info$Attrition)

t.test(employee_info$PercentSalaryHike ~ employee_info$Attrition)

employee_info$StandardHours <- NULL

t.test(employee_info$StockOptionLevel ~ employee_info$Attrition)

t.test(employee_info$TotalWorkingYears ~ employee_info$Attrition)

t.test(employee_info$TrainingTimesLastYear ~ employee_info$Attrition)

t.test(employee_info$YearsAtCompany ~ employee_info$Attrition)

t.test(employee_info$YearsSinceLastPromotion ~ employee_info$Attrition)

t.test(employee_info$YearsWithCurrManager ~ employee_info$Attrition)


out <- aov(employee_info$MonthlyIncome ~ employee_info$Attrition)
summary(out)

wilcox.test(employee_info$MonthlyIncome ~ employee_info$Attrition)


out <- aov(employee_survey$WorkLifeBalance ~ employee_info$Attrition)
summary(out)

library(ggplot2)

ggplot(employee_info) + 
  geom_histogram(aes(x = DistanceFromHome)) + 
  facet_grid(Attrition ~ .)


ggplot(df,aes(y = Attrition))+
  geom_point(aes(x = Age,color = factor(Department))
             ,size = 2)


boxplot(employee_info$Age ~ employee_info$Attrition)

employee_info$Attrition <-
  ifelse(employee_info$Attrition == "No",0,1)

