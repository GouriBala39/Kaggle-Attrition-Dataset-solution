library(ResourceSelection)
library(InformationValue)


hoslem <- hoslem.test(test$Attrition,test$pred)
sprintf("%f",hoslem)
somersD(test$Attrition,test$pred)

print.htest(...,digits = getOption("digits"),...){
  fp <- format.pval(p.value,digits = max(1L,digits -3L))
}



test$Attrition <- ifelse(test$Attrition == "No",0,1)

test$pred <- predict(fit11,newdata = test)
res <- test$Attrition - test$pred
hist(res)
plot(test$pred,jitter(test$Attrition))

library(ggplot2)


