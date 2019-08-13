library(MASS)

test1 <- glm(Attrition ~ ., df, family = binomial())
test2 <- glm(Attrition ~ 1, df,family = binomial())


stepAIC(test2,direction="backward")


stepAIC(test2,direction="forward",scope=list(upper=test1,lower=test2))


stepAIC(test2,direction="both",scope=list(upper=test1,lower=test2))
