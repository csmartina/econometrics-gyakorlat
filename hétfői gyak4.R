rm(list = ls())
library(car)
library(lmtest)
library(stargazer)

data(mlb1, package='wooldridge')
attach(mlb1)
hist(salary)
mean(salary)
min(salary)
max(salary)
#35% sample
size_count <- round(0.35*nrow(mlb1),0)
mlb2 <- mlb1[sample(1:nrow(mlb1),size_count,replace=FALSE),]
model1 <- lm(lsalary ~ years+gamesyr+bavg+hrunsyr+rbisyr,data=mlb1)
model2 <- lm(lsalary ~ years+gamesyr+bavg+hrunsyr+rbisyr,data=mlb2)
stargazer(model1,model2,type="text")
model3 <- lm(lsalary ~years+gamesyr,data = mlb1)
stargazer(model1,model3,type = "text")
anova(model1,model3)
linearHypothesis(model1,c("bavg=0","hrunsyr=0","rbisyr=0"))
cor(hrunsyr,rbisyr)
model4 <- lm(lsalary ~ years+gamesyr+bavg+hrunsyr,data=mlb1)
stargazer(model1,model4,type="text")

model5 <- lm(lsalary ~ years+gamesyr+hrunsyr,data=mlb1)

anova(model1,model5)

summary(model1)

bptest(model5) #-> heteroscedastic
#White standard errors
coeftest(model5,vcov=hccm)
summary(model5)

resid_m5 <- resid(model5)
fitted_m5 <- fitted(model5)
plot(fitted_m5,resid_m5,xlab="Fitted values",ylab="Residuals")

rm(list = ls())
#Wooldridge, Chapter 6, C3.
data(wage2,package='wooldridge')
attach(wage2)

#iii.
educ_exper <- educ*exper
attach(wage2)

model1 <- lm(lwage ~ educ+exper+educ_exper)
summary(model1)

#iv.
educ_exper_10 <- educ*exper-10*educ
attach(wage2)
model2 <- lm(lwage ~ educ+exper+educ_exper_10)
summary(model2)
confint(model2)

rm(list=ls())
