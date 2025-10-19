install.packages("wooldridge")
library(wooldridge)
#Wooldridge, Chapter 17, C1.

data("pntsprd",package = "wooldridge")
attach(pntsprd)
hist(spread)
boxplot(spread)

model1 <- lm(favwin ~ spread)
summary(model1)
library(car)
linearHypothesis(model1,"(Intercept)=0.5")
bptest(model1)
install.packages("lmtest")
library(lmtest)
resid_model1 <- resid(model1)
fitted_model1 <- fitted(model1)
plot(fitted_model1,resid_model1)
install.packages("estimatr")
library(estimatr)
model2 <- lm_robust(favwin ~ spread)
summary(model2)
linearHypothesis(model2,"(Intercept)=0.5")
x <- data.frame(spread=10)
pred_lpm_spread10 <- predict(model2,x)
pred_lpm_spread10
model3 <- glm(favwin~spread, family=binomial(link=probit))
summary(model3)
pred_probit_spread10 <- predict(model3,x)
pred_probit_spread10
pred_probit_spread0 <- predict(model3,data.frame(spread=0),type = "response")
pred_probit_spread0
pred_probit_spread120 <- predict(model3,x,type = "response")
pred_probit_spread120
model4 <- glm(favwin~spread, family=binomial(link=logit))
summary(model4)
pred_logit_spread0 <- predict(model4,data.frame(spread=0),type = "response")
pred_logit_spread0
pred_logit_spread120 <- predict(model4,x,type = "response")
pred_logit_spread120
install.packages("stargazer")
library(stargazer)
stargazer(model1,model3,model4,type="text")
model5 <- glm(favwin~spread + favhome + fav25 + und25, family=binomial(link=probit))
summary(model5)
lrtest(model5,model3)
