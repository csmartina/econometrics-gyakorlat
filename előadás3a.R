rm(list = ls())

# Wooldridge Introductory Econometrics Chapter 6
# based on urfie.net
# https://www.urfie.net/downloads/PDF/URfIE_web.pdf
# modified by Peter Elek


library(car); library(lmtest); library(stargazer)

data(wage1, package='wooldridge')

############################

# model with quadratics
reg.quadr<-lm(log(wage)~educ+exper+I(exper^2), data=wage1)
summary(reg.quadr)

# educ parameter
coef(reg.quadr)["educ"]
# interpretation of educ parameter: 
# percentage effect of one more year of education on wage
100*(exp(coef(reg.quadr)["educ"])-1)
# not a large difference from the simple multiplication by 100
100*coef(reg.quadr)["educ"]

# quadratic effect of exper
# where does it attain the maximum?
-coef(reg.quadr)["exper"]/(2*coef(reg.quadr)["I(exper^2)"])

# how does the function look like?
install.packages("effects")
library(effects)
plot(effect("exper", reg.quadr))
# at educ=8, 12 and 16
plot(effect("exper", reg.quadr, fixed.predictors=list(given.values=c(educ=8)),se=FALSE))
plot(effect("exper", reg.quadr, fixed.predictors=list(given.values=c(educ=12)),se=FALSE))
plot(effect("exper", reg.quadr, fixed.predictors=list(given.values=c(educ=16)),se=FALSE))

# create the function manually for educ=8, 12 and 16
predlogwage<-matrix(nrow=50, ncol=3)
for (i in 1:3) {
  X <- data.frame(educ=4*(i+1),exper=seq(1,50))
  predlogwage[,i] <- predict(reg.quadr, X)
}
matplot(X$exper, predlogwage, type="l", lwd=2, ylab="log(wage)", xlab="experience")
legend("bottom", legend=c("educ=8", "educ=12", "educ=16"),
       col=1:3, lty=1:3, lwd=2)

# obtain directly the marginal effect at exper=17, at the average experience 
# only the linear term and the intercept term change
summary(lm(log(wage)~educ+I(exper-17)+I((exper-17)^2), data=wage1))

############################

# linear terms without interaction
summary(lm(log(wage)~educ+exper, data=wage1))

# with interaction
summary(lm(log(wage)~educ+exper+educ:exper, data=wage1))
regint1<-(lm(log(wage)~educ*exper, data=wage1))

coeftest(regint1, vcov=hccm)

# reparametrization of the interaction model
regint2<-(lm(log(wage)~I(educ-8)*exper, data=wage1))
coeftest(regint2, vcov=hccm)

