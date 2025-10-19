
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

##################

# 95% CI for prediction
plot(effect("exper", reg.quadr, fixed.predictors=list(given.values=c(educ=12))), 
     main="", xlab="experience (year)")

#################

# 95% prediction interval and CI for prediction

eff <- effect("exper", reg.quadr, 
              fixed.predictors = list(given.values = c(educ = 12)), xlevels = list(exper = 200))
df <- as.data.frame(eff)

# Residual standard deviation (sigma) from model
sigma <- sigma(reg.quadr)

# Compute prediction intervals:
# sqrt(se_mean^2 + sigma^2) is the standard error for new observation
df$lower_PI <- df$fit - 1.96 * sqrt(df$se^2 + sigma^2)
df$upper_PI <- df$fit + 1.96 * sqrt(df$se^2 + sigma^2)

df$wage_fit <- (df$fit)
df$wage_lower <- df$lower_PI
df$wage_upper <- df$upper_PI

# Plot using base graphics
plot(df$exper, df$wage_fit, type = "l", lwd = 2, col = "blue",
     ylim = range(-0.5, df$wage_upper),
     xlab = "experience (year)",
     ylab="log(wage)",
     main = "")
lines(df$exper, df$wage_lower, lty = 2, lwd=2, col="darkgrey")
lines(df$exper, df$wage_upper, lty = 2, lwd=2, col="darkgrey")
lines(df$exper, df$lower, lty = 2, lwd=2, col="skyblue")
lines(df$exper, df$upper, lty = 2, lwd=2, col="skyblue")

legend("bottom",
       legend = c("Predicted wage", "95% CI (mean)", "95% PI (individual)"),
       lty = c(1, 2, 2), lwd = c(2, 2, 2),
       col = c("blue", "skyblue", "darkgrey"),
       bty = "n")

########################

# residual plot

plot(reg.quadr$fitted.values, resid(reg.quadr),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "", cex=0.5)
abline(h = 0, col = "red", lwd = 2)

############################

# predicting wage from the log(wage) model

mean(reg.quadr$fitted.values)
mean(log(wage1$wage))

mean(exp(reg.quadr$fitted.values))
mean(wage1$wage)

exp(sigma^2/2)*mean(exp(reg.quadr$fitted.values))

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


