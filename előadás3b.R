
# Wooldridge Introductory Econometrics Chapter 8
# created by Peter Elek

data(wage1, package='wooldridge')

library(car); library(lmtest); library(stargazer)

# regression (log wage)
reg<-lm(log(wage) ~ educ+exper+I(exper^2), data=wage1)

# usual standard errors
coeftest(reg)
# heteroscedasticity-robust standard errors (White S.E.)
coeftest(reg, vcov=hccm)
# table
stargazer(list(coeftest(reg),coeftest(reg,vcov=hccm)), type="text", digits=4)

# H0: beta(exper)=beta(exper2)=0
# usual F-test
linearHypothesis(reg, c("exper","I(exper^2)"))
# heteroscedasticity-robust F-test
linearHypothesis(reg, c("exper","I(exper^2)"),vcov=hccm)

# testing for heteroscedasticity: are robust standard errors necessary?
# Breusch-Pagan test
bptest(reg)
# special form of White-test
bptest(reg, ~ fitted(reg) + I(fitted(reg)^2))

# heteroscedasticity is not very strong
plot(wage1$educ, reg$residuals, cex=0.5)


# regression (wage level)
reglevel<-lm(wage ~ educ+exper+I(exper^2), data=wage1)

# usual standard errors
coeftest(reglevel)
# heteroscedasticiy-robust standard errors (White)
coeftest(reglevel, vcov=hccm)
# table
stargazer(list(coeftest(reglevel),coeftest(reglevel,vcov=hccm)), type="text", digits=4)

# testing for heteroscedasticity
bptest(reglevel)

# illustrate that heteoscedasticity is present
plot(wage1$educ, reglevel$residuals, cex=0.5)

