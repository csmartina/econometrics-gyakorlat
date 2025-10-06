
# Wooldridge Introductory Econometrics Chapter 7
# based on urfie.net
# modified by Peter Elek

library(car)

data(wage1, package='wooldridge')

# models with a single dummy independent variable
summary(lm(log(wage) ~ female, data=wage1))
summary(lm(log(wage) ~ female+educ, data=wage1))
summary(lm(log(wage) ~ female+educ+exper+I(exper^2)+tenure+I(tenure^2), data=wage1))

# model with additive gender and marriage effect
summary(lm(log(wage) ~ female+married+educ+exper+I(exper^2)+tenure+I(tenure^2), data=wage1))

# interaction between female and married
summary(lm(log(wage)~married*female+educ+exper+I(exper^2)+tenure+I(tenure^2), data=wage1))
summary(lm(log(wage)~married+female+married:female+educ+exper+I(exper^2)+tenure+I(tenure^2), data=wage1))

# the same model 
wage1$marrmale=wage1$married*(1-wage1$female)
wage1$marrfem=wage1$married*wage1$female
wage1$singfem=(1-wage1$married)*wage1$female
summary(lm(log(wage) ~ marrmale+marrfem+singfem+educ+exper+I(exper^2)+tenure+I(tenure^2), data=wage1))

# interaction between female and educ (allowing for different slopes)
summary(lm(log(wage)~female*educ+exper+I(exper^2)+tenure+I(tenure^2), data=wage1))

# the same with educ centered at 12.5 years
summary(lm(log(wage)~female*I(educ-12.5)+exper+I(exper^2)+tenure+I(tenure^2), data=wage1))

# plot
regfemeduc<-lm(log(wage)~educ+female*educ+exper+I(exper^2)+tenure+I(tenure^2), data=wage1)
predlogwage<-matrix(nrow=19, ncol=2)
for (i in 1:2) {
  X <- data.frame(female=i-1, educ=seq(0,18),exper=mean(wage1$exper), tenure=mean(wage1$tenure))
  predlogwage[,i] <- predict(regfemeduc, X)
}
matplot(X$educ, predlogwage, type="l", lwd=2, ylab="log(wage)", xlab="education")
legend("topleft", legend=c("male", "female"), col=1:2, lty=1:2, lwd=2)

# differences in regression functions across groups 
regfull<-lm(log(wage)~female*educ+female*exper+female*tenure, data=wage1)
linearHypothesis(regfull, c("female:educ", "female:exper", "female:tenure"))
linearHypothesis(regfull, c("female", "female:educ", "female:exper", "female:tenure"))


