
# Wooldridge Introductory Econometrics Chapter 6
# created by Peter Elek

data(bwght, package='wooldridge')
names(bwght)

# transformation from ounces to grams
bwght$bwghtgr<-bwght$bwght*28.35

# fatheduc is missing for many observations
summary(bwght)

# so we omit those where it is missing 
# (not necessary but makes things simple)
# there can be much more elegant solutions for this issue
bwght<-bwght[!is.na(bwght$fatheduc),]
bwght<-bwght[!is.na(bwght$motheduc),]

# different regressions
reg1<-lm(bwghtgr~cigs, data=bwght)
reg2<-lm(bwghtgr~cigs+male+parity+white, data=bwght)
reg3<-lm(bwghtgr~cigs+male+parity+white+faminc, data=bwght)
reg4<-lm(bwghtgr~cigs+male+parity+white+fatheduc, data=bwght)
reg5<-lm(bwghtgr~cigs+male+parity+white+fatheduc+motheduc, data=bwght)  
reg6<-lm(bwghtgr~cigs+male+parity+white+faminc+fatheduc+motheduc, data=bwght)
library(stargazer)
stargazer(list(reg1,reg2,reg3,reg4,reg5,reg6),type="text", digits = 3)

library(car)
linearHypothesis(reg5, c("fatheduc","motheduc"))

# AIC and BIC
cbind(AIC(reg1),AIC(reg2),AIC(reg3),AIC(reg4),AIC(reg5))
cbind(BIC(reg1),BIC(reg2),BIC(reg3),BIC(reg4),BIC(reg5))

# in-sample fit from the "best" model
pred4conf<-predict(reg4, interval="conf")
pred4pred<-predict(reg4, interval="pred")
# example
pred4conf[1:5,]
pred4pred[1:5,]

# plotted in-sample fit from the simple model
pred1conf<-predict(reg1, interval="conf")
pred1pred<-predict(reg1, interval="pred")
matplot(bwght$cigs,pred1conf, pch=1, cex=0.5)
matplot(bwght$cigs,pred1pred, pch=1, cex=0.5)

# prediction on new data
predict(reg4, newdata=data.frame(cigs=0,male=1,parity=1,white=0,fatheduc=12), interval="conf")
predict(reg4, newdata=data.frame(cigs=0,male=1,parity=1,white=0,fatheduc=12), interval="pred")

# residuals
plot(bwght$faminc, reg4$residuals, cex=0.5)
hist(reg4$residuals)

# useful to detach at the end
detach(bwght)



