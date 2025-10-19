
# Wooldridge exercises Ch.7/C.8., Ch.17/C.2. 

# remove data
rm(list = ls())

library(lmtest)
library(car)
library(stargazer)
library(mfx)
install.packages("estimatr")
library(estimatr)  # for robust SE
install.packages("mfx")
data(loanapp,package='wooldridge')
attach(loanapp)

# descriptives
tab_appr_wh<-table(white, approve)
prop.table(tab_appr_wh, margin=1)

# LPM
# usual standard errors are used, 
# although, strictly speaking, White S.E-s are necessary,
model1 <- lm(approve ~ white)
summary(model1)
model2 <- lm(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr)
summary(model2)
stargazer(model1,model2,type="text")

# LPM, using robust SE (do not matter much)
lpmrob<-lm_robust(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, se_type = "HC1")
summary(lpmrob)

# predicted probabilities from LPM
par(mar = c(5, 5, 0.5, 2))
hist(model2$fitted.values, prob = TRUE,
     xlab = "Predicted probability", main = "")

# logit and probit
logitmodel <- glm(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,family=binomial(link=logit))
summary(logitmodel)
probitmodel <- glm(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,family=binomial(link=probit))
summary(probitmodel)

# predicted probabilities from logit
par(mar = c(5, 5, 0.5, 2))
hist(logitmodel$fitted.values, prob = TRUE,
     xlab = "Predicted probability", main = "")

# OR
exp(coef(logitmodel))

# APE
logitmfx(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data=loanapp, atmean=FALSE)
probitmfx(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data=loanapp, atmean=FALSE)

# PEA
logitmfx(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data=loanapp, atmean=TRUE)

# McFadden's pseudo R2
1 - logitmodel$deviance/logitmodel$null.deviance

# Overall significance
lrtest(logitmodel)

# in-sample sensitivity and specificity at cutoff 0.5
# (in sample - should define training and test samples for estimation and prediction) 
logit_classif <- ifelse(logitmodel$fitted.values > 0.5, 1, 0)
crtab<-table(model.frame(logitmodel)$approve, logit_classif)
crtab
prop.table(crtab, margin=1)

# cutoff 0.87 (average of approve)
logit_classif <- ifelse(logitmodel$fitted.values > 0.87, 1, 0)
crtab<-table(model.frame(logitmodel)$approve, logit_classif)
crtab
prop.table(crtab, margin=1)

