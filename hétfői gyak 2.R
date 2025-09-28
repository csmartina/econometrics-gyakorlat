rm(list = ls())

install.packages("wooldridge")

data(wage1,package='wooldridge')
attach(wage1)

#1.
mean(wage)
median(wage)
sd(wage)
quantile(wage,prob=c(0.25,.5,.75))

#2.
boxplot(wage)
boxplot(lwage)

boxplot(educ)

hist(female)

#3.
plot(educ,wage)
plot(exper,wage)
plot(tenure,wage)

#4.
#male - 95% CI
lower_male <- mean(wage[female==0])-qnorm(0.975)*sd(wage[female==0])/sqrt(nrow(wage1))
upper_male <- mean(wage[female==0])+qnorm(0.975)*sd(wage[female==0])/sqrt(nrow(wage1))

#female - 95% CI
lower_female <- mean(wage[female==1])-qnorm(0.975)*sd(wage[female==1])/sqrt(nrow(wage1))
upper_female <- mean(wage[female==1])+qnorm(0.975)*sd(wage[female==1])/sqrt(nrow(wage1))

cat("95% CI for males:",lower_male,"-",upper_male,"\n")
cat("95% CI for females:",lower_female,"-",upper_female)

#5.
cor(wage1)
new_data <- data.frame(wage,educ,exper,tenure)
cor(new_data)

#Wooldridge, Chapter 1, C2.
data(bwght,package='wooldridge')
attach(bwght)

#i.
nrow(bwght)
sum(cigs>0)

#ii.
mean(cigs)

#iii.
mean(cigs[cigs>0])

#iv.
mean(fatheduc, na.rm=TRUE)

#v.
mean(faminc)*1000
sd(faminc)*1000

#Wooldridge, Chapter 2, C2.
data(ceosal2,package='wooldridge')
attach(ceosal2)

#i.
mean(salary)
mean(ceoten)

#ii.
sum(ceoten==0)
max(ceoten)

#iii.
model1 <- lm(lsalary ~ ceoten)
summary(model1)

#Wooldridge, Chapter 2, C3.
data(sleep75,package='wooldridge')
attach(sleep75)

#i.
model1 <- lm(sleep ~ totwrk)
summary(model1)

cor(sleep,totwrk)^2
cov(sleep,totwrk)/var(totwrk)
