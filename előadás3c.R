
# https://www.urfie.net/downloads/PDF/URfIE_web.pdf
# section 7.3
install.packages("AER")
library(AER)

data(CPS1985,package="AER")

# Table of categories and frequencies for two factor variables:
table(CPS1985$gender)
table(CPS1985$occupation)

# Directly using factor variables in regression formula:
summary(lm(log(wage) ~ education+experience+gender+occupation, data=CPS1985))

# Manually redefine the  reference category:
CPS1985$gender <- relevel(CPS1985$gender,"female")
CPS1985$occupation <- relevel(CPS1985$occupation,"management")

# Rerun regression:
reg<-lm(log(wage) ~ education+experience+gender+occupation, data=CPS1985)
summary(reg)
