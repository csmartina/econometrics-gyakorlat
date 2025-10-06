library(tidyverse)
library(wooldridge)

attend <- attend

mean(attend$atndrte)
min(attend$atndrte)
max(attend$atndrte)

attend |>
  select(atndrte, priGPA, ACT) |>
  summary()

summary(attend$atndrte)
summary(attend$priGPA)

model_attend <- lm(atndrte ~ priGPA + ACT,
                   data = attend)
summary(model_attend)

priGPA_value <- 3.65
ACT_value <- 20

model_attend$coefficients
model_attend$coefficients[1] + model_attend$coefficients

prediction_values <- tibble(priGPA = priGPA_value,
                            ACT = ACT_value)
####

htv <- htv
diff(range(htv$educ))

htv |>
  filter(educ == 12) |>
  summarise(n = n()) |>
#####
  
mean(htv$educ)
mean(htv$motheduc)
mean(htv$fatheduc)

model_htv <-  lm(educ ~ motheduc + fatheduc,
                 data = htv)
summary(model_htv)

model_htv2 <- lm(educ ~ motheduc + fatheduc + abil,
                 data = htv)
summary(model_htv2)
model_htv3 <- lm(educ ~ motheduc + fatheduc + abil + I(abil^2),
                 data = htv)
summary(model_htv3)
