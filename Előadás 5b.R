
# remove data
rm(list = ls())

install.packages("nnet")
install.packages("foreign")
install.packages("reshape2")
install.packages("ggplot2")
library(nnet)
library(foreign)
library(reshape2)
library(ggplot2)

# High School and Beyond survey
hsbdemo <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

# descriptives
table(hsbdemo$prog)

table(hsbdemo$ses, hsbdemo$prog)
prop.table(table(hsbdemo$ses, hsbdemo$prog), 1)

aggregate(hsbdemo$write, list(hsbdemo$prog), FUN=mean)

hsbdemo$prog2 <- relevel(as.factor(hsbdemo$prog), ref = "academic")

# mlogit
mod<-multinom(prog2~ses+write, data=hsbdemo)
summary(mod)
confint(mod)

# example of predicted probabilities
head(fitted(mod))

# predict probabilities at average writing score
dses <- data.frame(ses = c("low", "middle", "high"), write = mean(hsbdemo$write))
predict(mod, newdata = dses, type="probs")

# predict probabilities as a function of writing score for different SES
dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41),
                     write = rep(c(30:70),3))
pp.write <- cbind(dwrite, predict(mod, newdata = dwrite, type = "probs"))
lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
ggplot(lpp, aes(x = write, y = probability, colour = ses)) + geom_line() + facet_grid(variable ~., scales = "free")

