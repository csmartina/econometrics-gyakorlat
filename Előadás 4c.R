#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.

# Chapter 14
# CH14B Predicting AirBnB apartment prices: selecting a regression model
# using the airbnb dataset
# version 0.92 2021-07-05
#########################################################################################

# modified by P?ter Elek

# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

install.packages("tidyverse")
install.packages("caret")
install.packages("skimr")
install.packages("grid")
install.packages("glmnet")
install.packages("stargazer")
install.packages("xtable")
install.packages("directlabels")
install.packages("knitr")
install.packages("cowplot")
# Descriptive statistics and regressions
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)

data<-read.csv(paste0("/Users/Shared/Files From d.localized/corvinus/econometrics/gyak/airbnb_hackney_work_book.csv"), stringsAsFactors = FALSE)

data <- data %>%
  filter(n_accommodates < 8
  )

#####################################
# Look at some descriptive statistics
#####################################

#How is the average price changing in my district by `property_type`, `room_type` and the `bed_type`?
data %>%
  group_by(f_property_type, f_room_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

data %>%
  group_by(f_bed_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

Hmisc::describe(data$price)

install.packages("Hmisc")
library(Hmisc)

# NB all graphs, we exclude  extreme values of price
datau <- subset(data, price<400)

# Distribution of price by type below 400

# Histograms
# price
g3a<-ggplot(datau, aes(price)) +
  geom_histogram(binwidth = 25, alpha = 0.8, size = 0.25) +
  ylab("count") +
  xlab("Price")
g3a

# lnprice
g3b <- ggplot(datau, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, alpha = 0.8, size = 0.25) +
  ylab("Count") +
  xlab("Log price")
g3b

## Boxplot of price by room type
g4 <- ggplot(data = datau, aes(x = f_room_type, y = price)) +
  stat_boxplot(aes(group = f_room_type), geom = "errorbar", width = 0.3,
               size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_room_type),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Room type",y = "Price (US dollars)")
g4

# Boxplot
g5 <- ggplot(datau, aes(x = factor(n_accommodates), y = price,
                        fill = factor(f_property_type), color=factor(f_property_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  labs(x = "Accomodates (Persons)",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 400), breaks = seq(0,400, 50)) +
  theme(legend.position = c(0.3,0.8)        )
g5

#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("n_accommodates", "n_beds", "f_property_type", "f_room_type", "n_days_since", "flag_days_since")

# Factorized variables
basic_add <- c("f_bathroom","f_cancellation_policy","f_bed_type")
reviews <- c("f_number_of_reviews","n_review_scores_rating", "flag_review_scores_rating")
# Higher orders
poly_lev <- c("n_accommodates2", "n_days_since2", "n_days_since3")

#not use p_host_response_rate due to missing obs

# Dummy variables: Extras -> collect all options and create dummies
amenities <-  grep("^d_.*", names(data), value = TRUE)

#################################################
# Look for interactions
################################################

# a function looking for interactions

price_diff_by_variables2 <- function(df, factor_var, dummy_var, factor_lab, dummy_lab){
  # Looking for interactions.
  # It is a function it takes 3 arguments: 1) Your dataframe,
  # 2) the factor variable (like room_type)
  # 3)the dummy variable you are interested in (like TV)
  
  # Process your data frame and make a new dataframe which contains the stats
  factor_var <- as.name(factor_var)
  dummy_var <- as.name(dummy_var)
  
  stats <- df %>%
    group_by(!!factor_var, !!dummy_var) %>%
    dplyr::summarize(Mean = mean(price, na.rm=TRUE),
                     se = sd(price)/sqrt(n()))
  
  stats[,2] <- lapply(stats[,2], factor)
  
  ggplot(stats, aes_string(colnames(stats)[1], colnames(stats)[3], fill = colnames(stats)[2]))+
    geom_bar(stat='identity', position = position_dodge(width=0.9), alpha=0.8)+
    geom_errorbar(aes(ymin=Mean-(1.96*se),ymax=Mean+(1.96*se)),
                  position=position_dodge(width = 0.9), width = 0.25)+
    ylab('Mean Price')+
    xlab(factor_lab) +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line(),
          legend.position = "top",
          #legend.position = c(0.7, 0.9),
          legend.box = "vertical",
          legend.text = element_text(size = 5),
          legend.title = element_text(size = 5, face = "bold"),
          legend.key.size = unit(x = 0.4, units = "cm")
    )
}

#Look up room type interactions
price_diff_by_variables2(data, "f_room_type", "d_familykidfriendly", "Room type", "Family kid friendly")
price_diff_by_variables2(data, "f_room_type", "f_property_type", "Room type", "Property type")
#Look up canelation policy
price_diff_by_variables2(data, "f_cancellation_policy", "d_familykidfriendly", "Cancellation policy", "Family kid friendly")
price_diff_by_variables2(data, "f_cancellation_policy", "d_tv", "Cancellation policy", "TV")
#Look up property type
price_diff_by_variables2(data, "f_property_type", "d_cats", "Property type", "Cats")
price_diff_by_variables2(data, "f_property_type", "d_dogs", "Property type", "Dogs")

# dummies suggested by graphs
X1  <- c("f_room_type*f_property_type",  "f_room_type*d_familykidfriendly")

# Additional interactions of factors and dummies
X2  <- c("d_airconditioning*f_property_type", "d_cats*f_property_type", "d_dogs*f_property_type")
X3  <- c(paste0("(f_property_type + f_room_type + f_cancellation_policy + f_bed_type) * (",
                paste(amenities, collapse=" + "),")"))

# Create models in levels models: 1-8
modellev1 <- " ~ n_accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev, basic_add,reviews),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities,X3),collapse = " + "))

#################################
# Separate hold-out set #
#################################

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(data))

# Set the random number generator: It will make results reproducable
set.seed(20180123)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$holdout <- 0
data$holdout[holdout_ids] <- 1

#Hold-out set Set
data_holdout <- data %>% filter(holdout == 1)

#Working data set
data_work <- data %>% filter(holdout == 0)

#Example for model coefficients

model_name <-  paste0("modellev",2)
model_pretty_name <- paste0("(",2,")")
yvar <- "price"
xvars <- eval(parse(text = model_name))
formula <- formula(paste0(yvar,xvars))
regexample<-(lm(formula,data = data_work))
summary(regexample)

##############################
#      cross validation      #
##############################

## N = 5
n_folds=5
# Create the folds
set.seed(20180124)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()


for (i in (1:8)){
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("(",i,")")
  
  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))
  
  # Initialize values
  rmse_train <- c()
  rmse_test <- c()
  
  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared
  
  # Do the k-fold estimation
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    # Train sample: all except test_i
    data_train <- data_work[-test_i, ]
    # Test sample
    data_test <- data_work[test_i, ]
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)
    
    # Criteria evaluation
    rmse_train[k] <- mean((prediction_train - data_train[,yvar])^2)^(1/2)
    rmse_test[k] <- mean((prediction_test - data_test[,yvar])^2)^(1/2)
    
  }
  
  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}

model <- lm(formula,data = data_train)
prediction_train <- predict(model, newdata = data_train)
prediction_test <- predict(model, newdata = data_test)

skim(data_train$ln_days_since)

t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1
column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE",
                  "Test RMSE")

# R2, BIC on full work data-n.
# In sample rmse: average on training data; avg test : average on test data

# RMSE training vs test graph
t1_levels <- t1 %>%
  dplyr::select("nvars", "rmse_train", "rmse_test") %>%
  gather(var,value, rmse_train:rmse_test) %>%
  mutate(nvars2=nvars+1) %>%
  mutate(var = factor(var, levels = c("rmse_train", "rmse_test"),
                      labels = c("RMSE Training","RMSE Test")))

model_result_plot_levels <- ggplot(data = t1_levels,
                                   aes(x = factor(nvars2), y = value, color=factor(var), group = var)) +
  geom_line(size=1,show.legend=FALSE, na.rm = TRUE) +
  scale_y_continuous(name = "RMSE", limits = c(26, 50), breaks = seq(26,50, 2)) +
  scale_x_discrete( name = "Number of coefficients", expand=c(0.01, 0.01)) +
  geom_dl(aes(label = var),  method = list("last.points", dl.trans(x=x-1), cex=0.4))
model_result_plot_levels

