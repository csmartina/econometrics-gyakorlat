library(readxl)
health <- read_excel('/Users/Shared/Files From d.localized/corvinus/econometrics/beadandó/health_small.xlsx')
attach(health)
health |> 
  filter(YEAR == 1997)
filter(.data = health, YEAR == 1997)
health_1997 <- filter(.data = health, YEAR == 1997)

#First task

#a) Plot DALE as a function of GDPC and (in a separate plot) DALE as a function of the logarithm of GDPC. Interpret the plots.

ggplot(data = health_1997,
       mapping = aes(y = DALE,
                     x = GDPC)) +
  geom_point()

ggplot(data = health_1997,
       mapping = aes(y = DALE,
                     x = log(GDPC))) +
  geom_point()

health_1997 <- health_1997 |> 
  mutate(log_GDPC = log(GDPC))

ggplot(data = health_1997,
       mapping = aes(y = DALE,
                     x = log_GDPC)) +
  geom_point()
#b) Estimate with OLS a model where DALE is the dependent variable and the logarithm of GDPC is the explanatory variable (besides, of course, the constant).
model1 <- lm(DALE ~ log_GDPC, data = health_1997)
summary(model1)

#c) Draw the scatterplot of the residuals and the logarithm of GDPC. What do you think: are the residuals homoscedastic or heteroscedastic?
ggplot(data = health_1997,
       mapping = aes(y = resid(model1),
                     x = log_GDPC)) +
  geom_point()

plot(data = health_1997, log_GDPC, resid(model1),
     xlab = "log_GDPC",
     ylab = "Residuals",
     main = "Residuals vs log(GDP per capita)")
abline(h = 0, col = "red", lty = 2)

ggplot(health_1997, aes(x = log(GDPC), y = resid(model1))) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs log(GDP per capita)",
    x = "log(GDP per capita)",
    y = "Residuals"
  ) +
  theme_minimal()
#d) Calculate the usual and the heteroscedasticity-robust standard errors of the estimated parameters of the above model estimated with OLS. 

coeftest(model1)

install.packages("wooldridge")
library(wooldridge)
coeftest(model1)
install.packages("lmtest")
library(lmtest)
coef(model1)
coeftest(model1, vcov=hccm)

stargazer(list(coeftest(model1),coeftest(model1,vcov=hccm)), type="text", digits=4)
# Install (only once)
install.packages("stargazer")

# Load it
library(stargazer)

model1 <- lm(DALE ~ log_GDPC, data = health_1997)
summary(model1)
beta <- coef(model1)["log_GDPC"]
se_robust <- sqrt(diag(hccm(model1)))["log_GDPC"]

lower <- beta - qnorm(0.975) * se_robust
upper <- beta + qnorm(0.975) * se_robust

c(lower = lower, upper = upper)
t_stat <- beta / se_robust
t_stat
df <- nrow(health_1997) - length(coef(model1))  # n - k
p_value_t <- 2 * (1 - pt(abs(t_stat), df = df))
p_value_t
if (p_value_t < 0.01) {
  print("Reject H0: coefficient is significant at 1% level")
} else {
  print("Fail to reject H0: coefficient is NOT significant at 1% level")
}

# simple scatter plot about ceo salary and roe
ggplot(data = health_1997,
       mapping = aes(y = DALE,
                     x = log_GDPC)) +
  geom_point()

# save "main settings"
DALE_log_GDPC_plot <- ggplot(data = health_1997,
                          mapping = aes(y = DALE,
                                        x = log_GDPC))

# add a scatter plot and linear line to the main settings -- akkor is ez a megoldás ha errort ír
DALE_log_GDPC_plot +
  geom_point() +
  geom_smooth(method = lm)

model2 <- lm(DALE ~ log_GDPC, data = health_1997)


coef_part1 <- coef(model1)["log_GDPC"]
coef_part3 <- coef(model3)["log_GDPC"]
coef_gini <- coef(model3)["GINI"]
delta <- coef(lm(GINI ~ log_GDPC, data = health_1997))["log_GDPC"]

lhs <- coef_part1
rhs <- coef_part3 + coef_gini * delta

lhs
rhs
