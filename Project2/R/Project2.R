#################################################
################## PROJECT 2 ####################
## Part 1. Introduction to logistic regression ##
#################################################


### PROJECT 2 ###
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(GGally)
library(gridExtra)
library(knitr)
library(tidyr)

# load data
kommuner <- read_excel("data/kommunerProject2.xlsx")
summary(kommuner)

### 1a ###

kommuner |> mutate(highcars = as.numeric(Cars > 600)) -> kommuner

kommuner <- mutate(kommuner,
                   highcars_cat = factor(highcars,
                                         levels = c(0, 1),
                                         labels = c("low", "high")))

ggplot(kommuner, aes(x = Children, y = highcars, color = highcars_cat)) +
  geom_point() +
  labs(title = "Scatter Plot with High Cars Categories",
       x = "Children", y = "High Cars (0/1)",
       color = "Car Category") +
  theme_minimal()


# Part of Sweden: 1 = Götaland; 2 = Svealand; 3 = Norrland
kommuner |> count(Part, highcars_cat) -> count_data
count_data$Part <- factor(count_data$Part, levels = c(1, 2, 3), 
                          labels = c("Götaland", "Svealand", "Norrland"))
table_1a <- pivot_wider(count_data, names_from = highcars_cat, 
                        values_from = n, values_fill = list(n = 0))

# Probability p, odds and log odds
table_1a <- table_1a %>%
  mutate(
    total = high + low,
    p = high / total,
    odds = p / (1 - p),
    log_odds = log(odds)
  )

# Odds ratios and corresponding log odds ratios
reference_odds <- data$odds[data$Part == "Götaland"]
table_1a <- table_1a %>%
  mutate(
    odds_ratio = odds / reference_odds,
    log_odds_ratio = log(odds_ratio)
  )

# When changing from Götaland to Norrland, the odds of having a high number 
# of cars increase significantly. 
table_1a


### 1b ###
# Fit the logistic regression model
model_1b <- glm(highcars_cat ~ Part, family = "binomial", data = kommuner)
model_1b_sum <- summary(model_1b)

# Interval for beta, Odds and Odds Ratio
beta <- model_1b$coefficients
ci.beta <- confint(model_1b)

cbind(b = beta, ci.beta, `exp(b)` = exp(beta), exp(ci.beta)) |> round(digits = 2)

# Wald-based intervals
bhat <- model_1b$coefficients
ci.beta <- confint(model_1b)
cbind(beta = bhat, ci.beta) |> round(digits = 2)

# exp(beta0), exp(beta1)
or = exp(bhat)
ci.or <- exp(ci.beta)
cbind(`exp(beta)` = or, ci.or) |> round(digits = 2)

se.bhat <- summary(model_1b)$coefficients[, "Std. Error"]
ci.wald <- cbind(lo = bhat - 1.96*se.bhat, hi = bhat + 1.96*se.bhat)
ci.wald |> round(digits = 2)

exp(ci.wald) |> round(digits = 2)

## Significance
# Wald test for beta_j
# lambda = 1.96
lambda <- qnorm(1 - 0.05/2)
model_1b_sum$coefficients
b1 <- model_1b_sum$coefficients[2, "Estimate"]
se.b1 <- model_1b_sum$coefficients[2, "Std. Error"]
# z.b1 = -7.08040, 7.08 > 1.96, reject H0
z.b1 <- model_1b_sum$coefficients[2, "z value"]

b0 <- model_1b_sum$coefficients[1, "Estimate"]
se.b0 <- model_1b_sum$coefficients[1, "Std. Error"]
# z.b0 = 4.304683, 4.30 > 1.96, reject H0
z.b0 <- model_1b_sum$coefficients[1, "z value"]

# P-value
# Intercept Pr(>|z|)=1.44e-12 < 0.05
# Part Pr(>|z|) = 1.67e-05 < 0.05
model_1b_sum


# Prediction of model_1b
pred_1b <- cbind(
  kommuner,
  phat = predict(model_1b, type = "response"))

# # Conf.int. for the linear predictor, logodds
pred_1b <- cbind(
  pred_1b,
  logit = predict(model_1b, se.fit = TRUE))
glimpse(pred_1b)

pred_1b |> mutate(logit.residual.scale = NULL) -> pred_1b

lambda <- qnorm(1 - 0.05/2)
pred_1b |> mutate(
  logit.lwr = logit.fit - lambda*logit.se.fit,
  logit.upr = logit.fit + lambda*logit.se.fit) -> pred_1b

# Confidence interval for the odds
pred_1b |> mutate(
  odds.lwr = exp(logit.lwr),
  odds.upr = exp(logit.upr)) -> pred_1b

# Confidence interval for the probabilities
pred_1b |> mutate(
  p = exp(logit.fit)/(1 + exp(logit.fit)),
  p.lwr = odds.lwr/(1 + odds.lwr),
  p.upr = odds.upr/(1 + odds.upr)) -> pred_1b
glimpse(pred_1b)

# Filter and select the rows where Part is 1, 2, or 3
table_1b <- pred_1b %>%
  group_by(Part) %>%
  slice(1) %>%
  filter(Part %in% c(1, 2, 3)) %>%
  select(Part, logit.fit, logit.se.fit, logit.lwr, logit.upr, p, p.lwr, p.upr)


# Display the resulting table
table_1b |> round(digits = 3)


### 1c ###
# Plot with smooth
ggplot(kommuner, aes(Transit, highcars)) +
  geom_point() +
  geom_smooth() +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars") +
  theme(text = element_text(size = 14))

# Fit the logistic regression model for 1c
model_1c <- glm(highcars_cat ~ Transit, family = "binomial", data = kommuner)
model_1c_sum <- summary(model_1c)

# Interval for beta, Odds and Odds Ratio
beta_1c <- model_1c$coefficients
ci.beta_1c <- confint(model_1c)
cbind(b = beta_1c, ci.beta_1c, `exp(b)` = exp(beta_1c), exp(ci.beta_1c)) |> round(digits = 2)

pred_1c <- cbind(
  kommuner,
  phat = predict(model_1c, type = "response"))

ggplot(pred_1c, aes(Transit, highcars)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars",
       caption = "red = fitted line, blue dashed = moving average") +
  theme(text = element_text(size = 14))

# # Conf.int. for the linear predictor, logodds
pred_1c <- cbind(
  pred_1c,
  logit = predict(model_1c, se.fit = TRUE))
glimpse(pred_1c)

pred_1c |> mutate(logit.residual.scale = NULL) -> pred_1c

lambda <- qnorm(1 - 0.05/2)
pred_1c |> mutate(
  logit.lwr = logit.fit - lambda*logit.se.fit,
  logit.upr = logit.fit + lambda*logit.se.fit) -> pred_1c

# Confidence interval for the odds
pred_1c |> mutate(
  odds.lwr = exp(logit.lwr),
  odds.upr = exp(logit.upr)) -> pred_1c

# Confidence interval for the probabilities
pred_1c |> mutate(
  p = exp(logit.fit)/(1 + exp(logit.fit)),
  p.lwr = odds.lwr/(1 + odds.lwr),
  p.upr = odds.upr/(1 + odds.upr)) -> pred_1c
glimpse(pred_1c)


ggplot(pred_1c, aes(Transit, highcars)) +
  geom_point() +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars",
       caption = "red = fitted line, with 95% confidence interval") +
  theme(text = element_text(size = 14))

## Significance
# Wald test for beta_j
# lambda = 1.96
lambda <- qnorm(1 - 0.05/2)
model_1c_sum$coefficients
b1 <- model_1c_sum$coefficients[2, "Estimate"]
se.b1 <- model_1c_sum$coefficients[2, "Std. Error"]
# z.b1 = -6.796793, 6.79 > 1.96, reject H0
z.b1 <- model_1c_sum$coefficients[2, "z value"]

b0 <- model_1c_sum$coefficients[1, "Estimate"]
se.b0 <- model_1b_sum$coefficients[1, "Std. Error"]
# z.b0 = 4.541837, 4.54 > 1.96, reject H0
z.b0 <- model_1c_sum$coefficients[1, "z value"]

# P-value
# Intercept Pr(>|z|)=5.58e-06 < 0.05, reject H0
# Transit Pr(>|z|) = 1.07e-11 < 0.05, reject H0
model_1c_sum

### 1d ###
model_1c_infl <- influence(model_1c)
glimpse(model_1c_infl)

pred_1c <- cbind(kommuner,
                   xbeta = predict(model_1c),
                   v = model_1c_infl$hat)
glimpse(pred_1c)

pplus1_1c <- length(model_1c$coefficients)
n <- nobs(model_1c)

ggplot(pred_1c, aes(x = xbeta, y = v)) +
  geom_point() +
  geom_hline(yintercept = c(2*pplus1_1c/n)) +
  facet_wrap(~ highcars)

pred_1c |> slice_max(v, n = 8)

ggplot(pred_1c, aes(x = Transit, y = v)) +
  geom_point() +
  geom_point(data = filter(pred_1c, v > 0.021), 
             aes(color = "v > 0.021"), size = 3) +
  geom_hline(yintercept = c(1/n, 2*pplus1_1c/n)) +
  facet_wrap(~ highcars) +
  labs(color = "Highlight",
       title = "Leverage vs linear predictor by Y=0/Y=1") +
  theme(legend.position = "top",
        text = element_text(size = 14))

### 1e ###
table(kommuner$Part, kommuner$Transit)

aic <- AIC(model_1b, model_1c)
bic <- BIC(model_1b, model_1c)
collect.AICetc <- data.frame(aic, bic)
collect.AICetc |> mutate(df.1 = NULL) -> collect.AICetc
collect.AICetc

lnL0 <- logLik(model_1b)[1]
lnL0

collect.AICetc |> mutate(
  loglik =  c(logLik(model_1b)[1],
              logLik(model_1c)[1])) -> collect.AICetc
collect.AICetc

collect.AICetc |> mutate(
  R2McF = 1 - loglik/lnL0,
  R2McF.adj = 1 - (loglik - (df - 1)/2)/lnL0) -> collect.AICetc
collect.AICetc
