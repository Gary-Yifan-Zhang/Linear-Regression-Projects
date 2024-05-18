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
library(tidyverse)
library(caret)
library(pROC)
# library(MASS)
library(ResourceSelection)
library(ggrepel)


# load data
kommuner <- read_excel("data/kommunerProject2.xlsx")
summary(kommuner)

##### 1a #####

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
reference_odds <- table_1a$odds[table_1a$Part == "Götaland"]
table_1a <- table_1a %>%
  mutate(
    odds_ratio = odds / reference_odds,
    log_odds_ratio = log(odds_ratio)
  )

# When changing from Götaland to Norrland, the odds of having a high number 
# of cars increase significantly. 
table_1a


##### 1b #####
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


##### 1c #####
# Plot with smooth
ggplot(kommuner, aes(Transit, highcars)) +
  geom_point() +
  geom_smooth() +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars") +
  theme(text = element_text(size = 14))

# Fit the logistic regression model for 1c
model_1c <- glm(highcars ~ Transit, family = "binomial", data = kommuner)
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
se.b0 <- model_1c_sum$coefficients[1, "Std. Error"]
# z.b0 = 4.541837, 4.54 > 1.96, reject H0
z.b0 <- model_1c_sum$coefficients[1, "z value"]

# P-value
# Intercept Pr(>|z|)=5.58e-06 < 0.05, reject H0
# Transit Pr(>|z|) = 1.07e-11 < 0.05, reject H0
model_1c_sum

# ???LR-test against the null model
D_diff <- model_1c_sum$null.deviance - model_1c_sum$deviance
df_diff <- model_1c_sum$df.null - model_1c_sum$df.residual
chi2_alpha <- qchisq(p = 1 - 0.05, df = df_diff)
Pvalue <- pchisq(q = D_diff, df = df_diff, lower.tail = FALSE)

cbind(D_diff, df_diff, chi2_alpha, Pvalue)



##### 1d #####
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

##### 1e #####
table(kommuner$Part, kommuner$Transit)

# Model Null
model_null <- glm(highcars ~ 1, family = "binomial", data = kommuner)

aic <- AIC(model_1b, model_1c)
bic <- BIC(model_1b, model_1c)
collect.AICetc <- data.frame(aic, bic)
collect.AICetc |> mutate(df.1 = NULL) -> collect.AICetc
collect.AICetc

lnL0 <- logLik(model_null)[1]
lnL0

collect.AICetc |> mutate(
  loglik =  c(logLik(model_1b)[1],
              logLik(model_1c)[1])) -> collect.AICetc
collect.AICetc

collect.AICetc |> mutate(
  R2McF = 1 - loglik/lnL0,
  R2McF.adj = 1 - (loglik - (df - 1)/2)/lnL0) -> collect.AICetc
collect.AICetc


#############################################################
#############################################################
## Part 2. Variable selection and influential observations ##
#############################################################
#############################################################

##### 2(a). Imputation of missing data #####
kommuner <- read_excel("Data/kommunerProject2.xlsx")
summary(kommuner)

kommuner |> mutate(highcars = as.numeric(Cars > 600)) -> kommuner

kommuner <- mutate(kommuner,
                   highcars_cat = factor(highcars,
                                         levels = c(0, 1),
                                         labels = c("low", "high")))

kommuner |> mutate(Fertility = as.numeric(Fertility)) -> kommuner

kommuner |> filter(Part == 3 & Coastal == 0) |>
  summarise(meanfertility = mean(Fertility, na.rm = TRUE))
I <- which(is.na(kommuner$Fertility))
kommuner$Fertility[I] <- 1.57 # meanfertility = 1.57


##### 2(b). Variable selection #####
model_full <- glm(highcars ~ log(Higheds) + Children + Seniors + log(Income) + 
                    log(GRP) + Persperhh + Fertility + Urban + Transit + log(Apartments), 
                  family = "binomial", 
                  data = kommuner)
summary(model_full)
model_full_sum <- summary(model_full)
vif(model_full)

# Model Null
model_null <- glm(highcars ~ 1, family = "binomial", data = kommuner)

# AIC stepwise selection
model_aic <- step(model_null,
                       scope = list(lower = model_null, upper = model_full),
                       direction = "both",
                       trace = TRUE,  # trace=TRUE detail information for every steps
                       k = 2)  # k=2 means using AIC

model_aic_sum <- summary(model_aic)

summary(model_aic)
confint(model_aic)


# BIC stepwise selection
model_bic <- step(model_null,
                       scope = list(lower = model_null, upper = model_full),
                       direction = "both",
                       trace = TRUE,
                       k =  log(nobs(model_full)))  # BIC
model_bic_sum <- summary(model_bic)

# VIF
vif(model_aic)
vif(model_bic)

## Significance
# Wald test for beta_j
# lambda = 1.96
lambda <- qnorm(1 - 0.05/2)
model_aic_sum$coefficients

# Fertility
b6 <- model_aic_sum$coefficients[6, "Estimate"]
se.b6 <- model_aic_sum$coefficients[6, "Std. Error"]
# z.b6 = -1.882, 1.882 < 1.96, reject H1
z.b6 <- model_aic_sum$coefficients[6, "z value"]

# Transit
b0 <- model_aic_sum$coefficients[7, "Estimate"]
se.b0 <- model_aic_sum$coefficients[7, "Std. Error"]
# z.b0 = -1.721, 1.721 < 1.96, reject H1
z.b0 <- model_aic_sum$coefficients[7, "z value"]

# P-value
# Fertility Pr(>|z|)=0.05978  > 0.05, reject H1
# Transit Pr(>|z|) = 0.08518 > 0.05, reject H1
model_aic_sum

# LR-test
# D_bic - D_aic
D_diff <- model_bic_sum$deviance - model_aic_sum$deviance
df_diff <- model_bic_sum$df.residual - model_aic_sum$df.residual
cbind(D_diff, df_diff)

# P-value: 0.07175513 > 0.05, reject H1
chi2_alpha <- qchisq(1 - 0.05, df_diff)
Pvalue <- pchisq(D_diff, df_diff, lower.tail = FALSE)
cbind(D_diff, df_diff, chi2_alpha, Pvalue)

### AIC and BIC ###
aic <- AIC(model_aic,model_bic)
bic <- BIC(model_aic,model_bic)
collect.AICetc <- data.frame(aic, bic)
collect.AICetc |> mutate(df.1 = NULL) -> collect.AICetc
collect.AICetc

### Pseudo R2 ###
model_null_glm <- glm(highcars ~ 1, family = "binomial", data = kommuner)

# log likelihood
lnL0 <- logLik(model_null_glm)[1]
collect.AICetc |> mutate(
  loglik =  c(logLik(model_aic)[1],
              logLik(model_bic)[1])) -> collect.AICetc
collect.AICetc

# McFadden
collect.AICetc |> mutate(
  R2McF = 1 - loglik/lnL0,
  R2McF.adj = 1 - (loglik - (df - 1)/2)/lnL0) -> collect.AICetc
collect.AICetc

# Choose BIC model as the best model
model_2b <- model_bic


##### 2(c). Influential observations #####
model_2b_infl <- influence(model_2b)
glimpse(model_2b_infl)

# Get the linear predictor and the hat values for the Model 2b
model_2b_pred <- cbind(kommuner,
                   xbeta = predict(model_2b),
                   v = model_2b_infl$hat,
                   D = cooks.distance(model_2b))
glimpse(model_2b_pred)

# Plot leverage against the linear predictor with horizontal line at 2*(p+1)/n
pplus1_2b <- length(model_2b$coefficients)
n <- nobs(model_2b)

ggplot(model_2b_pred, aes(x = xbeta, y = v)) +
  geom_point() +
  geom_hline(yintercept = c(2*pplus1_2b/n)) +
  facet_wrap(~ highcars)

# Find the highest leverages
highest_leverages <- model_2b_pred %>%
  slice_max(v, n = 8)
model_2b_pred |> slice_max(v, n = 8)

ggplot(model_2b_pred, aes(x = xbeta, y = v)) +
  geom_point() +
  geom_point(data = filter(model_2b_pred, v > 0.06), aes(color = "v > 0.06"), size = 3) +
  geom_point(data = highest_leverages, aes(color = "Highest Leverages"), size = 4) +
  geom_text_repel(data = filter(model_2b_pred, v > 0.06), aes(label = Kommun), 
            nudge_x = -3,
            nudge_y = 0.01,
            hjust = 0, color = "blue") +
  geom_hline(yintercept = c(2*pplus1_2b/n)) +
  geom_hline(yintercept = 0.06, linetype = "dashed", color = "red") +
  facet_wrap(~ highcars) +
  labs(color = "Highlight", title = "Leverage vs linear predictor by Y=0/Y=1", 
       caption = "v > 0.06 = Red points, Highest Leverages = Blue points\n
       Dashed red line: v = 0.06, Horizontal line: 2*pplus1_2b/n") +
  theme(legend.position = "top", text = element_text(size = 14))

# Cook's Distance 
model_2b_pred <- mutate(model_2b_pred, 
                    Dcook = cooks.distance(model_2b))

top_cooks <- kommuner_pred_DFBETAS %>%
  arrange(desc(D)) %>%
  slice(1:6)

# Filter kummor with Cook's Distance > 4/n
high_cooks <- filter(model_2b_pred, Dcook > 4/n) 

# Create a table of kummor and Cook's Distance above threshold
high_cooks <- high_cooks[, c("Kommun", "D")]

# Print the table
high_cooks

# DFBETAS of Municipalities with top cook's distance 
ggplot(model_2b_pred, aes(x = xbeta, y = Dcook, color = as.factor(highcars))) +
  geom_point() +
  geom_point(data = filter(model_2b_pred, v > 0.06), shape = 24,
             color = "black", size = 3) +
  geom_point(data = top_cooks, shape = 19,
             color = "red", size = 3) +
  geom_hline(yintercept = 4/n) +
  #  geom_hline(yintercept = 1) +
  facet_grid(cols = vars(highcars)) +
  labs(color = "Y = 0/1",
       title = "Scatter plot of Cook's Distance against xbeta",
       x = "xbeta",
       y = "Cook's Distance",
       caption = "Black points represent outliers with v > 0.06.\n
       Red points indicate top Cook's Distance municipalities") +
  geom_text_repel(data = top_cooks,
            aes(label = Kommun),
            nudge_x = -6,
            nudge_y = 0,
            hjust = 0) +
  theme(axis.text.x = element_blank())

high_cook_municipalities <- filter(model_2b_pred, Dcook > 4/n) %>%
  select(Kommun)

f1.kommuner <- pplus1_2b
f2.kommuner <- model_2b$df.residual
cook.limit.kommuner <- qf(0.1, f1.kommuner, f2.kommuner)

kommuner_pred_DFBETAS <- mutate(
  model_2b_pred,
  df0 = dfbetas(model_2b)[, "(Intercept)"],
  df1 = dfbetas(model_2b)[, "Urban"],
  df2 = dfbetas(model_2b)[, "log(Apartments)"],
  df3 = dfbetas(model_2b)[, "Persperhh"],
  df4 = dfbetas(model_2b)[, "log(Income)"],
  fit = predict(model_2b),
  r = rstudent(model_2b),
  D = cooks.distance(model_2b))

top_cooks <- kommuner_pred_DFBETAS %>%
  arrange(desc(D)) %>%
  slice(1:6)

# DFBETAS of Municipalities with top cook's distance 
top_cooks_DFBETAS <- kommuner_pred_DFBETAS %>%
  arrange(desc(D)) %>%
  slice(1:6) %>%
  select(Kommun, D, df0, df1, df2, df3, df4)

topcooks_DFBETAS <- top_cooks_DFBETAS[, c("Kommun", "D", "df0", "df1", "df2", "df3", "df4")]

topcooks_DFBETAS_round <- topcooks_DFBETAS
topcooks_DFBETAS_round[, -1] <- round(topcooks_DFBETAS[, -1], 3)

print(topcooks_DFBETAS_round)


highlightshapes <- c("Cook's D>0.1" = 24)
highlightcolors <- c("|r*|>3" = "red",
                     "length>200" = "magenta", 
                     "all data" = "orange",
                     "excl.length>200" = "blue")

# Top influential municipalities in DFBETAS
top_influential <- kommuner_pred_DFBETAS %>%
  arrange(desc(abs(df2))) %>%
  arrange(desc(abs(df2))) %>%
  slice(1:6)

# Change the y axis into df0 ~ df4, check if the resulting plot 
# is the same as the previous plot of log(PM10) vs variables.
ggplot(kommuner_pred_DFBETAS, aes(x = fit, y = df4)) +
  geom_point(size = 2) +
  geom_point(data = filter(kommuner_pred_DFBETAS, abs(r) > 3),
             aes(color = "|r*|>3"), size = 3) +
  geom_point(data = filter(kommuner_pred_DFBETAS, D > 0.1),
             aes(shape = "Cook's D>0.1"), size = 3) +
  geom_point(data = top_cooks, color = "green", size = 4) +
  geom_text(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "blue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.kommuner) * c(-1, 1), color = "red") +
  geom_hline(yintercept = 2 / sqrt(n) * c(-1, 1), color = "red", linetype = "dashed") +
  ylab("DFBETAS for log(Income)") +
  xlab("Fitted values") +
  labs(title = "Impact on the log(Income) by Municipality",
       subtitle = "Highlighting municipalities with significant influence",
       caption = "Red lines indicate critical values for influence") +
  theme(text = element_text(size = 18), 
        legend.position = "bottom") +
  scale_color_manual(values = highlightcolors) +
  scale_shape_manual(values = highlightshapes)

dfbetas_rounded <- round(dfbetas(model_2b), 4)
head(dfbetas_rounded)

##### 2(d). Deviance residuals #####
model_2b_pred |> mutate(devresid = model_2b_infl$dev.res,
                    stddevresid = devresid/sqrt(1 - v)) -> model_2b_pred
glimpse(model_2b_pred)

# QQ-plot of the standardized deviance residuals
ggplot(model_2b_pred, aes(sample = stddevresid)) +
  geom_qq() + geom_qq_line()

top_cooks <- model_2b_pred %>%
  arrange(desc(D)) %>%
  slice(1:6)

large_res <- model_2b_pred %>% filter(stddevresid > 3 | stddevresid < -3)

# Plot the standardized deviance residuals against the linear predictor
ggplot(model_2b_pred, aes(x = xbeta, 
                          y = stddevresid, 
                          color = as.factor(highcars))) +
  geom_point() +
  geom_point(data = top_cooks, color = "blue", size = 4) +
  geom_text(data = top_cooks,
            aes(label = Kommun),
            nudge_x = -3,
            nudge_y = 0.01,
            hjust = 0) +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), linetype = "dashed") +
  labs(title = "Plot of Standardized Deviance Residuals against Linear Predictor",
       x = "Linear Predictor (xbeta)",
       y = "Standardized Deviance Residuals",
       color = "Y = 0/1",
       caption = "Blue points represent top Cook's Distance municipalities\n
       Horizontal lines at y = ±2, ±3")

# Plot the standardized deviance residuals vs Urban
# We need to plot all the x-variables
# Urban   log(Apartments)    Persperhh  log(Income)
ggplot(model_2b_pred, aes(x = Persperhh , y = stddevresid, 
                      color = as.factor(highcars))) +
  geom_point() +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), linetype = "dashed") +
  geom_point(data = top_cooks, color = "green", size = 4) +
  labs(title = "Plot of Standardized Deviance Residuals against Persperhh",
       x = "Persperhh",
       y = "Standardized Deviance Residuals",
       color = "Y = 0/1",
       caption = "Green points represent top Cook's Distance municipalities\n
       Horizontal lines at y = ±2, ±3") +
  geom_text(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "blue") 


#############################################################
#############################################################
################# Part 3. Goodness-of-fit ###################
#############################################################
#############################################################

##### 3(a). Confusion #####


pred_phat <- cbind(
  kommuner,
  p_null = predict(model_null, type = "response"),
  p_1b = predict(model_1b, type = "response"), 
  p_1c = predict(model_1c, type = "response"), 
  p_aic = predict(model_aic, type = "response"), 
  p_bic = predict(model_bic, type = "response"), 
  p_full = predict(model_full, type = "response")
)
glimpse(pred_phat)

pred_phat |> mutate(
  yhat_kommuner = factor(p_1c > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high"))) -> pred_phat

cm_bic <- confusionMatrix(
  data = pred_phat$yhat_kommuner, 
  reference = pred_phat$highcars_cat,
  positive = "high")
cm_bic

# 
model_predictions <- pred_phat |> mutate(
  yhat_null = factor(p_null > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_1b = factor(p_1b > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_1c = factor(p_1c > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_bic = factor(p_bic > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_aic = factor(p_aic > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_full = factor(p_full > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high"))
)

# Get confusion matrix for all the models
cm_null <- confusionMatrix(data = model_predictions$yhat_null, reference = model_predictions$highcars_cat, positive = "high")
cm_1b <- confusionMatrix(data = model_predictions$yhat_1b, reference = model_predictions$highcars_cat, positive = "high")
cm_1c <- confusionMatrix(data = model_predictions$yhat_1c, reference = model_predictions$highcars_cat, positive = "high")
cm_bic <- confusionMatrix(data = model_predictions$yhat_bic, reference = model_predictions$highcars_cat, positive = "high")
cm_aic <- confusionMatrix(data = model_predictions$yhat_aic, reference = model_predictions$highcars_cat, positive = "high")
cm_full <- confusionMatrix(data = model_predictions$yhat_full, reference = model_predictions$highcars_cat, positive = "high")

### Confusion matrix ###
# Create a table to collect the metrics
# Extract needed statistics from each confusion matrix 
extract_stats <- function(cm) { 
  list( Accuracy = cm$overall['Accuracy'], 
  P_Value_Acc_Greater_NIR = cm$overall['AccuracyPValue'], 
  Cohen_Kappa = cm$overall['Kappa'], 
  P_Value_McNemars = cm$overall['McnemarPValue'], 
  Sensitivity = cm$byClass['Sensitivity'], 
  Specificity = cm$byClass['Specificity'] ) } 

# Apply the function to all models 
stats_null <- extract_stats(cm_null) 
stats_1b <- extract_stats(cm_1b) 
stats_1c <- extract_stats(cm_1c) 
stats_bic <- extract_stats(cm_bic) 
stats_aic <- extract_stats(cm_aic) 
stats_full <- extract_stats(cm_full) 

# Combine all stats into a data frame 
table_3a <- data.frame( 
  Model = c("Null", "1b", "1c", "BIC", "AIC", "Full"), 
  Accuracy = c(stats_null$Accuracy, stats_1b$Accuracy, 
               stats_1c$Accuracy, stats_bic$Accuracy, 
               stats_aic$Accuracy, stats_full$Accuracy),
  AccuracyPValue = c(stats_null$P_Value_Acc_Greater_NIR, 
                  stats_1b$P_Value_Acc_Greater_NIR, 
                  stats_1c$P_Value_Acc_Greater_NIR, 
                  stats_bic$P_Value_Acc_Greater_NIR, 
                  stats_aic$P_Value_Acc_Greater_NIR, 
                  stats_full$P_Value_Acc_Greater_NIR), 
  Kappa = c(stats_null$Cohen_Kappa, 
                  stats_1b$Cohen_Kappa, 
                  stats_1c$Cohen_Kappa, 
                  stats_bic$Cohen_Kappa, 
                  stats_aic$Cohen_Kappa, 
                  stats_full$Cohen_Kappa), 
  McnemarPValue = c(stats_null$P_Value_McNemars, 
                       stats_1b$P_Value_McNemars, 
                       stats_1c$P_Value_McNemars, 
                       stats_bic$P_Value_McNemars, 
                       stats_aic$P_Value_McNemars, 
                       stats_full$P_Value_McNemars), 
  Sensitivity = c(stats_null$Sensitivity, stats_1b$Sensitivity, 
                  stats_1c$Sensitivity, stats_bic$Sensitivity, 
                  stats_aic$Sensitivity, stats_full$Sensitivity), 
  Specificity = c(stats_null$Specificity, stats_1b$Specificity, 
                  stats_1c$Specificity, stats_bic$Specificity, 
                  stats_aic$Specificity, stats_full$Specificity) )
table_3a

##### 3(b). ROC-curves and AUC #####
roc_null <- roc(highcars ~ p_null, data = pred_phat)
roc_null
glimpse(roc_null)

roc_bic <- roc(highcars ~ p_bic, data = pred_phat)
roc_bic
coords(roc_bic) |> head()
ggroc(roc_bic) +
  coord_fixed() +
  labs(title = "ROC-curve for model bic")

ggroc(list(null = roc_null, BIC = roc_bic)) +
  coord_fixed() +
  labs(title = "ROC-curves for model oslo and the null model")

# Find best threshold for bic 
youden <- coords(roc_bic, "best")
youden

topleft <- coords(roc_bic, "best", best.method = "closest.topleft")
topleft

youden$name <- "youden"
topleft$name = "topleft"

ggroc(list(null = roc_null, oslo = roc_bic), linewidth = 1) +
  geom_point(data = topleft, aes(x = specificity, y = sensitivity), size = 3) +
  geom_point(data = youden, aes(x = specificity, y = sensitivity), size = 3) +
  coord_fixed() +
  labs(title = "ROC-curve for model oslo",
       subtitle = "with optimal thresholds") +
  theme(text = element_text(size = 18))


# ROC-curves for all the models
roc_null <- roc(highcars ~ p_null, data = pred_phat)
roc_1b <- roc(highcars ~ p_1b, data = pred_phat)
roc_1c <- roc(highcars ~ p_1c, data = pred_phat)
roc_aic <- roc(highcars ~ p_aic, data = pred_phat)
roc_bic <- roc(highcars ~ p_bic, data = pred_phat)
roc_full <- roc(highcars ~ p_full, data = pred_phat)


ggroc(list( `Model Null` = roc_null, `Model 1b` = roc_1b, `Model 1c` = roc_1c, 
           `Model AIC` = roc_aic, `Model BIC` = roc_bic, `Model Full` = roc_full),
      linewidth = 1) +
  coord_fixed() +
  labs(title = "ROC-curves for all the models") +
  theme(text = element_text(size = 14))

# AUC for all models
aucs <- 
  data.frame(
    model = c("Model Null", "Model 1b", "Model 1c", "Model AIC", "Model BIC", "Model Full"),
    auc = c(auc(roc_null), auc(roc_1b), auc(roc_1c), auc(roc_aic),
            auc(roc_bic), auc(roc_full)),
    lwr = c(ci(roc_null)[1], ci(roc_1b)[1],
            ci(roc_1c)[1], ci(roc_aic)[1],
            ci(roc_bic)[1], ci(roc_full)[1]),
    upr = c(ci(roc_null)[3], ci(roc_1b)[3],
            ci(roc_1c)[3], ci(roc_aic)[3],
            ci(roc_bic)[3], ci(roc_full)[3]))
aucs

roc_2b <- roc_bic

### pair-wise tests comparing the AUC ###
# Perform ROC tests
test_2b_vs_null = roc.test(roc_null, roc_2b)
test_2b_vs_1b = roc.test(roc_1b, roc_2b)
test_2b_vs_1c = roc.test(roc_1c, roc_2b)
test_2b_vs_aic = roc.test(roc_aic, roc_2b)
test_2b_vs_full = roc.test(roc_full, roc_2b)

# Create a data frame to store the results
roc_comparison_results <- data.frame(
  Comparison = c("Model 2b vs Null", "Model 2b vs 1b", "Model 2b vs 1c", "Model 2b vs AIC", "Model 2b vs Full"),
  p_Value = c(test_2b_vs_null$p.value, test_2b_vs_1b$p.value, test_2b_vs_1c$p.value, test_2b_vs_aic$p.value, test_2b_vs_full$p.value),
  Test_Statistic = c(test_2b_vs_null$statistic, test_2b_vs_1b$statistic, test_2b_vs_1c$statistic, test_2b_vs_aic$statistic, test_2b_vs_full$statistic),
  Alternative_Hypothesis = rep(test_2b_vs_null$alternative, 5)
)

roc_comparison_results

##### 3(c). Optimal thresholds #####
extract_metrics <- function(roc_obj) { 
  youden <- coords(roc_obj, "best") 
  topleft <- coords(roc_obj, "best", best.method = "closest.topleft") 
  data.frame( Threshold_Youden = youden$threshold, 
              Sensitivity_Youden = youden$sensitivity, 
              Specificity_Youden = youden$specificity, 
              Threshold_TopLeft = topleft$threshold, 
              Sensitivity_TopLeft = topleft$sensitivity, 
              Specificity_TopLeft = topleft$specificity ) } 

# Calculate metrics for all required models 
metrics_1b <- extract_metrics(roc_1b) 
metrics_1c <- extract_metrics(roc_1c) 
metrics_aic <- extract_metrics(roc_aic) 
metrics_bic <- extract_metrics(roc_bic) 
metrics_full <- extract_metrics(roc_full) 

# Combine all metrics into a single data frame 
roc_metrics <- data.frame( 
  Model = c("1b", "1c", "AIC", "BIC", "Full"), 
  rbind(metrics_1b, metrics_1c, metrics_aic, metrics_bic, metrics_full) 
  ) 

# Display the table 
roc_metrics

### Table 3c using optimal thresholds
model_predictions_3c <- pred_phat |> mutate(
  yhat_null = factor(p_null > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_1b = factor(p_1b > 0.1655906, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_1c = factor(p_1c > 0.1977642, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_bic = factor(p_bic > 0.2253320, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_aic = factor(p_aic > 0.3066619, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_full = factor(p_full > 0.2452759, levels = c(FALSE, TRUE), labels = c("low", "high"))
)

# Get confusion matrix for all the models
cm_null_3c <- confusionMatrix(data = model_predictions_3c$yhat_null, reference = model_predictions_3c$highcars_cat, positive = "high")
cm_1b_3c <- confusionMatrix(data = model_predictions_3c$yhat_1b, reference = model_predictions_3c$highcars_cat, positive = "high")
cm_1c_3c <- confusionMatrix(data = model_predictions_3c$yhat_1c, reference = model_predictions_3c$highcars_cat, positive = "high")
cm_bic_3c <- confusionMatrix(data = model_predictions_3c$yhat_bic, reference = model_predictions_3c$highcars_cat, positive = "high")
cm_aic_3c <- confusionMatrix(data = model_predictions_3c$yhat_aic, reference = model_predictions_3c$highcars_cat, positive = "high")
cm_full_3c <- confusionMatrix(data = model_predictions_3c$yhat_full, reference = model_predictions_3c$highcars_cat, positive = "high")

### Confusion matrix ###
# Create a table to collect the metrics
# Extract needed statistics from each confusion matrix 
extract_stats <- function(cm) { 
  list( Accuracy = cm$overall['Accuracy'], 
        P_Value_Acc_Greater_NIR = cm$overall['AccuracyPValue'], 
        Cohen_Kappa = cm$overall['Kappa'], 
        P_Value_McNemars = cm$overall['McnemarPValue'], 
        Sensitivity = cm$byClass['Sensitivity'], 
        Specificity = cm$byClass['Specificity'] ) } 

# Apply the function to all models 
stats_null_3c <- extract_stats(cm_null_3c) 
stats_1b_3c <- extract_stats(cm_1b_3c) 
stats_1c_3c <- extract_stats(cm_1c_3c) 
stats_bic_3c <- extract_stats(cm_bic_3c) 
stats_aic_3c <- extract_stats(cm_aic_3c) 
stats_full_3c <- extract_stats(cm_full_3c) 

# Combine all stats into a data frame 
table_3c <- data.frame( 
  Model = c("Null", "1b", "1c", "BIC", "AIC", "Full"), 
  Accuracy = c(stats_null_3c$Accuracy, stats_1b_3c$Accuracy, 
               stats_1c_3c$Accuracy, stats_bic_3c$Accuracy, 
               stats_aic_3c$Accuracy, stats_full_3c$Accuracy),
  AccuracyPValue      = c(stats_null_3c$P_Value_Acc_Greater_NIR, 
                  stats_1b_3c$P_Value_Acc_Greater_NIR, 
                  stats_1c_3c$P_Value_Acc_Greater_NIR, 
                  stats_bic_3c$P_Value_Acc_Greater_NIR, 
                  stats_aic_3c$P_Value_Acc_Greater_NIR, 
                  stats_full_3c$P_Value_Acc_Greater_NIR), 
  Cohen_Kappa = c(stats_null_3c$Cohen_Kappa, 
                  stats_1b_3c$Cohen_Kappa, 
                  stats_1c_3c$Cohen_Kappa, 
                  stats_bic_3c$Cohen_Kappa, 
                  stats_aic_3c$Cohen_Kappa, 
                  stats_full_3c$Cohen_Kappa), 
  McnemarPValue  = c(stats_null_3c$P_Value_McNemars, 
                       stats_1b_3c$P_Value_McNemars, 
                       stats_1c_3c$P_Value_McNemars, 
                       stats_bic_3c$P_Value_McNemars, 
                       stats_aic_3c$P_Value_McNemars, 
                       stats_full_3c$P_Value_McNemars), 
  Sensitivity = c(stats_null_3c$Sensitivity, stats_1b_3c$Sensitivity, 
                  stats_1c_3c$Sensitivity, stats_bic_3c$Sensitivity, 
                  stats_aic_3c$Sensitivity, stats_full_3c$Sensitivity), 
  Specificity = c(stats_null_3c$Specificity, stats_1b_3c$Specificity, 
                  stats_1c_3c$Specificity, stats_bic_3c$Specificity, 
                  stats_aic_3c$Specificity, stats_full_3c$Specificity) )
table_3c

### Best Model: AIC

summary(model_aic)
ggpairs(kommuner,columns=c(8,11,12,13,14,15))

# Model car
model_car <- glm(highcars ~ Cars, family = "binomial", data = kommuner)
pred_phat_car <- cbind(
  kommuner,
  p_car = predict(model_car, type = "response")
)
glimpse(pred_phat_car)

pred_phat_car |> mutate(
  yhat_kommuner = factor(p_car > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high"))) -> pred_phat_car

cm_car <- confusionMatrix(
  data = pred_phat_car$yhat_kommuner, 
  reference = pred_phat_car$highcars_cat,
  positive = "high")
cm_car


