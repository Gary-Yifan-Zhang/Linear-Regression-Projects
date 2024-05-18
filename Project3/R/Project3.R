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
kommuner <- read_excel("data/kommunerProject3.xlsx")
summary(kommuner)

kommuner |> mutate(Fertility = as.numeric(Fertility)) -> kommuner

kommuner |> filter(Part == 3 & Coastal == 0) |>
  summarise(meanfertility = mean(Fertility, na.rm = TRUE))
I <- which(is.na(kommuner$Fertility))
kommuner$Fertility[I] <- 1.57 # meanfertility = 1.57

coast <- factor(kommuner$Coastal, labels = c("No", "Yes"))
kommuner <- mutate(kommuner, Coastal = coast)
kommuner$Part <- factor(kommuner$Part, labels = c("Gotaland", "Svealand", "Norrland"))

kommuner <-
  mutate(kommuner, NewParts =
           as.numeric(Part == "Gotaland" | Coastal == "Yes") +
           #            as.numeric(Part == "Gotaland") +
           2*as.numeric(Part == "Svealand" & Coastal == "No") +
           3*as.numeric(Part == "Norrland" & Coastal == "No"))
kommuner$NewParts <- factor(kommuner$NewParts, labels = c("GotalandYes", "SvealandNo",
                                                          "NorrlandNo"))


ggplot(kommuner, aes(x = Cars_nbr, y = log(Apartments))) + geom_point() + 
  xlab("Vehicles (1000/capita)") +
  ylab("PM10 (g)") +
  labs(title = "PM10: by amount of vehicles") +
  theme(text = element_text(size = 18))

ggplot(kommuner, aes(x = log(Cars_nbr), y = log(Apartments))) + geom_point() + 
  xlab("Vehicles (1000/capita)") +
  ylab("PM10 (g)") +
  labs(title = "PM10: by amount of vehicles") +
  theme(text = element_text(size = 18))


model_full <- glm(Cars_nbr ~ log(Higheds) + Children + Seniors + log(Income) + 
                    log(GRP) + Persperhh + Fertility + Urban + Transit + log(Apartments)
                  +Builton + log(Vehicles) + Population + NewParts, 
                  family = "poisson", 
                  data = kommuner)
summary(model_full)
model_full_sum <- summary(model_full)
vif(model_full)
ggpairs(kommuner,columns=c(5,6,7,8,9,10,11,14,15,16,17,18,20))

model_test <- glm(log(Cars_nbr) ~ log(Higheds) + Seniors + log(Income) + 
                    log(GRP)  + Fertility + Urban + Transit + log(Apartments)
                  +Builton + log(Population) + NewParts, 
                  family = "poisson", 
                  data = kommuner)
summary(model_test)

vif(model_test)


model_Popuation <- glm(log(Cars_nbr) ~ log(Population), 
                       family = "poisson", 
                       data = kommuner)
summary(model_Popuation)

Popuation_pred <- cbind(
  kommuner,
  xb = predict(model_Popuation, se.fit = TRUE))
Popuation_pred |> mutate(
  xb.residual.scale = NULL,
  xb.lwr = xb.fit - 1.96*xb.se.fit,
  xb.upr = xb.fit + 1.96*xb.se.fit,
  muhat = exp(xb.fit),
  mu.lwr = exp(xb.lwr),
  mu.upr = exp(xb.upr)) -> Popuation_pred


glimpse(Popuation_pred)

ggplot(Popuation_pred, aes(log(population), log(Cars_nbr))) +
  geom_point() +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.5) +
  labs(title = "Expected number of Cars",
       caption = "95% confidence interval") +
  theme(text = element_text(size = 18))


model_lm <- lm(log(Cars_nbr) ~ log(Higheds), data=kommuner)
summary(model_lm)

model_Urban <- glm(log(Cars_nbr) ~ log(Higheds), 
                  family = "poisson", 
                  data = kommuner)
summary(model_Urban)
model_Urban_sum <- summary(model_Urban)

b0 <- model_Urban_sum$coefficients[2, "Estimate"]


# Model Null
model_null <- glm(log(Cars_nbr) ~ 1, family = "poisson", data = kommuner)

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

full_pred <- cbind(
  kommuner,
  xb = predict(model_Urban, se.fit = TRUE))
full_pred |> mutate(
  xb.residual.scale = NULL,
  xb.lwr = xb.fit - 1.96*xb.se.fit,
  xb.upr = xb.fit + 1.96*xb.se.fit,
  muhat = exp(xb.fit),
  mu.lwr = exp(xb.lwr),
  mu.upr = exp(xb.upr)) -> full_pred


glimpse(full_pred)

ggplot(full_pred, aes(log(Higheds), log(Cars_nbr))) +
  geom_point() +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.5) +
  labs(title = "Expected number of Cars",
       caption = "95% confidence interval") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ Part)

full_pred |> mutate(v = hatvalues(model_Urban)) -> full_pred

pplus1 <- length(model_Popuation$coefficients)
n <- nobs(model_Popuation)

full_pred <- mutate(kommuner,
                    yhat_linear = predict(model_test),    
                    yhat = predict(model_test),
                    r = rstudent(model_test),
                    v = hatvalues(model_test),
                    D = cooks.distance(model_test))

full_pred_DFBETAS <- mutate(
  full_pred,
  fit = predict(model_test),
  r = rstudent(model_test),
  D = cooks.distance(model_test))

ggplot(full_pred, aes(x = yhat, y = v)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "Leverage",
       color = "program", caption = "horizontal line = 2(p+1)/n") +
  theme(text = element_text(size = 18)) 

################################################################################
# 3(b). Cook’s distance. 
f1.kommuner <- pplus1
f2.kommuner <- model_full$df.residual
cook.limit.kommuner <- qf(0.1, f1.kommuner, f2.kommuner)

top_cooks <- full_pred %>%
  arrange(desc(D)) %>%
  slice(1:6)

ggplot(full_pred, aes(x = yhat, y = D)) + 
  geom_point(size = 3) +
  geom_point(data = top_cooks, color = "red", size = 4) +  
  geom_text(data = top_cooks, aes(label = Kommun), vjust = -1, color = "blue") + 
  #geom_hline(yintercept = cook.limit.kommuner, color = "blue") +  
  geom_hline(yintercept = 4/n, linetype = 2, color = "red") +  
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Kommuner: Cook's D",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18))

# 3(c). Studentized residuals.
filter(full_pred, abs(r) > 3)

top_cooks <- full_pred %>%
  arrange(desc(D)) %>%
  slice(1:6)

# Plot studentized residuals vs fitted values
ggplot(full_pred, aes(x = yhat, y = r)) +
  geom_point(size = 2) +  
  geom_point(data = top_cooks, aes(color = "Top Cook's D"), size = 4) + 
  geom_text(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "red") + 
  geom_point(data = filter(full_pred, abs(r) > 3 & !(Kommun %in% top_cooks$Kommun)),
             aes(color = "|r*|>3"), size = 3) + 
  geom_text(data = filter(full_pred, abs(r) > 3),
            aes(label = Kommun), vjust = 1.5, color = "blue", size = 3) +  
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed") +
  labs(title = "Studentized residuals vs fitted values",
       subtitle = "Highlighting municipalities with high influence",
       caption = "Horizontal lines at y = ±2, ±3",
       color = "Highlight") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  scale_color_manual(values = c("Top Cook's D" = "red", "|r*|>3" = "blue"))

test_infl <- influence(model_test)
glimpse(test_infl)

full_pred |> mutate(devres = test_infl$dev.res,
                     std.devres = devres/sqrt(1 - v)) ->
  full_pred
glimpse(full_pred)


ggplot(full_pred, aes(yhat, std.devres)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", linewidth = 1) +
  labs(title = "Standardized deviance residuals",
       color = "program") +
  theme(text = element_text(size = 18)) 

