
# PROJECT 3 ####
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
library(ResourceSelection)
library(MASS)

# load data
kommuner <- read_excel("data/kommunerProject3.xlsx")
summary(kommuner)
coast <- factor(kommuner$Coastal, labels = c("No", "Yes"))

kommuner <- mutate(kommuner, Coastal = coast)
kommuner$Part <- factor(kommuner$Part, labels = c("Gotaland", "Svealand", "Norrland"))

meanf <- kommuner |> filter(Part == "Norrland" & Coastal == "No") |>
  summarise(meanfertility = mean(Fertility, na.rm = TRUE))


I <- which(is.na(kommuner$Fertility))
kommuner$Fertility[I] <- meanf
kommuner <- mutate(kommuner, Fertility = unlist(Fertility))

kommuner |> mutate(Fertility = as.numeric(Fertility)) -> kommuner

kommuner |> filter(Part == 3 & Coastal == 0) |>
  summarise(meanfertility = mean(Fertility, na.rm = TRUE))
I <- which(is.na(kommuner$Fertility))
kommuner$Fertility[I] <- 1.57 # meanfertility = 1.57

cars_poission <- glm(Cars_nbr ~ Vehicles + Builton + Children
                     + Seniors + log(Higheds) + log(Income) + log(GRP) + Urban
                     + Transit + log(Apartments) + Fertility + Persperhh
                     + Population, family = "poisson", data = kommuner)
summary(cars_poission)
vif(cars_poission)

beta <- cars_poission$coefficients
RR <- exp(beta)
ci_beta = confint(cars_poission)
ci_RR = exp(ci_beta)

cbind(beta = beta, ci_beta, RR = RR, ci_RR) |> round(digits = 2)

xb = predict(cars_poission, se.fit = TRUE)
kom_pred <- cbind(
  kommuner, xb)
kom_pred |> mutate(
  xb.residual.scale = NULL,
  xb.lwr = xb$fit - 1.96*xb$se.fit,
  xb.upr = xb$fit + 1.96*xb$se.fit,
  muhat = exp(xb$fit),
  mu.lwr = exp(xb.lwr),
  mu.upr = exp(xb.upr)) -> kom_pred
pp
model_null <- glm(Cars_nbr ~ 1, family = "poisson", data = kommuner)
model_full <- cars_poission

## AIC and BIC stepwise selection #####
model_aic <- step(model_null,
                  scope = list(lower = model_null, upper = model_full),
                  direction = "both",
                  trace = TRUE,  # trace=TRUE detail information for every steps
                  k = 2)  # k=2 means using AIC

model_aic_sum <- summary(model_aic)


summary(model_aic)

model_bic <- step(model_null,
                  scope = list(lower = model_null, upper = model_full),
                  direction = "both",
                  trace = TRUE,
                  k =  log(nobs(model_full)))  # BIC
model_bic_sum <- summary(model_bic)
model_bic_sum

model_red <- glm(Cars_nbr ~ Builton + Children + log(Higheds) 
                 + log(Income) + log(GRP) 
                 + Transit  + Fertility 
                 + Population, family = "poisson", data = kommuner)
vif(model_red)
confint(model_red)

carspop <- glm(Cars_nbr ~ Builton + Children + log(Higheds) 
               + log(Income) + log(GRP) 
               + Transit  + Fertility + log(Population), family = "poisson", data = kommuner)
summary(carspop)
confint(carspop)

carspopoff <- glm(Cars_nbr ~ Builton + Children + log(Higheds) 
                  + log(Income) + log(GRP) 
                  + Transit  + Fertility, offset(log(Population)), family = "poisson", data = kommuner)
summary(carspopoff)
confint(carspopoff)

## LEVERAGE

model_red <- glm(Cars_nbr ~ Builton + Children + log(Higheds) 
+ log(Income) + log(GRP) + Urban 
+ Transit  + Fertility + 
+ Population, family = "poisson", data = kommuner)
summary(model_red)

plot_regression <- function(model) {
  # Leverage plot
  pplus1 <- length(model$coefficients)
  n <- nobs(model)
  kom_pred <- cbind(kommuner, v = hatvalues(model), pred = predict(model))
  leverage_plot <- ggplot(kom_pred, aes(pred, v)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 2*pplus1/n, color = "red") +
    labs(title = "Leverage",
         color = "program", caption = "horizontal line = 2(p+1)/n") +
    theme(text = element_text(size = 18))
  
  # Cook's Distance plot
  test_infl <- influence(model)
  kom_pred <- mutate(kommuner,
                     yhat_linear = predict(model),    
                     yhat = predict(model),
                     r = rstudent(model),
                     v = hatvalues(model),
                     D = cooks.distance(model),
                     devres = test_infl$dev.res,
                     std.devres = devres/sqrt(1 - v))
  f1 <- pplus1
  f2 <- model$df.residual
  cook.limit <- qf(0.1, f1, f2)
  top_cooks <- kom_pred %>%
    arrange(desc(D)) %>%
    slice(1:6)
  cooks_distance_plot <- ggplot(kom_pred, aes(x = yhat, y = D)) + 
    geom_point(size = 3) +
    geom_point(data = top_cooks, color = "red", size = 4) +  
    geom_text(data = top_cooks, aes(label = Kommun), vjust = -1, color = "blue") + 
    geom_hline(yintercept = 4/n, linetype = 2, color = "red") +  
    xlab("Fitted values") +
    ylab("D_i") +
    labs(title = "Cook's Distance",
         caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
         color = "Highlight") +
    theme(text = element_text(size = 18))
  
  # Standardized residuals plot
  kom_pred <- kom_pred %>%
    mutate(std.devres = devres/sqrt(1 - v))
  top_cooks <- top_cooks %>% mutate(color = "Top Cook's D")
  std_residuals_plot <- ggplot(kom_pred, aes(yhat, std.devres)) +
    geom_point(size = 2) +
    geom_point(data = top_cooks, aes(color = color), size = 4) + 
    geom_text(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "red") +
    geom_hline(yintercept = 0, linewidth = 1) +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = c(-3, 3), linetype = "dotted", linewidth = 1) +
    labs(title = "Standardized Deviance Residuals",
         color = "program") +
    theme(text = element_text(size = 18))
  
  # Return a list of plots
  plot_list <- list(leverage_plot, cooks_distance_plot, std_residuals_plot)
  names(plot_list) <- c("Leverage", "Cook's Distance", "Standardized Residuals")
  return(plot_list)
}
plots <- plot_regression(model_red)
plots

cars_nb <- glm.nb(Cars_nbr ~ Vehicles + Builton + Children
                     + Seniors + log(Higheds) + log(Income) + log(GRP) + Urban
                     + Transit + log(Apartments) + Fertility + Persperhh
                     + Population, data = kommuner)
summary(cars_nb)
vif(cars_nb)

model_nb <- glm.nb(Cars_nbr ~ Builton + Children + log(Higheds) 
                 + log(Income) + log(GRP) 
                 + Transit  + Fertility 
                 + Population,  data = kommuner)
vif(model_nb)
confint(model_nb)
summary(model_nb)

beta <- model_nb$coefficients
ci_beta = confint(model_nb)
cbind(beta = beta, ci_beta) |> round(digits = 2)
cbind(RR = exp(beta), exp(ci_beta)) |> round(digits = 2)

nb_pred <- cbind(
  kommuner,
  xb = predict(model_nb, se.fit = TRUE))
nb_pred |> 
  mutate(
    xb.residual.scale = NULL,
    xb.lwr = xb.fit - 1.96*xb.se.fit,
    xb.upr = xb.fit + 1.96*xb.se.fit,
    muhat = exp(xb.fit),
    mu.lwr = exp(xb.lwr),
    mu.upr = exp(xb.upr)) ->
  nb_pred

ggplot(nb_pred, aes(Builton, Cars_nbr)) +
  geom_point() +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) 

plots <- plot_regression(model_nb)
plots

model_full_nb <- glm.nb(Cars_nbr ~ log(Higheds) + Children + Seniors + log(Income) + 
                    log(GRP) + Persperhh + Fertility + Urban + Transit + log(Apartments)
                  +Builton + log(Vehicles) + Population + NewParts, 
                  data = kommuner)
summary(model_full_nb)
model_full_sum <- summary(model_full_nb)
vif(model_full_nb)

# Model Null
model_null_nb <- glm.nb(Cars_nbr ~ 1, data = kommuner)

# AIC stepwise selection
model_aic_nb <- step(model_null_nb,
                  scope = list(lower = model_null_nb, upper = model_full_nb),
                  direction = "both",
                  trace = TRUE,  # trace=TRUE detail information for every steps
                  k = 2)  # k=2 means using AIC

model_aic_sum <- summary(model_aic_nb)

summary(model_aic_nb)
confint(model_aic_nb)

plots_aic <- plot_regression(model_aic_nb)
plots_aic
