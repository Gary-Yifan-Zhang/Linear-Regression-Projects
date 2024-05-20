
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
library(ggrepel)

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

kommuner <-
  mutate(kommuner, NewParts =
           as.numeric(Part == "Gotaland" | Coastal == "Yes") +
           #            as.numeric(Part == "Gotaland") +
           2*as.numeric(Part == "Svealand" & Coastal == "No") +
           3*as.numeric(Part == "Norrland" & Coastal == "No"))
kommuner$NewParts <- factor(kommuner$NewParts, labels = c("GotalandYes", "SvealandNo",
                                                          "NorrlandNo"))

## Poission Regression ####
cars_poission <- glm(Cars_nbr ~ log(Vehicles) + Builton + Children
                     + Seniors + log(Higheds) + log(Income) + log(GRP) + Urban
                     + Transit + log(Apartments) + Fertility + Persperhh
                     + Population + NewParts, family = "poisson", data = kommuner)
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

model_null <- glm(Cars_nbr ~ 1, family = "poisson", data = kommuner)
model_full <- cars_poission

### AIC and BIC stepwise selection ####
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
                 + log(Population), family = "poisson", data = kommuner)
vif(model_red)
confint(model_red)

carspop <- glm(Cars_nbr ~ Builton + Children + log(Higheds) 
               + log(Income) + log(GRP) 
               + Transit  + Fertility + log(Population), family = "poisson", data = kommuner)
summary(carspop)
confint(carspop)

#### Offset ####
# log μ − log t = α + βx
# μ = t exp(α + βx) 
carspopoff <- glm(Cars_nbr ~ Builton + Children + log(Higheds) 
                  + log(Income) + log(GRP) 
                  + Transit  + Fertility, offset(log(Population)), family = "poisson", data = kommuner)
summary(carspopoff)
confint(carspopoff)

#### Model only using population ####
model_pop <- glm(Cars_nbr ~ Population, family = "poisson", data = kommuner)
summary(model_pop)
confint(model_pop)

pop_pred <- cbind(
  kommuner,
  xb = predict(model_pop, se.fit = TRUE))
pop_pred |> mutate(
  xb.residual.scale = NULL,
  xb.lwr = xb.fit - 1.96*xb.se.fit,
  xb.upr = xb.fit + 1.96*xb.se.fit,
  muhat = exp(xb.fit),
  mu.lwr = exp(xb.lwr),
  mu.upr = exp(xb.upr)) -> pop_pred

glimpse(pop_pred)

ggplot(pop_pred, aes(Population, Cars_nbr)) +
  geom_point() +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  labs(title = "Expected number of awards",
       caption = "95% confidence interval",
       color = "program") +
  theme(text = element_text(size = 18)) 

model_logpop <- glm(Cars_nbr ~ log(Population), family = "poisson", data = kommuner)
summary(model_logpop)
confint(model_logpop)

logpop_pred <- cbind(
  kommuner,
  xb = predict(model_logpop, se.fit = TRUE))
logpop_pred |> mutate(
  xb.residual.scale = NULL,
  xb.lwr = xb.fit - 1.96*xb.se.fit,
  xb.upr = xb.fit + 1.96*xb.se.fit,
  muhat = exp(xb.fit),
  mu.lwr = exp(xb.lwr),
  mu.upr = exp(xb.upr)) -> logpop_pred

glimpse(logpop_pred)

ggplot(logpop_pred, aes(log(Population), Cars_nbr)) +
  geom_point() +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  labs(title = "Expected number of awards",
       caption = "95% confidence interval",
       color = "program") +
  theme(text = element_text(size = 18)) 

model_llpop <- glm(log(Cars_nbr) ~ log(Population), family = "poisson", data = kommuner)
summary(model_llpop)
confint(model_llpop)

llpop_pred <- cbind(
  kommuner,
  xb = predict(model_llpop, se.fit = TRUE))
llpop_pred |> mutate(
  xb.residual.scale = NULL,
  xb.lwr = xb.fit - 1.96*xb.se.fit,
  xb.upr = xb.fit + 1.96*xb.se.fit,
  muhat = exp(xb.fit),
  mu.lwr = exp(xb.lwr),
  mu.upr = exp(xb.upr)) -> llpop_pred

glimpse(llpop_pred)

ggplot(llpop_pred, aes(log(Population), log(Cars_nbr))) +
  geom_point() +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  labs(title = "Expected number of awards",
       caption = "95% confidence interval",
       color = "program") +
  theme(text = element_text(size = 18)) 

### Plots Function ####

model_red <- glm(Cars_nbr ~ Builton + Children + log(Higheds) 
+ log(Income) + log(GRP) + Urban 
+ Transit  + Fertility + 
+ log(Population), family = "poisson", data = kommuner)
summary(model_red)

plot_regression <- function(model, data) {
  # Leverage plot
  pplus1 <- length(model$coefficients)
  n <- nobs(model)
  kom_pred <- cbind(data, v = hatvalues(model), pred = predict(model))
  leverage_plot <- ggplot(kom_pred, aes(pred, v)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 2*pplus1/n, color = "red") +
    labs(title = "Leverage",
         color = "program", caption = "horizontal line = 2(p+1)/n") +
    theme(text = element_text(size = 18))
  
  # Cook's Distance plot
  test_infl <- influence(model)
  kom_pred <- mutate(data,
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
    geom_text_repel(data = top_cooks, aes(label = Kommun), vjust = -1, color = "blue") + 
    geom_hline(yintercept = 4/n, linetype = 2, color = "red") +  
    xlab("Fitted values") +
    ylab("D_i") +
    labs(title = "Cook's Distance",
         caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
         color = "Highlight") +
    theme(text = element_text(size = 18))
  
  # Create a new variable to indicate if the residuals are greater than 3
  kom_pred <- kom_pred %>% mutate(outlier = ifelse(abs(std.devres) > 3, TRUE, FALSE))
  
  # Filter the data to get the outliers with abs(std.devres) > 3
  outliers <- kom_pred %>% filter(outlier)
  
  # Standardized residuals plot
  kom_pred <- kom_pred %>%
    mutate(std.devres = devres/sqrt(1 - v))
  top_cooks <- top_cooks %>% mutate(color = "Top Cook's D")
  std_residuals_plot <- ggplot(kom_pred, aes(yhat, std.devres)) +
    geom_point(size = 2, aes(color = outlier)) +
    geom_point(data = top_cooks, aes(color = color), size = 4) + 
    geom_text_repel(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "red") +
    geom_text_repel(data = outliers, aes(label = Kommun), vjust = -1.5, color = "red") +
    geom_hline(yintercept = 0, linewidth = 1) +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = c(-3, 3), linetype = "dotted", linewidth = 1) +
    labs(title = "Standardized Deviance Residuals",
         color = "program") +
    theme(text = element_text(size = 18))
  
  std_residuals_plot <- std_residuals_plot + 
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "blue")) +
    guides(color = guide_legend(override.aes = list(shape = c(16, 16))))
  
  # Return a list of plots
  plot_list <- list(leverage_plot, cooks_distance_plot, std_residuals_plot)
  names(plot_list) <- c("Leverage", "Cook's Distance", "Standardized Residuals")
  return(plot_list)
}

### Plots ####
plots_poission <- plot_regression(model_red, kommuner)
plots_poission

plots_poission_pop <- plot_regression(carspop, kommuner)
plots_poission_pop

plots_poission_popoff <- plot_regression(carspopoff, kommuner)
plots_poission_popoff


## Negative Binomial Regression ####
### Models ####
cars_nb <- glm.nb(Cars_nbr ~ Vehicles + Builton + Children
                     + Seniors + log(Higheds) + log(Income) + log(GRP) + Urban
                     + Transit + log(Apartments) + Fertility + Persperhh
                     + log(Population), data = kommuner)
summary(cars_nb)
vif(cars_nb)

# Model using log(Population)
model_lognb <- glm.nb(Cars_nbr ~ Builton + Children + log(Higheds) 
                 + log(Income) + log(GRP) 
                 + Transit  + Fertility 
                 + log(Population),  data = kommuner)
vif(model_lognb)
confint(model_lognb)
summary(model_lognb)

# Model using Population
model_nb <- glm.nb(Cars_nbr ~ Builton + Children + log(Higheds) 
                      + log(Income) + log(GRP) 
                      + Transit  + Fertility 
                      + Population,  data = kommuner)

# Which one is better? Do a linear regression to test?
plots_nb <- plot_regression(model_nb, kommuner)
plots_nb

plots_lognb <- plot_regression(model_lognb, kommuner)
plots_lognb

# Model only using log(Population)
model_logpop_nb <- glm.nb(Cars_nbr ~ log(Population),  data = kommuner)

plots_logpop_nb <- plot_regression(model_logpop_nb,kommuner)
plots_logpop_nb

# Model only using Population
model_pop_nb <- glm.nb(Cars_nbr ~ Population,  data = kommuner)

### CI plots ####
#### model_nb ####
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

ggplot(nb_pred, aes(Population, Cars_nbr)) +
  geom_point() +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) 

beta <- model_nb$coefficients
ci_beta = confint(model_nb)
cbind(beta = beta, ci_beta) |> round(digits = 2)
cbind(RR = exp(beta), exp(ci_beta)) |> round(digits = 2)

#### model_lognb ####
lognb_pred <- cbind(
  kommuner,
  xb = predict(model_lognb, se.fit = TRUE))
lognb_pred |> 
  mutate(
    xb.residual.scale = NULL,
    xb.lwr = xb.fit - 1.96*xb.se.fit,
    xb.upr = xb.fit + 1.96*xb.se.fit,
    muhat = exp(xb.fit),
    mu.lwr = exp(xb.lwr),
    mu.upr = exp(xb.upr)) ->
  lognb_pred

ggplot(lognb_pred, aes(log(Population), Cars_nbr)) +
  geom_point() +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) 

beta <- model_nb$coefficients
ci_beta = confint(model_nb)
cbind(beta = beta, ci_beta) |> round(digits = 2)
cbind(RR = exp(beta), exp(ci_beta)) |> round(digits = 2)

####  model_logpop #### 
logpop_pred <- cbind(
  kommuner,
  xb = predict(model_logpop_nb, se.fit = TRUE))
logpop_pred |> 
  mutate(
    xb.residual.scale = NULL,
    xb.lwr = xb.fit - 1.96*xb.se.fit,
    xb.upr = xb.fit + 1.96*xb.se.fit,
    muhat = exp(xb.fit),
    mu.lwr = exp(xb.lwr),
    mu.upr = exp(xb.upr)) ->
  logpop_pred

ggplot(logpop_pred, aes(log(Population), Cars_nbr)) +
  geom_point() +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) 

beta <- model_logpop_nb$coefficients
ci_beta = confint(model_logpop_nb)
cbind(beta = beta, ci_beta) |> round(digits = 2)
cbind(RR = exp(beta), exp(ci_beta)) |> round(digits = 2)

### Exclude & Refit ####

# remove_municipalities <- c("0180 Stockholm", "1480 Göteborg", "1280 Malmö", "0380 Uppsala")

remove_municipalities <- c("0183 Sundbyberg", "0127 Botkyrka", "0128 Salem", "0138 Tyresö",
                           "1480 Göteborg", "0136 Haninge", "0126 Huddinge")

#remove_municipalities <- c("0481 Oxelösund", "1082 Karlshamn", "0861 Mönsterås",
#                           "2523 Gällivare", "1480 Göteborg", "2584 Kiruna")
# newdata <- kommuner %>%
#   filter(!Kommun %in% remove_municipalities)

newdata <- kommuner
model_excl <- update(model_lognb, data = newdata)

plots_excl <- plot_regression(model_excl, newdata)
plots_excl

model_full_nb <- glm.nb(Cars_nbr ~ log(Higheds) + Children + Seniors + log(Income) + 
                    log(GRP) + Persperhh + Fertility + Urban + Transit + log(Apartments)
                  +Builton + log(Vehicles) + log(Population) + NewParts, 
                  data = newdata)
summary(model_full_nb)
vif(model_full_nb)

# Model Null
model_null_nb <- glm.nb(Cars_nbr ~ 1, data = newdata)

# AIC stepwise selection
model_aic_nb <- step(model_null_nb,
                  scope = list(lower = model_null_nb, upper = model_excl),
                  direction = "both",
                  trace = TRUE,  # trace=TRUE detail information for every steps
                  k = 2)  # k=2 means using AIC

model_aic_sum <- summary(model_aic_nb)
summary(model_aic_nb)
confint(model_aic_nb)

# BIC stepwise selection
model_bic_nb <- step(model_null_nb,
                     scope = list(lower = model_null_nb, upper = model_excl),
                     direction = "both",
                     trace = TRUE,  # trace=TRUE detail information for every steps
                     k =  log(nobs(model_excl)))  # k=2 means using AIC

model_bic_sum <- summary(model_bic_nb)
summary(model_bic_nb)
confint(model_bic_nb)


plots_aic_nb <- plot_regression(model_aic_nb,newdata)
plots_aic_nb

plots_bic_nb <- plot_regression(model_bic_nb,newdata)
plots_bic_nb


pseudo.R2 <- data.frame(
  model = c("0:Null", "1:log(Population)", "2:AIC", "3:BIC", "4.Full"),
  D0 = c(model_null_nb$null.deviance, 
         model_logpop_nb$null.deviance,
         model_aic_nb$null.deviance,
         model_bic_nb$null.deviance,
         model_full_nb$null.deviance),
  D = c(model_null_nb$deviance, 
        model_logpop_nb$deviance,
        model_aic_nb$deviance,
        model_bic_nb$deviance,
        model_full_nb$deviance),
  p = c(model_null_nb$df.null - model_null_nb$df.residual,
        model_logpop_nb$df.null - model_null_nb$df.residual,
        model_aic_nb$df.null - model_null_nb$df.residual,
        model_bic_nb$df.null - model_null_nb$df.residual,
        model_full_nb$df.null - model_null_nb$df.residual))
pseudo.R2

pseudo.R2 |> mutate(
  R2 = round(100*(1 - D/D0), digits = 1),
  R2.adj = round(100*(1 - (D + p)/D0), digits = 1)) -> pseudo.R2

pseudo.R2 
