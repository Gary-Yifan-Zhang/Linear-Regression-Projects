
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
library(DHARMa)

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
                     + log(Population) + NewParts, family = "poisson", data = kommuner)
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

model_red <- glm(Cars_nbr ~ log(Vehicles) + Builton + Children + log(Higheds) 
                 + log(Income) 
                 + Urban +log(Apartments) 
                 + log(Population) + NewParts, family = "poisson", data = kommuner)
summary(model_red)
vif(model_red)
confint(model_red)

D_diff_red <- model_red$deviance - model_full$deviance
D_diff_red

df_diff_red <- model_red$df.residual - model_full$df.residual
df_diff_red

anova(model_red, model_full)
qchisq(1 - 0.05, df_diff_red)
pchisq(D_diff_red, df_diff_red, lower.tail = FALSE)

carspop <- glm(Cars_nbr ~ Builton + Children + log(Higheds) 
               + log(Income) + log(GRP) 
               + Transit  + Fertility + log(Population), family = "poisson", data = kommuner)
summary(carspop)
confint(carspop)``

#### Offset ####
# log μ − log t = α + βx
# μ = t exp(α + βx) 
carspopoff <- glm(Cars_nbr ~ Builton + Children + log(Higheds) 
                  + log(Income) + log(GRP) 
                  + Transit  + Fertility , offset(log(Population)), family = "poisson", data = kommuner)
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

beta <- model_logpop$coefficients
RR <- exp(beta)
ci_beta = confint(model_logpop)
ci_RR = exp(ci_beta)

cbind(beta = beta, ci_beta, RR = RR, ci_RR) |> round(digits = 2)

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
  labs(title = "Expected number of Cars Number",
       caption = "95% confidence interval",
       x = "Log(Population)",
       y = "Cars Number") +
  theme_bw() +
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
  theme_bw() +
  theme(text = element_text(size = 18)) 

### Plots Function ####

model_red <- glm(Cars_nbr ~ Builton + Children + log(Higheds) 
+ log(Income) + log(GRP) + Urban 
+ Transit  + Fertility + 
+ log(Population) + NewParts, family = "poisson", data = kommuner)
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
  
  # # Create a new variable to indicate if the residuals are greater than 3
  # kom_pred <- kom_pred %>% mutate(outlier = ifelse(abs(std.devres) > 3, TRUE, FALSE))
  # 
  # # Filter the data to get the outliers with abs(std.devres) > 3
  # outliers <- kom_pred %>% filter(outlier)
  
  # Standardized residuals plot
  kom_pred <- kom_pred %>%
    mutate(std.devres = devres/sqrt(1 - v))
  top_cooks <- top_cooks %>% mutate(color = "Top Cook's D")
  std_residuals_plot <- ggplot(kom_pred, aes(yhat, std.devres, color = abs(std.devres) > 3)) +
    geom_point(size = 2) +
    geom_point(data = top_cooks, aes(color = "Top Cook's D"), size = 4) + 
    #geom_text_repel(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "red") +
    #geom_text_repel(data = filter(kom_pred, abs(std.devres) > 3), aes(label = Kommun), 
     #              color = "blue") +
    geom_hline(yintercept = 0, linewidth = 1) +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = c(-3, 3), linetype = "dotted", linewidth = 1) +
    labs(title = "Standardized Deviance Residuals") +
    theme(text = element_text(size = 18)) +
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

plots_logpoppoission <- plot_regression(model_logpop, kommuner)
plots_logpoppoission


plots_poission_pop <- plot_regression(carspop, kommuner)
plots_poission_pop

plots_poission_popoff <- plot_regression(carspopoff, kommuner)
plots_poission_popoff

fig3a_m3_simulation <- simulateResiduals(fittedModel = carspopoff,
                                         n = 250)
plot(fig3a_m3_simulation, asFactor = FALSE)

plots_poission_full <- plot_regression(cars_poission, kommuner)
plots_poission_full

D_diff.cars <- -2*(logLik(carspop)[1] - logLik(carspopoff)[1])
D_diff.cars
qchisq(1 - 0.05, 1)
pchisq(D_diff.cars, 1, lower.tail = FALSE)


## Negative Binomial Regression ####
### Models ####
cars_nb <- glm.nb(Cars_nbr ~ Vehicles + Builton + Children
                     + Seniors + log(Higheds) + log(Income) + log(GRP) + Urban
                     + Transit + log(Apartments) + Fertility + Persperhh
                     + log(Population) + NewParts, data = kommuner)
summary(cars_nb)
vif(cars_nb)

# Model using log(Population)

model_lognb <- glm.nb(Cars_nbr ~ log(Vehicles) + Builton + Seniors + log(Higheds) 
                 + log(Income) 
                 + Urban  +log(Apartments) 
                 + log(Population) + NewParts,  data = kommuner)
vif(model_lognb)
confint(model_lognb)
summary(model_lognb)

# Model using Population
model_nb <- glm.nb(Cars_nbr ~ Builton + Children + log(Higheds) 
                      + log(Income) + log(GRP) 
                      + Transit  + Fertility 
                      + Population ,  data = kommuner)

# Which one is better? Do a linear regression to test?
plots_nb <- plot_regression(model_nb, kommuner)
plots_nb


plots_lognb <- plot_regression(model_lognb, kommuner)
plots_lognb

D_diff_red <- model_lognb$deviance - cars_nb$deviance
D_diff_red

df_diff_red <- model_lognb$df.residual - cars_nb$df.residual
df_diff_red

anova(model_lognb, cars_nb)
qchisq(1 - 0.05, df_diff_red)
pchisq(D_diff_red, df_diff_red, lower.tail = FALSE)

anova(model_lognb, test="Chisq")
anova(cars_nb, test="Chisq")

# Model only using log(Population)
model_logpop_nb <- glm.nb(Cars_nbr ~ log(Population),  data = kommuner)
summary(model_logpop_nb)

plots_logpop_nb <- plot_regression(model_logpop_nb,kommuner)
plots_logpop_nb

beta <- model_logpop_nb$coefficients
RR <- exp(beta)
ci_beta = confint(model_logpop_nb)
ci_RR = exp(ci_beta)

cbind(beta = beta, ci_beta, RR = RR, ci_RR) |> round(digits = 2)

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
  labs(title = "Expected number of car",
       caption = "95% confidence interval",
       color = "program") +
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

model_full_nb <- glm.nb(Cars_nbr ~ log(Vehicles) + log(Higheds) + Children + Seniors + log(Income) + 
                    log(GRP) + Persperhh + Fertility + Urban + Transit + log(Apartments)
                  +Builton  + NewParts + log(Population) , 
                  data = newdata)
summary(model_full_nb)
vif(model_full_nb)

model_fulloff_nb <- glm.nb(Cars_nbr ~ log(Vehicles) + log(Higheds) + Children + Seniors + log(Income) + 
                             log(GRP) + Persperhh + Fertility + Urban + Transit + log(Apartments)
                           +Builton  + NewParts + offset(log(Population)) , 
                           data = newdata)

# Model Null
model_null_nb <- glm.nb(Cars_nbr ~ 1, data = newdata)
model_nulloff_nb <- glm.nb(Cars_nbr ~ 1 + offset(log(Population)), data = newdata)

# AIC stepwise selection
model_aic_nb <- step(model_null_nb,
                  scope = list(lower = model_nulloff_nb, upper = model_fulloff_nb),
                  direction = "both",
                  trace = TRUE,  # trace=TRUE detail information for every steps
                  k = 2)  # k=2 means using AIC

model_aic_sum <- summary(model_aic_nb)
summary(model_aic_nb)
confint(model_aic_nb)

# BIC stepwise selection
model_bic_nb <- step(model_null_nb,
                     scope = list(lower = model_nulloff_nb, upper = model_fulloff_nb),
                     direction = "both",
                     trace = TRUE,  # trace=TRUE detail information for every steps
                     k =  log(nobs(model_fulloff_nb)))  # k=2 means using AIC

model_bic_sum <- summary(model_bic_nb)
summary(model_bic_nb)
confint(model_bic_nb)


plots_aic_nb <- plot_regression(model_aic_nb,newdata)
plots_aic_nb

plots_bic_nb <- plot_regression(model_bic_nb,newdata)
plots_bic_nb


# 计算每个模型的AIC和BIC
aic_values <- c(AIC(model_null_nb),  AIC(model_aic_nb), 
                AIC(model_bic_nb), AIC(model_lognb), AIC(model_full_nb))

bic_values <- c(BIC(model_null_nb), BIC(model_aic_nb), 
                BIC(model_bic_nb), BIC(model_lognb), BIC(model_full_nb))

# 创建数据框并加入AIC和BIC
pseudo.R2 <- data.frame(
  model = c("0:Null",  "2:AIC", "3:BIC", "reduced", "4.Full"),
  D0 = c(model_null_nb$null.deviance, 
         model_aic_nb$null.deviance,
         model_bic_nb$null.deviance,
         model_lognb$null.deviance,
         model_full_nb$null.deviance),
  D = c(model_null_nb$deviance, 
        model_aic_nb$deviance,
        model_bic_nb$deviance,
        model_lognb$deviance,
        model_full_nb$deviance),
  p = c(model_null_nb$df.null - model_null_nb$df.residual,
        model_null_nb$df.residual - model_aic_nb$df.residual,
        model_null_nb$df.residual - model_bic_nb$df.residual,
        model_null_nb$df.residual- model_lognb$df.residual,
        model_null_nb$df.residual - model_fulloff_nb$df.residual),
  AIC = aic_values,
  BIC = bic_values
)

# 查看更新后的数据框
print(pseudo.R2)


pseudo.R2 |> mutate(
  R2 = round(100*(1 - D/D0), digits = 1),
  R2.adj = round(100*(1 - (D + p)/D0), digits = 1)) -> pseudo.R2

pseudo.R2 

D_diff.cars <- -2*(logLik(model_bic_nb)[1] - logLik(model_aic_nb)[1])
D_diff.cars
qchisq(1 - 0.05, 1)
pchisq(D_diff.cars, 1, lower.tail = FALSE)


D_diff.cars <- -2*(logLik(model_logpop_nb)[1] - logLik(model_bic_nb)[1])
D_diff.cars
qchisq(1 - 0.05, 1)
pchisq(D_diff.cars, 1, lower.tail = FALSE)

carspopoff_nb <- glm.nb(Cars_nbr ~ log(Vehicles)  + Seniors + log(Apartments) + 
                          Builton   + log(Income) + Urban + log(Higheds) + NewParts , 
                     offset(log(Population)),  data = kommuner)
summary(carspopoff_nb)
confint(carspopoff_nb)
summary(model_bic_nb)
confint(model_bic_nb)

bicpopoff2_nb <- glm.nb(Cars_nbr ~ log(Vehicles)  + Seniors + 
                          Builton   + log(Income) + NewParts + 
                        offset(log(Population)),  data = kommuner)

aicpopoff2_nb <- glm.nb(Cars_nbr ~ log(Vehicles)  + Seniors + Urban +  Transit +           
                          Builton   + log(Income) + NewParts + log(Higheds) +  log(GRP)+   
                          offset(log(Population)),  data = kommuner)

summary(aicpopoff2_nb)
confint(aicpopoff2_nb)

plots_popoff2_nb <- plot_regression(model_bic_nb,kommuner)
plots_popoff2_nb

fig3a_m3_simulation <- simulateResiduals(fittedModel = model_full_nb,
                                         n = 250)
plot(fig3a_m3_simulation, asFactor = FALSE)

fig3a_m2_simulation_refit <- simulateResiduals(fittedModel = bicpopoff2_nb,
                                               n = 250,
                                               refit = TRUE)
fig3a_m2_test_dispersion <- testDispersion(fig3a_m2_simulation_refit)

D_diff.cars <- -2*(logLik(model_aic_nb)[1] - logLik(aicpopoff2_nb)[1])
D_diff.cars
qchisq(1 - 0.05, 1)
pchisq(D_diff.cars, 1, lower.tail = FALSE)

