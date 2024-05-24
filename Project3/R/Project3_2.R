
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
library(scales)

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

# 绘制直方图和密度图
ggplot(kommuner, aes(x = Cars_nbr)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.7, color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Cars_nbr", x = "Number of Cars", y = "Density") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# 计算均值和方差
mean_cars_nbr <- mean(kommuner$Cars_nbr)
var_cars_nbr <- var(kommuner$Cars_nbr)

## Poisson Regression ####
cars_pos <- glm(Cars_nbr ~ log(Vehicles) + Builton + Children
                + Seniors + log(Higheds) + log(Income) + log(GRP) + Urban
                + Transit + log(Apartments) + Fertility + Persperhh
                + NewParts + log(Population), family = "poisson", data = kommuner)

cars_bin <- glm.nb(Cars_nbr ~ log(Vehicles) + Builton + Children
                   + Seniors + log(Higheds) + log(Income) + log(GRP) + Urban
                   + Transit + log(Apartments) + Fertility + Persperhh
                   + NewParts + (log(Population)), data = kommuner)
## BINOMIAL FULL MODEL STD RESIDUALS
bin_pred <- cbind(
  kommuner,
  xb = predict(cars_pos, se.fit = TRUE))

bin_pred |> 
  mutate(
    xb.residual.scale = NULL,
    xb.lwr = xb.fit - 1.96*xb.se.fit,
    xb.upr = xb.fit + 1.96*xb.se.fit,
    muhat = exp(xb.fit),
    mu.lwr = exp(xb.lwr),
    mu.upr = exp(xb.upr)) ->
  bin_pred
bin_infl <- influence(cars_bin)
bin_pred |> mutate(
  v = hatvalues(cars_bin),
  devres = bin_infl$dev.res,
  std.devres = devres/sqrt(1 - v)) ->
  bin_pred

bin_pred |> mutate(
  xb_pois = predict(cars_bin),
  v_pois = hatvalues(cars_bin),
  devres_pois = bin_infl$dev.res,
  std.devres_pois = devres_pois/sqrt(1 - v_pois)) -> bin_pred

bin_pred |> summarise(
  min_nb = min(std.devres),
  max_nb = max(std.devres),
  min_pois = min(std.devres_pois),
  max_pois = max(std.devres_pois))

ggplot(bin_pred, aes(x = xb_pois)) +
  geom_point(aes(y = std.devres_pois), size = 2) +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), 
             linetype = 2, size = 1) +
  expand_limits(y = c(-4.5, 7.5)) +
  labs(y = "std dev.res", x = "xb",
       title = "Cars: Binomial model") +
  theme(text = element_text(size = 18))

## POISSON FULL MODEL STD RESIDUALS ##
pos_pred <- cbind(
  kommuner,
  xb = predict(cars_pos, se.fit = TRUE))

pos_pred |> 
  mutate(
    xb.residual.scale = NULL,
    xb.lwr = xb.fit - 1.96*xb.se.fit,
    xb.upr = xb.fit + 1.96*xb.se.fit,
    muhat = exp(xb.fit),
    mu.lwr = exp(xb.lwr),
    mu.upr = exp(xb.upr)) ->
  pos_pred
pos_infl <- influence(cars_pos)
pos_pred |> mutate(
  v = hatvalues(cars_pos),
  devres = pos_infl$dev.res,
  std.devres = devres/sqrt(1 - v)) ->
  pos_pred

pos_pred |> mutate(
  xb_pois = predict(cars_pos),
  v_pois = hatvalues(cars_pos),
  devres_pois = pos_infl$dev.res,
  std.devres_pois = devres_pois/sqrt(1 - v_pois)) -> pos_pred

pos_pred |> summarise(
  min_nb = min(std.devres),
  max_nb = max(std.devres),
  min_pois = min(std.devres_pois),
  max_pois = max(std.devres_pois))

ggplot(pos_pred, aes(x = xb_pois)) +
  geom_point(aes(y = std.devres_pois), size = 2) +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), 
             linetype = 2, size = 1) +
  expand_limits(y = c(-4.5, 7.5)) +
  labs(y = "std dev.res", x = "xb",
       title = "Cars: Poisson model") +
  theme(text = element_text(size = 18))

## ALSO LR TEST FOR BIN VS POIS
D_pois <- -2*logLik(cars_pos)[1]
D_nb <- -2*logLik(cars_bin)[1]

D_diff <- D_pois - D_nb
D_qchisq(1 - 0.05, 1) 
pchisq(D_diff, 1, lower.tail = FALSE) #p-value
# We should reject H_0 The extra parameter that allows for the 
# extra variability and turns a poisson regression into a negative 
# binomial regression, is significant.

## BINOMIAL MODEL IS BETTER
summary(cars_bin)
vif(cars_bin)
model_full <- cars_bin
beta <- model_full$coefficients
RR <- exp(beta)
ci_beta = confint(model_full)
ci_RR = exp(ci_beta)

cbind(beta = beta, ci_beta, RR = RR, ci_RR) |> round(digits = 2)

xb = predict(model_full, se.fit = TRUE)
kom_pred <- cbind(
  kommuner, xb)
kom_pred |> mutate(
  xb.residual.scale = NULL,
  xb.lwr = xb$fit - 1.96*xb$se.fit,
  xb.upr = xb$fit + 1.96*xb$se.fit,
  muhat = exp(xb$fit),
  mu.lwr = exp(xb.lwr),
  mu.upr = exp(xb.upr)) -> kom_pred

model_null <- glm.nb(Cars_nbr ~ 1, data = kommuner)

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

# removed vehicles, seniors, Urban, apartments, Persperhh
model_red <- glm.nb(Cars_nbr ~ log(Vehicles) + Builton + Seniors
                    + log(Higheds) + log(Income) + Urban
                    + log(Apartments) 
                    + NewParts + (log(Population)), data = kommuner)
summary(model_red)
vif(model_full)
vif(model_red)
confint(model_red)
# START BY REMOVING CHILDREN -> P = 0.5267 > 0.05
# REMOVE PERSPERHH -> P = 0.817 > 0.05
# REMOVE TRANSIT -> P = 0.196 > 0.05
# REMOVE FERTILITY -> P = 0.189 > 0.05
# REMOVE LOG(GRP) -> P = 0.096 > 0.05
anova(model_red, model_full, test = "Chisq")

# OFFSET MODEL
model_redoffset <- glm.nb(Cars_nbr ~ log(Vehicles) + Builton + Seniors
                          + log(Higheds) + log(Income) + Urban
                          + log(Apartments) 
                          + NewParts + offset(log(Population)), data = kommuner)
summary(model_redoffset)
vif(model_redoffset)
confint(model_redoffset)

model_redpos <- glm(Cars_nbr ~ log(Vehicles) + Builton + Seniors
                    + log(Higheds) + log(Income) + Urban
                    + log(Apartments) + Children
                    + NewParts + offset(log(Population)),family = "poisson", data = kommuner)
D_diff <- model_redpos$null.deviance - cars_pos$deviance
df_diff <- model_redpos$df.null - cars_pos$df.residual
chi <- qchisq(1 - 0.05, df_diff)
cbind(D_diff,chi)
vif(model_redpos)
#### Model only using population ####
model_logpop <- glm.nb(Cars_nbr ~ log(Population), data = kommuner)
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
  labs(title = "Expected number of cars",
       caption = "95% confidence interval",
       color = "program") +
  theme(text = element_text(size = 18)) 

model_popoffset <- glm.nb(Cars_nbr ~ log(Population),offset(log(Population)), data = kommuner)
summary(model_popoffset)
confint(model_popoffset)

logpop_pred <- cbind(
  kommuner,
  xb = predict(model_popoffset, se.fit = TRUE))
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
  labs(title = "Expected number of cars",
       caption = "95% confidence interval",
       color = "program") +
  theme(text = element_text(size = 18))

### Plots Function ####

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
logpop <- log(kommuner$Population)
plot_regression_offset <- function(model, data) {
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
  std_residuals_plot <- ggplot(kom_pred/logpop, aes(yhat, std.devres)) +
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
plots_red <- plot_regression(model_red, kommuner)
plots_red

plots_redoffset <- plot_regression(model_redoffset, kommuner)
plots_redoffset

plots_full <- plot_regression(model_full, kommuner)
plots_full


## Negative Binomial Regression ####
### Models ####
cars_nb <- glm.nb(Cars_nbr ~ Vehicles + Builton + Children
                  + Seniors + log(Higheds) + log(Income) + log(GRP) + Urban
                  + Transit + log(Apartments) + Fertility + Persperhh
                  + log(Population), data = kommuner)
summary(cars_nb)
vif(cars_nb)

# Model using log(Population)
model_lognb <- glm.nb(Cars_nbr ~ Builton + Seniors + log(Higheds) 
                      + log(Income) + log(Apartments) +
                        + Urban  + log(Vehicles) + NewParts + 
                        log(Population), data = kommuner)

vif(model_lognb)
confint(model_lognb)
summary(model_lognb)

model_nopop_nb <- glm.nb(Cars_nbr ~ Builton + Seniors + log(Higheds) 
                      + log(Income) + log(Apartments) +
                        + Urban  + log(Vehicles) + NewParts, data = kommuner)

vif(model_nopop_nb)
confint(model_nopop_nb)
summary(model_nopop_nb)
plot_model_nopop_nb <- plot_regression(model_nopop_nb, kommuner)
plot_model_nopop_nb

D_diff <- model_lognb$deviance - cars_nb$deviance
df_diff <- model_lognb$df.residual - cars_nb$df.residual
cbind(D_diff, df_diff)
chi2_alpha <- qchisq(1 - 0.05, df_diff)
Pvalue <- pchisq(D_diff, df_diff, lower.tail = FALSE)
cbind(D_diff, df_diff, chi2_alpha, Pvalue)

model_nboff <- glm.nb(Cars_nbr ~ Builton + Seniors + log(Higheds) 
                      + log(Income) + log(Apartments) +
                        + Urban  + log(Vehicles) + NewParts + offset(log(Population)), data = kommuner)
summary(model_lognb)
summary(model_nboff)

plots_model_nboff <- plot_regression(model_nboff,kommuner)
plots_model_nboff

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

model_null2_nb <- glm.nb(Cars_nbr ~ 1, data = kommuner)
pseudo1.R2 <- data.frame(
  model = c("0:Null", "1:log(population)", "no pop", "offset"),
  D0 = c(model_null2_nb$null.deviance, 
         model_lognb$null.deviance,
         model_nopop_nb$null.deviance,
         model_nboff$null.deviance),
  D = c(model_null2_nb$deviance, 
        model_lognb$deviance,
        model_nopop_nb$deviance,
        model_nboff$deviance),
  p = c(model_null2_nb$df.null- model_null2_nb$df.residual,
        model_lognb$df.null - model_lognb$df.residual,
        model_nopop_nb$df.null -model_nopop_nb$df.residual,
        model_nboff$df.null -model_nboff$df.residual),
  AIC = c(AIC(model_null_nb),
          AIC(model_lognb),
          AIC(model_nopop_nb),
          AIC(model_nboff)),
  BIC =c(BIC(model_null_nb),
         BIC(model_lognb),
         BIC(model_nopop_nb),
         BIC(model_nboff)))
pseudo1.R2

pseudo1.R2 |> mutate(
  R2 = round(100*(1 - D/D0), digits = 1),
  R2.adj = round(100*(1 - (D + p)/D0), digits = 1)) -> pseudo1.R2

pseudo1.R2 


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
model_null_nb <- glm.nb(Cars_nbr ~ 1 + offset(log(Population)), data = newdata)
model_redoff <- glm.nb(Cars_nbr ~ log(Vehicles) + Builton + Seniors
                       + log(Higheds) + log(Income) + Urban
                       + log(Apartments) 
                       + NewParts + offset(log(Population)), data = kommuner)
model_fulloff <- glm.nb(Cars_nbr ~ log(Vehicles) + Builton + Children
                        + Seniors + log(Higheds) + log(Income) + log(GRP) + Urban
                        + Transit + log(Apartments) + Fertility + Persperhh
                        + NewParts + offset(log(Population)), data = kommuner)

# AIC stepwise selection
model_aic_nb <- step(model_null_nb,
                     scope = list(lower = model_null_nb, upper = model_fulloff),
                     direction = "both",
                     trace = TRUE,  # trace=TRUE detail information for every steps
                     k = 2)  # k=2 means using AIC

model_aic_sum <- summary(model_aic_nb)
summary(model_aic_nb)
confint(model_aic_nb)

# BIC stepwise selection
model_bic_nb <- step(model_null_nb,
                     scope = list(lower = model_null_nb, upper = model_fulloff),
                     direction = "both",
                     trace = TRUE,  # trace=TRUE detail information for every steps
                     k =  log(nobs(model_fulloff)))  # k=2 means using AIC

model_bic_sum <- summary(model_bic_nb)
summary(model_bic_nb)
confint(model_bic_nb)


plots_aic_nb <- plot_regression(model_aic_nb,newdata)
plots_aic_nb

plots_bic_nb <- plot_regression(model_bic_nb,newdata)
plots_bic_nb


pseudo.R2 <- data.frame(
  model = c("0:Null", "1:log(Population)", "2:AIC", "3:BIC","4. Red", "5.Full"),
  D0 = c(model_null_nb$null.deviance, 
         model_logpop_nb$null.deviance,
         model_aic_nb$null.deviance,
         model_bic_nb$null.deviance,
         model_redoff$null.deviance,
         model_fulloff$null.deviance),
  D = c(model_null_nb$deviance, 
        model_logpop_nb$deviance,
        model_aic_nb$deviance,
        model_bic_nb$deviance,
        model_redoff$deviance,
        model_fulloff$deviance),
  p = c(model_null_nb$df.null- model_null_nb$df.residual,
        model_logpop_nb$df.null - model_logpop_nb$df.residual,
        model_aic_nb$df.null -model_aic_nb$df.residual,
        model_bic_nb$df.null -model_bic_nb$df.residual,
        model_redoff$df.null -model_redoff$df.residual,
        model_fulloff$df.null-model_fulloff$df.residual),
  AIC = c(AIC(model_null_nb),
          AIC(model_logpop_nb),
          AIC(model_aic_nb),
          AIC(model_bic_nb),
          AIC(model_redoff),
          AIC(model_fulloff)),
  BIC =c(BIC(model_null_nb),
         BIC(model_logpop_nb),
         BIC(model_aic_nb),
         BIC(model_bic_nb),
         BIC(model_redoff),
         BIC(model_fulloff)))
pseudo.R2

pseudo.R2 |> mutate(
  R2 = round(100*(1 - D/D0), digits = 1),
  R2.adj = round(100*(1 - (D + p)/D0), digits = 1)) -> pseudo.R2

pseudo.R2 

bicpopoff2_nb <- glm.nb(Cars_nbr ~ log(Vehicles)  + Seniors +           
                          Builton   + log(Income) + NewParts + log(Higheds) +  log(GRP)+   
                          log(Population),  data = kommuner)

plots_popoff2_nb <- plot_regression(bicpopoff2_nb,kommuner)
plots_popoff2_nb

fig3a_m3_simulation <- simulateResiduals(fittedModel = cars_pos,
                                         n = 250)
plot(fig3a_m3_simulation, asFactor = FALSE)


fig3a_m3_simulation <- simulateResiduals(fittedModel = model_bic_nb,
                                         n = 250)
plot(fig3a_m3_simulation, asFactor = FALSE)

plots_popoff2_nb <- plot_regression(model_bic_nb,kommuner)
plots_popoff2_nb

