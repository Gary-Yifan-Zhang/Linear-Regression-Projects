######################################
############# PROJECT 1 ##############
## Part 1. Simple linear regression ##
######################################


### PROJECT 1 ###
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(GGally)
library(gridExtra)
library(knitr)

# load data
kommuner <- read_excel("Data/kommuner.xlsx")
summary(kommuner)

# 1(a) #
ggplot(kommuner, aes(x = Vehicles, y = PM10)) + geom_point() + 
  xlab("Vehicles (1000/capita)") +
  ylab("PM10 (g)") +
  labs(title = "PM10: by amount of vehicles") +
  theme(text = element_text(size = 18))

ggplot(kommuner, aes(x = Vehicles, y = log(PM10))) + geom_point() +
  xlab("Vehicles (1000/capita)") +
  ylab("PM10 (g)") +
  labs(title = "log(PM10): by amount of vehicles") +
  theme(text = element_text(size = 18))

lin_model <- lm(log(PM10) ~ Vehicles, data=kommuner)
log_model <- lm(PM10 ~ Vehicles, data=kommuner)
yhatlin = predict(lin_model)
yhatlog = predict(log_model)

kommuner <- mutate(kommuner, elin = lin_model$residuals)
kommuner <- mutate(kommuner, elog = log_model$residuals)

# residual plot without log
ggplot(kommuner, aes(x = yhatlin, y = elin)) +
  geom_point() +
  geom_hline(yintercept = 0)

# plot of residuals with log 
ggplot(kommuner, aes(x = yhatlog, y = elog)) +
  geom_point() +
  geom_hline(yintercept = 0)

# Q-Q plot 
ggplot(data = kommuner, aes(sample = elin)) +
  geom_qq(size = 3) + geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") 

ggplot(data = kommuner, aes(sample = elog)) +
  geom_qq(size = 3) + geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") 


# 1(b). #
ll_model <- lm(log(PM10) ~ log(Vehicles), data=kommuner)
ll_model$coefficients # the beta coefficients
confint(ll_model) # confidence intervals for the beta parameters
predict(ll_model, newdata = kommuner, interval = "prediction")

c(min(kommuner$Vehicles), max(kommuner$Vehicles))
kom_seq <- data.frame(Vehicles = seq(366, 2224))
cbind(kom_seq, 
      fit = predict(ll_model, newdata = kom_seq),
      conf = predict(ll_model, newdata = kom_seq, 
                     interval = "confidence"),
      pred = predict(ll_model, newdata = kom_seq, 
                     interval = "prediction")) ->
  kom_ints
mutate(kom_ints, conf.fit = NULL, pred.fit = NULL) -> kom_ints

## plot with conf and pred intervals in log scale
# so the relationship is linear
ggplot(kom_ints, aes(x = log(Vehicles))) + 
  geom_point(data = kommuner, aes(y = log(PM10)), size = 3) +
  geom_line(aes(y = fit), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
  geom_line(aes(y = pred.lwr), color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = pred.upr), color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Vehicles (1000/capita)") +
  ylab("PM10 (g)") +
  labs(title = "PM10: by amount of vehicles",
       caption = "data, fitted line, 95% confidence and prediction intervals") +
  theme(text = element_text(size = 18))

## plot with conf and pred intervals 
#transformed back from logarithmic scale
ggplot(kom_ints, aes(x = Vehicles)) + 
  geom_point(data = kommuner, aes(y = PM10), size = 3) +
  geom_line(aes(y = exp(fit)), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = exp(conf.lwr), ymax = exp(conf.upr)), alpha = 0.2) +
  geom_line(aes(y = exp(pred.lwr)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = exp(pred.upr)), color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Vehicles (1000/capita)") +
  ylab("PM10 (g)") +
  labs(title = "PM10: by amount of vehicles",
       caption = "data, fitted line, 95% confidence and prediction intervals") +
  theme(text = element_text(size = 18))


# 1(c). #
# Beta and s.e.
beta1 <- coef(ll_model)["log(Vehicles)"]
beta0 <- coef(ll_model)["Intercept"]
beta1_se <- summary(ll_model)$coefficients["log(Vehicles)", "Std. Error"]

summary(ll_model)
confint(ll_model)

# ln(0.9)
ln_0_9 <- log(0.9)

# Changed PM10
delta_ln_PM10 <- beta1 * ln_0_9

# Calculate percentage of the change
percent_change_PM10 <- (exp(delta_ln_PM10) - 1) * 100

# 95% vi
z_value <- qnorm(0.975)
ci_lower <- percent_change_PM10 - z_value * beta1_se * 10
ci_upper <- percent_change_PM10 + z_value * beta1_se * 10

cat("Expected change in PM10 for a 10% decrease in vehicles:", percent_change_PM10, "%\n")
cat("95% CI for this change rate: [", ci_lower, ",", ci_upper, "]\n")

# To determine the reduction in vehicles needed to halve PM10 emissions
ln_0_5 <- log(0.5)

percentage_change_x <- exp(ln_0_5 / beta1) * 100 

# VI
beta1_low <- beta1 - 1.96 * beta1_se # 1.170085
beta1_high <- beta1 + 1.96 * beta1_se # 1.403780

x_change_low <- exp(ln_0_5 / 1.403780)  # beta1 upper limit
x_change_high <- exp(ln_0_5 / 1.170085)  # beta1 lower limit

print(paste("X needs to decrease by approximately between", 
            round(x_change_low*100, 2), 
            "% and", 
            round(x_change_high*100, 2), 
            "% with 95% confidence."))

print(paste("X needs to decrease by approximately", round(percentage_change_x, 2), "%"))


################################################################################
## PART 2 ##
# 2(a). #
# t-test, H0: Beta1 = 0, no impact
summary(ll_model)
# t-value is 21.68 for vehicles
# 1 and 288 degrees of freedom
# P-value is very small ~ 0, smaller than alpha = 0.05
# we reject H0, vehicles have an impact on PM10

# 2(b) #
coast <- factor(kommuner$Coastal, labels = c("No", "Yes"))
kommuner <- mutate(kommuner, Coastal = coast)
kommuner$Part <- factor(kommuner$Part, labels = c("Gotaland", "Svealand", "Norrland"))

new_model = lm(log(PM10) ~ log(Vehicles) + Coastal*Part, data=kommuner)
new_model$coefficients
# the "missing" coefficients are in the intercept, the others are
# measured in reference to the 'missing' ones so they are 0
# themselves, coastalNo is 0 and PartGotaland is 0
# those two are the reference categories
count(kommuner, Part == "Gotaland", Coastal == "Yes")
count(kommuner, Part == "Svealand", Coastal == "Yes")
count(kommuner, Part == "Norrland", Coastal == "Yes")

confint(new_model) # conf intervals

# to test if any of the added parameters are different from 0
# partial F-test is used, ANOVA
# HO all of the added coefficients are zero
# p-value is 0.0001194 < 0.05, reject HO, not all are 0

anova(ll_model,new_model)

#model without interaction
model_noint <- lm(log(PM10) ~ log(Vehicles) + Coastal+Part, data=kommuner)
#anova with and without interaction to see if the interaction
#is significant 
anova(model_noint,new_model)
# P-value is 0.003839 < 0.05, reject H0, interaction is
# significant according to test

# define testing data
test_data <- data.frame(
  Vehicles = 1000,
  Coastal = c("Yes", "Yes", "Yes", "No", "No", "No"),
  Part = c("Gotaland", "Svealand", "Norrland", "Gotaland", "Svealand", "Norrland")
)

# Iterate over each test data
for (i in 1:nrow(test_data)) {
  prediction <- predict(new_model, newdata = test_data[i,], interval = "confidence")
  cat("Confidence interval for", test_data[i, "Coastal"], test_data[i, "Part"], ":", prediction, "\n")
  cat("Exponential confidence interval for", test_data[i, "Coastal"], test_data[i, "Part"], ":", exp(prediction), "\n \n")
}

# 2(c). # 
kommuner <- mutate(kommuner, Coastal = relevel(Coastal,"Yes"))
model_lev <- lm(log(PM10) ~ log(Vehicles) + Coastal*Part, data=kommuner)
confint(model_lev)
summary(model_lev)

## göra anova med den nya reducerade modellen, stort F värden 
# så är den större modellen sämre/onödig

kommuner <-
  mutate(kommuner, NewParts =
           as.numeric(Part == "Gotaland" | Coastal == "Yes") +
#            as.numeric(Part == "Gotaland") +
           2*as.numeric(Part == "Svealand" & Coastal == "No") +
           3*as.numeric(Part == "Norrland" & Coastal == "No"))
kommuner$NewParts <- factor(kommuner$NewParts, labels = c("GotalandYes", "SvealandNo",
                                                          "NorrlandNo"))

#kommuner <-
#  mutate(kommuner, NewParts =
#           as.numeric(Part == "Gotaland" & Coastal == "No") +
#           2*as.numeric(Part == "Svealand" & Coastal == "No") +
#           3*as.numeric(Part == "Norrland" | Coastal == "Yes"))
#kommuner$NewParts <- factor(kommuner$NewParts, labels = c("GotalandNo", "SvealandNo", "NorrlandYes"))

test_model <- lm(log(PM10)~ log(Vehicles) + NewParts, data=kommuner)
summary(test_model)
anova(test_model,model_lev)
confint(test_model)


### 2(d)
# Higheds, Builton
model_x <- lm(log(PM10)~ log(Vehicles)+log(Higheds)+log(Builton), data=kommuner)
summary(model_x)

vif(model_x)
ggpairs(kommuner,columns=c(5,6,9))

model_2d <- lm(log(PM10)~log(Vehicles)+log(Higheds), data=kommuner)
summary(model_2d)
confint(model_2d)
vif(model_2d)

### 2(e)
model_2ee <- lm(log(PM10)~log(Vehicles)+log(Higheds)+Children+Seniors+log(Income)+
                  log(GRP)+NewParts, data=kommuner)
vif(model_2ee)
ggpairs(kommuner,columns=c(5,7,8,9,10,11,15)) #remove seniors
model_2e <- lm(log(PM10)~log(Vehicles)+log(Higheds)+Children+log(Income)+log(GRP)+
                 NewParts, data=kommuner)
vif(model_2e)
summary(model_2e)


################################################################################
##### Part 3
# 3(a). Leverage.
model_linear_3a <- lm(PM10 ~ Vehicles + Higheds + Children + Income + GRP + 
                        NewParts, data = kommuner)
kommuner_pred <- mutate(kommuner,
                    yhat_linear = predict(model_linear_3a),    
                    yhat = predict(model_2e),
                    r = rstudent(model_2e),
                    v = hatvalues(model_2e),
                    D = cooks.distance(model_2e))

# with 1/n and 2(p+1)/n horizontal lines:
# p+1 = 
pplus1 <- length(model_2e$coefficients)
n <- nobs(model_2e)

# Get top leverage
top_leverage <- kommuner_pred %>%
  arrange(desc(v)) %>%
  slice(1:6)

ggplot(kommuner_pred, aes(x = yhat, y = v)) +
#  facet_wrap(~NewParts) +
  geom_point(size = 2)  +
  geom_point(data = top_leverage, aes(x = yhat, y = v), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = yhat, y = v, label = Kommun), 
            vjust = -1, color = "blue") + 
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "Kommuner: leverage vs predictor",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))

# I might to it wrong 
#ggpairs(kommuner,columns=c(5,7,9,10,11,15))+
#geom_point(data = top_leverage, aes(x = log(Vehicles), y = v), color = "red", size = 3) +  
#  geom_text(data = top_leverage, aes(x = log(Vehicles), y = v, label = Kommun), vjust = -1, color = "blue") +
#  facet_wrap(~NewParts)

# define columns with interested variables
columns_interest <- c(5, 7, 9, 10, 11)
column_names <- names(kommuner)[columns_interest]

# plot all the combinations of x-variables
plot_list <- list()
for (i in seq_along(column_names)) {
  for (j in seq_along(column_names)) {
    if (i < j) {
      p <- ggplot(kommuner, aes_string(x = column_names[i], y = column_names[j])) +
        geom_point() +
        facet_wrap(~NewParts) +
        geom_point(data = top_leverage, aes_string(x = column_names[i], y = column_names[j]), color = "red") +
        geom_text(data = top_leverage, aes_string(x = column_names[i], y = column_names[j], label = "Kommun"), vjust = -1.5, color = "blue", size = 3)
      plot_list[[length(plot_list) + 1]] <- p
    }
  }
}


# use gridExtra to arrange this plots
do.call(gridExtra::grid.arrange, c(plot_list, ncol = length(column_names) - 1))

for (p in plot_list) {
  print(p)  
}

################################################################################
# 3(b). Cook’s distance. 
f1.kommuner <- pplus1
f2.kommuner <- model_2e$df.residual
cook.limit.kommuner <- qf(0.1, f1.kommuner, f2.kommuner)

top_cooks <- kommuner_pred %>%
  arrange(desc(D)) %>%
  slice(1:6)

ggplot(kommuner_pred, aes(x = yhat, y = D)) + 
  geom_point(size = 3) +
  geom_point(data = top_cooks, color = "red", size = 4) +  
  geom_text(data = top_cooks, aes(label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = cook.limit.kommuner, color = "blue") +  
  geom_hline(yintercept = 4/n, linetype = 2, color = "red") +  
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Kommuner: Cook's D",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18))

# DFBETAS 
head(dfbetas(model_2e))

dfbetas_values <- dfbetas(model_2e)

max_dfbetas_indices <- apply(dfbetas_values, 2, which.max)

# Get the name of these municipalities
influential_municipalities <- kommuner_pred$Kommun[max_dfbetas_indices]

# Plot log(PM10) vs variables, maybe we need to change the β-parameter
# model_2e <- lm(log(PM10)~log(Vehicles)+log(Higheds)+Children+log(Income)+log(GRP)+NewParts, data=kommuner)
independent_vars <- c("log(Vehicles)", "log(Higheds)", "Children", "log(Income)", "log(GRP)")


for (var in independent_vars) {
  ggplot(kommuner_pred, aes_string(x = var, y = "log(PM10)")) +
    geom_point() +
    geom_point(data = top_cooks, color = "red", size = 4) +
    geom_text(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "blue") +
    xlab(paste(var, "(1000/capita)")) +
    ylab("log(PM10) (g)") +
    labs(title = "Influence of Municipalities on PM10",
         subtitle = paste("Red points indicate municipalities with significant influence on beta parameters for", var)) +
    theme(text = element_text(size = 16))
  print(ggplot2::last_plot())  # plot all the vars
}


kommuner_pred_DFBETAS <- mutate(
  kommuner_pred,
  df0 = dfbetas(model_2e)[, "(Intercept)"],
  df1 = dfbetas(model_2e)[, "log(Vehicles)"],
  df2 = dfbetas(model_2e)[, "log(Higheds)"],
  df3 = dfbetas(model_2e)[, "Children"],
  df4 = dfbetas(model_2e)[, "log(Income)"],
  df5 = dfbetas(model_2e)[, "log(GRP)"],
  fit = predict(model_2e),
  r = rstudent(model_2e),
  D = cooks.distance(model_2e))

top_cooks <- kommuner_pred_DFBETAS %>%
  arrange(desc(D)) %>%
  slice(1:6)

# DFBETAS of Municipalities with top cook's distance 
top_cooks_DFBETAS <- kommuner_pred_DFBETAS %>%
  arrange(desc(D)) %>%
  slice(1:6) %>%
  select(Kommun, D, df0, df1, df2, df3, df4, df5)

print(top_cooks_DFBETAS)


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

# Change the y axis into df0 ~ df5, check if the resulting plot 
# is the same as the previous plot of log(PM10) vs variables.
ggplot(kommuner_pred_DFBETAS, aes(x = fit, y = df2)) +
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
  ylab("DFBETAS for log(Higheds)") +
  xlab("Fitted values") +
  labs(title = "Impact on the Intercept by Municipality",
       subtitle = "Highlighting municipalities with significant influence",
       caption = "Red lines indicate critical values for influence") +
  theme(text = element_text(size = 18), 
        legend.position = "bottom") +
  scale_color_manual(values = highlightcolors) +
  scale_shape_manual(values = highlightshapes)


################################################################################
# 3(c). Studentized residuals.
filter(kommuner_pred_DFBETAS, abs(r) > 3)

top_cooks <- kommuner_pred_DFBETAS %>%
  arrange(desc(D)) %>%
  slice(1:6)

# Plot studentized residuals vs fitted values
ggplot(kommuner_pred_DFBETAS, aes(x = fit, y = r)) +
  geom_point(size = 2) +  
  geom_point(data = top_cooks, aes(color = "Top Cook's D"), size = 4) + 
  geom_text(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "red") + 
  geom_point(data = filter(kommuner_pred_DFBETAS, abs(r) > 3 & !(Kommun %in% top_cooks$Kommun)),
             aes(color = "|r*|>3"), size = 3) + 
  geom_text(data = filter(kommuner_pred_DFBETAS, abs(r) > 3),
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

high_residuals <- filter(kommuner_pred_DFBETAS, abs(r) > 3)

# Plot sqrt(|r*|) against fitted values and label the points where |r*| > 3.
ggplot(kommuner_pred_DFBETAS, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point(size = 2) +
  geom_point(data = high_residuals, aes(color = "|r*|>3"), size = 4) +
  geom_text(data = high_residuals, aes(label = Kommun), vjust = 2, color = "blue", size = 3) +  
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = "dashed") +
  labs(title = "sqrt(|r*|) vs fitted values",
       subtitle = "Analysis of variance stabilization",
       caption = "Reference lines at y = sqrt(0.75 quantile of normal), sqrt(2), sqrt(3)") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  scale_color_manual(values = c("|r*|>3" = "red"))


################################################################################
# 3(d). Explain, exclude, refit. 
remove_municipalities <- c("0481 Oxelösund", "1082 Karlshamn", "0861 Mönsterås",
                           "2523 Gällivare", "2514 Kalix", "2584 Kiruna",
                           "1480 Göteborg", "1761 Hammarö", "1484 Lysekil",
                           "1494 Lidköping", "1882 Askersund", "2284 Örnsköldsvik",
                           "0319 Älvkarleby", "1460 Bengtsfors",  "1781 Kristinehamn", 
                           "2262 Timrå",  "0980 Gotland", "1272 Bromölla",  
                           "1885 Lindesberg", "1764 Grums")

#remove_municipalities <- c("0481 Oxelösund", "1082 Karlshamn", "0861 Mönsterås",
#                           "2523 Gällivare", "1480 Göteborg", "2584 Kiruna")
newdata <- kommuner %>%
  filter(!Kommun %in% remove_municipalities)
kommuner_excl_lm <- update(model_2e, data = newdata)
kommuner_pred <- mutate(newdata,
                        yhat = predict(kommuner_excl_lm),
                        r = rstudent(kommuner_excl_lm),
                        v = hatvalues(kommuner_excl_lm),
                        D = cooks.distance(kommuner_excl_lm))
kommuner_pred_excl <- mutate(
  kommuner_pred,
  df0 = dfbetas(kommuner_excl_lm)[, "(Intercept)"],
  df1 = dfbetas(kommuner_excl_lm)[, "log(Vehicles)"],
  df2 = dfbetas(kommuner_excl_lm)[, "log(Higheds)"],
  df3 = dfbetas(kommuner_excl_lm)[, "Children"],
  df4 = dfbetas(kommuner_excl_lm)[, "log(Income)"],
  df5 = dfbetas(kommuner_excl_lm)[, "log(GRP)"],
  fit = predict(kommuner_excl_lm),
  r = rstudent(kommuner_excl_lm),
  D = cooks.distance(kommuner_excl_lm))

# Get municipality with high residuals
high_residuals_excl <- filter(kommuner_pred_excl, abs(r) > 3)

# Plot sqrt(|r*|) against fitted values and label the points where |r*| > 3.
ggplot(kommuner_pred_excl, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point(size = 2) +
  geom_point(data = high_residuals_excl, aes(color = "|r*|>3"), size = 4) +
  geom_text(data = high_residuals_excl, aes(label = Kommun), vjust = 2, color = "blue", size = 3) +  
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = "dashed") +
  labs(title = "sqrt(|r*|) vs fitted values",
       subtitle = "Analysis of variance stabilization",
       caption = "Reference lines at y = sqrt(0.75 quantile of normal), sqrt(2), sqrt(3)") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  scale_color_manual(values = c("|r*|>3" = "red"))

# summary & confint of both models
summary(kommuner_excl_lm)
confint(kommuner_excl_lm)

summary(model_2e)
confint(model_2e)

################################################################################
# 3(e). Variable selection.

# prepare models for stepwise selection
model_null <- lm(log(PM10) ~ 1, data = newdata)
model_null_sum <- summary(model_null)

model_1b <- lm(log(PM10) ~ log(Vehicles), data = newdata)
model_1b_sum <- summary(model_1b)

model_2c <- lm(log(PM10)~ log(Vehicles) + NewParts, data=newdata)
model_2c_sum <- summary(model_2c)

model_3d <- update(model_2e, data = newdata)
model_3d_sum <- summary(model_3d)

# AIC stepwise selection
step_model_aic <- step(model_1b,
                       scope = list(lower = model_null, upper = model_3d),
                       direction = "both",
                       trace = TRUE,  # trace=TRUE detail information for every steps
                       k = 2)  # k=2 means using AIC

summary(step_model_aic)


# BIC stepwise selection
step_model_bic <- step(model_1b,
                       scope = list(lower = model_null, upper = model_3d),
                       direction = "both",
                       trace = TRUE,
                       k =  log(nobs(model_3d)))  # BIC
summary(step_model_bic)

# Gathering statistics for each model
model_stats <- function(model) {
  data.frame(
    Beta_Parameters = length(coef(model)),  # Number of beta parameters
    Residual_SD = sigma(model),  # Residual Standard Deviation
    R_Squared = summary(model)$r.squared,  # R-squared
    Adjusted_R_Squared = summary(model)$adj.r.squared,  # Adjusted R-squared
    AIC = AIC(model),  # AIC
    BIC = BIC(model)  # BIC
  )
}

# Apply function to each model and combine results
model_comparison <- data.frame(
  Model = c("Null", "Model 1(b)", "Model 2(c)", "Model 3(d)", "AIC Model", "BIC Model"),
  rbind(
    model_stats(model_null),
    model_stats(model_1b),
    model_stats(model_2c),
    model_stats(model_3d),
    model_stats(step_model_aic),
    model_stats(step_model_bic)
  )
)

# Print the comparison table
print(model_comparison)

