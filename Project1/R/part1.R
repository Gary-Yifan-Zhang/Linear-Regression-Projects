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
beta1_se <- summary(ll_model)$coefficients["log(Vehicles)", "Std. Error"]

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

# To determine the reduction in vehicles needed to halve PM10 emissions
required_reduction <- exp((log(0.5) / beta1) - 1)

cat("Expected change in PM10 for a 10% decrease in vehicles:", percent_change_PM10, "%\n")
cat("95% CI for this change rate: [", ci_lower, ",", ci_upper, "]\n")
cat("Reduction in vehicles needed to halve PM10 emissions:", required_reduction * 100, "%\n")



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



ggplot(kommuner, aes(x = Vehicles, y = log(PM10))) + geom_point()
       
  
ggplot(kommuner, aes(x = log(Vehicles), y = log(PM10))) + geom_point()     

ggplot(kommuner, aes(x = log(Builton), y = log(PM10))) + geom_point() ##  

ggplot(kommuner, aes(x = Children, y = log(PM10))) + geom_point() 

ggplot(kommuner, aes(x = log(GRP), y = log(PM10))) + geom_point() 

ggplot(kommuner, aes(x = log(Income), y = log(PM10))) + geom_point() 

# Higheds, Builton
model_x <- lm(log(PM10)~ log(Vehicles)+log(Higheds)+log(Builton), data=kommuner)
summary(model_x)

vif(model_x)
ggpairs(kommuner,columns=c(5,6,9))

model_2d <- lm(log(PM10)~log(Vehicles)+log(Higheds), data=kommuner)
summary(model_2d)
confint(model_2d)
vif(model_2d)

model_2ee <- lm(log(PM10)~log(Vehicles)+log(Higheds)+Children+Seniors+log(Income)+log(GRP)+NewParts, data=kommuner)
vif(model_2ee)
ggpairs(kommuner,columns=c(5,7,8,9,10,11,15)) #remove seniors
model_2e <- lm(log(PM10)~log(Vehicles)+log(Higheds)+Children+log(Income)+log(GRP)+NewParts, data=kommuner)
vif(model_2e)



################################################################################
##### Part 3
# 3(a). Leverage.
kommuner_pred <- mutate(kommuner,
                    yhat = predict(model_2e),
                    r = rstudent(model_2e),
                    v = hatvalues(model_2e),
                    D = cooks.distance(model_2e))

# with 1/n and 2(p+1)/n horizontal lines:
# p+1 = 
pplus1 <- length(model_2e$coefficients)
n <- nobs(model_2e)

# Get top 6 city
top_leverage <- kommuner_pred %>%
  arrange(desc(v)) %>%
  slice(1:6)

ggplot(kommuner_pred, aes(x = log(Vehicles), y = v)) +
  facet_wrap(~NewParts) +
  geom_point(size = 2)  +
  geom_point(data = top_leverage, aes(x = log(Vehicles), y = v), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = log(Vehicles), y = v, label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "Kommuner: leverage vs log Vehicles",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))

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

# Get the name of these municipalities
influential_municipalities <- kommuner_pred$Kommun[max_dfbetas_indices]

# Plot log(PM10) vs Vehicles, maybe we need to change the β-parameter
ggplot(kommuner_pred, aes(x = Vehicles, y = log(PM10))) +
  geom_point() +
  geom_point(data = kommuner_pred[kommuner_pred$Kommun %in% influential_municipalities, ],
             aes(x = Vehicles, y = log(PM10)), color = "red", size = 4) + 
  geom_text(data = kommuner_pred[kommuner_pred$Kommun %in% influential_municipalities, ],
            aes(x = Vehicles, y = log(PM10), label = Kommun), vjust = -1, color = "blue") +
  xlab("Vehicles (1000/capita)") +
  ylab("log(PM10) (g)") +
  labs(title = "Influence of Municipalities on PM10",
       subtitle = "Red points indicate municipalities with significant influence on beta parameters") +
  theme(text = element_text(size = 16))


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

highlightshapes <- c("Cook's D>0.1" = 24)
highlightcolors <- c("|r*|>3" = "red",
                     "length>200" = "magenta", 
                     "all data" = "orange",
                     "excl.length>200" = "blue")

# Top influential municipalities in DFBETAS
top_influential <- kommuner_pred_DFBETAS %>%
  arrange(desc(abs(df0))) %>%
  arrange(desc(abs(df0))) %>%
  slice(1:6)

ggplot(kommuner_pred_DFBETAS, aes(x = fit, y = df0)) +
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
  ylab("DFBETAS for Intercept (DFBETAS_0(i))") +
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
  geom_point(data = filter(kommuner_pred_DFBETAS, abs(r) > 3 & !(Kommun %in% top_cooks$Kommun)),
             aes(color = "|r*|>3"), size = 3) +
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

# 绘制sqrt(|r*|)对拟合值的图，并为|r*| > 3的点添加名称
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
