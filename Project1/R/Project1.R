### PROJECT 1 ###
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(GGally)

kommuner <- read_excel("Data/kommuner.xlsx")
summary(kommuner)

## PART 1 ##
# 1(a) #
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

ggplot(data = kommuner, aes(sample = elin)) +
  geom_qq(size = 3) + geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") 

ggplot(data = kommuner, aes(sample = elog)) +
  geom_qq(size = 3) + geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") 

loglin_model <- lm(log(PM10) ~ Vehicles, data=kommuner)
loglog_model <- lm(log(PM10) ~ log(Vehicles), data=kommuner)
yhatlin = predict(loglin_model)
yhatlog = predict(loglog_model)

kommuner <- mutate(kommuner, elin = loglin_model$residuals)
kommuner <- mutate(kommuner, elog = loglog_model$residuals)

# residual plot without log
ggplot(kommuner, aes(x = yhatlin, y = elin)) +
  geom_point() +
  geom_hline(yintercept = 0)

# plot of residuals with log 
ggplot(kommuner, aes(x = yhatlog, y = elog)) +
  geom_point() +
  geom_hline(yintercept = 0)

## could also add QQ-plot to verify which model is better
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
confint(ll_model)
# change = -log(0.9)*Beta1


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

data_1000 <- data.frame(Vehicles = 1000,Coastal = "Yes", Part = "Gotaland")
conf1 <- predict(new_model, newdata = data_1000, interval = "confidence")
conf1
exp(conf1)

data_1000 <- data.frame(Vehicles = 1000,Coastal = "Yes", Part = "Svealand")
conf2 <- predict(new_model, newdata = data_1000, interval = "confidence")
conf2
exp(conf2)

data_1000 <- data.frame(Vehicles = 1000,Coastal = "Yes", Part = "Norrland")
conf3 <- predict(new_model, newdata = data_1000, interval = "confidence")
conf3
exp(conf3)

data_1000 <- data.frame(Vehicles = 1000,Coastal = "No", Part = "Gotaland")
conf4 <- predict(new_model, newdata = data_1000, interval = "confidence")
conf4
exp(conf4)

data_1000 <- data.frame(Vehicles = 1000,Coastal = "No", Part = "Svealand")
conf5 <- predict(new_model, newdata = data_1000, interval = "confidence")
conf5
exp(conf5)

data_1000 <- data.frame(Vehicles = 1000,Coastal = "No", Part = "Norrland")
conf6 <- predict(new_model, newdata = data_1000, interval = "confidence")
conf6
exp(conf6)

# 2(c). # 
kommuner <- mutate(kommuner, Coastal = relevel(Coastal,"Yes"))
model_lev <- lm(log(PM10) ~ log(Vehicles) + Coastal*Part, data=kommuner)
confint(model_lev)
summary(model_lev)

## göra anova med den nya reducerade modellen, stort F värden 
# så är den större modellen sämre/onödig

kommuner <-
  mutate(kommuner, NewParts =
           as.numeric(Part == "Gotaland" & Coastal == "No") +
           2*as.numeric(Part == "Svealand" & Coastal == "No") +
           3*as.numeric(Part == "Norrland" | Coastal == "Yes"))
kommuner$NewParts <- factor(kommuner$NewParts, labels = c("GotalandNo", "SvealandNo", "NorrlandYes"))
test_model <-lm(log(PM10)~ log(Vehicles) + NewParts, data=kommuner)
summary(test_model)
anova(test_model,model_lev)
confint(test_model)


ggplot(kommuner, aes(x =Higheds , y = log(PM10))) +
  geom_point() 

ggplot(kommuner, aes(x =log(Higheds) , y = log(PM10))) +
  geom_point() #Should be log

ggplot(kommuner, aes(x =Builton , y = log(PM10))) +
  geom_point() 

ggplot(kommuner, aes(x =log(Builton) , y = log(PM10))) +
  geom_point() #should be log

ggplot(kommuner, aes(x =Income , y = log(PM10))) +
  geom_point() 

ggplot(kommuner, aes(x =log(Income) , y = log(PM10))) +
  geom_point() #should be log

ggplot(kommuner, aes(x =GRP , y = log(PM10))) +
  geom_point() 

ggplot(kommuner, aes(x =log(GRP) , y = log(PM10))) +
  geom_point() #should be log

ggplot(kommuner, aes(x =Children , y = log(PM10))) +
  geom_point() 

ggplot(kommuner, aes(x =log(Children) , y = log(PM10))) +
  geom_point() #not log

ggplot(kommuner, aes(x =Seniors , y = log(PM10))) +
  geom_point() 

ggplot(kommuner, aes(x =log(Seniors) , y = log(PM10))) +
  geom_point() #not log

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
