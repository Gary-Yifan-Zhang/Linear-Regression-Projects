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
model_1b <- glm(highcars_cat ~ Part, family = "binomial", data = kommuner)
model_1b_sum <- summary(model_1b)

bhat <- model_1b$coefficients
ci.beta <- confint(model_1b)
cbind(beta = bhat, ci.beta) |> round(digits = 2)

# exp(beta0), exp(beta1)
or = exp(bhat)
ci.or <- exp(ci.beta)
cbind(`exp(beta)` = or, ci.or) |> round(digits = 2)
