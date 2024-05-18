# Lecture 10, Poisson bootstrap prediction intervals####
# Example 1: Extra, 6/5-24
# Calculates bootstrap prediction intervals for the awards data:

#Create the bootstrap function####
# by running the file containing the code:
source("R/boot_poisson.R")

# The data used to fit the model####
award <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
award <- mutate(
  award, 
  prog = factor(prog, 
                levels = c(1, 2, 3), 
                labels = c("General", "Academic", "Vocational")))

award_glm <- glm(num_awards ~ prog + math, family = "poisson", data = award)

#A data frame to predict on####

math.x0 <- seq(33, 75)
award.predint <- data.frame(
  math = rep(math.x0, 3),
  prog = c(rep("General", length(math.x0)),
           rep("Academic", length(math.x0)),
           rep("Vocational", length(math.x0))))

# Use the boot.pois function on the example####
boot.predint <- boot.pois(award_glm, award.predint, 5000, 0.95)

##Extract the prediction limits####
award.predint$pred.lwr <- boot.predint$lower
award.predint$pred.upr <- boot.predint$upper

##Plot the intervals####
ggplot(award.predint, aes(x = math, color = prog)) +
  geom_line(aes(y = pred.lwr)) +
  geom_line(aes(y = pred.upr)) +
  facet_wrap(~ prog)

# save(award.predint, file = "Data/award_predint.RData")

