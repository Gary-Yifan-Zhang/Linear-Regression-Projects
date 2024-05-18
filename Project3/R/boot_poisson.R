# Define a function that simulates bootstrap prediction
# intervals for Poisson regression.

# Parameters;
# model = the fitted model from the glm()
# newdata = data frame x0 to predict on,
# N = number of bootstrap samples
# p = confidence level of the interval

boot.pois <- function(model, newdata, N, p) {
  odata <- model$data
  lp <- (1 - p) / 2
  up <- 1 - lp
  # comment out the following line:
  # set.seed(2016)
  for (i in seq(1, N)) {
    bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
    bpred <- predict(update(model, data = bdata), type = "response", newdata = newdata)
    new_y <- rpois(length(bpred), lambda = bpred)
    if (i == 1) {
      boot_y <- new_y
    } else {
      boot_y <- rbind(boot_y, new_y)
    }
  }
  boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
  return(data.frame(pred = predict(model, newdata = newdata, 
                                   type = "response"), 
                    lower = boot_ci[, 1], 
                    upper = boot_ci[, 2]))
}
