library(gbex)
library(evd)

# Set type 1 or 2
type <- 1
if (type == 1) source("sim_GP.R") else source("sim_lnorm.R")

# Target quantile levels
tau <- c(0.25, 0.5, 0.8, 0.9, 0.95, 0.99, 0.999, 0.9999)

# Set experiment number
no.experiment <- 1

# Considered sample sizes
ns <- c(1e4, 1e6)


for (n in ns) {
  set.seed(no.experiment) # Set seed for experiment

  if (type == 1) {
    # GP training data
    train.sim <- sim(n)
    # Get covariates and true parameter values
    Y <- train.sim$Y
    sigma <- train.sim$sigma
    xi <- train.sim$xi
    X <- train.sim$X
  }
  if (type == 2) {
    # log-normal training data
    train.sim <- sim(20 * n)
    # Get covariates and true parameter values
    Y <- train.sim$Y
    meanlog <- train.sim$meanlog
    sdlog <- train.sim$sdlog
    X <- train.sim$X
    # Get true 95% quantiles
    exceed <- apply(cbind(meanlog, sdlog), 1, function(x) qlnorm(0.95, x[1], x[2]))
    # Take exceedances only
    X <- X[Y > exceed, ]
    sigma <- sigma[Y > exceed]
    xi <- xi[Y > exceed]
    Y <- (Y - exceed)[Y > exceed]
  }

  # Fit gradient boosting model
  fit <- gbex(Y, X, initial_values = c(30, 0.1))



  # Get test data
  set.seed(no.experiment + 1)
  test.sim <- sim(n)

  Y.test <- test.sim$Y
  X.test <- test.sim$X
  if (type == 1) test.sigma <- test.sim$sigma
  test.xi <- test.sim$xi
  if (type == 2) test.meanlog <- test.sim$meanlog
  test.sdlog <- test.sim$sdlog

  # Get out-of-sample test GP predictions
  preds <- predict(fit, list(
    X.1 = X.test[, 1], X.2 = X.test[, 2], X.3 = X.test[, 3], X.4 = X.test[, 4],
    X.5 = X.test[, 5], X.6 = X.test[, 6], X.7 = X.test[, 7], X.8 = X.test[, 8]
  ))

  # Evaluate MSE
  MSE <- rep(0, length(tau))
  for (j in 1:length(MSE)) {
    # Get predicted quantiles
    pred.quant <- apply(preds, 1, function(x) qgpd(tau[j], 0, x[1], x[2]))
    # Get true quantiles
    if (type == 1) true.quant <- apply(cbind(test.sigma, test.xi), 1, function(x) qgpd(tau[j], 0, x[1], x[2]))
    if (type == 2) true.quant <- apply(cbind(test.meanlog, test.sdlog), 1, function(x) qlnorm(0.95 + 0.05 * tau[j], x[1], x[2]))

    # Evaluate MSE for tau[j] quantile

    MSE[j] <- sqrt(mean((((pred.quant - true.quant) / true.quant)^2)))

    print(paste0("n=", n, "; tau=", tau[j], "; MSE=", MSE[j]))
  }


  # Save results
  if (type == 1) save(MSE, file = paste0("sim_study_results/GPD/gbex/n", n, "_exp", no.experiment, ".Rdata"))
  if (type == 2) save(MSE, file = paste0("sim_study_results/lnorm/gbex/n", n, "_exp", no.experiment, ".Rdata"))
}
