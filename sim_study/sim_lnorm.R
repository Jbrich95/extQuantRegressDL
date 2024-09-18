sim <- function(n) {
  # Create predictors
  Cor <- matrix(0.3, nrow = 8, ncol = 8)
  diag(Cor) <- 1
  X <- mvnfast::rmvn(n, rep(0, 8), sigma = Cor)

  # Non-stationary parameters
  meanlog <- 3 - 0.2 * (0.5 * (X[, 1] * X[, 2] * 0.7 - 5 + X[, 2] * (1 - cos(pi * X[, 2] * X[, 3])) + 3 * sin(X[, 3]) / (abs(X[, 8] - X[, 4]) + 2)
    + 0.2 * (X[, 7] + X[, 4] * X[, 5] / 2 - 1)^2 - exp(rowSums(X[, 1:6] / 10 - 3))) -
    0.5 * abs(X[, 3] * X[, 4] * 0.7 - 5 + X[, 1] * (1 - cos(pi * X[, 4] * X[, 7])) + 3 * sin(X[, 5]) / (abs(X[, 2] - X[, 6]) + 2)
      + 0.2 * (X[, 6] + X[, 6] * X[, 7] / 2 - 1)^2 - exp(rowSums(X[, 1:8] / 10 - 3))) + 2 * sqrt(abs(X[, 3] + X[, 5] + X[, 7])))


  sdlog <- 0.1 + 0.25 * (5 + exp(-2 * sqrt(abs(X[, 8] + X[, 5] + X[, 2])) - X[, 1] + X[, 2]^2 + X[, 3]^2))^{-1} +
    0.25 * (3 + exp(2 * sqrt(abs(X[, 3] + X[, 5] + X[, 7])) - X[, 1] + X[, 2]^2 + X[, 4]^2))^{  -1  } # Set xi


  # Generate non-stationary log-normal
  Y <- apply(cbind(meanlog, sdlog), 1, function(x) rlnorm(1, x[1], x[2]))
  return(list(Y = Y, meanlog = meanlog, sdlog = sdlog, X = X))
}
