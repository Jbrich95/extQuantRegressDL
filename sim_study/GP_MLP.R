reticulate::use_virtualenv("eQRDL", required = T)
library(keras)
library(tensorflow)
library(evd)

# Define the GPD loss function used in training

GPD_loss <- function(y_true, y_pred) {
  K <- backend()
  
  sig <- y_pred[all_dims(), 1]
  xi <- y_pred[all_dims(), 2]
  y <- y_true[all_dims(), 1]
  
  # Evaluate log-likelihood
  ll1 <- -(1 / xi + 1) * K$log(1 + xi * y / sig)
  
  ll2 <- -K$log(sig)
  
  return(-(K$mean(ll1) + K$mean(ll2)))
}


# Set type 1 or 2
type <- 1
if (type == 1) source("sim_GP.R") else source("sim_lnorm.R")

# Target quantile levels
tau <- c(0.25, 0.5, 0.8, 0.9, 0.95, 0.99, 0.999, 0.9999)

# Set experiment number
no.experiment <- 1

# Considered sample sizes
ns <- c(1e4, 1e6)

# Define the architecture for the MLP.
# nunits.xi defines the architecture for xi, while nunits.sig defines the architecture for sigma
nunits.xi <- nunits.sig <- c(16, 16)


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

  # Make 20% validation data
  valid.inds <- sample(1:nrow(X), nrow(X) / 5)
  Y.train <- Y[-valid.inds]
  X.train <- X[-valid.inds, ]
  Y.valid <- Y[valid.inds]
  X.valid <- X[valid.inds, ]
  Y.train <- as.matrix(Y.train)
  Y.valid <- as.matrix(Y.valid)

  # Build MLP model
  input_nn <- layer_input(shape = dim(X)[2], name = "nn_input") # Define the input layer

  # We first define the model for xi
  
  # Build hidden layers - Note that we define L1 and L2 regularisation via the kernel_regularizer
  xiBranch <- input_nn %>%
    layer_dense(
      units = nunits.xi[1], activation = "relu",
      input_shape = dim(X)[2], name = "nonlin_xi_dense1", kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)
    )
  for (i in 2:length(nunits.xi)) {
    xiBranch <- xiBranch %>%
      layer_dense(
        units = nunits.xi[i], activation = "relu", name = paste0("nonlin_xi_dense", i),
        kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)
      )
  }
  # Add final output layer. We used an exponential activation function to ensure xi > 0.
  xiBranch <- xiBranch %>% layer_dense(units = 1, activation = "exponential", name = paste0("nonlin_xi_dense"))

  # We now define the model for sigma
  
  # Build hidden layers - Note that we define L1 and L2 regularisation via the kernel_regularizer
  sigBranch <- input_nn %>%
    layer_dense(
      units = nunits.sig[1], activation = "relu",
      input_shape = dim(X)[2], name = "nonlin_sig_dense1", kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)
    )
  for (i in 2:length(nunits.sig)) {
    sigBranch <- sigBranch %>%
      layer_dense(
        units = nunits.sig[i], activation = "relu", name = paste0("nonlin_sig_dense", i),
        kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)
      )
  }
  
  # Add final output layer. We used an exponential activation function to ensure sigma > 0.
  sigBranch <- sigBranch %>% layer_dense(units = 1, activation = "exponential", name = paste0("nonlin_sig_dense"))

  # Concatenate sigma and xi into single tensor.
  output <- layer_concatenate(c(sigBranch, xiBranch))

  # Build Keras model
  model <- keras_model(
    inputs = c(input_nn),
    outputs = c(output)
  )
  summary(model)


  # Compile Keras model with adam optimiser and GPD loss
  model %>% compile(
    optimizer = "adam",
    loss = GPD_loss,
    run_eagerly = T
  )
  
  # Train model
  n.epochs = 250  #Train for 250 epochs
  mini.batch.size = 512 #Mini-batch size of 512
  
  history <- model %>% fit(
    list(X.train), Y.train,
    epochs = n.epochs, batch_size = mini.batch.size,
    callback = list(callback_early_stopping(  # Here we define the early stopping criterion
      monitor = "val_loss", min_delta = 0, patience = 5 )),
    validation_data = list(list(nn_input = X.valid), Y.valid),
    verbose = 2
  )



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
  preds <- model %>% predict(list(X.test))

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
  if (type == 1) save(MSE, file = paste0("sim_study_results/GPD/GPD_NN/n", n, "_exp", no.experiment, ".Rdata"))
  if (type == 2) save(MSE, file = paste0("sim_study_results/lnorm/GPD_NN/n", n, "_exp", no.experiment, ".Rdata"))
}
