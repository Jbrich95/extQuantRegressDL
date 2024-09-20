rm(list = ls())


## Note that training of the bGEV deep regression model can be computationally intensive.
## Training of a single model on a laptop using CPUs will take roughly 1 to 2 hours.
## The predictions from 200 bootstrap samples are saved in `Predictions/`.
reticulate::use_virtualenv("eQRDL", required = T)
library(keras)
library(tensorflow)

source("bGEV_loss_functions.R")

# Set bootstrap number

boot.num <- 1

# If boot.num <- 0, no bootstrap resampling is done

# Define the architecture for the MLP.
# L = 5 layers, each of width 10
nunits <- rep(10, 5)

# Load data
load("monthly_max_data.Rdata")


# Normalise inputs
for (i in 1:dim(X)[3]) {
  temp <- X[, , i]
  m <- mean(temp, na.rm = T)
  s <- sd(temp, na.rm = T)
  temp <- (temp - m) / s
  X[, , i] <- temp
}

# Re-sample data to get a bootstrap sample
set.seed(boot.num)
N <- dim(Y)[1]
all_inds <- sample(1:N, N, replace = T)
if(boot.num ==0) all_inds = 1:N
Y.boot <- Y[all_inds, ]
X.boot <- X[all_inds, , ]

# Transform Y and X to long vector and matrix
dim(X.boot)
Y.boot <- c(Y.boot)
tmp <- matrix(nrow = length(Y), ncol = dim(X.boot)[3])
for (i in 1:dim(X)[3]) tmp[, i] <- X.boot[, , i]
X.boot <- tmp
dim(X.boot)

# Subset into validation and training data
# Make 10% validation data and 10% test data
valid.inds <- sample(1:nrow(X.boot), nrow(X.boot) / 10)
test.inds <- sample((1:nrow(X.boot))[-valid.inds], nrow(X.boot) / 10)
Y.train <- Y.boot[-valid.inds]
X.train <- X.boot[-valid.inds, ]
Y.valid <- Y.boot[valid.inds]
X.valid <- X.boot[valid.inds, ]

# Define data used for testing
Y.test <- Y.boot[test.inds]
X.test <- X.boot[test.inds, ]

Y.train <- as.matrix(Y.train)
Y.valid <- as.matrix(Y.valid)
Y.test <- as.matrix(Y.test)

# Build MLP model
input_nn <- layer_input(shape = dim(X.boot)[2], name = "nn_input") # This defines the input layer

# Define inital values for the parameters
init_xi <- 0.1
init_loc <- 3
init_spread <- 1

# Build hidden layers - Note that we define L1 and L2 regularisation via the kernel_regularizer
dropout_rate <- 0.3 # Define dropout rate at the end of each hidden layer
Branch <- input_nn %>%
  layer_dense(
    units = nunits[1], activation = "relu",
    input_shape = dim(X.boot)[2], name = "nonlin_dense1", kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)
  ) %>%
  layer_dropout(dropout_rate)
for (i in 2:length(nunits)) {
  Branch <- Branch %>%
    layer_dense(
      units = nunits[i], activation = "relu", name = paste0("nonlin_dense", i),
      kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)
    ) %>%
    layer_dropout(dropout_rate)
}

# Add final output layer. We used an exponential activation function to ensure all parrameters are strictly positive.

Branch <- Branch %>% layer_dense(
  units = 3, activation = "exponential", name = paste0("nonlin_dense"),
  weights = list(matrix(0, nrow = nunits[length(nunits)], ncol = 3), array(c(log(init_loc), log(init_spread), log(init_xi)))),
  kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)
)

# Build Keras model
model <- keras_model(
  inputs = c(input_nn),
  outputs = c(Branch)
)
summary(model)

# Defines loss function. This uses the default hyper-parameterisation for the bGEV
loss <- bGEV_loss()


# Define the checkpoints. This will save the model weights during training.

checkpoint <- callback_model_checkpoint(paste0("Models/boot_", boot.num),
  monitor = "val_loss", verbose = 0,
  save_best_only = TRUE, save_weights_only = TRUE, mode = "min",
  save_freq = "epoch"
)

# Compile Keras model with adam optimiser and bGEV loss

model %>% compile(
  optimizer = "adam",
  loss = loss,
  run_eagerly = T
)

# Train model
n.epochs <- 250 # Train for 250 epochs. It is very unlikely that training will run for the full length.
mini.batch.size <- 512 # Mini-batch size of 512

## You may encounter NaN values during training, at which point training will stop.
## These values are caused by numerical precision errors in evaluation of the loss function,
## particularly when the parameter estimates become very small.
## The model fit that is returned should still be reasonable.


history <- model %>% fit(
  list(X.train), Y.train,
  shuffle = T,
  epochs = n.epochs, batch_size = mini.batch.size,
  callback = list(checkpoint, callback_early_stopping(
    monitor = "val_loss", min_delta = 0, patience = 5
  )),
  validation_data = list(list(nn_input = X.valid), Y.valid),
  verbose = 2
)

# Load the best weights from the checkpoint
model <- load_model_weights_tf(model, filepath = paste0("Models/boot_", boot.num))

# Get out-of-sample test predictions
preds.test <- model %>% predict(X.test)

# Evaluate the loss on the test data set
print(paste0(
  "Test loss = ",
  round(
    k_get_value(loss(k_constant(Y.test), k_constant(preds.test))),
    digits = 3
  )
))


# Get original covariates
tmp <- matrix(nrow = length(Y), ncol = dim(X)[3])
for (i in 1:dim(X)[3]) tmp[, i] <- c(X[, , i])
X.pred <- tmp

# Predicted parameter estimates
preds <- model %>% predict(X.pred)
dim(preds) <- c(nrow(X), ncol(X), 3)

# Save predicted paramters for this bootstrap sample
save(preds, file = paste0("Predictions/preds_boot", boot.num, ".Rdata"))
