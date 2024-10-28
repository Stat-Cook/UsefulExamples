# tag: torch
# tag: dimensional_reduction
# tag: auto_embedder

library(torch)
library(luz)

# Create artificial data
n <- 1e3
m <- rbind(
  matrix(rnorm(n * 100), ncol = 100),
  matrix(rnorm(n * 100, mean = c(0, 1, 2)), ncol = 100)
)
index <- c(rep(1, n), rep(2, n))

# Define a torch dataset using the R6 template
example_dataset <- dataset(
  name = "Example",
  initialize = function(data) {
    self$data <- torch_tensor(data)
  },
  .getitem = function(i) {
    list(
      x = self$data[i, ],
      y = self$data[i, ]
    )
  },
  .length = function() {
    nrow(self$data)
  }
)

# Define a torch neural network module
auto_embed <- nn_module(
  name = "auto_embedder",
  initialize = function() {
    self$lin1 <- nn_linear(100, 50)
    self$lin2 <- nn_linear(50, 20)
    self$lin3 <- nn_linear(20, 3)
    self$lin4 <- nn_linear(3, 20)
    self$lin5 <- nn_linear(20, 50)
    self$lin6 <- nn_linear(50, 100)
  },
  middle = function(x) {
    x %>%
      self$lin1() %>%
      nnf_relu() %>%
      self$lin2() %>%
      nnf_relu() %>%
      self$lin3()
  },
  forward = function(x) {
    # Use `nnf_` functions for activation layers
    x %>%
      self$lin1() %>%
      nnf_relu() %>%
      self$lin2() %>%
      nnf_relu() %>%
      self$lin3() %>%
      nnf_relu() %>%
      self$lin4() %>%
      nnf_relu() %>%
      self$lin5() %>%
      nnf_relu() %>%
      self$lin6()
  }
)

# Initialize the dataset and create a batched data loader
e1 <- example_dataset(m)
train_dl <- dataloader(e1, batch_size = 64, shuffle = TRUE)

# Create the training loop with `setup` and `fit`
# Using the ADAM optimizer and MSE loss function
fitted <- auto_embed %>%
  setup(
    loss = function(yhat, ytrue) nnf_mse_loss(yhat, ytrue),
    optimizer = optim_adam
  ) %>%
  fit(train_dl, epochs = 10)
