# tag: torch
# tag: dimensional_redcution
# tag: atuo_embeder

library(torch)
library(luz)

# Artificial data
n <- 1e3
m <- rbind(
    matrix(rnorm(n*100), ncol=100),
    matrix(rnorm(n*100, c(0, 1, 2)), ncol=100)
  )
index <- c(rep(1, n), rep(2, n))

# torch::dataset 
# R6 template to define a torch data object.
# Note 'torch_tensor(...)' usage.
example_dataset <- dataset(
  "Example",
  initialize = function(.data){
     self$data = torch_tensor(.data)
  },
 
 .getitem = function(i){
   list(
     x = self$data[i,],
     y = self$data[i,]
   )
  },
   
 .length = function(){
   nrow(self$data)
 }
)

# torch::nn_module
# 2 major methods:
#   'initialize' - define any persistent weight levels.
#   `forward` - defines the predictive steps
auto_embed <- nn_module(
  "auto_embeder",
  initialize = function(){
    self$lin1 = nn_linear(100, 50)
    self$lin2 = nn_linear(50, 20)
    self$lin3 = nn_linear(20, 3)
    self$lin4 = nn_linear(3, 20)
    self$lin5 = nn_linear(20, 50)
    self$lin6 = nn_linear(50, 100)
  
  },
  
  middle = function(x){
    x %>%
      self$lin1()  %>%
      nnf_relu() %>%
      self$lin2()  %>%
      nnf_relu() %>%
      self$lin3()  
  },

  forward = function(x){
    # Note the use of `nnf_` functions for activation layers. 
    x %>%
      self$lin1()  %>%
      nnf_relu() %>%
      self$lin2()  %>%
      nnf_relu() %>%
      self$lin3()  %>%
      nnf_relu() %>%
      self$lin4()  %>%
      nnf_relu() %>%
      self$lin5()  %>%
      nnf_relu() %>%
      self$lin6()

  }
)

{
  # initializes the dataset and creates a batched data loader
  e1 <- example_dataset(m)  
  train_dl <- dataloader(e1, 64, shuffle=T)
}

# Crate the training loop with `setup` and `fit`
# The ADAM optimizier is a standard choice
# by default 'loss' will compare the outcome of 'forward' and the y term of dataset$.getitem(...)
fitted <- auto_embed %>% 
  setup(
    loss = function(yhat, ytrue) nnf_mse_loss(yhat, ytrue),
    optimizer = optim_adam
  ) %>% 
  fit(train_dl, epoch=10)
