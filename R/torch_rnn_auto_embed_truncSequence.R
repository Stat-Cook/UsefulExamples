library(luz)
library(torch)

{
  seq.x <- c(rnorm(5*2*1e3), rnorm(5*2*1e3, 2))
  seq.x <- torch_reshape(seq.x, c(-1, 5, 2))
  dim(seq.x)
}

sequence_dataset <- dataset(
  name = "numeric_dataset",
  
  initialize = function(indices) {
    data <- seq.x[indices,,]
    self$x <- data
    self$y <- data
  },
  
  .getitem = function(i) {
    x <- self$x[i,,]
    y <- self$y[i,,]
    
    list(x = x, y = y)
  },
  
  .length = function() {
    dim(self$x)[1]
  }
)

{
  .n <- dim(seq.x)[1]
  
  train_index <- sample(.n, floor(0.8*.n))
  
  train_sequence <- sequence_dataset(train_index)
  train_seq_dl <- dataloader(train_sequence, 64)
  
  test_sequence <- sequence_dataset(-train_index)
  test_seq_dl <- dataloader(test_sequence, 64)
}

n_layers <- 1
hidden_size <- 50
input_size <- dim(seq.x)[3]

embed_size <- 4



sequence.rnn <- nn_module(
  "sequence.rnn",
  initialize = function(){
    self$rnn1 <- nn_gru(
      input_size,
      hidden_size, 
      n_layers,
      batch_first = T
    )
    
    self$f1 <- nn_linear(hidden_size, 20)
    self$f2 <- nn_linear(20, embed_size)

    self$b2 <- nn_linear(embed_size, 20)
    self$b1 <- nn_linear(20, 200)
    
    self$rnn2 <- nn_gru(
      200,
      50,
      n_layers,
      batch_first = T
    )
    
    self$embed_sequence <- nn_sequential(
      self$f1,
      nn_relu(),
      self$f2
    )
    
    self$linear_sequence <- nn_sequential(
      self$embed_sequence,
      nn_relu(),
      self$b2,
      nn_relu(),
      self$b1
    )
    
    self$output <- nn_sequential(
      nn_linear(50, 200),
      nn_relu(),
      nn_linear(200, input_size)
    )
    
  },
  forward = function(x){
    embed <- self$rnn1(x)[[1]]
    
    seq_length <- dim(embed)[2]
    # Limit data to last item of the sequence
    linear <- self$linear_sequence(embed[,-1,])

    # Replicate and stack and re-embed to reproduce data.
    linear <- torch_stack(
      replicate(seq_length, linear),
      2
    )

    self$rnn2(linear)[[1]]  |>
      self$output()
    
  },
  embed = function(x){
    self$rnn1(x)[[1]][,-1,] |>
      self$embed_sequence()
  }
)

model <- setup(
  sequence.rnn,
  loss = function(yhat, ytrue) nnf_mse_loss(yhat, ytrue),
  optimizer = optim_adam
)

fitted <- fit(model, train_seq_dl, epochs = 20)

m <- model()
seq.x[1:3,,] |>
  dim()
  m$forward() |>
  dim()

hat <- predict(fitted, seq.x)

seq.x[1,,]
hat[1,,]

seq.x[1001,,]
hat[1001,,]

{
  .embed <- fitted$model$embed(seq.x)
  dim(.embed)
  library(tidyverse)
  
  .class <- c(rep("A", 1e3),rep("B", 1e3))
  .frm <- as.matrix(.embed) |>
    as_tibble() |>
    mutate(
      Class = .class
    )
  
  .frm |>
    ggplot(aes(x=V1, y=V2, color=Class)) + geom_point()
  .frm |>
    ggplot(aes(x=V1, y=V3, color=Class)) + geom_point()
  .frm |>
    ggplot(aes(x=V2, y=V4, color=Class)) + geom_point()
  .frm |>
    ggplot(aes(x=V3, y=V4, color=Class)) + geom_point()
}
