# plumber.R

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="", ...) {
  print(msg)
  l <- list(...)
  print(l)
  list(msg = paste0("The message is: '", msg, "'"))
}



#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
  rand <- rnorm(100)
  hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
  as.numeric(a) + as.numeric(b)
}
