# Detect the size of a discrete y-axis in a ggplot.
# For use with ggsave to scale relative to elements being plotted.

get_discrete_y_axis_size <- function(.plt) {
  #' Get the range of a discrete y-axis.
  #'
  #' @param .plt A ggplot object.
  #'
  #' @return The range of the y-axis.
  #'
  #' @examples
  #' plt <- ggplot(mtcars, aes(x = mpg, y = as.factor(cyl))) + geom_point()
  #' get_discrete_y_axis_size(plt)
  
  # Build the ggplot object
  .built <- ggplot_build(.plt)
  
  # Check if y-axis is discrete
  is_discrete <- all(map_lgl(.built$data, ~ "mapped_discrete" %in% class(.x$y)))
  
  if (!is_discrete) {
    rlang::abort("Expecting 'discrete' y-axis mapping.")
  }
  
  # Get the maximum value of the y-axis
  max_y <- max(map_dbl(.built$data, ~ max(.x$y, na.rm = TRUE)))
  
  return(max_y)
}
