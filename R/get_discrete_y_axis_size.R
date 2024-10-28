# Detect the size of a discrete y-axis in a ggplot.
# For use with ggsave to scale relative to elements being plotted.

get_discrete_y_axis_size <- function(.plt){
  #' Get the range of a discrete y-axis.
  #' 
  #' @param .plt A ggplot object.
  #' 
  #' @return The range of the y-axis.
  #' 
  #' @examples
  #' 
  #' plt <- ggplot(mtcars, aes(x=mpg, y=as.factor(cyl))) + geom_point()
  #' 
  #' get_discrete_y_axis_size(plt)
  #' 
  .built <- ggplot_build(.plt)
  
  .bool <- .built$data |>
    map("y") |>
    map(~"mapped_discrete" %in% class(.)) |>
    simplify() |>
    all()
  
  if (!.bool){
    rlang::abort("Expecting 'discrete' y-axis mapping.")
  }
  
  .built$data |>
    map("y") |>
    map(max) |>
    simplify() |>
    max()
  
}
