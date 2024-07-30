#' Utilities for handling tidyeval/ delayed evaluation 

dots_as_names <- function(...){
  #' Capture input
  
  quo <- rlang::enquos(...)
  map(quo, rlang::quo_squash)
}

args_kwargs <- function(...){
  #' seperate out positional arguments (args) 
  #' from key word arguments (kwargs)
  dots <- dots_as_names(...)
  
  args <- dots[names(dots) == ""]
  kwargs <- dots[names(dots) != ""]
  
  list(args = args,
       kwargs = kwargs)  
  
}

group_and_mutate <- function(data, ...){
  #' Example of how to unpack args/kwargs when needed
  ak <- args_kwargs(...)
  
  data |>
    dplyr::group_by(!!!ak$args) %>% 
    dplyr::mutate(
      !!!ak$kwargs
    )
}
