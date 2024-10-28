# tag:string
# function to split text over multiple lines

split_text <- function(text, width = 30) {
  #' This code will split the `text` into multiple lines.
  #' You can adjust the `width` parameter to control 
  #' the maximum width of each line.
  #' 
  #' @param text A character vector of text to split.
  #' @param width An integer specifying the maximum width of each line.
  #' 
  #' @return A character vector of text split into multiple lines.
  #' 
  #' @examples 
  #' split_text("This is a long string that needs to be split into multiple lines.", width = 20)
  #' 
  map(text, strwrap, width = width) |>
    map(paste, collapse="\n") |>
    unlist()

}
