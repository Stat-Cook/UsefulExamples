{
  # Setup
  library(gapminder)
  library(gganimate)
  library(tidyverse)
  
  frm <- expand.grid(
    Ward = LETTERS[1:10],
    Year = 2020:2030
  ) %>% 
    tibble() %>%
    mutate(
      A = rnorm(nrow(.))
    )
}

# Animated Plot
frm %>% 
  ggplot(aes(x=Ward, y=A))  + geom_point() +  
  transition_time(Year) +
  ease_aes('linear')
