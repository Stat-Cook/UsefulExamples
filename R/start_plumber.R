library(plumber)
"plumber_examples.r"

# 'plumber.R' is the location of the file shown above
pr("plumber_examples.r") %>%
  pr_run(port=8000)
