# Tag:regex
# Tag: string_manipulation
library(stringr)

.str <- c("A:B", "1:2")

str_replace(.str, "(.*):(.*)", "\\2 - \\1")
