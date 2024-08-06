library(httr)

get.response <- GET("http://127.0.0.1:8000/echo?msg=Barry")
content(get.response)



options <- list(
  a = 2,
  b = 6
)
post.response <- POST("http://127.0.0.1:8000/sum", body = options, encode = "form")
content(post.response)


