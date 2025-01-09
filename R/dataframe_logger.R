library(logger)

log_data_frame <- function(df, name=NA){
  if (is.na(name)){
    name <- substitute(df)
  }
  
  log_info(paste("Data frame", name, "has", nrow(df), "rows and", ncol(df), "columns"))
}

log_data_frame_pattern <- "Data frame (.*?) has (.*?) rows and (.*?) columns"

log_as_df <- function(path, pattern=" \\[|\\] ", 
                      .colnames=c("Level", "Timestamp", "Message")){
  .log <- str_split(readLines(path), pattern) 
  .log <- as_tibble(reduce(.log, rbind))
  colnames(.log) <- .colnames
  .log
}

parse_df_log <- function(log_df,. pattern=log_data_frame_pattern){
  log_df |> 
    filter(str_detect(Message, pattern)) |>
    mutate(
      Name = str_replace(Message, pattern, "\\1"),
      P = str_replace(Message, pattern, "\\2"),
      Q = str_replace(Message, pattern, "\\3")
    )  
}
