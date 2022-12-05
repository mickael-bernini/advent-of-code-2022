setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 03

data.fn <- get_file_name(day, is.test)
data.in <- fread(data.fn, header = FALSE, col.names = "input")
list.in <- data.in[,input]

get_value <- function(x, ...) {
  len <- nchar(x)
  h.len <- len / 2
  let.x <- unlist(strsplit(x, ""))
  
  pt.1 <- let.x[1:h.len]
  pt.2 <- let.x[(1:h.len) + h.len]
  
  com.let <- intersect(pt.1, pt.2)
  val.x <- which(c(letters, LETTERS) == com.let)
  
  return(val.x)
}


res.1 <- sapply(X=list.in, FUN=get_value)
message(sum(res.1))

in_bag <- function(x, ...) {
  let.x <- strsplit(x, "")
  com.let <- intersect(let.x[[1]], intersect(let.x[[2]], let.x[[3]]))
  val.x <- which(c(letters, LETTERS) == com.let)
  
  return(val.x)
}
data.in[,bag := (1:.N - 1) %/% 3]
res.2 <- data.in[,in_bag(input), by=bag]
message(sum(res.2[,V1]))
