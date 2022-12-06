setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 06

data.fn <- get_file_name(day, is.test)
data.in <- readLines(data.fn)

find_message <- function(letrs, num.dist, ...) {
  for (i in num.dist:length(letrs)) {
    let.4 <- letrs[i + 1 - (1:num.dist)]
    if (length(unique(let.4)) == num.dist) {
      message(i)
      break
    }
  }
  
}

letrs <- strsplit(data.in, '')[[1]]
find_message(letrs=letrs, num.dist=4)
find_message(letrs=letrs, num.dist=14)
