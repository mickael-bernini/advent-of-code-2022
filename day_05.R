setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 05

data.fn <- get_file_name(day, is.test)
data.in <- readLines(data.fn)

mid.line <- which(data.in == "")

part.1 <- data.in[1:(mid.line-1)]
part.1 <- rev(part.1)
crates <- strsplit(part.1, "")
crates <- as.data.table(crates)
crates <- crates[V1 != " "]
crates <- crates[, V1 := NULL]
crates.l <- transpose(crates)
crates.l <- as.list(crates.l)
cleanup <- function(li, ...) {
  li[li != " "]
}
crates.l <- lapply(X = crates.l, FUN = cleanup)

part.2 <- data.in[-(1:mid.line)]

sol_1 <- function(crates.l, part.2, ...) {
  pat.mess <- "move ([0-9]+) from ([0-9]+) to ([0-9]+)"
  for (mess.i in part.2) {
    
    ind.1 <- as.numeric(regex(mess.i, pat.mess, '\\1'))
    ind.2 <- as.numeric(regex(mess.i, pat.mess, '\\2'))
    ind.3 <- as.numeric(regex(mess.i, pat.mess, '\\3'))
    for (move.i in 1:ind.1) {
      crates.l[[ind.3]] <- c(crates.l[[ind.3]], tail(crates.l[[ind.2]], 1))
      crates.l[[ind.2]] <- head(crates.l[[ind.2]], -1)
    }
  }
  
  top <- sapply(X= crates.l, FUN = tail, 1)
  message(top)
}

sol_2 <- function(crates.l, part.2, ...) {
  pat.mess <- "move ([0-9]+) from ([0-9]+) to ([0-9]+)"
  for (mess.i in part.2) {
    
    ind.1 <- as.numeric(regex(mess.i, pat.mess, '\\1'))
    ind.2 <- as.numeric(regex(mess.i, pat.mess, '\\2'))
    ind.3 <- as.numeric(regex(mess.i, pat.mess, '\\3'))
    
    crates.l[[ind.3]] <- c(crates.l[[ind.3]], tail(crates.l[[ind.2]], ind.1))
    crates.l[[ind.2]] <- head(crates.l[[ind.2]], -ind.1)
  }
  
  top <- sapply(X= crates.l, FUN = tail, 1)
  message(top)
}

sol_1(crates.l, part.2)
sol_2(crates.l, part.2)