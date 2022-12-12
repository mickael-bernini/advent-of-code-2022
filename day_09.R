setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 09

data.fn <- get_file_name(day, is.test)

all.lines <- readLines(data.fn)

input_to_moves <- function(all.lines, ...) {
  moves <- c()
  for (line.i in all.lines) {
    dir <- regex(line.i, '^(R|L|U|D) ([0-9]+)$', '\\1')
    len <- as.numeric(regex(line.i, '^(R|L|U|D) ([0-9]+)$', '\\2'))
    
    moves <- c(moves, rep(dir, len))
  }
  moves
}

frikkie_to_amin <- function(amin, frik, ...) {
  af.1 <- frik[1] - amin[1]
  af.2 <- frik[2] - amin[2]
  
  max.diff <- max(abs(af.1), abs(af.2))
  if (max.diff >= 2) {
    # Frikkie has to move to join Amin
    if (af.1 != 0) {
      frik[1] <- frik[1] - sign(af.1)
    }
    if (af.2 != 0) {
      frik[2] <- frik[2] - sign(af.2)
    }
  }
  frik
}


move_2 <- function(moves, len = 2, ...) {
  amin <- c(0, 0)
  frik <- c(0, 0)
  relive <- list(amin, frik)
  while (length(relive) < len) {
    relive <- c(relive, list(frik))
  }
  done.pos <- list()
  
  for (move.i in moves) {
    dir.sign <- ifelse(move.i %in% c('R', 'U'), +1, -1)
    dir.dire <- ifelse(move.i %in% c('R', 'L'), 1, 2)
    
    for (frik.i in 1:len) {
      if (frik.i == 1) {
        # That's Amin
        amin <- relive[[1]]
        amin[dir.dire] <- amin[dir.dire] + dir.sign
        amin -> relive[[1]]
      } else {
        frik <- relive[[frik.i]]
        amin <- relive[[frik.i - 1]]
        frik <- frikkie_to_amin(amin, frik)
        frik -> relive[[frik.i]]
      }
    }
    
    done.pos <- c(done.pos, list(relive[[len]]))
  }
  
  return(length(unique(done.pos)))
}

list.moves <- input_to_moves(all.lines)
message(move_2(list.moves, 2))
message(move_2(list.moves, 10))
