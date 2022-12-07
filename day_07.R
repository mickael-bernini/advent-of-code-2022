setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 07

data.fn <- get_file_name(day, is.test)
all.lines <- readLines(data.fn)

list.dirs <- c()
curr.path <- c()
line.i <- all.lines[1]
for (line.i in all.lines) {
  if (regex(line.i, "\\$ cd")) {
    # Moving to a folder
    new.path <- regex(line.i, "\\$ cd (.*)$", '\\1')
    if (new.path == '..') {
      curr.path <- head(curr.path, -1)
    } else {
      curr.path <- c(curr.path, new.path)
      lay.name <- paste0(curr.path, collapse='/')
      list.dirs[[lay.name]] <- 0
    }
    
  } else if (regex(line.i, "^([0-9]+) (.*)$")) {
    # That's a file
    f.size <- as.numeric(regex(line.i, "^([0-9]+) (.*)$", "\\1"))

    for (layer.i in 1:length(curr.path)) {
      lay.name <- paste0(curr.path[1:layer.i], collapse='/')
      list.dirs[[lay.name]] <- list.dirs[[lay.name]] + f.size
    }
  }
}

max.size <- 100000
small.dirs <- list.dirs[list.dirs < max.size]
message(sum(small.dirs))


tot.size <- max(list.dirs)
max.keep <- 70000000 - 30000000
need.remove <- tot.size - max.keep
can.remove <- list.dirs[list.dirs > need.remove]
min(can.remove)
