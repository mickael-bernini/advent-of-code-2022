setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 08

data.fn <- get_file_name(day, is.test)

data.dt <- readLines(data.fn)
data.dt <- strsplit(data.dt, '')
data.dt <- lapply(X = data.dt, FUN=as.numeric)
mat.dt <- as.matrix(as.data.table(data.dt))

is_visible <- function(amin, relive, ...) {
  amin == max(relive) && (sum(relive == amin) == 1)
}
count_visible <- function(amin, relive, ...) {
  relive <- tail(relive, -1)
  if (all(relive < amin)) {
    return(length(relive))
  } else {
    first <- which.max(relive >= amin)
    return(first)
  }
}


tot.visible <- 0
view.max <- 0
for (i in 1:nrow(mat.dt)) {
  for (j in 1:ncol(mat.dt)) {
    is.visible <- 
      is_visible(mat.dt[i, j], mat.dt[1:i, j]) ||
      is_visible(mat.dt[i, j], mat.dt[i:nrow(mat.dt), j]) ||
      is_visible(mat.dt[i, j], mat.dt[i, 1:j]) ||
      is_visible(mat.dt[i, j], mat.dt[i, j:ncol(mat.dt)])
    
    view <-
      count_visible(mat.dt[i, j], mat.dt[i:1, j]) * 
      count_visible(mat.dt[i, j], mat.dt[i:nrow(mat.dt), j]) * 
      count_visible(mat.dt[i, j], mat.dt[i, j:1]) * 
      count_visible(mat.dt[i, j], mat.dt[i, j:ncol(mat.dt)])
    view.max <- max(view, view.max)
    tot.visible <- tot.visible + is.visible
  }
}

message(tot.visible)
message(view.max)

