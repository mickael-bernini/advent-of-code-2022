setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- TRUE
day <- 12

data.fn <- get_file_name(day, is.test)
data.in <- readLines(data.fn)
data.in <- strsplit(data.in, "")
data.in <- as.data.table(data.in)
data.index <- as.matrix(data.in)

mat.alps <- matrix(data = data.index, nrow = data.in[,.N])

macfly.xy <- which(mat.alps == 'S', arr.ind=TRUE)
galibier.xy <- which(mat.alps == 'E', arr.ind=TRUE)

mat.alps[macfly.xy] <- 'a'
mat.alps[galibier.xy] <- 'z'
get_elev <- function(climb) {
  which(letters == climb)
}

elev.alps <- apply(X=mat.alps, MARGIN = c(1,2), FUN = get_elev)



try_climb <- function(climbs.done, elev.alps, i, j, current.elev, current.km) {
  message(i, '-', j)
  if (i <= 0 || j <= 0 || i > nrow(elev.alps) || j > ncol(elev.alps)) {
    return(climbs.done)
  }
  
  new.elev <- elev.alps[i, j]
  if (abs(current.elev - new.elev) > 2) {
    # Gradient too high
    return(climbs.done)
  }
  
  curr.dist <- current.km + 1
  if (is.na(climbs.done[i, j])) {
    climbs.done[i, j] <- curr.dist
  } else if (curr.dist < climbs.done[i, j]) {
    # There is a shortcut
    climbs.done[i, j] <- curr.dist
  } else {
    return(climbs.done)
  }
  
  # Let's try the next climb
  climbs.done <- try_climb(climbs.done, elev.alps, i - 1, j, new.elev, current.km+1)
  climbs.done <- try_climb(climbs.done, elev.alps, i + 1, j, new.elev, current.km+1)
  climbs.done <- try_climb(climbs.done, elev.alps, i, j + 1, new.elev, current.km+1)
  
  climbs.done <- try_climb(climbs.done, elev.alps, i, j + 1, new.elev, current.km+1)
  
  climbs.done <- try_climb(climbs.done, elev.alps, i, j + 1, new.elev, current.km+1)
  climbs.done <- try_climb(climbs.done, elev.alps, i + 1, j, new.elev, current.km+1)
  climbs.done <- try_climb(climbs.done, elev.alps, i - 1, j, new.elev, current.km+1)
  
  return(climbs.done)
}

climbs.done <- NA_integer_ * elev.alps
# climbs.done[macfly.xy] <- 0

climbs.done <- try_climb(climbs.done, elev.alps, macfly.xy[2], macfly.xy[1], elev.alps[macfly.xy], 0)

climbs.done
