setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 14

get_routes <- function() {
  data.fn <- get_file_name(day, is.test)
  data.in <- readLines(data.fn)
  
  routes <- strsplit(x = data.in, split = ' -> ')
  max.y <- 200
  new.routes <- list()
  
  for (route.i in routes) {
    lis.x <- as.numeric(sapply(X = route.i, FUN=regex, pat='^([0-9]*),([0-9]*)$', repl='\\1'))
    lis.y <- as.numeric(sapply(X = route.i, FUN=regex, pat='^([0-9]*),([0-9]*)$', repl='\\2'))
    
    for (i in 2:length(route.i)) {
      route.lims <- list(
        from.x = lis.x[i-1],
        from.y = lis.y[i-1],
        to.x = lis.x[i],
        to.y = lis.y[i]
      )
      
      new.routes <- c(new.routes, list(route.lims))
    }
  }
  
  new.routes <- rbindlist(new.routes)
  new.routes[,max(from.y)]
  new.routes[,max(to.y)]
  
  new.routes
}

is_free <- function(pos, routes, used.pos, ...) {
  
}

routes <- get_routes()

attaques <- function(routes, ...) {
  pos.ini <- c(500, 0)
  
  while (TRUE) {
    pos.i <- pos.ini
    while (TRUE) {
      new.pos <- pos.i + c(0, 1)
      if (is_free(new.pos, routes, used.pos)) {
        pos.i <- new.pos
        next
      }
      
      new.pos <- pos.i + c(-1, 1)
      if (is_free(new.pos, routes, used.pos)) {
        pos.i <- new.pos
        next
      }
      
      new.pos <- pos.i + c(1, 1)
      if (is_free(new.pos, routes, used.pos)) {
        pos.i <- new.pos
        next
      }
      
      
    }
  }
}