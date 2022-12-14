setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 13

get_pairs <- function() {
  data.fn <- get_file_name(day, is.test)
  data.in <- readLines(data.fn)
  
  pairs <- list()
  pair <- c()
  for (line.i in data.in) {
    if (line.i != "") {
      pair <- c(pair, line.i)
      if (length(pair) == 2) {
        pairs[[length(pairs) + 1]] <- pair
        pair <- c()
      }
    }
  }
  
  pairs
}

comp_1 <- function(elem, ...) {
  is.list(elem) || regex(elem, "^-?[0-9]*$")
}
comp_2 <- function(elem, type, ...) {
  !is.list(elem) && (elem == type)
}

tab_to_list <- function(tab, is.amin, ...) {
  tab
  if (length(tab) == 0) {
    return("-1")
  } else if (all(sapply(FUN=comp_1, X=tab))) {
    al.list <- sapply(X = tab, FUN = is.list)
    tab[!al.list] <- as.numeric(tab[!al.list])
    return(tab)
  } else {
    ops <- which(sapply(FUN=comp_2, X=tab, type='['))
    clo <- which(sapply(FUN=comp_2, X=tab, type=']'))
    
    ind.1 <- tail(ops, 1)
    clo <- clo[clo > ind.1]
    ind.2 <- head(clo, 1)
    
    if (ind.1 > 1) {
      pt.1 <- tab[1:(ind.1-1)]
    } else {
      pt.1 <- NULL
    }
    if (ind.2 < length(tab)) {
      pt.2 <- tab[(ind.2+1):length(tab)]
    } else {
      pt.2 <- NULL
    }
    
    if (ind.2 > ind.1 + 1) {
      pt.m <- list(tab_to_list(tab[(ind.1+1):(ind.2-1)], is.amin=is.amin))
    } else {
      if (!is.amin) {
        pt.m <- "-1"
      } else {
        pt.m <- "1000"
      }
    }
    res <- c(
      pt.1,
      pt.m,
      pt.2
    )
    
    return(tab_to_list(res, is.amin=is.amin))
  }
  
}

get_item <- function(strg, ...) {
  message(strg)
  
  pat = "^,?(\\[|\\]|([0-9]*))(.*)$"
  result <- c()
  while (nchar(strg)) {
    elem <- regex(strg, pat, '\\1')
    result <- c(result, elem)
    strg <- regex(strg, pat, '\\3')
  }
  
  
  result <- as.list(result)
  res.list <- tab_to_list(result, ...)
  
  res.list
}



compare_af <- function(amin, frik) {
  if (is.null(frik)) {
    return(FALSE)
  }
  if (is.null(amin)) {
    return(TRUE)
  }
  if (is.list(amin) && is.list(frik)) {
    amin.l <- length(amin)
    frik.l <- length(frik)
    max.len <- max(amin.l, frik.l)
    for (i in 1:max.len) {
      if (i > frik.l) {
        return(FALSE)
      } else if (i > amin.l) {
        return(TRUE)
      }
      res <- compare_af(amin[[i]], frik[[i]])
      if (!is.na(res)) {
        return(res)
      }
    }
    
    return(NA)
    
    # if (length(amin) < length(frik)) {
    #   message("len(frik) < len(amin)")
    #   return(FALSE)
    # }
    # 
    # if (length(amin)) {
    #   amin <- amin[1:length(frik)]
    #   res <- mapply(FUN=compare_af, amin=amin, frik=frik)
    #   
    #   return(all(res))
    # } else {
    #   message("len(amin) == 0")
    #   return(TRUE)
    # }
  } else if (is.list(amin) && !is.list(frik)) {
    frik <- list(frik)
    return(compare_af(amin, frik))
  } else if (!is.list(amin) && is.list(frik)) {
    amin <- list(amin)
    return(compare_af(amin, frik))
  } else {
    # num vs num
    message(amin, " vs ", frik)
    if (amin == frik) {
      return(NA)
    }
    return(amin < frik)
  }
}

analyse_pair <- function(pair) {
  
  amin <- get_item(pair[[1]], is.amin=TRUE)
  frik <- get_item(pair[[2]], is.amin=FALSE)
  
  res <- compare_af(amin, frik)
  message(" ---------------", res)
  res
}

pairs <- get_pairs()
result <- sapply(FUN=analyse_pair, X=pairs)
message(sum(which(result)))
# 88 is not the correct answer
# 86 is not the correct answer
# 8681 is not the correct answer
# 113 too low