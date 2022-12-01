#Global functions to be called in different files
#Do not include calls in this file (even for testing)


library(Rlab)
library(bayesAB)
library(data.table)
library(RPostgres)
library(googledrive)
library(googlesheets4)
library(RMySQL)
library(getPass)



# eval(parse(text=txt))


#' fswrite----
#' @param file file to save
#' @return NULL
fswrite <- function(file, ...) {
  tryCatch(
    expr={
      fwrite(file=file, ...)
    } , error=function(e) {
      message("Whoups! ", file, " is open when trying to save it.")
      tmp <- readline("Try again?")
      fswrite(file=file, ...)
    }
  )
}



#' coalesce----
#' @param v1 first vector
#' @param v2 second vector (or value)
#' @return 
coalesce <- function(v1, v2, ...) {
  if (length(v2) == length(v1)) {
    v1[is.na(v1)] <- v2[is.na(v1)]
  } else if (length(v2) == 1) {
    v1[is.na(v1)] <- v2
  } else {
    "Warning: wrong element size. v1 returned."
  }
  
  v1
}


#' nullif----
#' @param v1 first vector
#' @param v2 second vector (or value)
#' @return 
nullif <- function(v1, v2, ...) {
  if (length(v2) == length(v1) || length(v2) == 1) {
    v1[v1 == v2] <- NA
  } else {
    "Warning: wrong element size. v1 returned."
  }
  
  v1
}


#' regex
#' @param x string or vector
#' @param pat pattern
#' @param repl replacement value. If null, no replacement done
#' @param value get value or index. If null, boolean values are returned
#' @param ignore.case case sensitive or not
#' @param invert get matching or unmatching values
#' @param count write the number of element replaced
#' @param fixed if TRUE, pat is considered as a string, not a regular expression
regex <- function(x, pat, repl=NULL, value=NULL, ignore.case=FALSE, invert=FALSE, count=FALSE, fixed=FALSE) {
  if (count) {
    res <- gregexpr(text=x, pattern = pat, ignore.case = ignore.case, fixed=fixed)
    
    res1 <- sapply(X=res, FUN="[", 1)
    res2 <- sapply(X=res, FUN=length)
    res2[res1 == -1] <- 0
    
    return(res2)
  } else if (!is.null(repl)) {
    if (!is.null(value) && length(pat) == 1 && length(repl) == 1) {
      res <- regex(x=x, pat=pat, repl=NULL, value=value, ignore.case=ignore.case, invert=invert, count=count, fixed=fixed)
      res <- regex(x=res, pat=pat, repl=repl, value=NULL, ignore.case=ignore.case, invert=invert, count=count, fixed=fixed)
      return(res)
    }
    
    if (length(pat) == 1 && length(repl) == 1) {
      x <- gsub(x=x, pattern=pat, replacement=repl, ignore.case=ignore.case, fixed=fixed)
      return(x)
    }
    if (length(repl) == 1 && length(pat) > 1) {
      repl <- rep(x=repl, times=length(pat))
    }
    if (length(pat) == 1 && length(repl) > 1) {
      pat <- rep(x=pat, times=length(repl))
    }
    if (length(repl) != length(pat)) {
      size <- min(length(repl), length(pat))
      message("Warning: repl and pat sizes differ. Cut at ", size)
      repl <- repl[1:size]
      pat <- pat[1:size]
    }
    
    for (ind.i in 1:length(pat)) {
      x <- gsub(x=x, pattern=pat[ind.i], replacement=repl[ind.i], ignore.case=ignore.case, fixed=fixed)
    }
    return(x)
  } else if (!is.null(value)) {
    res <- grep(x=x, pattern=pat, ignore.case=ignore.case, invert=invert, value=value, fixed=fixed)
    return(res)
  } else {
    res <- xor(invert, grepl(x=x, pattern=pat, ignore.case=ignore.case, fixed=fixed))
    return(res)
  }
  message("Error: arguments not matching")
  return(x)
}



#' split_csv
#' split a csv into smaller files
#' @name split_csv
#' @param file.in name of the input csv file
#' @param samp.size the size of each batch (except the last one)
#' @param max.batch the maximal numbe of batches we want. A final file is saved with the remaining users
split_csv <- function(file.in, samp.size=10000, max.batch=Inf, random=TRUE, ...) {
  
  data.all <- fread(input = file.in)
  
  if (random) {
    data.all <- data.all[sample(x = .N, size = .N, replace=FALSE)]
  }
  part <- 0
  while (data.all[,.N]) {
    
    part <- part + 1
    if (part > max.batch) {
      file.out <- gsub(x=file.in, pattern = "\\.csv$", replacement=paste0(" (others).csv"))
      fwrite(x = data.all, file = file.out)
      break()
    }
    
    file.out <- gsub(x=file.in, pattern = "\\.csv$", replacement=paste0(" (", part, ").csv"))
    data.i <- data.all[1:min(samp.size, .N)]
    
    fwrite(x = data.i, file = file.out)
    
    if (data.all[,.N <= samp.size]) {
      break()
    }
    data.all <- data.all[(samp.size + 1):.N]
  }
}


#' execute_trycatch
#' execute a function with try-catch to prevent execution halted on errors
#' @name execute_trycatch
#' @param FUN function to execute
#' @param ... other parameters of the function
execute_trycatch <- function(FUN, ...) {
  tryCatch(
    expr={
      FUN(...)
    } , error=function(e) {
      e <- paste(e)
      logger(action='error', label=e, ...)
    }
  )
}

