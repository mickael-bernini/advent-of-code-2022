setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 11

data.fn <- get_file_name(day, is.test)
data.in <- readLines(data.fn)

get_amins <- function(lines, ...) {
  amins <- list()
  for (line.i in lines) {
    if (regex(line.i, "Monkey ([0-9]*):")) {
      amin <- list()
      amin.i <- as.numeric(regex(line.i, "Monkey ([0-9]*):", "\\1")) + 1
      amins[[amin.i]] <- list(bike.checks=0)
      
    } else if (regex(line.i, "Starting items")) {
      bikes <- regex(line.i, "Starting items: *([0-9, ])", "\\1")
      bikes <- strsplit(bikes, ",")[[1]]
      bikes <- as.numeric(bikes)
      
      amins[[amin.i]]$bikes <- bikes
    } else if (regex(line.i, " *Operation: new = old ([+\\*/-]) ([0-9]*)")) {
      fun <- regex(line.i, " *Operation: ", "")
      fun <- regex(fun, "=", "<-")
      fun <- regex(fun, "new", "new.bike")
      amins[[amin.i]]$ope <- fun
    } else if (regex(line.i, "Test: divisible by ([0-9]*)")) {
      amins[[amin.i]]$div_by <- as.numeric(regex(line.i, "Test: divisible by ([0-9]*)", "\\1"))
    } else if (regex(line.i, "If true: throw to monkey ([0-9]*)")) {
      amins[[amin.i]]$if_true <- as.numeric(
        regex(line.i, "If true: throw to monkey ([0-9]*)", "\\1")
      )     
    } else if (regex(line.i, "If false: throw to monkey ([0-9]*)")) {
      amins[[amin.i]]$if_false <- as.numeric(
        regex(line.i, "If false: throw to monkey ([0-9]*)", "\\1")
      )     
    } else if (line.i == "") {
    } else {
      message("oups")
    }
  }
  
  amins
}

get_bikes <- function(amins, ...) {
  bikes <- list()
  for (amin.i in 1:length(amins)) {
    amin.bikes <- amins[[amin.i]]$bikes
    for (bike.i in amin.bikes) {
      bikes[[length(bikes) + 1]] <- list(
        value = bike.i,
        owner = amin.i,
        current_owner = amin.i,
        is_looped = FALSE,
        bike.check = 0
      )
    }
  }
  
  bikes
}

process_amin <- function(amins, index, div.3 = TRUE, ...) {
  amin <- amins[[index]]
  for (bike in amin$bikes) {
    amin$bike.checks <- amin$bike.checks + 1
    new.bike.str <- regex(x=amin$ope, pat="old", repl='bike')
    eval(parse(text=new.bike.str))
    
    if (div.3) {
      new.bike <- floor(new.bike / 3)
    }
    is.new.bike.day <- (new.bike %% amin$div_by) == 0
    frikkie <- ifelse(is.new.bike.day, amin$if_true, amin$if_false) + 1
    
    
    
    amins[[frikkie]]$bikes <- c(amins[[frikkie]]$bikes, new.bike)
  
  }
  amin$bikes <- c()
  amin -> amins[[index]]
  
  amins
}


process_bikes <- function(bikes, amins, index, div.3 = TRUE, ...) {
  get_flat_bikes <-function(bike, index) {
    (bike$current_owner == index) && !bike$is_looped
  }
  flat.bikes <- sapply(X=bikes, FUN=get_flat_bikes, index=index)
  amin <- amins[[index]]
  
  for (bike.i in which(flat.bikes)) {
    bike <- bikes[[bike.i]]
    bike$bike.check <- bike$bike.check + 1
    bike.value <- bike$value
    
    new.bike.str <- regex(x=amin$ope, pat="old", repl='bike.value')
    eval(parse(text=new.bike.str))
    
    if (div.3) {
      new.bike <- floor(new.bike / 3)
    }
    is.new.bike.day <- (new.bike %% amin$div_by) == 0
    new.owner <- ifelse(is.new.bike.day, amin$if_true, amin$if_false) + 1
    
    
    bike$value <- new.bike
    
    if (bike$owner == new.owner) {
      bike$is_looped <- TRUE
    }
    bike$owner <- new.owner
    bike -> bikes[[bike.i]]
  }
  
  bikes
}





round_one <- function(amins, ...) {
  for (round in 1:20) {
    for (amin in 1:length(amins)) {
      amins <- process_amin(amins, amin, TRUE)
    }
  }
  
  check.count <- sapply(X = amins, FUN = "[[", "bike.checks")
  most.checks <- sort(check.count, decreasing = TRUE)[1:2]
  message(most.checks[1] * most.checks[2])
}


round_two <- function(amins, ...) {
  bikes <- get_bikes(amins)
  bikes.dt <- rbindlist(bikes)
  amin.count <- 0
  while (TRUE) {
    for (amin.i in 1:length(amins)) {
      bikes <- process_bikes(bikes = bikes, amins = amins, index = amin.i, div.3 = FALSE)
    }
    flat.bikes <- sapply(X=bikes, FUN="[[", "is_looped")
    message(sum(flat.bikes))
    if (all(flat.bikes)) {
      break
    }
    bikes
  }
  
  bikes
}

amins <- get_amins(lines=data.in)
# round_one(amins)
round_two(amins)

