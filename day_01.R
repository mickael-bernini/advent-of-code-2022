setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- TRUE
day <- 20

data.in <- paste0('data/day_', day, ifelse(is.test,'_test',''), '.txt')
lines <- readLines(data.in)
pat <- '^([A-Z])([A-Z]) -> ([A-Z])$'

lines <- strsplit(lines, '')
