setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 04

data.fn <- get_file_name(day, is.test)
data.in <- fread(data.fn, header = FALSE, col.names = c("a", "b"), sep = ',')

pat.split <- '^([0-9]*)-([0-9]*)$'
data.in[,a.min := as.numeric(regex(a, pat.split, '\\1'))]
data.in[,a.max := as.numeric(regex(a, pat.split, '\\2'))]
data.in[,b.min := as.numeric(regex(b, pat.split, '\\1'))]
data.in[,b.max := as.numeric(regex(b, pat.split, '\\2'))]

data.in[, a.in.b := a.min >= b.min & a.max <= b.max]
data.in[, b.in.a := b.min >= a.min & b.max <= a.max]

data.in[,message(sum(a.in.b | b.in.a))]


data.in[, overlap :=
          ((a.min <= b.max) & (a.min >= b.min))
          | ((b.min <= a.max) & (b.min >= a.min))
          | ((a.max <= b.max) & (a.max >= b.min))
          | ((b.max <= a.max) & (b.max >= a.min))
        ]
data.in[,message(sum(overlap))]