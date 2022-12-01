setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 01

data.fn <- get_file_name(day, is.test)

data.dt <- fread(data.fn, col.names = c("cal"))
data.dt[,elf := cumsum(is.na(cal))]
per.elf <- data.dt[,.(cal=sum(cal, na.rm=TRUE)), by=elf]

message(per.elf[,max(cal)])
setorder(per.elf, -cal)
message(per.elf[1:3, sum(cal)])
