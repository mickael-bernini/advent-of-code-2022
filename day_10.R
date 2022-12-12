setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 10

data.fn <- get_file_name(day, is.test)
data.in <- fread(data.fn, header = FALSE, col.names = c("task", "moves"), fill=TRUE)

data.in[,cycles := ifelse(task == 'noop', 1, 2)]
data.in[, step := cumsum(cycles)]

all.cycles <- data.table(step = 0:240)

data.all <- merge(all.cycles, data.in, by="step", all = TRUE)
data.all[is.na(moves), moves := 0]
data.all[,value_added := (step==0) + moves]
data.all[,at_end_of_step := cumsum(value_added)]

res <- data.all[step %in% (c(20,60,100,140,180,220) - 1), (step + 1) * at_end_of_step]
message(sum(res))

data.all[,col := (step - 1) %% 40]
data.all[,line := (step - col - 1) / 40]
data.all[,at_start_of_step := c(0, head(at_end_of_step, -1))]

print_pos <- function(col, at_start_of_step) {
  amin <- ifelse(abs(col - at_start_of_step) <= 1, "#", ".")
  message(paste(amin, collapse=''))
}
data.all[,print_pos(col, at_start_of_step),by=line]
