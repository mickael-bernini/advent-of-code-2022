setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('global_functions.R')

is.test <- FALSE
day <- 02

data.fn <- get_file_name(day, is.test)

data.dt <- fread(data.fn, col.names = c("value_abc", "value_xyz"), header = FALSE)
data.dt[,amin := switch(value_abc, A="Rock", B="Paper", C="Scissor"), by=value_abc]
data.dt[,macfly := switch(value_xyz, X="Rock", Y="Paper", Z="Scissor"), by=value_xyz]


data.dt[,result := 0]

data.dt[amin == "Rock" & macfly == "Rock",result := 3]
data.dt[amin == "Rock" & macfly == "Paper",result := 6]
data.dt[amin == "Rock" & macfly == "Scissor",result := 0]

data.dt[amin == "Paper" & macfly == "Rock",result := 0]
data.dt[amin == "Paper" & macfly == "Paper",result := 3]
data.dt[amin == "Paper" & macfly == "Scissor",result := 6]

data.dt[amin == "Scissor" & macfly == "Rock",result := 6]
data.dt[amin == "Scissor" & macfly == "Paper",result := 0]
data.dt[amin == "Scissor" & macfly == "Scissor",result := 3]

data.dt[,myvalue := 0]
data.dt[,myvalue := switch(macfly, Rock=1, Paper=2, Scissor=3), by=macfly]

message(data.dt[,sum(result + myvalue)])

data.dt[,macfly := ""]
data.dt[,result := switch(value_xyz, X=0, Y=3, Z=6), by=value_xyz]

data.dt[amin == "Rock" & result == 0, macfly := "Scissor"]
data.dt[amin == "Rock" & result == 3, macfly := "Rock"]
data.dt[amin == "Rock" & result == 6, macfly := "Paper"]

data.dt[amin == "Paper" & result == 0, macfly := "Rock"]
data.dt[amin == "Paper" & result == 3, macfly := "Paper"]
data.dt[amin == "Paper" & result == 6, macfly := "Scissor"]

data.dt[amin == "Scissor" & result == 0, macfly := "Paper"]
data.dt[amin == "Scissor" & result == 3, macfly := "Scissor"]
data.dt[amin == "Scissor" & result == 6, macfly := "Rock"]

data.dt[,myvalue := 0]
data.dt[,myvalue := switch(macfly, Rock=1, Paper=2, Scissor=3), by=macfly]
data.dt[,result := switch(value_xyz, X=0, Y=3, Z=6), by=value_xyz]

message(data.dt[,sum(result + myvalue)])


