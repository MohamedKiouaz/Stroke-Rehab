# rm(list = ls())

##### Sources #####

library(compiler)
enableJIT(3)

source(file = "src/variable.R")
source(file = "src/acquire.R")
source(file = "src/process.R")
source(file = "src/render.R")

cat("Source files included\n")

##### Code #####

files = paste(paste(data_frame$folder, "/", sep = ""),
							data_frame$filename,
							sep = "")

if (!exists("d")) {
	d = sapply(files, Acquire)
	
	for (i in 1:length(filenames))
		d[, i]$exercice = extype[i]
	
	cat("Data loaded\n")
} else
	cat("Data already loaded\n")

start_time = Sys.time()

d[, 1]$time = 1:length(d[, 1]$time) / 25
d[, 2]$time = 1:length(d[, 2]$time) / 25
d[, 3]$time = 1:length(d[, 3]$time) / 25
d[, 4]$time = 1:length(d[, 4]$time) / 25
d[, 5]$time = 1:length(d[, 5]$time) / 25

dd = apply(d, 2, ProcessData)

print(Sys.time() - start_time)

sapply(dd, informations)

print(summary(dd))

print(summary(dd[[1]]))
