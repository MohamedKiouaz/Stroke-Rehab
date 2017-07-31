# rm(list = ls())

##### Sources #####

library(compiler)
enableJIT(3)

source(file = "src/variable.R")
source(file = "src/acquire.R")
source(file = "src/process.R")
source(file = "src/render.R")

print("Source files included", quote = FALSE)

##### Code #####

if (!exists("d")) {
	d = sapply(paste(data_folder, "/", filenames, sep = ""), Acquire)
	
	for (i in 1:length(filenames))
		d[, i]$exercice = extype[i]
	
	print("Data loaded", quote = FALSE)
} else
	print("Data already loaded", quote = FALSE)

start_time = Sys.time()

d[, 1]$time = 1:length(d[, 1]$time) / 25
d[, 2]$time = 1:length(d[, 2]$time) / 25
d[, 3]$time = 1:length(d[, 3]$time) / 25
d[, 4]$time = 1:length(d[, 4]$time) / 25
d[, 5]$time = 1:length(d[, 5]$time) / 25

dd = apply(d, 2, ProcessData)

print(Sys.time() - start_time)

sapply(dd, informations)

# sapply(dd, PlotData)

print(summary(dd))

print(summary(dd[[1]]))
