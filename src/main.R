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
} else {
	print("Data already loaded", quote = FALSE)
}

start_time = Sys.time()

dd = apply(d, 2, ProcessData)

print(Sys.time() - start_time)

sapply(dd, informations)

sapply(dd, PlotData)

summary(dd)
summary(dd[[1]])
