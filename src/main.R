rm(list = ls())

##### Sources #####

source(file = "src/variable.R")
source(file = "src/acquire.R")
source(file = "src/process.R")
source(file = "src/render.R")

print("Source files included")

##### Code #####

if (!exists("d")) {
	d = sapply(paste(data_folder, "/", filenames, sep = ""), Acquire)
	for (i in 1:length(filenames))
		d[, i]$exercice = extype[i]
}

dd = apply(d, 2, ProcessData)

sapply(dd, informations)

sapply(dd, PlotData)
