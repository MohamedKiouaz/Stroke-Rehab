rm(list = ls())

##### Sources #####

source(file = "variable.R")
source(file = "acquire.R")
source(file = "process.R")
source(file = "render.R")

print("Source files included")

##### Code #####

if(!exists("d")) {
  d = sapply(filenames, Acquire)
  for(i in 1:length(filenames))
    d[, i]$exercice = extype[i]
}

dd = apply(d, 2, ProcessData)

sapply(dd, informations)

sapply(dd, PlotData)
