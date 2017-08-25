#rm(list = ls())

##### Sources #####

library(compiler)
enableJIT(3)

source(file = "src/variable.R")
source(file = "src/acquire.R")
source(file = "src/process.R")
source(file = "src/render.R")

cat("Source files included\n")

##### Code #####

if (exists("d")) {
	cat("Data already loaded\n")
} else {
	d = LoadData(informations)
	cat("Data loaded\n")
}

dd = apply(d, 1, ProcessData)

print(str(dd[[1]]))
