# rm(list = ls())

##### Variables #####

n = 25
filenames = c("1.xlsx", "2.xlsx", "3.xlsx", "4.xlsx", "5.xlsx")
NullVectorNormThreshold = .15

##### Sources #####

source(file="acquire.R")
source(file="process.R")
source(file="render.R")

##### Code #####

# filenames = filenames[1:2]

if(! exists("d")) {
  d = sapply(filenames, AcquireDataFromXlsx)
}

d[, 1]$exercice = 1
d[, 2]$exercice = 2
d[, 3]$exercice = 3
d[, 4]$exercice = 4
d[, 5]$exercice = 5

dd = apply(d, 2, ProcessData)

#sapply(dd, PlotData)
sapply(dd, NicePlotData)

c = sapply(dd, CountMovement)

