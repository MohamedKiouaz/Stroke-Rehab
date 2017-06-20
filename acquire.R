# This file contain the functions that create the data from a file
# the data must be in a list of 4
#### x y z : a matrix of numeric
#### time : vector
#### filename : string
#### exercice : type of exercice


AcquireDataFromXlsx = function(f, exercice = NA) {
  library(xlsx)
  
  data_ = read.xlsx(f, 1, header = FALSE)
  colnames(data_) = c("x", "y", "z")
  data_ = list(data_, 1:nrow(data_), f, exercice)
  names(data_) = c("raw", "time", "filename", "exercice")
  print(paste("Data acquired from", f))
  data_
}
