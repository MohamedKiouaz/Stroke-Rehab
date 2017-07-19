# This file contain the functions that create the data from a file
# the data must be in a list of 4
#### x y z : a matrix of numeric
#### time : vector
#### filename : string
#### exercice : type of exercice

Acquire = function(f, exercice = NA) {
	if (substr(f, nchar(f) - 3, nchar(f)) == ".csv") {
		data_ = AcquireDataFromCsv(f, exercice)
	}
	else {
		if (substr(f, nchar(f) - 4, nchar(f)) == ".xlsx") {
			data_ = AcquireDataFromXlsx(f, exercice)
		}
		else {
			print(paste("Failed to load ", f))
			return(NA)
		}
	}
	
	return(data_)
}

AcquireDataFromXlsx = function(f, exercice = NA) {
	library(xlsx)
	
	data_ = read.xlsx(f, 1, header = FALSE)
	colnames(data_) = c("x", "y", "z")
	data_ = list(data_, 1:nrow(data_), f, exercice)
	names(data_) = c("raw", "time", "filename", "exercice")
	print(paste("Data acquired from", f))
	data_
}


AcquireDataFromCsv = function(f, exercice = NA) {
	data_ = read.csv(f, header = FALSE)
	colnames(data_) = c("time", "x", "y", "z")
	data_ = list(data_[2:4], data_[[1]], f, exercice)
	names(data_) = c("raw", "time", "filename", "exercice")
	print(paste("Data acquired from", f))
	data_
}
