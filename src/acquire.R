# This file contain the functions that create the data from a file
# the data must be in a list of 4
#### x y z : a matrix of numeric
#### time : vector
#### filename : string
#### exercice : type of exercice

Acquire = function(f) {
	if (substr(f, nchar(f) - 3, nchar(f)) == ".csv") {
		data_ = AcquireDataFromCsv(f)
	}
	else {
		if (substr(f, nchar(f) - 4, nchar(f)) == ".xlsx") {
			data_ = AcquireDataFromXlsx(f)
		}
		else {
			cat("Failed to load", f, "\n")
			return(NA)
		}
	}
	
	return(data_)
}

AcquireDataFromXlsx = function(f) {
	library(xlsx)
	
	data_ = read.xlsx(f, 1, header = FALSE)
	
	colnames(data_) = c("x", "y", "z")
	data_ = list(data_, 1:nrow(data_))
	names(data_) = c("raw", "time")
	
	cat("Data acquired from", f, "\n")
	
	data_
}


AcquireDataFromCsv = function(f) {
	data_ = read.csv(f, header = FALSE)
	
	colnames(data_) = c("time", "x", "y", "z")
	data_ = list(data_[2:4], data_[[1]])
	names(data_) = c("raw", "time")
	
	cat("Data acquired from", f, "\n")
	
	data_
}


LoadData = function(informations) {
	d = c()
	
	for (i in 1:nrow(informations)) {
		data_ = c(Acquire(
			paste(informations$folder[i], "/", informations$filename[i], sep = "")
		), informations[i, ])
		d = rbind(d, data_)
	}
	
	d
}

