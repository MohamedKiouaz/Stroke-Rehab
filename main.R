rm(list = ls())

##### Variables #####

n = 25
filenames = c("1.xlsx", "2.xlsx", "3.xlsx", "4.xlsx", "5.xlsx")
NullVectorNormThreshold = .15

#####################

library(xlsx)
library(pracma)

for(f in filenames) {
  data_ = read.xlsx(f, 1, header = FALSE)
  colnames(data_) = c("x", "y", "z")
  data_$time = 1:nrow(data_)
  
  average_filter = function(X) {
    avg_filter = matrix(0, 1, nrow(data_))
    avg_filter[1, 1:n] = 1/n
    return(convolve(X, avg_filter))
  }
  
  data_avg = apply(data_, 2, average_filter)
  
  data_avg = apply(data_avg, 2, function(X) detrend(X, 'linear'))
  
  data_null = apply(data_avg[, 1:3], 1, function(X) norm(as.matrix(X))<NullVectorNormThreshold)
  data_null = which(data_null %in% TRUE)
  
  for
  
  data_peaks = apply(data_avg, 2, function(X) findpeaks(X, minpeakdistance = 45))
  
  pdf(file = paste("output_", f, ".pdf", sep = ""), 15, 10)
  par(mfrow = c(3, 1), bg = "lightgray")
  for(i in c("x", "y", "z")) {
    plot(data_$time, data_avg[, i], type="l", lwd = 2, col = "blue", axes = FALSE, ann = FALSE, ylim = c(-1.5, 1.5), panel.first = grid(col = "white", lty = "solid"))
    axis(1, las = 1, at = seq(0, length(data_$time), by = 25))
    axis(2, las = 1)
    box()
    title(xlab = "Time", ylab = "Signal", main = paste("Axis", i))
    points(data_[i], type = "p", lwd = 1, col = "red")
    points(data_peaks[[i]][, 2], data_peaks[[i]][, 1], type = "p", lwd = 2, col = "black", pch = 0)
    points(data_null, integer(length(data_null)), type = "p", col = "green", pch = 5)
    legend("topright", c("Raw Signal", paste("Averaged and detrended Signal", n), "Peaks", "Null Vector"), col = c("red","blue", "black", "Green"), pch = c(1, NA, 0, 5), lty = c(0, 1, 0, 0), lwd = c(2, 1, 2, 1), bg = "white");
  }
  dev.off() 
  print(f)
}

