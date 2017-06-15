rm(list = ls())

##### Variables #####

n = 25
filenames = c("1.xlsx", "2.xlsx", "3.xlsx", "4.xlsx", "5.xlsx")

#####################

library(xlsx)
library(pracma)

for(f in filenames) {
  data_ = read.xlsx(f, 1, header = FALSE)
  colnames(data_) = c("x", "y", "z")
  data_$time = 1:nrow(data_)
  
  #d = data.matrix(data_)
  
  average_filter = function(X) {
    avg_filter = matrix(0, 1, nrow(data_))
    avg_filter[1, 1:n] = 1/n
    return(convolve(X, avg_filter))
  }
  
  data_avg = apply(data_, 2, average_filter)
  
  data_avg = apply(data_avg, 2, function(X) { return(detrend(X, 'linear')) })
  
  data_peaks = apply(data_avg, 2, function(X) { return(findpeaks(X, minpeakdistance = 45)) })
  
  pdf(file = paste("output_", f, ".pdf", sep = ""), 15, 10)
  par(mfrow = c(3, 1))
  for(i in c("x", "y", "z")) {
    plot(data_$time, data_avg[, i], type="l", lwd = 2, col = "blue", axes = FALSE, ann = FALSE, ylim = c(-1.5, 1.5), panel.first = grid(NA, 6, lwd = 2, lty = "solid"))
    axis(1, las = 1)
    axis(2, las = 1)
    box()
    title(xlab = "Time", ylab = "Signal", main = paste("Axis", i))
    points(data_[i], type = "p", lwd = 1, col = "red")
    points(data_peaks[[i]][, 2], data_peaks[[i]][, 1], type = "p", lwd = 2, col = "black", pch = 0)
    legend("topright", c("Raw Signal", paste("Averaged and detrended Signal", n), "Peaks"), col = c("red","blue", "black"), pch = c(1, NA, 0), lty = c(0, 1, 0), lwd = c(2, 1, 2), bg = "white");
  }
  dev.off() 
  print(f)
}

