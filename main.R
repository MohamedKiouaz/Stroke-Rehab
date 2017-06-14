rm(list = ls())

##### Variables #####

n = 10
filenames = c("1.xlsx", "2.xlsx", "3.xlsx", "4.xlsx", "5.xlsx")

#####################

library(xlsx)
library(peaks)
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
  
  pdf(file = paste("output_", f, ".pdf", sep = ""), 15, 10)
  par(mfrow = c(3, 1))
  for(i in c("x", "y", "z")) {
    plot(data_$time, data_avg[, i], type="l", lwd = 2, col = "blue", axes = FALSE, ann = FALSE, ylim = c(-1.5, 1.5))
    grid(NA, 6, lwd = 2, lty = "solid")
    axis(1, las = 1)
    axis(2, las = 1)
    box()
    title(xlab = "Time", ylab = "Signal", main = paste("Axis", i))
    points(data_[i], type = "p", lwd = 1, col = "red")
    legend("topright", c("Raw Signal", paste("Averaged and detrended Signal", n)), col = c("red","blue"), pch = c(1, NA), lty = c(0, 1), lwd = 2);
  }
  dev.off() 
  print(f)
}

