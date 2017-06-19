rm(list = ls())

##### Variables #####

n = 25
filenames = c("1.xlsx", "2.xlsx", "3.xlsx", "4.xlsx", "5.xlsx")
NullVectorNormThreshold = .15

##### Libraries ######

library(xlsx)
library(pracma)

##### Functions #####

AcquireData = function(f) {
  data_ = read.xlsx(f, 1, header = FALSE)
  colnames(data_) = c("x", "y", "z")
  data_ = list(data_, 1:nrow(data_), f)
  names(data_) = c("raw", "time", "filename")
  print(paste("Data acquired from", f))
  data_
}
  
ProcessData = function(data_) {
  print(paste("Processing data from", data_$filename))
  
  avg_filter = matrix(0, 1, length(data_$time))
  avg_filter[1, 1:n] = 1/n

  data_$avg = apply(data_$raw, 2, function(X) convolve(X, avg_filter))
  colnames(data_$avg) = c("x", "y", "z")
  data_$avg = apply(data_$avg, 2, function(X) detrend(X, 'linear'))
  
  data_$null = apply(data_$avg, 1, function(X) norm(as.matrix(X))<NullVectorNormThreshold)
  data_$null = which(data_$null %in% TRUE)
  i = length(data_$null)
  while(i > 1) {
    if(abs(data_$null[i - 1] - data_$null[i]) < 5) {
      data_$null = data_$null[-i]
      #print(c(data_$null[(i-5):i], i))
    }
    i = i - 1
  }
  
  #data_$peaks = apply(data_$avg, 2, function(X) findpeaks(X, minpeakdistance = 45))
  
  data_
}


PlotData = function(data_) {
  pdf(file = paste("output_", data_$filename, ".pdf", sep = ""), 15, 10)
  par(mfrow = c(3, 1), bg = "lightgray")
  print(data_$peaks)
  for(i in c("x", "y", "z")) {
    plot(data_$time, data_$avg[, i], type="l", lwd = 2, col = "blue", axes = FALSE, ann = FALSE, ylim = c(-1.5, 1.5), panel.first = grid(col = "white", lty = "solid"))
    axis(1, las = 1, at = seq(0, length(data_$time), by = 25))
    axis(2, las = 1)
    box()
    title(xlab = "Time", ylab = "Signal", main = paste("Axis", i))
    points(data_$raw[, i], type = "p", lwd = 1, col = "red")
    #points(data_$peaks[[i]][, 2], data_$peaks[[i]][, 1], type = "p", lwd = 2, col = "black", pch = 0)
    points(data_$null, integer(length(data_$null)), type = "p", col = "green", pch = 5)
    legend("topright", c("Raw Signal", paste("Averaged and detrended Signal", n), "Peaks", "Null Vector"), col = c("red","blue", "black", "Green"), pch = c(1, NA, 0, 5), lty = c(0, 1, 0, 0), lwd = c(2, 1, 2, 1), bg = "white");
  }
  dev.off()
  print(paste("Render in", data_$filename))
}

##### Code #####

# filenames = filenames[1:2]

if(! exists("d")) {
  d = sapply(filenames, AcquireData)
}

dd = apply(d, 2, ProcessData)

sapply(dd, PlotData)
