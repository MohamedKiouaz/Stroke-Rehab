rm(list = ls())

##### Variables #####

n = 25
workingdirectory = "E:/Google Drive/2016 - 2017/Stage 2A/Processing/R"
filenames = c("1.xlsx", "2.xlsx", "3.xlsx", "4.xlsx", "5.xlsx")

#####################

library(xlsx)

setwd(workingdirectory)

for(f in filenames[1]) {
  data_ = read.xlsx(f, 1, header = FALSE)
  colnames(data_) = c("x", "y", "z")
  data_["time"] = 1:nrow(data_)
  
  d = data.matrix(data_)
  
  average_filter = function(X) {
    avg_filter = matrix(0, 1, nrow(d))
    avg_filter[1, 1:n] = 1/n
    return(convolve(X, avg_filter))
  }
  
  d_avg = apply(d, 2, average_filter)
  
  d_fit = apply(d_avg, 2, function(X) { return(lm(X ~ time, data = data_)) })
  
  #pdf(file = paste("output_", f, ".pdf", sep = ""), 15, 10)
  par(mfrow = c(3, 1))
  for(i in c("x", "y", "z")) {
    plot(d_avg[, i], type="l", lwd = 2, col = "blue", axes = FALSE, ann = FALSE, ylim = c(-1.5, 1.5))
    axis(1, las = 1)
    axis(2, las = 1)
    box()
    title(xlab = "Time", ylab = "Signal", main = paste("Axis", i))
    abline(d_fit[i], col = "green")
    points(d[, i], type = "p", lwd = 1, col = "red")
    grid(NA, 6, lwd = 2, lty = "solid")
    legend("topright", c("Raw Signal", paste("Averaged Signal", n)), col = c("red","blue"), pch = c(1, NA), lty = c(0, 1), lwd = 2);
  }
  #dev.off() 
  print(f)
}

