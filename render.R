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

SimplePlotData = function(data_) {
  pdf(file = paste("simple_", data_$filename, ".pdf", sep = ""), 15, 10)
  par(mfrow = c(3, 1), bg = "lightgray")
  print(data_$peaks)
  for(i in c("x", "y", "z")) {
    plot(data_$time, data_$avg[, i], type="l", lwd = 2, col = "blue", axes = FALSE, ann = FALSE, ylim = c(-1.5, 1.5), panel.first = grid(col = "white", lty = "solid"))
    axis(1, las = 1, at = seq(0, length(data_$time), by = 25))
    axis(2, las = 1)
    box()
    title(xlab = "Time", ylab = "Signal", main = paste("Axis", i))
    points(data_$raw[, i], type = "p", lwd = 1, col = "red")
    legend("topright", c("Raw Signal", paste("Averaged and detrended Signal", n)), col = c("red","blue"), pch = c(1, NA), lty = c(0, 1), lwd = c(2, 1), bg = "white");
  }
  dev.off()
  print(paste("Render in", data_$filename))
}

NicePlotData = function(data_) {
  library(ggplot2)
  
  e = data.frame(data_$raw[, 1], data_$avg[, 1], data_$time)
  names(e) = c("raw", "avg", "time")
  
  p = ggplot(e, aes(time)) + geom_line(aes(y = raw), colour = "grey") + geom_line(aes(y = avg), colour = "blue") + coord_cartesian(ylim = c(-1.5, 1.5))
  print(p)
}
