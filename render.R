PlotData = function(data_) {
  outputfile = paste(sample(0:1000, 1), "_", data_$filename, ".pdf", sep = "")

  pdf(file = outputfile, 15, 10)

  par(mfrow = c(4, 1), bg = "lightgray")


  data_$time = data_$time - data_$time[1]

  plot(data_$time, data_$avg[, "norm"], type = "l", lwd = 2, col = "blue", ylim = c(0, 3), ann = FALSE, panel.first = grid(col = "white", lty = "solid"))

  abline(h = data_$top_threshold, col = "darkorchid4")

  abline(h = data_$bot_threshold, col = "darkorchid")

  points(data_$time, data_$raw[, "norm"], type = "p", lwd = 1, col = "red")

  points(data_$time[data_$null], integer(length(data_$null)), type = "p", col = "green", pch = 5)

  title(xlab = "Time", ylab = "Norm of signal", main = paste("Norm of signal, exercice", data_$exercice))
  legend("topright", c("Norm of raw Signal", paste("Norm of averaged and detrended Signal", n)), col = c("red", "blue"), pch = c(1, NA, 5), lty = c(0, 1), lwd = c(2, 1), bg = "white")

  for(i in c("x", "y", "z")) {
    plot(data_$time, data_$avg[, i], type="l", lwd = 2, col = "blue", ann = FALSE, ylim = c(-1.5, 1.5), panel.first = grid(col = "white", lty = "solid"))
    title(xlab = "Time", ylab = "Signal", main = paste("Axis", i))

    points(data_$time, data_$raw[, i], type = "p", lwd = 1, col = "red")

    points(data_$time[data_$null], integer(length(data_$null)), type = "p", col = "green", pch = 5)

    legend("topright", c("Raw Signal", paste("Averaged and detrended Signal", n), "Null Vector"), col = c("red","blue", "Green"), pch = c(1, NA, 5), lty = c(0, 1, 0), lwd = c(2, 1, 1), bg = "white")
  }

  dev.off()

  print(paste("Render in", outputfile))
}

informations = function(data_) {
  print(paste("Exercice", data_$exercice, "Score", data_$score, "Number", length(data_$null), "Period =", data_$period))
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
