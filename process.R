ProcessData = function(data_) {
  library(pracma)
  
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

CountMovement = function(data_) {
  length(data_$null) - 1
}