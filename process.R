ProcessData = function(data_) {
  library(pracma)
  
  print(paste("Processing data from", data_$filename))
  
  avg_filter = matrix(0, 1, length(data_$time))
  avg_filter[1, 1:n] = 1/n
  
  A = apply(data_$raw, 1, function(X) norm(as.matrix(X)))
  data_$raw = cbind(data_$raw, A)
  colnames(data_$raw) = c("x", "y", "z", "norm")
  
  data_$avg = apply(data_$raw[1:3], 2, function(X) convolve(X, avg_filter))
  data_$avg = apply(data_$avg, 2, function(X) detrend(X, 'linear'))
  A = apply(data_$avg, 1, function(X) norm(as.matrix(X)))
  data_$avg = cbind(data_$avg, A)
  colnames(data_$avg) = c("x", "y", "z", "norm")
  
  data_$null = data_$avg[, "norm"] < NullVectorNormThreshold[data_$exercice]
  data_$null = which(data_$null %in% TRUE)
  i = length(data_$null)
  while(i > 1) {
    if(abs(data_$null[i - 1] - data_$null[i]) < 15) {
      data_$null = data_$null[-i]
    }
    i = i - 1
  }

  data_$period = median(tail(data_$null, length(data_$null) - 1) - head(data_$null, length(data_$null) - 1))
  print(paste("Period =", data_$period))
  
  data_$pos = apply(data_$avg[, 1:3], 2, function(X) acc2pos(data_$time, X))
  
  #data_$peaks = apply(data_$avg, 2, function(X) findpeaks(X, minpeakdistance = 45))
  
  data_
}

acc2pos = function(t, a_t) {
  library(caTools)
  
  v_t = a_t * 0
  p_t = v_t
  
  for(i in 2:length(t)) {
    v_t[i] = trapz(t[1:i], a_t[1:i])
  }
  for(i in 3:length(t)) {
    p_t[i] = trapz(t[1:i], v_t[1:i])
  }
  p_t
  par(mfrow = c(3, 1))
  plot(t, a_t)
  plot(t, v_t)
  plot(t, p_t)
}

CountMovement = function(data_) {
  count = 0
  
  for(i in 1:(length(data_$null) - 1)) {
    if(max(data_$avg[data_$null[i]:data_$null[i + 1], "norm"]) >  1.5 * NullVectorNormThreshold[data_$exercice]) 
      count = count + 1
  }
  
  # if(data_$exercice == 1) {
  #   
  #   return((length(data_$null) - 1)/2)
  # }
  # 
  # if(data_$exercice == 2) {
  #   
  #   return((length(data_$null) - 1)/2)
  # }
  # 
  # if(data_$exercice == 3) {
  #   
  #   return((length(data_$null) - 1)/2)
  # }
  # 
  # if(data_$exercice == 4) {
  #   
  #   return((length(data_$null) - 1)/2)
  # }
  # 
  # if(data_$exercice == 5) {
  #   
  #   return((length(data_$null) - 1)/2)
  # }
  
  return(count)
}