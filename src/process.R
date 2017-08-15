ProcessData = function(data_) {
	library(pracma)
	
	cat("Processing from", data_$filename, "\n")
	
	start_time = Sys.time()
	
	A = apply(data_$raw, 1, function(X)
		norm(as.matrix(X)))
	data_$raw = cbind(data_$raw, A)
	colnames(data_$raw) = c("x", "y", "z", "norm")
	
	avg_filter = as.numeric(matrix(1 / AVERAGING_FILTER_STEPS, 1, AVERAGING_FILTER_STEPS))
	
	data_$avg = apply(data_$raw[1:3], 2, function(X)
		filter(X, avg_filter, method = "convolution", circular = TRUE))
	data_$avg = apply(data_$avg, 2, detrend)
	A = apply(data_$avg, 1, function(X)
		norm(as.matrix(X)))
	data_$avg = cbind(data_$avg, A)
	colnames(data_$avg) = c("x", "y", "z", "norm")
	
	data_ = Movement(data_)
	
	data_$count = CountMovement(data_)
	
	data_$period = median(tail(data_$null, data_$count) - head(data_$null, data_$count))
	
	source(file = "src/score.R")
	data_$score = Score(data_)
	
	source(file = "src/score_dtw.R")
	data_$similarity_dtw = DTW_Similarity(data_, "norm")
	data_$score_dtw = median(data_$similarity_dtw)
	
	source(file = "src/score_xcorr.R")
	data_$similarity_xcorr = XCORR_Similarity(data_, "norm")
	data_$score_xcorr = median(data_$similarity_xcorr)
	
	cat(Sys.time() - start_time, "sec to process", data_$filename, "\n")
	
	PlotData(data_)
	
	data_
}

distance = function(t, a_t) {
	a_t = as.matrix(a_t)
	
	library(pracma)
	
	v_t = a_t * 0
	p_t = v_t
	
	v_t[, 1] = cumtrapz(t, a_t[, 1])
	v_t[, 2] = cumtrapz(t, a_t[, 2])
	v_t[, 3] = cumtrapz(t, a_t[, 3])
	
	p_t[, 1] = cumtrapz(t, v_t[, 1])
	p_t[, 2] = cumtrapz(t, v_t[, 2])
	p_t[, 3] = cumtrapz(t, v_t[, 3])
	
	norm(p_t[nrow(p_t), 1:3] - p_t[1, 1:3], t = "F")
}

Movement = function(data_) {
	data_$top_threshold = quantile(data_$avg[, "norm"], .55)
	data_$bot_threshold = quantile(data_$avg[, "norm"], .2)
	
	data_$null = data_$avg[, "norm"] > data_$top_threshold
	data_$null = which(data_$null %in% TRUE)
	
	i = length(data_$null)
	while (i > 1) {
		if (abs(data_$time[data_$null[i - 1]] - data_$time[data_$null[i]]) < .2)
			data_$null = data_$null[-i]
		i = i - 1
	}
	
	i = 1
	while (i < length(data_$null) - 1) {
		if (min(data_$avg[data_$null[i]:data_$null[i + 1], "norm"]) > data_$bot_threshold)
			data_$null = data_$null[-i - 1]
		else
			i = i + 1
	}
	
	data_
}

CountMovement = function(data_) {
	length(data_$null) - 1
}

GetAvgPeriod = function(data_, i, col) {
	data_$avg[data_$null[i]:(data_$null[i + 1] - 1), col]
}

GetRawPeriod = function(data_, i, col) {
	data_$raw[data_$null[i]:(data_$null[i + 1] - 1), col]
}

GetTimePeriod = function(data_, i) {
	data_$time[data_$null[i]:(data_$null[i + 1] - 1)]
}



