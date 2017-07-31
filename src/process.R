ProcessData = function(data_) {
	library(pracma)
	
	print(paste("Processing from", data_$filename), quote = FALSE)
	
	start_time = Sys.time()
	
	avg_filter = as.numeric(matrix(1 / n, 1, n))
	
	A = apply(data_$raw, 1, function(X)
		norm(as.matrix(X)))
	data_$raw = cbind(data_$raw, A)
	colnames(data_$raw) = c("x", "y", "z", "norm")
	
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
	
	data_$score = Score(data_)
	
	data_$similarity = DTWSimilarity(data_, "norm")
	#data_$similarity_x = DTWSimilarity(data_, "x")
	#data_$similarity_y = DTWSimilarity(data_, "y")
	#data_$similarity_z = DTWSimilarity(data_, "z")
	
	data_$score2 = sum(data_$similarity < .05) / data_$count ^ 2
	
	data_$similarity2 = CORSimilarity(data_, "norm")
	
	data_$score3 = sum(data_$similarity2 < .18) / data_$count ^ 2
	
	#data_$pos = apply(data_$pos[, 1:3], 2, function(X) acc2pos(data_$time, X))
	
	print(Sys.time() - start_time)
	
	PlotData(data_)
	
	data_
}

acc2pos = function(t, a_t) {
	library(caTools)
	
	v_t = a_t * 0
	p_t = v_t
	
	for (i in 2:length(t)) {
		v_t[i] = trapz(t[1:i], a_t[1:i])
	}
	for (i in 3:length(t)) {
		p_t[i] = trapz(t[1:i], v_t[1:i])
	}
	
	# par(mfrow = c(3, 1))
	# plot(t, a_t)
	# plot(t, v_t)
	# plot(t, p_t)
	
	tail(p_t) - head(p_t)
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
	count = length(data_$null) - 1
	
	count
}

Score = function(data_) {
	theta = function(a, b)
		acos(sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b))))
	
	resp = matrix(0, 1, length(data_$null))
	
	for (i in 1:data_$count)
	{
		for (j in data_$null[i]:data_$null[i + 1])
			if (theta(data_$avg[j, 1:3], data_$avg[j + 1, 1:3]) / 2 / pi * 360 > 7)
				resp[i] = resp[i] + 1
			resp[i] = resp[i] / (data_$null[i + 1] - data_$null[i])
	}
	
	median(1 - resp) ^ 5
}

GetPeriod = function(data_, i, col) {
	data_$raw[data_$null[i]:(data_$null[i + 1] - 1), col]
}

DTWSimilarity = function(data_, col) {
	library(dtw)
	
	similarity = matrix(0, data_$count, data_$count)
	
	for (i in 1:data_$count)
		for (j in 1:i) {
			alignment = dtw(GetPeriod(data_, i, col),
											GetPeriod(data_, j, col),
											keep = TRUE)
			
			
			similarity[i, j] = alignment$normalizedDistance
			
			if (i != j) {
				similarity[j, i] = alignment$normalizedDistance
				
				if (alignment$normalizedDistance < .02) {
					outputfile = paste(
						"render/dtw/",
						i,
						"_",
						j,
						"_",
						data_$exercice,
						"_",
						sample(0:10000, 1),
						".pdf",
						sep = ""
					)
					
					pdf(file = outputfile, 15, 10)
					
					plot(alignment,
							 type = "twoway",
							 main = paste("DTW", i, j, data_$filename))
					dev.off()
				}
			}
		}
	
	similarity
}

CORSimilarity = function(data_, col) {
	s = matrix(0, data_$count, data_$count)
	
	for (i in 1:data_$count)
		for (j in 1:i) {
			a = ccf(GetPeriod(data_, i, col), GetPeriod(data_, j, col), plot = FALSE)
			s[i, j] = max(a$acf)
			s[j, i] = max(a$acf)
		}
	
	s
}
