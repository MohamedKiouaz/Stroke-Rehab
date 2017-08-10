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
