XCORR_Similarity = function(data_, col) {
	s = matrix(0, data_$count, data_$count)
	
	for (i in 1:data_$count)
		for (j in 1:i) {
			a = ccf(GetPeriod(data_, i, col), GetPeriod(data_, j, col), plot = FALSE)
			s[i, j] = max(a$acf)
			s[j, i] = max(a$acf)
		}
	
	s
}
