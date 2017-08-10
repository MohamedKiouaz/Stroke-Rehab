DTW_Similarity = function(data_, col) {
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
