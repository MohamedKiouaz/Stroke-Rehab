PlotData = function(data_) {
	library(ggplot2)
	
	#print(str(data_))
	
	data_render = data.frame(time = data_$time,
													 avg = data_$avg,
													 raw = as.matrix(data_$raw))
	
	data_render2 = data.frame(time = data_$time[data_$null],
														y = data_$avg[data_$null, "norm"],
														name = as.character(0:(length(data_$null) - 1)))
	
	info = paste(
		data_$filename,
		", exercice ",
		data_$extype,
		", ",
		floor(data_$count/2),
		" reps, score = ",
		data_$score,
		", dtw score = ",
		data_$score_dtw,
		", xcorr score = ",
		data_$score_xcorr,
		sep = ""
	)
	
	p0 = ggplot()  + coord_cartesian()
	p0 = p0 + geom_line(aes(x = time, y = raw.norm, colour = "Raw"), data = data_render)
	p0 = p0 + geom_point(aes(x = time, y = 0, colour = "New half repetition"), data = data_render2)
	p0 = p0 + labs(x = "Time", y = "Norm", title = "Norm")
	p0 = p0 + scale_color_discrete(name = NULL)
	
	p1 = ggplot()  + coord_cartesian()
	p1 = p1 + geom_point(aes(x = time, y = y, colour = "New half repetition"), data = data_render2)
	p1 = p1 + geom_text(aes(x = time, y = 0, label = name), data = data_render2)
	p1 = p1 + geom_line(aes(x = time, y = avg.norm,	colour = paste("Averaged", AVERAGING_FILTER_STEPS)), data = data_render)
	p1 = p1 + geom_hline(aes(yintercept = y, colour = "Thresholds"), data = data.frame(y = c(
		data_$top_threshold, data_$bot_threshold
	)))
	#p1 = p1 + geom_vline(aes(xintercept = time, colour = "New reps"), data = data_render2)
	p1 = p1 + labs(x = "Time", y = "Norm", title = "Norm", subtitle = info)
	p1 = p1 + scale_color_discrete(name = NULL)
	
	p2 = ggplot(data_render, aes(time)) + coord_cartesian()
	p2 = p2 + geom_line(aes(y = avg.x, colour = "X"))
	p2 = p2 + geom_line(aes(y = avg.y, colour = "Y"))
	p2 = p2 + geom_line(aes(y = avg.z, colour = "Z"))
	p2 = p2 + geom_point(aes(x = time, y = 0, colour = "New half repetition"), data = data_render2)
	p2 = p2 + labs(x = "Time", y = "Signal", title = "Averaged values")
	p2 = p2 + scale_color_discrete(name = NULL)
	
	p3 = ggplot() + labs(x = "DTW Score", y = "Count", title = "DTW Score distribution")	
	p3 = p3 + geom_histogram(aes(y), bins = 50, data = data.frame(y = as.vector(data_$similarity_dtw)))
	
	p4 = ggplot() + labs(x = "XCORR Score", y = "Count", title = "Cross-correlation distribution")
	p4 = p4 + geom_histogram(aes(y), bins = 50, data = data.frame(y = as.vector(data_$similarity_xcorr)))
	
	outputfile = paste("render/",
										 data_$extype,
										 "_",
										 sample(0:10000, 1),
										 ".pdf",
										 sep = "")
	
	pdf(outputfile, 25, 9)
	
	library(gridExtra)
	P = grid.arrange(p0, p1, p2, ncol = 1)
	P = grid.arrange(p3, p4, ncol = 1)
	
	library(fields)
	
	par(mfrow = c(1, 2))
	
	image.plot(
		1:data_$count,
		1:data_$count,
		data_$similarity_xcorr,
		legend.lab = "",
		xlab = "",
		ylab = "",
		main = "Similarity Matrix using Correlation"
	)
	
	image.plot(
		1:data_$count,
		1:data_$count,
		1 - data_$similarity_dtw,
		legend.lab = "",
		xlab = "",
		ylab = "",
		main = "Similarity Matrix using DTW"
	)
	
	dev.off()
	
	cat("Plotted in", outputfile, "\n")
}
