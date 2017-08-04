PlotData = function(data_) {
	outputfile = paste("render/", data_$exercice, "_", sample(0:10000, 1), ".pdf", sep = "")
	
	pdf(file = outputfile, 15, 10)
	
	par(mfrow = c(4, 1), bg = "lightgray")
	
	data_$time = data_$time - data_$time[1]
	
	Y_MAX = max(abs(data_$avg) * 1.5)
	
	plot(
		data_$time,
		data_$avg[, "norm"],
		type = "l",
		lwd = 2,
		col = "blue",
		ylim = c(0, Y_MAX),
		ann = FALSE,
		panel.first = grid(col = "white", lty = "solid")
	)
	
	abline(h = data_$top_threshold, col = "darkorchid4")
	
	abline(h = data_$bot_threshold, col = "darkorchid")
	
	#points(data_$time, data_$raw[, "norm"], type = "p", lwd = 1, col = "red")
	
	points(
		data_$time[data_$null],
		integer(length(data_$null)),
		type = "p",
		col = "green",
		pch = 5
	)
	
	title(xlab = "Time",
				ylab = "Norm of signal",
				main = "Norm")
	
	legend(
		"topright",
		c(
			"Norm of raw Signal",
			paste(
				"Norm of averaged and detrended Signal",
				AVERAGING_FILTER_STEPS
			)
		),
		col = c("red", "blue"),
		pch = c(1, NA, 5),
		lty = c(0, 1),
		lwd = c(2, 1),
		bg = "white"
	)
	
	legend(
		"topleft",
		legend = c(
			paste("File =", data_$filename),
			paste("Exercice =", data_$exercice),
			paste("Score =", data_$score),
			paste("Score2 =", data_$score2),
			paste("Score3 =", data_$score3),
			paste("Reps =", data_$count)
		),
		bty = "n"
	)
	
	text(
		data_$time[data_$null[-1]],
		0,
		labels = 1:data_$count,
		cex = 0.7,
		pos = 2
	)
	
	for (i in c("x", "y", "z")) {
		plot(
			data_$time,
			data_$avg[, i],
			type = "l",
			lwd = 2,
			col = "blue",
			ann = FALSE,
			ylim = c(-Y_MAX, Y_MAX),
			panel.first = grid(col = "white", lty = "solid")
		)
		
		title(xlab = "Time",
					ylab = "Signal",
					main = paste("Axis", i))
		
		#points(data_$time, data_$raw[, i], type = "p", lwd = 1, col = "red")
		
		points(
			data_$time[data_$null],
			integer(length(data_$null)),
			type = "p",
			col = "green",
			pch = 5
		)
		
		legend(
			"topright",
			c(
				"Raw Signal",
				paste("Averaged and detrended Signal", AVERAGING_FILTER_STEPS),
				"Null Vector"
			),
			col = c("red", "blue", "Green"),
			pch = c(1, NA, 5),
			lty = c(0, 1, 0),
			lwd = c(2, 1, 1),
			bg = "white"
		)
	}
	
	library(fields)
	
	par(mfrow = c(1, 1), bg = "lightgray")
	
	image.plot(
		1:data_$count,
		1:data_$count,
		data_$similarity2,
		legend.lab = "Similarity",
		xlab = "Movement",
		ylab = "Movement",
		main = "Similarity Matrix using Correlation"
	)
	
	image.plot(
		1:data_$count,
		1:data_$count,
		1 - data_$similarity,
		legend.lab = "Similarity",
		xlab = "Movement",
		ylab = "Movement",
		main = "Similarity Matrix using DTW"
	)
	
	if (exists("similarity_x", where = data_))
		image.plot(1 - data_$similarity_x, col = colorRampPalette(c("white", "red"))(25))
	if (exists("similarity_y", where = data_))
		image.plot(1 - data_$similarity_y, col = colorRampPalette(c("white", "red"))(25))
	if (exists("similarity_z", where = data_))
		image.plot(1 - data_$similarity_z, col = colorRampPalette(c("white", "red"))(25))
	
	hist(data_$similarity, breaks	= 25, freq = FALSE)
	
	hist(data_$similarity2, breaks	= 25, freq = TRUE)
	
	dev.off()
	
	cat("Plotted in", outputfile, "\n")
}

NicePlotData = function(data_) {
	library(ggplot2)
	library(gridExtra)
	
	data_render = data.frame(time = data_$time,
													 avg = data_$avg,
													 raw = as.matrix(data_$raw))
	
	# print(str(data_render))
	
	p0 = ggplot(data_render, aes(time))  + coord_cartesian()
	p0 = p0 + geom_line(aes(y = raw.norm, colour = "Raw"))
	p0 = p0 + labs(x = "Time", y = "Norm", title = "Norm")
	p0 = p0 + scale_color_discrete(name = NULL)
	
	p1 = ggplot(data_render, aes(time))  + coord_cartesian()
	p1 = p1 + geom_line(aes(y = avg.norm, colour = "Averaged"))
	p1 = p1 + geom_hline(aes(yintercept = y, colour = "Thresholds"), data = data.frame(y = c(data_$top_threshold, data_$bot_threshold)))
	p1 = p1 + labs(x = "Time", y = "Norm", title = "Norm")
	p1 = p1 + scale_color_discrete(name = NULL)
	
	p2 = ggplot(data_render, aes(time)) + coord_cartesian()
	p2 = p2 + geom_line(aes(y = avg.x, colour = "X"))
	p2 = p2 + geom_line(aes(y = avg.y, colour = "Y"))
	p2 = p2 + geom_line(aes(y = avg.z, colour = "Z"))
	p2 = p2 + labs(x = "Time", y = "Signal", title = "Averaged values")
	p2 = p2 + scale_color_discrete(name = NULL)
	
	outputfile = paste("render/", data_$exercice, "_", sample(0:10000, 1), ".pdf", sep = "")
	
	pdf(outputfile, 16, 9)
	
	P = grid.arrange(p0, p1, p2, ncol = 1)
	
	dev.off()
	
	cat("Plotted in", outputfile, "\n")
}
