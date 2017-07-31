PlotData = function(data_) {
	outputfile = paste("render/",
										 data_$exercice,
										 "_",
										 sample(0:10000, 1),
										 ".pdf",
										 sep = "")
	
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
			paste("Norm of averaged and detrended Signal", n)
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
				paste("Averaged and detrended Signal", n),
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
	
	image.plot(1 - data_$similarity, col = colorRampPalette(c("white", "red"))(25))
	
	if(exists("similarity_x", where = data_))
		image.plot(1 - data_$similarity_x, col = colorRampPalette(c("white", "red"))(25))
	if(exists("similarity_y", where = data_))
		image.plot(1 - data_$similarity_y, col = colorRampPalette(c("white", "red"))(25))
	if(exists("similarity_z", where = data_))
		image.plot(1 - data_$similarity_z, col = colorRampPalette(c("white", "red"))(25))
	
	dev.off()
	
	print(paste("Render in", outputfile), quote = FALSE)
}

informations = function(data_) {
	print(
		paste(
			"Exercice",
			data_$exercice,
			"Score",
			data_$score,
			"Number",
			length(data_$null),
			"Period =",
			data_$period
		),
		quote = FALSE
	)
}

NicePlotData = function(data_) {
	library(ggplot2)
	
	e = data.frame(data_$raw[, 1], data_$avg[, 1], data_$time)
	names(e) = c("raw", "avg", "time")
	
	p = ggplot(e, aes(time)) + geom_line(aes(y = raw), colour = "grey") + geom_line(aes(y = avg), colour = "blue") + coord_cartesian(ylim = c(-1.5, 1.5))
	print(p)
}
