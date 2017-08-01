# Average filter range
AVERAGING_FILTER_STEPS = 40

# Files to analyze

data_folder = ("data")

filenames = c(
	"1.csv",
	"2.csv",
	"3.csv",
	"4.csv",
	"5.csv",
	"ex1_bis.csv",
	"ex1_bis_bis.csv",
	"ex1_bis_bis_bis.csv",
	"ex1_bis_bis_bis_bis.csv",
	"ex3.csv",
	"ex5.csv"
)

# filenames = filenames[1:5]

extype = c(1, 2, 3, 4, 5, 1, 1, 1, 1, 3, 5)

reps = c(48, 45, 46, 57, 5, 1, 1, 1, 1, 3, 5)

data_frame = data.frame(folder = data_folder, filename = filenames,
												extype = extype,
												reps = reps, stringsAsFactors=FALSE)

print(data_frame)
