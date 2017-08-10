# Average filter range
AVERAGING_FILTER_STEPS = 40

# Files to analyze

DATA_FOLDER = ("data")

informations = data.frame(
	folder = DATA_FOLDER,
	filename = c(
		"ex1_40reps.csv",
		"ex1_52reps.csv",
		"ex1_bis.csv",
		"ex1_bis_bis.csv",
		"ex1_bis_bis_bis.csv",
		"ex1_bis_bis_bis_bis.csv",
		"ex3.csv",
		"ex5.csv"
	),
	extype = c(1, 1, 1, 1, 1, 1, 3, 5),
	reps = c(40, 56, 1, 1, 1, 1, 3, 5),
	stringsAsFactors = FALSE
)
