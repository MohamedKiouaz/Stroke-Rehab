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

informations = data.frame(
	folder = DATA_FOLDER,
	filename = c(
		"AO_ex1.csv",
		"AO_ex3.csv",
		"LC_ex1.csv",
		"LC_ex3.csv",
		"SG_ex1.csv",
		"SG_ex3.csv"
	),
	extype = c(1, 3, 1, 3, 1, 3),
	reps = c(35, 35, 35, 35, 35, 35),
	stringsAsFactors = FALSE
)
