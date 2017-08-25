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


informations = data.frame(
	folder = DATA_FOLDER,
	filename = c(
		"CM_ex1.csv",
		"GS_ex1.csv",
		"UU_ex1.csv",
		"ZZ_ex1.csv",
		"MD_ex1.csv",
		"CM_ex2.csv",
		"GS_ex2.csv",
		"UU_ex2.csv",
		"ZZ_ex2.csv",
		"MD_ex2.csv",
		"CM_ex3.csv",
		"GS_ex3.csv",
		"UU_ex3.csv",
		"ZZ_ex3.csv",
		"MD_ex3.csv",
		"CM_ex4.csv",
		"GS_ex4.csv",
		"UU_ex4.csv",
		"ZZ_ex4.csv",
		"MD_ex4.csv",
		"CM_ex5.csv",
		"GS_ex5.csv",
		"UU_ex5.csv",
		"ZZ_ex5.csv",
		"MD_ex5.csv"
	),
	extype = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
	reps = c(35),
	stringsAsFactors = FALSE
)
