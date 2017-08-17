library(caret)
library(randomForest)

feature_matrix = function(data_) {
	f_matrix = matrix(0, data_$count, 7)
	for (i in 1:data_$count) {
		if (mod(i, 4) != 0) {
			period = GetRawPeriod(data_, i, col = 1:4)
			time = GetTimePeriod(data_, i)
			max_vector = as.matrix(period[which.max(period[, 4]), 1:3])
			max_vector = max_vector / norm(max_vector, t = "F")
			f_matrix[i, 1:3] = max_vector
			f_matrix[i, 4] = distance(time, period) / 9.81
			f_matrix[i, 5] = data_$extype
			f_matrix[i, 6] = (max(period[, 2]) - min(period[, 2])) / (max(period[, 1]) - min(period[, 1]))
			f_matrix[i, 7] = (max(period[, 3]) - min(period[, 3])) / (max(period[, 1]) - min(period[, 1]))
		}
	}
	
	f_matrix[f_matrix[, 5] != 0,]
}

test_matrix = function(data_) {
	f_matrix = matrix(0, data_$count, 7)
	for (i in 1:data_$count) {
		if (mod(i, 4) == 0) {
			period = GetRawPeriod(data_, i, col = 1:4)
			time = GetTimePeriod(data_, i)
			max_vector = as.matrix(period[which.max(period[, 4]), 1:3])
			max_vector = max_vector / norm(max_vector, t = "F")
			f_matrix[i, 1:3] = max_vector
			f_matrix[i, 4] = distance(time, period) / 9.81
			f_matrix[i, 5] = data_$extype
			f_matrix[i, 6] = (max(period[, 2]) - min(period[, 2])) / (max(period[, 1]) - min(period[, 1]))
			f_matrix[i, 7] = (max(period[, 3]) - min(period[, 3])) / (max(period[, 1]) - min(period[, 1]))
		}
	}
	
	f_matrix[f_matrix[, 5] != 0,]
}

features = sapply(dd, feature_matrix)
test_matrix = sapply(dd, test_matrix)

features = do.call(rbind, features)
test_matrix = do.call(rbind, test_matrix)

training_data = data.frame(vec = features[, 1:4],
						   exercice = factor(features[, 5]),
						   vec2 = features[, 6:7])
test_data = data.frame(vec = test_matrix[, 1:4],
					   exercice = factor(test_matrix[, 5]),
					   vec2 = test_matrix[, 6:7])

model.d_lm = train(exercice ~ .,
				   data = training_data,
				   method = 'rf',
				   ntree = 2000)

a = predict(model.d_lm$finalModel, newdata = test_data)
b = predict(model.d_lm$finalModel)

print(head(
	cbind(
		predicted = a,
		reference = test_data$exercice,
		is_right = a == test_data$exercice
	)
))

print(model.d_lm$finalModel)
