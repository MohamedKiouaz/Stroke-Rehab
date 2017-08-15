library(caret)
library(randomForest)

feature_matrix = function(data_) {
	f_matrix = matrix(0, data_$count, 5)
	for (i in 1:data_$count) {
		if (mod(i, 4) != 0) {
			period = GetRawPeriod(data_, i, col = 1:4)
			time = GetTimePeriod(data_, i)
			max_vector = period[which.max(period[, 4]), 1:3]
			max_vector = as.matrix(unname(max_vector / sqrt(sum(max_vector ^ 2))))
			f_matrix[i, 1:3] = max_vector
			f_matrix[i, 4] = distance(time, period)
			f_matrix[i, 5] = data_$extype
		}
	}
	
	f_matrix[!(f_matrix[, 4] == 3), ]
}

test_matrix = function(data_) {
	f_matrix = matrix(0, data_$count, 4)
	for (i in 1:data_$count) {
		if (mod(i, 4) == 0) {
			period = GetRawPeriod(data_, i, col = 1:4)
			time = GetTimePeriod(data_, i)
			max_vector = period[which.max(period[, 4]), 1:3]
			max_vector = as.matrix(unname(max_vector / sqrt(sum(max_vector ^ 2))))
			f_matrix[i, 1:3] = max_vector
			f_matrix[i, 4] = distance(time, period)
			f_matrix[i, 5] = data_$extype
		}
	}
	
	f_matrix[!(f_matrix[, 4] == 3), ]
}

features = sapply(dd, feature_matrix)
test_matrix = sapply(dd, test_matrix)

features = do.call(rbind, features)
test_matrix = do.call(rbind, test_matrix)

training_data = data.frame(vec = features[, 1:4], exercice = factor(features[, 5]))
test_data = data.frame(vec = test_matrix[, 1:4], exercice = factor(test_matrix[, 5]))

model.d_lm = train(exercice ~ ., data = training_data, method = 'rf')

a = predict(model.d_lm$finalModel, newdata = test_data)
b = predict(model.d_lm$finalModel)

print(cbind(predicted = a, reference = test_data$exercice, is_right = a == test_data$exercice))

print(sum(a == test_data$exercice)/length(a))
