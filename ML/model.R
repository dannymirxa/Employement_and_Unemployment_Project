install.packages("randomForest")
library(randomForest)
data <- read.csv("d:\\Users\\ys-lenovo\\Desktop\\clean-data.csv", header = T)
data
train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data), replace = FALSE)
train_data <- data[train_indices, ]  
test_data <- data[-train_indices, ]  
train_data$new <- as.factor(train_data$new)
test_data$new <- as.factor(test_data$new)
model <- randomForest(new ~ ., data = train_data, ntree = 100, mtry = 2, method = "classification")
predictions <- predict(model, newdata = test_data, type = "class")

library(ggplot2)
test_data$Index <- seq_len(nrow(test_data))
plot_data <- rbind(
  data.frame(Index = test_data$Index, Value = test_data$new, Type = "Actual"),
  data.frame(Index = test_data$Index, Value = predictions, Type = "Predicted")
)

ggplot(plot_data, aes(x = Index, y = Value, color = Type, group = Type)) +
  geom_line() +
  geom_point() +
  labs(x = "Index", y = "Value", color = "Type") +
  theme_minimal()
accuracy <- sum(predictions == test_data$new) / length(predictions)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))