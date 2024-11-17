################################################################################
################ Lab 4: Tree-based methods #####################################
################ and unsupervised learning ####################################
################################################################################

# Part 1 ------------------------------------------------------------------
animal = read.csv("C:/Users/mehri/Downloads/animal_shelter.csv")
attach(animal)


# Q1 - Classification Tree ------------------------------------------------
### No Coding Required

# Q2 - Growing a Tree -----------------------------------------------------
if (!require(rpart)) {
  install.packages("rpart")
  library(rpart)
}

if (!require(rpart.plot)) {
  install.packages("rpart.plot")
  library(rpart.plot)
}

# Growing 3 trees
tree_cp_0.5 <- rpart(outcome ~ age + number_animals_in_shelter + animal_type + is_sterilized + sex, data = animal, cp = 0.5)
tree_cp_0.05 <- rpart(outcome ~ age + number_animals_in_shelter + animal_type + is_sterilized + sex, data = animal, cp = 0.05)
tree_cp_0.0005 <- rpart(outcome ~ age + number_animals_in_shelter + animal_type + is_sterilized + sex, data = animal, cp = 0.0005)

# Plot trees
par(mfrow = c(1, 3))

# Plot trees
par(mfrow = c(1, 3))

# Use prp from rpart.plot with adjusted parameters
prp(tree_cp_0.5, main = "Tree (cp = 0.5)", extra = 1, cex = 0.08, srt = 45, branch.lty = 3)
prp(tree_cp_0.05, main = "Tree (cp = 0.05)", extra = 1, cex = 0.08, srt = 45, branch.lty = 3)
prp(tree_cp_0.0005, main = "Tree (cp = 0.0005)", extra = 1, cex = 0.08, srt = 45, branch.lty = 3)

# Fit the tree with different cp values
cp_values <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0)
errors <- numeric(length(cp_values))

for (i in seq_along(cp_values)) {
  tree <- rpart(outcome ~ age + number_animals_in_shelter + animal_type + is_sterilized + sex, 
                data = animal, 
                control = rpart.control(cp = cp_values[i]))
  
  # Cross-validation to estimate out-of-sample error
  cv_results <- train(outcome ~ age + number_animals_in_shelter + animal_type + is_sterilized + sex, 
                      data = animal, 
                      method = "rpart", 
                      trControl = trainControl(method = "cv"))
  errors[i] <- 1 - cv_results$results$Accuracy  # Relative x-error
}

# Plot results
plot(cp_values, errors, type = "b", pch = 19, col = "blue", main = "Out-of-Sample Error vs. cp", 
     xlab = "cp", ylab = "Relative X-Error")


# Find the optimal cp value
optimal_cp <- cp_values[which.min(errors)]

cat("Optimal cp value:", optimal_cp, "\n")


optimal_cp <- 0.001

tree <- rpart(outcome ~ age + number_animals_in_shelter + animal_type + is_sterilized + sex, 
              data = animal, 
              control = rpart.control(cp = optimal_cp))

# Plot the tree
prp(tree, main = "Optimal Tree with Highest Predictive Performance", extra = 1, cex = 0.8)


# Q3 - Growing a Random Forest --------------------------------------------

install.packages("gbm")

library(randomForest)
library(ggplot2)
library(ggplot2)
require(methods)
library(gbm)


myforest = randomForest(age~number_animals_in_shelter+animal_type+is_sterilized+sex, ntree = 1000, data=animal, importance=TRUE, na.action=na.omit, do.trace=100)

myforest

varImpPlot(myforest)

# Define the animal types
animal_types <- c("large_dog", "long_hair_cat", "short_hair_cat", "small_dog")

# Create dummy variables for each animal type
for (type in animal_types) {
  animal[paste0("animal_type_", type, "_dummy")] <- as.numeric(animal$animal_type == type)
}

# Q4 - Boosting --------------------------------------------------------------

# Convert outcome to numeric (0 or 1)
animal <- as.numeric(animal$outcome == "Adopted")

# Convert factors
animal$animal_type <- as.factor(animal$animal_type)
animal$sex <- as.factor(animal$sex)
animal$is_sterilized <- as.factor(animal$is_sterilized)

# Fit boosted tree
boost_tree <- gbm(outcome ~ age + number_animals_in_shelter + animal_type + is_sterilized + sex,
                  data = animal,
                  distribution = "bernoulli",
                  n.trees = 1000,
                  interaction.depth = 3)

# Predictions
predictions <- predict(boost_tree, newdata = animal, n.trees = 1000, type = "response")

# Convert probabilities to binary outcomes (0 or 1)
adopted <- ifelse(predictions > 0.5, 1, 0)

# Calculate classification error
classification_error <- sum(adopted != animal$outcome) / length(adopted)
print(classification_error)





# Part 2 ------------------------------------------------------------------

#detach(animal)
diabetes <- read.csv("C:/Users/valhk/Documents/MMA/MGSC - R/diabetes.csv")
attach(diabetes) 


#install.packages("GGally")
library(ggplot2)
library(GGally)
#install.packages("ggfortify")
library(ggfortify)

# Subset the diabetes dataset to include only columns 1 through 8
diabetes_vars <- diabetes[, c(1:8)]

# Perform Principal Component Analysis (PCA) on the selected variables
pca <- prcomp(diabetes_vars, scale = TRUE)

# Display the results of the PCA
pca

# Plot an autoplot of the PCA with loadings
autoplot(pca, data = diabetes_vars, loadings = TRUE, loadings.label = TRUE)

# Plot an autoplot of the PCA with loadings, colored by outcome
autoplot(pca, data = diabetes_vars, loadings = TRUE, col = ifelse(diabetes$outcome == 1, "red", "green"), loadings.label = TRUE)

# Calculate the proportion of variance explained (PVE) for each principal component
pve <- (pca$sdev^2) / sum(pca$sdev^2)

# Set up a 1x2 layout for side-by-side plots
par(mfrow = c(1, 2))

# Plot the PVE
plot(pve, ylim = c(0, 1))

# Plot the cumulative PVE
plot(cumsum(pve), ylim = c(0, 1))

# Fit a logistic regression model using BMI, blood pressure, and age as predictors
logit <- glm(outcome ~ BMI + blood_pressure + age, data = diabetes, family = 'binomial')

# Display the summary of the logistic regression model
summary(logit)

# Predict the probabilities using the logistic regression model
predicted_probabilities <- predict(logit, type = "response")

# Classify predictions based on a probability threshold of 0.5
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Load the caret package for performance evaluation
library(caret)

# Create a confusion matrix to evaluate the classification performance
confusionMatrix(as.factor(predicted_classes), as.factor(diabetes$outcome))

# Display a table of predicted vs actual outcomes
table(Predicted = predicted_classes, Actual = diabetes$outcome)
