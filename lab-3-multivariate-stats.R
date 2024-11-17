################################################################################
################ Lab 3: Classification Methods #################################
################################################################################


# Part 1 ------------------------------------------------------------------
elections = read.csv("C:/Users/mehri/Downloads/election_data.csv")
attach(elections)

# Importing libraries & packages
install.packages("stargazer")
library(stargazer)
library(ggplot2)
require(methods)

# QUESTION 1 --------------------------------------------------------------

# A - Linear Probability Model ------------------------------------------------

names(elections)
elections$winner = as.factor(elections$winner)

elections$winner = ifelse(winner=="Clinton", 1, 0)
attach(elections)
table(elections$winner)

lpm = lm(winner ~ pop2014 + under18 + density + black + hispanic, data = elections, na.action = na.exclude)
summary(lpm)

stargazer(lpm, type = "text")
stargazer(lpm, type = "html", out = "Probability_Model.html")


# B - Regression Coefficients ---------------------------------------------
# (Intercept)  1.692e-01  
# pop2014      1.734e-07  
# under18     -8.944e-01
# density      1.059e-04  
# black        8.973e-01  
# hispanic     5.892e-01


# C - Trump Prediction 1 --------------------------------------------------------
# Data frame with specific values for predictors (Prediction 1)
trump_1 <- data.frame(
  pop2014 = 250000,
  under18 = 0.10,
  density = 175,
  black = 0.45,
  hispanic = 0.10
)

# Use the predict function to obtain the predicted values
probTrump_1 = predict(lpm, newdata = trump_1, type = "response")

# Display the result
cat("Estimated probability that Trump will win is approximately:", probTrump_1 , "\n")

## Result: 0.6044077

# D - Trump Prediction 2 --------------------------------------------------
# Data frame with specific values for predictors (Prediction 2)
trump_2 <- data.frame(
  pop2014 = 1000000,
  under18 = 0.20,
  density = 1000,
  black = 0.85,
  hispanic = 0.05
)

# Use the predict function to obtain the predicted values
probTrump_2 = predict(lpm, newdata = trump_2, type = "response")

# Display the result
cat("Estimated probability that Trump will win is approximately:", probTrump_2 , "\n")

## Result: 1.061869

# E - Identifying problems of the 2 linear probability models -------------

# Problem 1: Invalid Probability Range
# Problem 2: Heteroskedasticity - variability of errors across different predictor levels

# [Optional]
residuals =  residuals(lpm)
# Residuals vs. Fitted values (predicted)
plot(fitted(lpm), residuals, ylab = "Residuals", xlab = "Fitted Values",
     main = "Residuals vs. Fitted Values")

# Residual vs. Predictor variable plot for each predictor
par(mfrow=c(2, 2))  
plot(elections$pop2014, residuals, ylab = "Residuals", xlab = "pop2014",
     main = "Residuals vs. pop2014")
plot(elections$under18, residuals, ylab = "Residuals", xlab = "under18",
     main = "Residuals vs. under18")
plot(elections$density, residuals, ylab = "Residuals", xlab = "density",
     main = "Residuals vs. density")
plot(elections$black, residuals, ylab = "Residuals", xlab = "black",
     main = "Residuals vs. black")
plot(elections$hispanic, residuals, ylab = "Residuals", xlab = "hispanic",
     main = "Residuals vs. hispanic")


# QUESTION 2 --------------------------------------------------------------
# A - Logistic Regression -------------------------------------------------

logit = glm(winner ~ pop2014 + under18 + density + black + hispanic, data = elections, family = "binomial")

summary(logit)

stargazer(logit, type = "text")
stargazer(logit, type = "html", out = "Logistic_Regression.html")


# B - Number of Fisher Iterations -----------------------------------------

## Number of iterations : 25


# C - Trump Prediction 3 (Logit) ------------------------------------------
# Data frame with specific values for predictors (Prediction 3)
trump_3 = data.frame(
  pop2014 = 1000000,
  under18 = 20,
  density = 1000,
  black = 85,
  hispanic = 5
)

# Use the predict function to obtain the predicted values
probTrump_3 = predict(logit, newdata = trump_3, type = "response")

# Display the result
cat("Estimated probability that Trump will win is approximately:", probTrump_3 , "\n")

## Result: 2.900701e-12


# D - R-Squared in Logistic Regression ------------------------------------

install.packages("rms")
require(rms)

logit2 =lrm(winner ~ pop2014 + under18 + density + black + hispanic)
logit2


# E - Prediction 4 --------------------------------------------------------

coef_black = coef(logit)[5]

# New data with specified predictor values
values1 = data.frame(
  pop2014 = 250000,
  under18 = 25,
  density = 100,
  black = seq(0, 100, by = 10),  # Varying levels of black population - intervals of 10%
  hispanic = 10
)

Prediction_4 = predict(logit, newdata = values1, type = "response")

# Threshold for the number of black people
threshold_black = which.max(Prediction_4 > 0.5)


minimum_nb_black = threshold_black * 10
predicted_winner_if_below_min = ifelse(coef_black > 0, "Trump", "Clinton")
predicted_winner_if_above_min = ifelse(coef_black > 0, "Clinton", "Trump")

# Display values
minimum_nb_black
predicted_winner_if_below_min
predicted_winner_if_above_min 


# Logistic Regression - Hispanic & Undergrad --------------------------

logit3 = lrm(winner ~ hispanic + undergrad)
logit3

# Extract coefficients from the logistic regression model
b0_logit = coef(logit3)[1] # intercept
b1_logit = coef(logit3)[2] # coefficient for hispanic predictor
b2_logit = coef(logit3)[3] # coefficient for undergrad predictor


# Case 1 ------------------------------------------------------------------
# Estimating the likelihood for b0 = -0.75, b1 = 0.03, b2 = 0.01 --------
## Recall: Clinton: 1; Trump: 0

b0_case1 = 0.75
b1_case1 = 0.03
b2_case1 = 0.01

# Define probability for Case 1:
#### Obs 1: winner=Trump, hispanic=0.15, undergrad=0.20
hispanic1_case1 = 0.15
undergrad1_case1 = 0.20
m_case1_obs1 = exp(b0_case1 + b1_case1 * hispanic1_case1 + b2_case1 * undergrad1_case1)
m2_case1_obs1 = m_case1_obs1 / (1 + m_case1_obs1)
prob_case1_obs1 = 1 - m2_case1_obs1

#### Obs 2: winner=Clinton, hispanic=0.25, undergrad=0.55
hispanic2_case1 = 0.25
undergrad2_case1 = 0.55
m_case1_obs2 = exp(b0_case1 + b1_case1 * hispanic2_case1 + b2_case1 * undergrad2_case1)
m2_case1_obs2 = m_case1_obs2 / (1 + m_case1_obs2)
prob_case1_obs2 = m2_case1_obs2

#### Obs 3: winner=Trump, hispanic=0.05, undergrad=0.05
hispanic3_case1 = 0.05
undergrad3_case1 = 0.05
m_case1_obs3 = exp(b0_case1 + b1_case1 * hispanic3_case1 + b2_case1 * undergrad3_case1)
m2_case1_obs3 = m_case1_obs3 / (1 + m_case1_obs3)
prob_case1_obs3 = 1 - m2_case1_obs3

#### Obs 4: winner=Clinton, hispanic=0.75, undergrad=0.10
hispanic4_case1 = 0.75
undergrad4_case1 = 0.10
m_case1_obs4 = exp(b0_case1 + b1_case1 * hispanic4_case1 + b2_case1 * undergrad4_case1)
m2_case1_obs4 = m_case1_obs4 / (1 + m_case1_obs4)
prob_case1_obs4 = m2_case1_obs4

# Combine probabilities into a vector
predicted_probs_case1 = c(prob_case1_obs1, prob_case1_obs2, prob_case1_obs3, prob_case1_obs4)

# Determine actual outcomes for Case 1
actual_case1 = c(0, 1, 0, 1)  # 0 represents Trump, 1 represents Clinton

# Calculate accuracy for Case 1
accuracy_case1 = sum((predicted_probs_case1 >= 0.5) == actual_case1) / length(actual_case1)

# Print accuracy for Case 1
cat("Accuracy for Case 1:", accuracy_case1, "\n")

# Case 2 ------------------------------------------------------------------
# Estimating the likelihood manually for b0 = -1, b1 = 2, b2 = 4 --------

b0_case2 = -1
b1_case2 = 2
b2_case2 = 4

# Define probability for Case 2:
#### Obs 1: winner=Trump, hispanic=0.15, undergrad=0.20
hispanic1_case2 = 0.15
undergrad1_case2 = 0.20
m_case2_obs1 = exp(b0_case2 + b1_case2 * hispanic1_case2 + b2_case2 * undergrad1_case2)
m2_case2_obs1 = m_case2_obs1 / (1 + m_case2_obs1)
prob_case2_obs1 = 1 - m2_case2_obs1

#### Obs 2: winner=Clinton, hispanic=0.25, undergrad=0.55
hispanic2_case2 = 0.25
undergrad2_case2 = 0.55
m_case2_obs2 = exp(b0_case2 + b1_case2 * hispanic2_case2 + b2_case2 * undergrad2_case2)
m2_case2_obs2 = m_case2_obs2 / (1 + m_case2_obs2)
prob_case2_obs2 = m2_case2_obs2

#### Obs 3: winner=Trump, hispanic=0.05, undergrad=0.05
hispanic3_case2 = 0.05
undergrad3_case2 = 0.05
m_case2_obs3 = exp(b0_case2 + b1_case2 * hispanic3_case2 + b2_case2 * undergrad3_case2)
m2_case2_obs3 = m_case2_obs3 / (1 + m_case2_obs3)
prob_case2_obs3 = 1 - m2_case2_obs3

#### Obs 4: winner=Clinton, hispanic=0.75, undergrad=0.10
hispanic4_case2 = 0.75
undergrad4_case2 = 0.10
m_case2_obs4 = exp(b0_case2 + b1_case2 * hispanic4_case2 + b2_case2 * undergrad4_case2)
m2_case2_obs4 = m_case2_obs4 / (1 + m_case2_obs4)
prob_case2_obs4 = m2_case2_obs4

# Combine probabilities into a vector
predicted_probs_case2 = c(prob_case2_obs1, prob_case2_obs2, prob_case2_obs3, prob_case2_obs4)

# Determine actual outcomes for Case 2
actual_case2 = c(0, 1, 0, 1)  # 0 represents Trump, 1 represents Clinton

# Calculate accuracy for Case 2
accuracy_case2 = sum((predicted_probs_case2 >= 0.5) == actual_case2) / length(actual_case2)

# Print accuracy for Case 2
cat("Accuracy for Case 2:", accuracy_case2, "\n")

### Accuracy for Case 1: 1.00 
### Accuracy for Case 2: 0.75



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Part 2 ------------------------------------------------------------------
wine_data <- read.csv("C:/Users/valhk/Documents/MMA/MGSC - R/wine_data.csv")
attach(wine_data)
library(MASS)
library(klaR)
library(caTools)
wine_data$cultivar=as.factor(wine_data$cultivar)


# QUESTION 3 --------------------------------------------------------------
### Linear discriminant analysis with two predictors ####


# A - Prior Probabilities of each class ------------------------------------------------
table(cultivar)


pi_1 = 59/178
pi_2 = 71/178
pi_3 = 48/178
pi_1
pi_2
pi_3


# B - Plotting Histograms of Alcohol levels for each class ------------------------------------------------

hists_alcohol=ggplot(wine_data, aes(x=alcohol))+geom_histogram(bins=50)+facet_grid(cultivar)
hists_alcohol


# C - Plotting Histograms of Acid levels for each class ------------------------------------------------

hists_acid=ggplot(wine_data, aes(x=acid))+geom_histogram(bins=50)+facet_grid(cultivar)
hists_acid


# D - Is it reasonable to assume that all functions are normally distributed? ------------------------------------------------


# E - Run a linear discriminant analysis to find Pr(Y=K|alcohol,acid) ------------------------------------------------

lda1 = lda(cultivar~alcohol+acid)
lda1

# F - Plot the classification regions using the partimat function ------------------------------------------------

partimat(cultivar~alcohol+acid, data = wine_data, method="lda", image.colors=c("pink","lightgreen","lightblue"))

# Interpret the results ------------------------------------------------

## The model creates 3 regions for each of the 3 cultivars. We see in red different misclassifications in red, the plot indicates an approx error rate of 0.191 which also means that the model classifies 80.9% of observations correctly (accuracy).
## From the position of the classes, alcohol seems to have a greater impact on the classification since the divisions are mostly lateral. 
## In conclusion from the plot: cultivar 1 wines seem to have higher alcohol levels while cultivar 2 wines have lower alcohol levels. Cultivar 3 wines are more acidic.


# G - Error rate of the discriminant model ------------------------------------------------

0.191
## This means that 19.1% of observations are misclassified which also means that the model classifies 80.9% of observations correctly (accuracy).


# H - The wine has an alcohol level of 14%, a malic acid level of 3g/L, and is supposedly from cultivar 1. What is the probability that the wine is not from cultivar 1? ------------------------------------------------

predict(lda1,data.frame(alcohol=14,acid=3))


# Probability that the seller is lying = 0.2756


# QUESTION 4 --------------------------------------------------------------
#### 4. Quadratic discriminant #####


# A - What is the difference between the linear and quadratic discriminant? ------------------------------------------------

## Quadratic discriminant analysis creates quadratic boundaries between classes allowing us to fit a better shape to each classification region.
## In linear discriminant analysis we consider that all classes have the same standard deviation, however in the quadratic discriminant analysis we use the standard deviation for each class.


# B - Running a quadratic discriminant analysis: Pr(Y=k|alcohol,acid) ------------------------------------------------

qda1=qda(cultivar~alcohol+acid)
qda1

# C - Plot the classification regions using the partimat() function ------------------------------------------------

partimat(cultivar~alcohol+acid, data = wine_data, method="qda", image.colors=c("pink","lightgreen","lightblue"))


# D - What is the error rate of the quadratic discriminant model? ------------------------------------------------
## The error rate of the quadratic discriminant model is 18.5%

# E - Which model performs better in the training data? ------------------------------------------------
## The quadratic model performs better as it has a slightly lower error rate and misclassifies less observations.
## However the error rates are close from each other so cross validation could give us more insights to which is the better model.


#### Cross-Validation for classification methods ####
set.seed(6)
sample = sample.split(wine_data$cultivar, SplitRatio = 0.5)
train_set = subset(wine_data, sample == TRUE)
test_set = subset(wine_data, sample == FALSE)

# Add the training and test points to the plot with different colors
plot <- ggplot(wine_data, aes(x=alcohol, y=acid)) + 
  geom_point(data=train_set, aes(color=factor(cultivar)), alpha=0.8) + # Training points with opacity
  geom_point(data=test_set, aes(shape=factor(cultivar)), color='grey', alpha=0.5) + # Test points in grey
  scale_color_manual(values=c("red", "blue", "green")) + # Custom colors for classes
  theme_minimal() + # Optional: sets a minimal theme for the plot
  labs(color='Cultivar', shape='Cultivar (Test Set)') # Labels for the legend

# Print the plot
print(plot)

fit1 = lda(cultivar~alcohol+acid, data = train_set)

actual = test_set$cultivar
predictions = predict(fit1, test_set)$class
conf_matrix = table(Predicted = predictions, Actual= actual)
accuracy = sum(diag(conf_matrix))/sum(conf_matrix)
conf_matrix
accuracy

fit2 = qda(cultivar~alcohol+acid, data = train_set)

actual = test_set$cultivar
predictions = predict(fit2, test_set)$class
conf_matrix = table(Predicted = predictions, Actual= actual)
accuracy = sum(diag(conf_matrix))/sum(conf_matrix)
conf_matrix
accuracy




