board_games <- read.csv("C:/Users/valhk/Documents/MMA/MGSC - R/board_games_fall_2023.csv")
attach(board_games) 
library(car)
library(boot)
library(splines)

######### MODEL ISSUES ###########
reg0 = lm(avg_rating ~ year + avg_timeplay + weight)

residualPlots(reg0)

summary(reg0)

# Problem 2 ---------------------------------------------------------------

reg1 = lm(avg_rating ~ avg_timeplay)
summary(reg1)


## Visual funnel test

## Method 1
residualPlot(reg1, quadratic = FALSE)
abline(h = 0, col = "red")

## Method 2
#install.packages("ggplot2")
library(ggplot2)

ggplot(board_games, aes(x = avg_timeplay, y = avg_rating)) + geom_point() + geom_smooth(method = "lm", se = FALSE)  # Add a linear regression line without confidence intervals


## NCV test

ncvTest(reg1)


## Correcting heteroskedasticity

#install.packages("lmtest")
#install.packages("plm")
require(lmtest)
require(plm)


summary(reg1)
coeftest(reg1, vcov=vcovHC(reg1, type = "HC1")) # applying the correction



# Problem 3 ---------------------------------------------------------------

#Outliers
reg_outlier = lm(avg_rating~min_players+age+num_votes)
qqPlot(reg_outlier, envelope=list(style="none"))
#returns obs 1197 and 3124 as outliers
#Numerical approach: Bonferroni outlier test:
outlierTest(reg_outlier)
#gives the probability of them not being outliers
outliers <- board_games[c(3124),]
print(outliers)
#Step 2: Remove outliers
board_games_2=board_games[-c(3124),] #after , is space for columns
reg_outlier1=lm(avg_rating~min_players+age+num_votes, data=board_games_2)
summary(reg_outlier)
summary(reg_outlier1)



# Problem 4 ---------------------------------------------------------------

#Colinearity
require(psych)
#Step 1 : Select quantitative variable
quantvars = board_games[,c(9,12,7,8)]
#Correlation matrix
corr_matrix = cor(quantvars)
round(corr_matrix, 2)
#VIF
#Step 2 : Look for variance inflation factors
reg_collinear=lm(avg_rating~year+age+min_timeplay+max_timeplay)
vif(reg_collinear)
reg_collinear1=lm(avg_rating~year+age+max_timeplay)
vif(reg_collinear1)
summary(reg_collinear)
summary(reg_collinear1)


# Problem 5 ---------------------------------------------------------------

#############Presenting professional regression tables##############
#STEP 1: 4 regressions
reg1 = lm(avg_rating~avg_timeplay)
reg2 = lm(avg_rating~min_players)
reg3 = lm(avg_rating~max_players)
mreg = lm(avg_rating~avg_timeplay+min_players+max_players)
#STEP 2: installing stargazer
#install.packages("stargazer")
library(stargazer)
stargazer(reg1,reg2,reg3,mreg, type="html")
summary(reg2)
summary(mreg)
#Proper stargaze table:
stargazer(reg1, reg2, reg3, mreg, title="Regression Results",
          align=TRUE, dep.var.labels=c("Board game rating"),
          covariate.labels=c("Average timeplay","Minimum number of players",
                             "Maximum number of players"),
          no.space=TRUE, type ="html", digits = 2 )



# Problems 6 to 8 ---------------------------------------------------------------

#############Polynomial regression : avg_timeplay###############
#STEP 1: 4 regressions
reg1_poly_timeplay = lm(avg_rating~avg_timeplay)
reg2_poly_timeplay = lm(avg_rating~poly(avg_timeplay,2))
reg3_poly_timeplay = lm(avg_rating~poly(avg_timeplay,3))
reg4_poly_timeplay = lm(avg_rating~poly(avg_timeplay,4))
summary(reg1_poly_timeplay)
summary(reg2_poly_timeplay)
summary(reg3_poly_timeplay)
summary(reg4_poly_timeplay)
#STEP 2: Stargazer plot
#1 rename the coefficients:
rename_coeffs <- function(model) {
  # Extract coefficients
  coefficients <- coef(model)
  # Rename coefficients
  names(coefficients) <- c("(Intercept)", "x", "x²", "x³", "x⁴")[1:length(coefficients)]
  # Replace the coefficients in the model
  model$coefficients <- coefficients
  return(model)
}
regp1 = rename_coeffs(reg1_poly_timeplay)
regp2 = rename_coeffs(reg2_poly_timeplay)
regp3 = rename_coeffs(reg3_poly_timeplay)
regp4 = rename_coeffs(reg4_poly_timeplay)
#2 table
stargazer(regp1, regp2, regp3, regp4,
          type="html", omit.stat="f", no.space=TRUE,dep.var.labels=c("Board game rating"),
          title="Polynomial Regression Results for variable: average play time",
          column.labels=c("Linear", "Quadratic", "Cubic", "Quartic"))
#STEP 3 : GGPlot scatterplots
library(ggplot2)
require(methods)
library(ggpubr)


# Base plot for avg_timeplay vs avg_rating
base_plot_timeplay <- ggplot(board_games, aes(x=avg_timeplay, y=avg_rating)) +
  geom_point(color = "grey")
# Plots with polynomial lines
plot1_timeplay <- base_plot_timeplay + 
  geom_smooth(aes(color = "1"), method = "lm", formula = y ~ x)
plot2_timeplay <- base_plot_timeplay + 
  geom_smooth(aes(color = "2"), method = "lm", formula = y ~ poly(x, 2))
plot3_timeplay <- base_plot_timeplay + 
  geom_smooth(aes(color = "3"), method = "lm", formula = y ~ poly(x, 3))
plot4_timeplay <- base_plot_timeplay + 
  geom_smooth(aes(color = "4"), method = "lm", formula = y ~ poly(x, 4))
# Combine the plots with ggarrange()
combined_plot_timeplay <- ggarrange(plot1_timeplay + scale_color_manual(name="Degree", values=c("1"="lightgreen")), 
                                    plot2_timeplay + scale_color_manual(name="Degree", values=c("2"="green")), 
                                    plot3_timeplay + scale_color_manual(name="Degree", values=c("3"="forest green")), 
                                    plot4_timeplay + scale_color_manual(name="Degree", values=c("4"="darkgreen")), 
                                    ncol = 1, nrow = 4)
print(combined_plot_timeplay)

#STEP 4 ANOVA TEST
anova(reg1_poly_timeplay,reg2_poly_timeplay,reg3_poly_timeplay,reg4_poly_timeplay)
anova(reg1_poly_timeplay,reg3_poly_timeplay)




#board_games_3=board_games[-c(1425),]


############## Polynomial regression : age ################
#STEP 1: 4 regressions

reg1_poly_age = lm(avg_rating~age)
reg2_poly_age = lm(avg_rating~poly(age,2))
reg3_poly_age = lm(avg_rating~poly(age,3))
reg4_poly_age = lm(avg_rating~poly(age,4))
#STEP 2: Stargazer 
#1 rename the coefficients:
rename_coeffs_age <- function(model) {
  coefficients <- coef(model)
  names(coefficients) <- c("(Intercept)", "age", "age²", "age³", "age⁴")[1:length(coefficients)]
  model$coefficients <- coefficients
  return(model)
}
rega1 = rename_coeffs_age(reg1_poly_age)
rega2 = rename_coeffs_age(reg2_poly_age)
rega3 = rename_coeffs_age(reg3_poly_age)
rega4 = rename_coeffs_age(reg4_poly_age)
#2 table
stargazer(rega1, rega2, rega3, rega4,
          type="html", omit.stat="f", no.space=TRUE,dep.var.labels=c("Board game rating"),
          title="Polynomial Regression Results for variable: age",
          column.labels=c("Linear", "Quadratic", "Cubic", "Quartic"))

# Base plot with scatter
base_plot <- ggplot(board_games, aes(x=age, y=avg_rating)) +
  geom_point(color = "grey")
# Plots with polynomial lines
plot1 <- base_plot + 
  geom_smooth(method = "lm", formula = y ~ x, aes(color = "1"))
plot2 <- base_plot + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "2"))
plot3 <- base_plot + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), aes(color = "3"))
plot4 <- base_plot + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), aes(color = "4"))

# Combine the plots with ggarrange()
combined_plot <- ggarrange(plot1 + scale_color_manual(name="Degree", values=c("1"="lightblue")),
                           plot2 + scale_color_manual(name="Degree", values=c("2"="cyan")), 
                           plot3 + scale_color_manual(name="Degree", values=c("3"="blue")), 
                           plot4 + scale_color_manual(name="Degree", values=c("4"="darkblue")), 
                           ncol = 2, nrow = 2)
print(combined_plot)
#Anova test
anova(reg1_poly_age,reg2_poly_age,reg3_poly_age,reg4_poly_age)

#Multiple polynomial regression
multipoly <- lm(avg_rating ~avg_timeplay + age + I(age^2) + I(age^3) + I(age^4), data=board_games)
summary(multipoly)
#Proper stargaze table:
stargazer(multipoly, title="Regression Results",
          align=TRUE, dep.var.labels=c("Board game rating"),
          covariate.labels=c("Average timeplay","Age","Age²","Age³","Age⁴"),
          no.space=TRUE, type ="html", digits = 2 )



# Problem 9 ---------------------------------------------------------------

####### SPLINE ##########

reg_spline1 = lm(avg_rating~bs(avg_timeplay, knots=quantile(avg_timeplay, c(0.25, 0.5, 0.75)), degree = 1))
reg_spline2 = lm(avg_rating~bs(avg_timeplay, knots=quantile(avg_timeplay, c(0.25, 0.5, 0.75)), degree = 2))
reg_spline3 = lm(avg_rating~bs(avg_timeplay, knots=quantile(avg_timeplay, c(0.25, 0.5, 0.75)), degree = 3))

stargazer(reg_spline1, reg_spline2, reg_spline3, 
          title="Spline Regression Results",
          align=TRUE, 
          dep.var.labels=c("Board game rating"),
          column.labels = c("Linear","Quadratic","Cubic"),
          covariate.labels=c("(Intercept Deg1)","Linear Spline 1","Linear Spline 2","Linear Spline 3",
                             "(Intercept Deg2)","Quadratic Spline 1","Quadratic Spline 2","Quadratic Spline 3","Quadratic Spline 4",
                             "(Intercept Deg3)","Cubic Spline 1","Cubic Spline 2","Cubic Spline 3","Cubic Spline 4","Cubic Spline 5"),
          no.space=TRUE, 
          type ="html", 
          digits = 2 )

#PLOT

library(ggplot2)
library(ggpubr)

k1 <- quantile(board_games$avg_timeplay, 0.25)
k2 <- quantile(board_games$avg_timeplay, 0.5)
k3 <- quantile(board_games$avg_timeplay, 0.75)
plot <- ggplot(board_games, aes(x = avg_timeplay, y = avg_rating)) + geom_point()
plot1 <- plot + 
  geom_smooth(method = "lm", formula = y ~ bs(x, knots = c(k1, k2, k3), degree = 1), color = "red", se = FALSE) +
  geom_vline(xintercept = c(k1, k2, k3), linetype = "dashed") +
  labs(title = "Degree 1 with 3 Knots")

plot2 <- plot + 
  geom_smooth(method = "lm", formula = y ~ bs(x, knots = c(k1, k2, k3), degree = 2), color = "red", se = FALSE) +
  geom_vline(xintercept = c(k1, k2, k3), linetype = "dashed") +
  labs(title = "Degree 2 with 3 Knots")

plot3 <- plot + 
  geom_smooth(method = "lm", formula = y ~ bs(x, knots = c(k1, k2, k3), degree = 3), color = "red", se = FALSE) +
  geom_vline(xintercept = c(k1, k2, k3), linetype = "dashed") +
  labs(title = "Degree 3 with 3 Knots")

arranged_plots <- ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1)
print(arranged_plots)



# Problem 10 --------------------------------------------------------------

########### VALIDATION ######################

mse_list = matrix(NA, nrow = 30, ncol = 10)
for (rep in 1:30){
  for (d in 1:10){
    sample=sample.split(board_games$avg_rating, SplitRatio = 0.5)
    train_set = subset(board_games, sample==TRUE)
    test_set = subset(board_games, sample==FALSE)
    fit_model = glm(avg_rating~poly(weight,d), data=train_set)
    actual = test_set$avg_rating
    prediction = predict(fit_model, test_set)
    squared_error = (actual - prediction)^2
    mse_list[rep, d] = mean(squared_error)
  }
}
average_mses <- colMeans(mse_list)
print(average_mses)


# Problem 11 --------------------------------------------------------------

######### LOOCV ############
#Linear
start_time <- Sys.time()
fit_l = glm(avg_rating~weight, data=board_games)
mse_l = cv.glm(board_games, fit_l)$delta[1]
end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)
mse_l


#Polynomial
mse_loocv_list <- rep(NA, 10)
for (d in 1:10){
  fit_loocv <- glm(avg_rating~poly(weight,d), data=board_games)
  mse_loocv_list[d] <- cv.glm(board_games,fit_loocv)$delta[1]
}
print(mse_loocv_list)


# Problem 12 --------------------------------------------------------------

####### K-fold CV ##########

mse_kfold = rep(NA,10) # running 10 tests

for (d in 1:10){
  fit_kfold=glm(avg_rating~poly(num_votes, d), data = board_games)
  mse_kfold[d]= cv.glm(board_games, fit_kfold, K=5)$delta[1]
}
print(mse_kfold)



# Problem 13 --------------------------------------------------------------

####### Multiple Spline Model CV ######

degrees = c(1, 2, 3, 4, 5)
best_combination = NULL
min_mse = 9999999

for (a in degrees) {
  for (b in degrees) {
    for (c in degrees) {
      for (d in degrees) {
        
        spline_model <- glm(avg_rating ~
                             bs(age, knots = quantile(age, c(0.25, 0.5, 0.75)), degree = a) +
                             bs(year, knots = quantile(year, c(0.25, 0.5, 0.75)), degree = b) +
                             bs(num_votes, knots = quantile(num_votes, c(0.25, 0.5, 0.75)), degree = c) +
                             bs(avg_timeplay, knots = quantile(avg_timeplay, c(0.25, 0.5, 0.75)), degree = d))

        set.seed(123)  
        cv_results <- cv.glm(board_games, spline_model, K = 20)
        mse_instance <- cv_results$delta[1]
       
        if (mse_instance < min_mse) {
          min_mse <- mse_instance
          best_combination <- c(a, b, c, d)
        }
      }
    }
  }
}
cat("Best Combination:", best_combination, "\n")
cat("Lowest MSE:", lowest_mse, "\n")