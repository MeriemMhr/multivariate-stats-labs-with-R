## -------------------------------------------------- ##
video_games_fall <- read.csv("C:/Users/valhk/Documents/MMA/MGSC - R/video_games_fall_2023.csv")
attach(video_games_fall)

names(video_games_fall)

#Score
summary(score)
boxplot(score, col="blue")
hist(score, breaks =20, border ="blue")

#sales global
summary(sales_global)
boxplot(sales_global, col="green")
hist(sales_global, breaks =100, border ="green")

#release_year
summary(release_year)
boxplot(release_year, col="orange")
hist(release_year, breaks =25, border ="orange")

#count_critic
summary(count_critic)
boxplot(count_critic, col="purple")
hist(count_critic, breaks =10, border ="purple")

par(mfrow=c(1,3))
colors <- as.numeric(as.factor(genre))
unique_genres <- unique(genre)
color_legend <- as.numeric(as.factor(unique_genres))
plot(sales_global,score, col = colors)
plot(release_year,score, col = colors)
plot(count_critic,score, col = colors) 

plot(1, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1), axes=FALSE)
legend("center", legend = unique_genres, col = color_legend, pch = 16)

#single regression sales_global
lm.fit.sales = lm(score~sales_global)
lm.fit.sales
par(mfrow=c(1,1))
plot(sales_global,score)
abline(lm.fit.sales, col="green")
b0_sales = coef(lm.fit.sales)[1]
b1_sales = coef(lm.fit.sales)[2]
summary(lm.fit.sales)
confint(lm.fit.sales,'sales_global', level=0.95)
require(visreg)     
visreg(lm.fit.sales,alpha=0.05)

b0=coef(lm.fit.sales)[1]
b1=coef(lm.fit.sales)[2]
b0+b1*0.75

#single regression release_year
lm.fit.year = lm(score~release_year)
lm.fit.year
par(mfrow=c(1,1))
plot(release_year,score)
abline(lm.fit.year, col="orange")
b0_year = coef(lm.fit.year)[1]
b1_year = coef(lm.fit.year)[2]
summary(lm.fit.year)
confint(lm.fit.year,'release_year', level=0.95)
require(visreg)     
visreg(lm.fit.year,alpha=0.05)

b0=coef(lm.fit.year)[1]
b1=coef(lm.fit.year)[2]
b0+b1*2009

#single regression count_critic
lm.fit.critic = lm(score~count_critic)
lm.fit.critic
par(mfrow=c(1,1))
plot(count_critic,score)
abline(lm.fit.critic, col="purple")
b0_critic = coef(lm.fit.critic)[1]
b1_critic = coef(lm.fit.critic)[2]
summary(lm.fit.critic)
confint(lm.fit.critic,'count_critic', level=0.95)
require(visreg)     
visreg(lm.fit.critic,alpha=0.05)

b0=coef(lm.fit.critic)[1]
b1=coef(lm.fit.critic)[2]
b0+b1*80

#multiple regression
mreg=lm(score~sales_global+release_year+count_critic)
summary(mreg)
#predict critic score of a game (sales=750000, released=2009 and count_critic=80)
b0=coef(mreg)[1]
b1=coef(mreg)[2]
b2=coef(mreg)[3]
b3=coef(mreg)[4]
b0+b1*0.75+b2*2009+b3*80

#nintendo as a category
video_games_fall$Nintendo <- ifelse(video_games_fall$publisher=="Nintendo", "1", "0")
attach(video_games_fall)
mreg2=lm(score~release_year+Nintendo)
summary(mreg2)
#plot
b0=coef(mreg2)[1]
b1=coef(mreg2)[2]
b2=coef(mreg2)[3]
plot(release_year, score, col=ifelse(Nintendo=="1", "green", "blue"),
     pch=15, xlab = "Release Year", ylab = "Score",
     xlim = c(min(release_year), max(release_year)),
     ylim = c(min(score), max(score)))
#regression line for Nintendo
abline(b0+b2, b1, col="green", lwd = 2, lty = 2)
#regression line for Other publishers
abline(b0,b1,col="blue", lwd = 2, lty = 2)
legend("topright",pch=15,col=c("green","blue"),c("Nintendo","Others"),lwd = 2, lty = 2)

#Categorical data
levels(genre)   #shows the categories in each variable
table(genre)    #shows the number of obs in each category

#regression with dummies for genre
video_games_fall$genre=as.factor(video_games_fall$genre)
attach(video_games_fall)
#changing the reference category
video_games_fall$genre=relevel(video_games_fall$genre, ref="Racing")
attach(video_games_fall)
levels(genre)
mreg3=lm(score~genre)
summary(mreg3)

#interaction terms
#strategy as a category
video_games_fall$Strategy <- ifelse(video_games_fall$genre=="Strategy", "1", "0")
attach(video_games_fall)
mreg4=lm(score~Nintendo+Strategy+Nintendo*Strategy)
summary(mreg4)

#interaction terms release year and Nintendo
mreg5=lm(score~release_year+Nintendo+release_year*Nintendo)
summary(mreg5)
#plot interaction terms
b0=coef(mreg5)[1]
b1=coef(mreg5)[2]
b2=coef(mreg5)[3]
b3=coef(mreg5)[4]
#interaction term
#price = b0 + b1(release_year) + b2(Nintendo) + b3(Nintendo*release_year)   this gives us:
#price = (b0 + b2) + (b1 + b3)(release_year) #Nintendo
#price = b0 + b1(release_year)               #release_year

plot(release_year, score, col=ifelse(Nintendo=="1", "green", "blue"),
     pch=2, xlab = "Release year", ylab = "Score")
#regression line for Nintendo
abline(b0+b2, b1+b3, col="green", lwd = 2, lty = 2)
#regression line for release_year
abline(b0,b1,col="blue", lwd = 2, lty = 2)
legend("bottomleft",pch=2,col=c("green","blue"),c("Nintendo game","Others"))