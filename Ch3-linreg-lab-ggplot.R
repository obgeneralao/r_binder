
# Lab: Linear Regression


## Libraries

###
library(MASS)
library(ISLR2)

## Simple Linear Regression

###
# head(Boston)

###
d <- Boston
lm.fit <- lm(medv ~ lstat, data = Boston)
###
lm.fit
summary(lm.fit)
###
d$predicted <- predict(lm.fit)   # Save the predicted values in an appended column
d$residuals <- residuals(lm.fit) # Save the residual values in an appended column
##______________________
# names(lm.fit)
# Quick look at the actual, predicted, and residual values
# library(dplyr)
# d %>% select(lstat, predicted, residuals) %>% head()


# library(ggplot2)
# ggplot(d, aes(x = lstat, y = medv)) +  # Set up canvas with outcome variable on y-axis
#   geom_point()  # Plot the actual points
# 
# ggplot(d, aes(x = lstat, y = medv)) +
#   geom_point() +
#   geom_point(aes(y = predicted), shape = 1)  # Add the predicted values



library(plotly)
library(ggplot2)

p <- ggplot(d, aes(x = lstat, y = medv)) + # Set up canvas with outcome variable on y-axis
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = lstat, yend = predicted), alpha = .2) +  # alpha to fade lines
  
  # > Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +  # Colors to use here
  guides(color = "none") +
  # <
  
  #geom_point() + # Plot the actual points
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  # Add theme for cleaner look

ggplotly(p)












coef(lm.fit)
###
confint(lm.fit)
###
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
        interval = "confidence")
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
        interval = "prediction")
###
plot(lstat, medv)
abline(lm.fit)
###
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)
###
par(mfrow = c(2, 2))
plot(lm.fit)
###
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
###
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

## Multiple Linear Regression

###
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)
###
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)
###
library(car)
vif(lm.fit)
###
lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)
###
lm.fit1 <- update(lm.fit, ~ . - age)

## Interaction Terms

###
summary(lm(medv ~ lstat * age, data = Boston))

## Non-linear Transformations of the Predictors

###
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)
###
lm.fit <- lm(medv ~ lstat)
anova(lm.fit, lm.fit2)
###
par(mfrow = c(2, 2))
plot(lm.fit2)
###
lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)
###
summary(lm(medv ~ log(rm), data = Boston))

## Qualitative Predictors

###
head(Carseats)
###
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, 
             data = Carseats)
summary(lm.fit)
###
attach(Carseats)
contrasts(ShelveLoc)

## Writing  Functions

###
LoadLibraries
LoadLibraries()
###
LoadLibraries <- function() {
  library(ISLR2)
  library(MASS)
  print("The libraries have been loaded.")
}
###
LoadLibraries
###
LoadLibraries()
###
