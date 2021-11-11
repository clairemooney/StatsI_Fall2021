setwd("C:/Users/Student/Desktop/Stats 3 Assignment")

options(scipen = 999) # to change the default output of numbers
library(tidyverse)
#install.packages("broom")
library(broom)
?broom

incumbents <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/incumbents_subset.csv")

head(incumbents)
glimpse(incumbents)
summary(incumbents)

#Question 1 part 1 

incumbents_lm <- lm(voteshare ~ difflog, data = incumbents)
summary(incumbents_lm)

#Question 1 part 2 

ggplot(incumbents, aes(difflog, voteshare)) +
  geom_point(alpha = 0.5) + #add a scatterplot # alpha show where over plotting
  geom_smooth(method = "lm") #add a linear regression line
  

#Question 1 part 3 

resids1 <- resid(incumbents_lm)
resids1
class(resids1)
#returned 1 "Numeric"

#Question 1 part 4 

#y = 0.041666 * x + 0.579031
$\beta_{0} = 0.579031 $
  $\beta_{1} = 0.041666 $
  $x_{1} = difflog$
  $y= voteshare$
  
  $\hat{y} = \beta_{0} + \beta_{1}x_{1}$
  $\hat{y} = 0.579031 + 0.041666x_{1} $

#when difflog the explanatory variable is equal to = 0 
#voteshare is equal to roughly 0.579031
#need to find the standard error of where the regression lines goes 
#through the y axis that is where x =0
#the estimate we get for beta nought is where regression lines crosses
#y intercepet - when x = 0 y intercept 0.579031
#y =mx+ c

#Question 2 part 1 

incumbents_lm2 <- lm(presvote ~ difflog, data = incumbents)
summary(incumbents_lm2)

#Question 2 part 2 

ggplot(incumbents, aes(difflog, presvote)) +
  geom_point(alpha = 0.5) + #add a scatterplot # alpha show where over plotting
  geom_smooth(method = "lm") #add a linear regression line

#Question 2 part 3 

resids2 <- resid(incumbents_lm2)
resids2
class(resids2)
#class numeric 


#Question 2 part 4 

#y = 0.023837 * x + 0.507583
$\beta_{0} = 0.507583 $
  $\beta_{1} = 0.023837 $
  $x_{1} = difflog$
  $y= presvote$
  
  $\hat{y} = \beta_{0} + \beta_{1}x_{1}$
  $\hat{y} = 0.507583 + 0.023837x_{1} $


#Question 3 part 1 

incumbents_lm3 <- lm(voteshare ~ presvote, data = incumbents)
summary(incumbents_lm3)

#Question 3 part 2 

ggplot(incumbents, aes(presvote, voteshare)) +
  geom_point(alpha = 0.5) + #add a scatterplot # alpha show where over plotting
  geom_smooth(method = "lm") #add a linear regression line

#Question 3 part 3 

#y = 0.388018 * x + 0.441330

$\beta_{0} = 0.441330 $
  $\beta_{1} = 0.388018 $
  $x_{1} = presvote$
  $y= voteshare$
  
  $\hat{y} = \beta_{0} + \beta_{1}x_{1}$
  $\hat{y} = 0.441330 + 0.388018x_{1} $

#Question 4 part 1 

incumbents_lm4 <- lm(resids1 ~ resids2, data = incumbents)
summary(incumbents_lm4)

#Question 4 part 2 

ggplot(incumbents, aes(resids2, resids1)) +
  geom_point(alpha = 0.5) + #add a scatterplot # alpha show where over plotting
  geom_smooth(method = "lm") #add a linear regression line

#Question 4 part 3 

#y = -0.00000000000000000486 * x + 0.25687701270009788423
$\beta_{0} = 0.25687701270009788423 $
  $\beta_{1} =  -0.00000000000000000486$
  $x_{1} = resids2$
  $y= resids1$
  
  $\hat{y} = \beta_{0} + \beta_{1}x_{1}$
  $\hat{y} = 0.25687701270009788423+ -0.00000000000000000486x_{1} $

#Question 5 part 1 
incumbents_lm5 <- lm(voteshare ~ difflog + presvote, data = incumbents)
summary(incumbents_lm5)

#Question 5 part 2 

#x1 = difflog
#x2 = presvote 

#y = 0.0355431 * x1 + 0.2568770 * x2 + 0.4486442

$\beta_{0} = 0.4486442 $
  $\beta_{1} = 0.0355431 $
  $\beta_{2} = 0.2568770
  
  $x_{1} = difflog$
  $x_{2} = presvote$
  $y= voteshare$
  
  $\hat{y} = \beta_{0} + \beta_{1}x_{1} + \beta_{2}x_{2}
  $\hat{y} = 0.4486442 + 0.0355431x_{1} + 0.2568770x_{2}

#Question 5 part 3

#The residuals for the output of the lm of incumbents_lm4 
#and the residuals for the out of the lm of incumbents_lm5
#returns the same values for min -0.25928, 1q -0.04737
#median -0.00121, 3q 0.04618, max 0.33126


