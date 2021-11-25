```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
options(scipen = 999)
library(tidyverse)
library(broom)
install.packages("stargazer")
library(stargazer)
```
Registered S3 methods overwritten by 'dbplyr':
  method         from
print.tbl_lazy     
print.tbl_sql      
-- Attaching packages ----------------- tidyverse 1.3.1 --
  v ggplot2 3.3.5     v purrr   0.3.4
v tibble  3.1.4     v dplyr   1.0.7
v tidyr   1.1.3     v stringr 1.4.0
v readr   2.0.1     v forcats 0.5.1
-- Conflicts -------------------- tidyverse_conflicts() --
  x dplyr::filter() masks stats::filter()
x dplyr::lag()    masks stats::lag()

Please cite as: 
  
  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.
R package version 5.2.2. https://CRAN.R-project.org/package=stargazer

install.packages("car")
library(car)
data(Prestige)
help(Prestige)

#Question 1 part A 


names(Prestige)[names(Prestige) == "type"] <- "professional"

Prestige <- Prestige %>% mutate(Prestige, professional = if_else(professional =="prof", 1, 0))
view(Prestige)
##this worked 


#Question 1 part B

Mod1 <- lm(prestige ~ income + as.factor(professional) + as.factor(professional):income, data = Prestige)
summary(Mod1)


#Question 1 part C 


#x1=income 
#x2= professional prof
#x3= professional wc 

#Prediction equation is
$\hat{y} = \beta_{0} + \beta_{1}x_{1} + \beta_{2}x_{2} + \beta_{3}x_{3}
$\hat{y} = 21.1422589 + 0.0031709 x_{1} + 37.7812800x_{2} + -0.0023257x_{3}

#1 if the person is a professional
#0 if the person is anything other such as working class 
 

#Question 1 part D

#On average a 1 unit increase in dollar is a 0.0031709 
#increase in the expected value of y holding my other covariates 
#constant at their empirical means 

#Question 1 part e 

# Professionals are predicted to have 
#a prestige score 37.7812800 or 38 higher than 
#than working class people
#controlling for all of the other independent variables.

#Question 1 part f 

$\hat{y} = \beta_{0} + \beta_{1}x_{1} + \beta_{2}x_{2} + \beta_{3}x_{3}
$\hat{y} = 21.1422589 + 0.0031709 x_{1} + 37.7812800x_{2} + -0.0023257x_{3}

effect1 <- 21.1422589 + 0.0031709 * 0 + 37.7812800*1 + (-0.0023257*0 *1)
effect1
effect2 <- 21.1422589 + 0.0031709 * 1000 + 37.7812800*1 + (-0.0023257* 1000*1)
effect2

effect3 <-effect2- effect1
effect3

#returns a value of 0.8452 

#Question 1 part g 

effect4 <- 21.1422589 + 0.0031709 * 6000 + 37.7812800*0 + (-0.0023257*0 *0)
effect4

effect5 <- 21.1422589 + 0.0031709 * 6000 + 37.7812800*1 + (-0.0023257* 6000*1)
effect5

effect6 <- effect5-effect4
effect6
#returns a value of 23.82708 

#Question 2 part 1 

#0.042 divided into 0.016 = 2.625 t score is bigger than 1.96 
#131-3 = 128 degrees of freedom 
#p value of 0.005 x 2 = 0.01 it is less than 0.05
#null hypothesis is that it makes no difference to voteshare having a yard sign 
#we reject the null hypothesis 

#Question 2 part 2 
#0.042 divided by 0.013 = 3.23076 t score is bigger than 1.96 
#degrees of freedom is 128 
#p value 0.000787 x 2 = 0.001574 this is again less than 0.05
#null hypothesis is that it makes no difference to voteshare have a yard sign
#we reject the null hypothesis

#Question 2 part 3 

#The coefficient suggests even if there are no lawn signs 
#there is still a 30% voteshare for Ken Cuccinelli
#the lawn signs increase voteshare by roughly 4% 

#Question part 4

#the r sqaured value offers us an insight into how much of the 
#the model is being explained 
#it explains 9.4% of the voteshare therefore there are 
#a lot of other variables to be considered 
#other factors that would influence voteshare could be anything
#from political opinions and associations, current residential or
#county issues to political debate 
#this model would suggest there would be a lot of residuals 

