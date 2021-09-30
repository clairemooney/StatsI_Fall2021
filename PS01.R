#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("C:/Users/Student/Desktop/Postgraduate Work/StatsI_Fall2021/problemSets/PS01")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)
")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)
")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

t.test(y,conf.level=0.9)

##Confidence interval for the 1Q is between 94 and 103
##This would mean 90% of the students IQ falls between 94 and 103

##The principal wants to know if the average IQ is higher than the countries average
mean(y)
#mean =98.44
sd(y)
#sd=13.09287
length(y)
#length =25

t=37.593 
##sample estimates :mean of x: 98.44
##p-value < 2.2e-16
school <- rnorm(n=25, mean= 98.44, sd=13.09287)

#lower tail is false 
pnorm(37.593, lower.tail = F)

t.test(y, mu= 100, alternative = "two.sided")
t.test(y, mu=100, alternative ="greater")
t.test(y, mu=100, alternative = "less")

#The null hypothesis is the the mu is less then or equal to 100 
#The alternative hypothesis is that the school's average IQ is greater than 100
#Running a one sided t-test with our alternative hypothesis yields a p value of 0.7215
#This test is not significant with alpha =o.o5 therefore p is greater than alpha




# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)

str(expenditure)
lines(expenditure$Y)
lines(expenditure$X1)
lines(expenditure$X2)
lines(expenditure$X3)

hist(expenditure$Y)
hist(expenditure$Y, probability = TRUE)
hist(expenditure$X1,probability = TRUE)
hist(expenditure$X2, probability =TRUE)
hist(expenditure$X3, probability =TRUE)

lines(density(expenditure$Y,expenditure$X1,expenditure$X2,expenditure$X3))

Lines(density(expenditure$Y,expenditure$X1,expenditure$X2,expenditure$X3))
plot(density(expenditure$Y,expenditure$X1,expenditure$X2,expdenditure$X3), main = "Histogram of the States in the US")

plot(expenditure, ylim=range(expenditure$Y,expenditure$X1,expenditure$X3),col='black')
lines(expenditure$Y,col='red')
lines(expenditure$X1,col='green')
lines(expenditure$X2,col='pink')
lines(expenditure$X3,col='blue')

plot(expenditure,ylim=range(expenditure$Y,expenditure$X1,expenditure$X2,expenditure$X3),col='black', main = "Expenditure of States in US")


#Part 2 

install.packages("ggplot2")
library(ggplot2)

expenditure$Region = as.factor(expenditure$Region)
library(ggplot2)

data=as.data.frame(expenditure[c(2,6)])
data$Region = as.factor(data$Region)
mode(data$Region)
#This then changes expenditure to a dataframe. 
#As I learnt R will regions as intergers unless you make them factors
#This has created a data frame out of the two variables we are looking at 
#In this case we are using Y(2) and Region (6) based on their position on the list 
#Mode(data$region)was to check if the region had changed to a factor

ggplot(aes(y = Y, x = Region, fill=Region), data = data)+ geom_boxplot()+ggtitle("Box plots of Expenditure by Region")
#I had originally tested this mode but had not realised that ggplot2 needed to be installed
#Following guidance I installed ggplot2
#I then created a box plot graph comparing expenditure and region 
#Having analysed the data it appears region 4 the 'West'has the highest per capita expenditure on housing assistance

#Part 3
library(ggplot2)
expenditure$X1 = as.factor(expenditure$X1)
library(ggplot2)

data=as.data.frame(expenditure[c(2,3)])
data$X1 = as.factor(data$X1)
mode(data$X1)

ggplot(aes(y = Y, x = X1, fill= X1), data = data)+ geom_boxplot()+ggtitle("Box plots of Expenditure per capita")

library(ggplot2)
expenditure$Region = as.factor(expenditure$Region)
library(ggplot2)

data=as.data.frame(expenditure[c(2,3,6)])
data$Region = as.factor(data$Region)
mode(data$Region)

ggplot(aes(y = Y, x = X1, fill= Region), data = data)+ geom_boxplot()+ggtitle("Box plots of Expenditure per capita")

ggplot(aes(y = Y, x = X1, fill= Region), data = data)+ geom_boxplot()+ggtitle("Box plots of Expenditure per capita")
