# 작업공간 설정
setwd("C:/R/Categorical Data Analysis")

#####################################
# CH01. Introduction                #
#####################################
# Reading data files
## header=TRUE : 변수명 지정
Clinical <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Clinical.dat", header=TRUE)
# Installing packages
install.packages("binom")
library(binom)

# Inference about proportion
prop.test(837, 1810, p=0.50, alternative="two.sided", correct=FALSE) #H_1: p =/= 0.5
prop.test(837, 1810, p=0.50, alternative="less", correct=FALSE) # H_1: p < 0.5

attach(Clinical)
y <- sum(response)
prop.test(y, n=10, conf.level=0.95, correct=FALSE)

prop.test(sum(Clinical$response), 10, correct=FALSE)$conf.int
with(Clinical, prop.test(sum(response), 10, correct=FALSE)$conf.int)

# binom package
library(binom)
binom.confint(9, 10, conf.level=0.95, method="asymptotic")  #Wald cI
with(Clinical, binom.confint(sum(response), nrow(Clinical), conf.level=0.95, method="asymptotic"))
binom.confint(9, 10, conf.level=0.95, method="wilson")  #Score CI
binom.confint(9, 10, conf.level=0.95, method="agresti-coull")  #Agresti & Coull cI

# To obtain exact P-value based on Binomial distribution
binom.test(9, 10, 0.50, alternative="two.sided") #H_1: p =/= 0.5
binom.test(9, 10, 0.50, alternative="greater") #H_1 : p > 0.5

# mid P-value
install.packages("exactci")
library(exactci)
binom.exact(9, 10, p=0.50, alternative="greater", midp=TRUE) 
install.packages("PropCIs")
library(PropCIs)
midPci(9, 10, 0.95) #CI based on test with mid P-value
