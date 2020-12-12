# 작업공간 설정
setwd("C:/R/Categorical Data Analysis")

# Chapter 7. 분할표 자료에 대한 로그 선형모형

#########################################################################
## 이차원 분할표에 대한 로그 선형모형
HappyHeaven <- read.table("http://www.stat.ufl.edu/~aa/cat/data/HappyHeaven.dat", header=TRUE)
HappyHeaven

# Loglinear Model of Independence
## 포아송 분포의 정준연결함수(canonical link function)는 log이므로 "(link=log)" 생략가능
fit <- glm(count ~ happy + heaven, family=poisson, data=HappyHeaven)
summary(fit)

# Saturated Loglinear Model
fit2 <- glm(count ~ happy * heaven, family=poisson, data=HappyHeaven)
summary(fit2)

# 교호작용의 유의성 검정
library(car)
Anova(fit2)

library(epiDisplay)
lrtest(fit2, fit)

######## 삼차원 분할표에 대한 로그 선형모형
Drugs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Substance.dat", header=TRUE)
Drugs

A <- Drugs$alcohol; C <- Drugs$cigarettes; M <- Drugs$marijuana

# 포화모형(Saturated Model) : (ACM)
fit.full <- glm(count ~ A*C*M, family=poisson, data=Drugs)
summary(fit.full)

# 동질연관성모형(Homogeneous Association Model) : (AC, AM, CM)
fit.asso <- glm(count ~ A + C + M + A:C + A:M + C:M, family=poisson, data=Drugs)
summary(fit.asso)

# 조건연관성모형(Conditional Association Model) 
 # 1. (AM, CM)
fit.AC.M <- glm(count ~ A + C + M + A:M + C:M, family=poisson, data=Drugs)
summary(fit.AC.M)
 # 2. (AC, AM)
fit.CM.A <- glm(count ~ A + C + M + A:C + A:M, family=poisson, data=Drugs)
summary(fit.CM.A)
 # 3. (AC, CM)
fit.AM.C <- glm(count ~ A + C + M + A:C + C:M, family=poisson, data=Drugs)
summary(fit.AM.C)

# 결합연관성모형(Joint Association Model)
 # 1. (AC, M)
fit.AC.M2 <- glm(count ~ A + C + M + A:C, family=poisson, data=Drugs)
summary(fit.AC.M2)
 # 2. (CM, A)
fit.CM.A2 <- glm(count ~ A + C + M + C:M, family=poisson, data=Drugs)
summary(fit.CM.A2)
 # 3. (AM, C)
fit.AM.C2 <- glm(count ~ A + C + M + A:M, family=poisson, data=Drugs)
summary(fit.AM.C2)

# 상호독립모형(Mutual Independence Model) : (A, C, M)
fit.ind <- glm(count ~ A + C + M , family=poisson, data=Drugs)
summary(fit.ind)
deviance(fit.ind)


##### 모형 적합성 검정
library(epiDisplay)
## 동질연관성모형
# 1)H_0: (AC, AM, CM) vs H_a: (ACM)
lrtest(fit.full, fit.asso)

## 조건연관성모형
# 2) H_0: (AM, CM) vs H_a: (AC, AM, CM)
lrtest(fit.asso, fit.AC.M)
# 3) H_0: (AC, AM) vs H_a: (AC, AM, CM)
lrtest(fit.asso, fit.CM.A)
# 4) H_0: (AC,  CM) vs H_a: (AC, AM, CM)
lrtest(fit.asso, fit.AM.C)

## 결합연관성모형
# 5) H_0: (AC, M) vs H_a: (AC, AM, CM)
lrtest(fit.asso, fit.AC.M2)
# 6) H_0: (CM, A) vs H_a: (AC, AM, CM)
lrtest(fit.asso, fit.CM.A2)
# 7) H_0: (AM, C) vs H_a: (AC, AM, CM)
lrtest(fit.asso, fit.AM.C2)

## 상호독립모형
# 5) H_0: (A, C, M) vs H_a: (AC, AM, CM)
lrtest(fit.asso, fit.ind)

### Table 7.2
data.frame(A, C, M, Drugs$count, Ind=fitted(fit.ind), AC.M=fitted(fit.AC.M2), 
           AM.CM=fitted(fit.AC.M), AM.CM.AC=fitted(fit.asso), Full=fitted(fit.full))

#########################################
## 동질연관성모형
##### 칸 표준화 잔차
fit <- glm(count ~ A + C + M + A:C + A:M + C:M, family=poisson, data=Drugs)
fit2 <- glm(count ~ A + C + M + A:M + C:M, family=poisson, data=Drugs)
deviance(fit)
deviance(fit2)
res <- rstandard(fit, type="pearson")
res2 <- rstandard(fit2, type="pearson")
data.frame(A, C, M, Drugs$count, fitted(fit), res, fitted(fit2), res2)

### 조건부연관성에 대한 유의성검정
library(car)
Anova(fit)

### 조건부 오즈비의 신뢰구간 : 프로파일 신뢰구간
exp(confint(fit))
