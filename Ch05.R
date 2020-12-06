# 작업공간 설정
setwd("C:/R/Categorical Data Analysis")

### CH 05. 로지스틱 회귀모형의 구축과 활용

#########################################################################
## Section 5.1. 모형선택의 전략
Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=TRUE)

 # explanatory variables : weight, width, color(categorical), spine(categorical)
fit <- glm(y ~ weight+width+factor(color)+factor(spine), family=binomial, data=Crabs)
 # full model
summary(fit)

# p-value for test that all beta's =0
# 적어도 하나의 예측변수가 유의한 효과를 갖고 있다는 것을 보여 주는 매우 강한 증거
1- pchisq(225.76-185.20, 172-165)

# likelihood-ratio tests for individual explanatory variables
# 개별 설명변수들에 대한 가능도 검정 : 다중공선성 존재
# explanatory variables : width, color(categorical), spine(categorical)
library(car)
Anova(fit) 

# 여러 로지스틱 회귀모형의 적합도 : residual deviance & AIC
m1 <- glm(y~1, family=binomial, data=Crabs) #M1
summary(m1)
m2 <- glm(y~factor(color), family=binomial, data=Crabs) #M2
summary(m2)
m3 <- glm(y~factor(spine), family=binomial, data=Crabs) #M3
summary(m3)
m4 <- glm(y~width, family=binomial, data=Crabs) #M4
summary(m4)

m5 <-  glm(y~width + factor(color), family=binomial, data=Crabs) #M5
summary(m5)

m6 <-  glm(y~width + factor(color) + factor(spine), family=binomial, data=Crabs) #M6
summary(m6)
m7 <-  glm(y~width * factor(color), family=binomial, data=Crabs) #M7
summary(m7)

# AIC
fit <- glm(y~width + factor(color), family=binomial, data=Crabs)
-2*logLik(fit) #-2(log-likelihood) 
AIC(fit) # -2(log-likelihood) + 2(number of parameters)

## Stepwise backward selection using AIC
fit <- glm(y ~ weight+width+factor(color)+factor(spine), family=binomial, data=Crabs)
library(MASS)
stepAIC(fit)

attach(Crabs)
Crabs2 <- data.frame(weight, width, color, spine, y) # response variable in last column
install.packages("leaps")
install.packages("bestglm")
library(leaps)
library(bestglm)
bestglm(Crabs2, family=binomial, IC="AIC") # color and width as integer(numerical)

fit2 <- glm(y~width*color, family=binomial, data=Crabs)
summary(fit2)
fit.fin <- glm(y~width+color, family=binomial, data=Crabs)
summary(fit.fin)

#########################################################################
## Section 5.2. Model Checking
  # 마리화나 사용조사 적합도 검정
  # 분할표 형태의 자료
Marijuana <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Marijuana.dat", header=TRUE)
Marijuana

  # 마리화나 사용여부에 대한 성별과 인종의 로지스틱 회귀모형 : 성별 & 인종은 범주형
fit <- glm(yes/(yes+no) ~ gender + race, weights = yes+no , 
           family=binomial, data=Marijuana)
summary(fit)
fit$deviance     # 잔차이탈도 모형적합도 검정통계량
fit$df.residual  # 잔차 자유도
  # 이탈도 모형적합도 검정의 p-값
1-pchisq(fit$deviance, fit$df.residual)

fitted(fit) # 성별과 인종 조합별 마리화나 사용 확률 추정값

n <- Marijuana$yes+Marijuana$no
fit.yes <- round(n*fitted(fit),2)
fit.no  <- round(n*(1-fitted(fit)),2)
attach(Marijuana)
data.frame(race, gender, yes, fit.yes, no, fit.no)
detach(Marijuana)

 # 이탈도 잔차
summary(fit)
 # 표준화잔차, 피어슨잔차, 표준화 이탈도 잔차, 이탈도 잔차
res <- cbind(rstandard(fit, type="pearson"), residuals(fit, type="pearson"),
             rstandard(fit, type="deviance"), residuals(fit, type="deviance"))
colnames(res) <- c("stand.Pearson.res", "Pearson.res",
                    "stand.deviance.res", "deviance.res")
res

 #영향점진단: influence.measures() 사용
influence.measures(fit)

  #### 심장병과 혈압
HeartBP <-read.table("http://www.stat.ufl.edu/~aa/cat/data/HeartBP.dat", header=TRUE)
fit.logit <- glm(y/n ~ bp, weights = n ,family=binomial, data=HeartBP)
summary(fit.logit)

Heart <- cbind(HeartBP$bp, HeartBP$n, HeartBP$y, round(HeartBP$n*fitted(fit.logit),2),
               round(rstandard(fit.logit, type="pearson"),2), dfbetas(fit.logit)[,2])
colnames(Heart) <- c("BP", "n", "y", "hat(y)","stand.resid", "Dfbeta")
Heart

pred.prob <- function(x){
   exp(-6.0820+0.02433*x)/(1+exp(-6.0820+0.02433*x))
}
plot(HeartBP$bp, HeartBP$y/HeartBP$n, xlim=c(110,200), ylim=c(0,0.2), 
     xlab="Blood Pressure", ylab="proportion",
     pch="x", col="red", cex=1.5)
curve(pred.prob, 110, 200, add=TRUE)

influence.measures(fit.logit)

#########################################################################
## Section 5.3. 로지스틱 회귀분석의 무한대 추정값
  # toy example
x <- c(10,20,30,40,60,70,80,90)
y <- c(0,0,0,0,1,1,1,1)
fit <- glm(y~x, family=binomial)
summary(fit) # Wald test
logLik(fit) # maximized log-likelihood = 0, maximized likelihood = 1
library(car)
Anova(fit) # likelihood ratio test of beta=0
  # profile likelihood CI 
install.packages("profileModel")
library(profileModel)
confintModel(fit, objective="ordinaryDeviance", 
             method="zoom", endpoint.tolerance = 1e-08)

  # 5.3.4. 자궁내막암 병기의 위험인자
Endo <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Endometrial.dat", 
                   header=TRUE)
  # HG = historical grade : response variable
xtabs(~ NV + HG, data=Endo) # contingency table
  # true estimated NV parameter = infinity
fit <- glm(HG ~ NV + PI + EH, family=binomial, data=Endo)
summary(fit)
  # 준완전 분리 자료이므로 로그우도함수값이 0은 아니다.
logLik(fit)
  # 우도비검정
library(car)
Anova(fit)
  #프로파일 우도 신뢰구간
library(profileModel) 
confintModel(fit, objective="ordinaryDeviance", 
             method="zoom", endpoint.tolerance = 1e-08)
  #분리(separation)에 의한 무한대 추정값 확인
install.packages("brglm2")
library(brglm2) 
glm(HG ~ NV + PI + EH, family=binomial, data=Endo, method="detectSeparation")