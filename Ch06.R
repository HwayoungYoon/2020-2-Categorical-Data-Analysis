# 작업공간 설정
setwd("C:/R/Categorical Data Analysis")

### Ch6. 다범주 로짓모형

#########################################################################
## Section 6.1. 기준범주 로짓 모형 (명목형 반응변수)
Gators <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Alligators.dat", header=TRUE)
str(Gators)
Gators$y.fac <- factor(Gators$y, levels=c("F","I","O"))
 # vglm : fitting vector generalized models
 # 기준범주는 Others (기타)
install.packages("VGAM")
library(VGAM) # package for multivariate GLMs, such as multinomial models
fit <- vglm(y.fac ~ x, family=multinomial, data=Gators)
 # 로짓방정식의 각 모수들을 행렬식으로 출력
coef(fit, matrix=TRUE)
 # hde.NA = FALSE : Wald test 결과가 NA로 출력되지 않도록.
summary(fit, hde.NA = FALSE) 

 # 연체류를 기준범주로 사용한 기준범주 로짓 모형
fit2 <- vglm(y.fac~x, family=multinomial(refLevel="I"), data=Gators)
summary(fit2, hde.NA = FALSE)
confint(fit2) # Wald confidence interval
confintvglm(fit2, method="profile") # profile likelihood CI 

fit0 <- vglm(y.fac~1, family=multinomial, data=Gators)
summary(fit0) # deviance for working model is 98.3412
 # 우도비 검정 : H0: beta_1=beta_2=0, H1: 적어도 하나의 beta_j =!=0 (j=1,2)
lrtest(fit, fit0) # lrtest function of VGAM package

## 6.1.3. 반응확률의 추정
 # estimated response probabilities for outcome categories
fitted(fit2)
 # 추정된 반응확률과 길이
cbind(fitted(fit2),Gators$x)

# 예측확률 도표
beta.hat<-coef(fit, matrix=TRUE)
# Plot each pi_j
 # 도표를 그리기 위한 그림 틀
curve(exp(beta.hat[1,2] + beta.hat[2,2]*x)/
        (1 + exp(beta.hat[1,1] + beta.hat[2,1]*x) + exp(beta.hat[1,2] + beta.hat[2,2]*x)-2),
      ylab = expression(hat(pi)), xlab = "Length", type="n", 
      xlim = c(min(Gators$x), max(Gators$x)), col = "black", lty = "solid", lwd = 2, 
      n = 1000, panel.first = grid(col = "gray", lty = "dotted"))
 # 기타에 대한 추정 확률 곡선
curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[2,1]*x) + exp(beta.hat[1,2] + beta.hat[2,2]*x)),
      col = "red", lty = 1, lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(Gators$x), max(Gators$x)))
 # 연체류에 대한 추정 확률 곡선
curve(exp(beta.hat[1,2] + beta.hat[2,2]*x)/
              (1 + exp(beta.hat[1,1] + beta.hat[2,1]*x) + exp(beta.hat[1,2] + beta.hat[2,2]*x)),
      col = "blue", lty =2, lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(Gators$x), max(Gators$x)))
 # 어류에 대한 추정 확률 곡선
curve(expr = exp(beta.hat[1,1] + beta.hat[2,1]*x)/
              (1 + exp(beta.hat[1,1] + beta.hat[2,1]*x) + exp(beta.hat[1,2] + beta.hat[2,2]*x)),
      col = "green", lty = 3, lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(Gators$x), max(Gators$x)))
legend(x = 3.0, y = 0.7, legend=c("기타", "연체류","어류"), lty=c(1,2,3),
       col=c("red","blue","green"), bty="n", lwd = c(2,2,2), seg.len = 4, cex=1.2)

#######
## 6.1.5. 설명변수가 모두 범주형인 다항모형
Afterlife <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Afterlife.dat", header=TRUE)
Afterlife
fit <- vglm(cbind(yes,undecided,no)~gender+race, family=multinomial, data=Afterlife)
summary(fit)

  # 인종만 설명변수로 사용한 다항모형
fit.race <- vglm(cbind(yes,undecided,no)~race, family=multinomial, data=Afterlife)
summary(fit.race)
 # 우도비 검정 : 성별변수의 유의성 검정
 # H0: betaG_1=betaG_2=0, H1: 적어도 하나의 betaG_j =!=0 (j=1,2)
lrtest(fit, fit.race)

  # 성별만 설명변수로 사용한 다항모형
fit.gender <- vglm(cbind(yes,undecided,no)~gender, family=multinomial, data=Afterlife)
summary(fit.gender)
 # 우도비 검정 : 인종변수의 유의성 검정
 # H0: betaR_1=betaR_2=0, H1: 적어도 하나의 betaR_j =!=0 (j=1,2)
lrtest(fit, fit.gender) 

# 반응확률의 추정
# estimated response probabilities for outcome categories
data.frame(Afterlife$race, Afterlife$gender, round(fitted(fit),3))

##########################################################
## Section 6.2. 누적 로짓 모형 (순서형 반응변수)
  # grouped data (분할표자료)
Polviews <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Polviews.dat", header=TRUE)
library(VGAM)
fit <- vglm(cbind(y1,y2,y3,y4,y5)~party+gender, 
            family=cumulative(parallel=TRUE), data=Polviews)
summary(fit)
# 반응확률의 추정
attach(Polviews)
data.frame(gender, party, fitted(fit))

####### 모형확립
 # 비례 오즈 성질이 없는 누적 로짓 모형 : 
 # 각 범주별 정당과 성별에 대한 기울기가 다른 모형
fit2 <- vglm(cbind(y1,y2,y3,y4,y5)~party+gender, 
             family=cumulative, data=Polviews)
summary(fit2)

 # H0: 비례 오즈 누적 로짓 모형이 적합하다
 # Ha: 비례 오즈 성질이 없는 누적 로짓 모형이 적합하다.
lrtest(fit2, fit)
#######

### 모수 추론 : H0: beta1=0 (정당가입 여부는 정치성향에 영향이 없다)
fit.g <- vglm(cbind(y1,y2,y3,y4,y5)~gender, 
              family=cumulative(parallel=TRUE), data=Polviews)
summary(fit.g)
lrtest(fit, fit.g)

 # 보수적인 정치성향 방향으로의 누적확률 P(Y>j)를 분자로 정의한 
 # 공화당원(x1=1)과 민주당원(x1=0)의  누적 오즈비의
 # 신뢰구간은 (exp(3.218), exp(4.072))
confint(fit, method="profile")

############################################
## 6.2.5. 총 가구 수입과 행복도도
Happy <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Happy.dat", header=TRUE)

library(VGAM)
 # 비례 오즈 누적로짓 모형 적합 : income변수 순서형
fit <- vglm(cbind(y1,y2,y3)~income, family=cumulative(parallel=TRUE), data=Happy)
summary(fit)

fit0 <- vglm(cbind(y1,y2,y3)~1, family=cumulative, data=Happy)
 # income에 대한 유의성 검정
lrtest(fit, fit0)

 # 추세검정(2장 참고)
library(vcdExtra)
CMHtest(cbind(Happy$y1,Happy$y2,Happy$y3), rscores=c(1,2,3))

 # 기준 범주 로짓 모형 적합 : income변수 범주형
fit2 <- vglm(cbind(y1,y2,y3)~ factor(income), family=multinomial, data=Happy)
summary(fit2)

fit0 <- vglm(cbind(y1,y2,y3)~ 1, family=multinomial, data=Happy)
 # income에 대한 유의성 검정
lrtest(fit2, fit0)

##########################################################
## Section 6.4. 이웃범주 로짓 (순서형 반응변수)
library(VGAM)
fit <- vglm(cbind(y1,y2,y3,y4,y5)~party+gender, 
            family=acat(parallel=TRUE, reverse=TRUE), data=Polviews)
summary(fit)