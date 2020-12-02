# 작업공간 설정
setwd("C:/R/Categorical Data Analysis")

# CH 04. 로지스틱 회귀모형

#########################################################################
## Section 4.1. 암참게가 부수체를 갖는지 여부 : 설명변수 - 넓이(width)
Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=TRUE)
 # 변수 y가 부수체를 갖는지의 여부에 대한 변수 (sat>0인 경우 y=1)
plot(y~width, data=Crabs)
plot(jitter(y,0.08)~width, data=Crabs, ylab="확률", xlab="넓이")

library(gam)
  #일반화가법모형(gam)에서 평활곡선 사용 : s(width) 평활모수는 주어진 값 그대로 사용
gam.fit <- gam(y ~ s(width), family=binomial, data=Crabs)
  #산점도에 적합한 일반화 가법모형 커브 덧붙이기
curve(predict(gam.fit, data.frame(width=x), type="response"), add=TRUE)
  #로지스틱회귀모형 적합
fit <- glm(y ~ width, family=binomial, data=Crabs)
  #산점도에 적합한 로지스틱 회귀모형 커브 덧붙이기
curve(predict(fit, data.frame(width=x), type="response"), add=TRUE, col=2, lty=2, lwd=2)

summary(fit)

  #넓이가 21인 암참게가 부수체를 갖을 확률에 대한 추정값
predict(fit, data.frame(width=21.0), type="response")
  #넓이의 평균값에서 암참게가 부수체를 갖을 확률에 대한 추정값
predict(fit, data.frame(width=mean(Crabs$width)), type="response")

#########################################################################
## Section 4.2. 로지스틱 호귀모형에 대한 추론
#효과에 대한 신뢰구간과 유의성검정
fit <- glm(y ~ width, family=binomial, data=Crabs)
summary(fit)

confint(fit) #프로파일 가능도 신뢰구간
confint.default(fit) #왈드 신뢰구간

library(car)
Anova(fit) #width변수에 대한 우도비검정

#확률에 대한 적합값과 신뢰구간
fit <- glm(y ~ width, family=binomial, data=Crabs)
pred.prob <- fitted(fit) #P(Y=1)에 대한 적합 추정값
lp <- predict(fit, se.fit=TRUE) #선형 추정값
LB <- lp$fit - qnorm(0.975)*lp$se.fit #선형추정값에 대한 95%
UB <- lp$fit + qnorm(0.975)*lp$se.fit #신뢰구간
LB.p <- exp(LB)/(1+exp(LB)) # P(Y=1)에 대한 95%
UB.p <- exp(UB)/(1+exp(UB)) # 신뢰범위
cbind(Crabs$width, pred.prob, LB.p, UB.p)

# 로지스틱 회귀모형 적합 곡선과 95% 신뢰범위 그림
plot(jitter(y,0.08)~width, xlim=c(18,34), pch=16, ylab="Prob(satellite)", 
     data=Crabs)
data.plot <- data.frame(width=c(18:34))
lp <- predict(fit, newdat=data.plot, se.fit=TRUE)
pred.prob <- exp(lp$fit)/(1+exp(lp$fit))
#pred.p <- predict(fit, newdata=data.plot, type="response")
LB <- lp$fit - qnorm(0.975)*lp$se.fit #선형추정값에 대한 95%
UB <- lp$fit + qnorm(0.975)*lp$se.fit #신뢰구간
LB.p <- exp(LB)/(1+exp(LB)) # P(Y=1)에 대한 95%
UB.p <- exp(UB)/(1+exp(UB)) # 신뢰범위
lines((18:34), pred.prob) #로지스틱 회귀모형 적합식
lines((18:34), LB.p, col="red", lwd=2)
lines((18:34), UB.p, col="blue", lwd=2)

#########################################################################
## Section 4.3. 범주형 예측변수를 갖는 로지스틱 회귀모형
# 마리화나 사용에 대한 조사
Marijuana <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Marijuana.dat", header=TRUE)
# 데이터의 structure 확인
str(Marijuana)
  #분할표 형태의 자료
  #마리화나 사용여부에 대한 성별과 인종의 로지스틱 회귀모형 : 성별 & 인종은 범주형
fit <- glm(yes/(yes+no) ~ gender + race, weights = yes+no , 
           family=binomial, data=Marijuana)
summary(fit)
library(car)
Anova(fit) #각각의 설명변수들의 유의성 검정 : 우도비 검정법

#########################################################################
## Section 4.4. 다중 로지스틱 회귀모형
# 1. 너비(양적변수)와 색깔(범주형변수)을 예측변수로
fit <- glm(y ~ width + factor(color), family=binomial, data=Crabs)
summary(fit)

# 유의한 변수 확인 : 가능도비 검정
library(car)
Anova(fit)

fit.s <- glm(y ~ width, family=binomial, data=Crabs)
summary(fit)

# 2. 너비(양적변수)와 색깔(양적변수)을 예측변수로
fit2 <- glm(y ~ width + color, family=binomial, data=Crabs)
summary(fit2)

# H0: Model 1 is correct vs. H1: Model 2 is correct
anova(fit2, fit, test="LRT")

# 3. 너비(양적변수)와 색깔 어두운색(이항변수)을 예측변수로
# 색깔=4(어두운색) 일때 c4=1, 색깔=다른색 일때 c4=0
Crabs$c4 <- ifelse(Crabs$color == 4, 1, 0)  
fit3 <- glm(y ~ width+c4, family=binomial, data=Crabs)
summary(fit3)

anova(fit3, fit, test="LRT")

# 교호작용을 포함한 경우
summary(glm(y~width+c4+width:c4, family=binomial, data=Crabs))
  # 이 자료에는 교호작용이 유의하지 않으므로 사용하지 않는다.

#########################################################################
## Section 4.5. 로지스틱 회귀모형의 효과에 대한 요약
fit3 <- glm(y ~ width+c4, family=binomial, data=Crabs)
# c4=1과 평균넓이에서의 예측값
predict(fit3, data.frame(c4=1, width=mean(Crabs$width)), type="response")
# c4=0과 평균넓이에서의 예측값
predict(fit3, data.frame(c4=0, width=mean(Crabs$width)), type="response")
# c4=c4의 추정확률과 넓이의 quantile 에서의 예측값
predict(fit3, data.frame(c4=mean(Crabs$c4), width=quantile(Crabs$width)), 
        type="response")
# c4=1과 넓이의 quantile 에서의 예측값
predict(fit3, data.frame(c4=1, width=quantile(Crabs$width)), type="response")
# c4=0과 넓이의 quantile 에서의 예측값
predict(fit3, data.frame(c4=0, width=quantile(Crabs$width)), type="response")

# 주변효과
fit3 <- glm(y ~ width+c4, family=binomial, data=Crabs)
library(mfx)
logitmfx(fit3, atmean=FALSE, data=Crabs) 
  # atmean=TRUE : 평균값에서의 부분효과 계산
  # atmean=FALSE : 평균주변효과 계산

#########################################################################
## Section 4.6. 예측력 요약

# 너비(양적변수)와 색깔(범주형변수)을 예측변수로
fit <- glm(y ~ width + factor(color), family=binomial, data=Crabs)
# 너비(양적변수)와 색깔(양적변수)을 예측변수로
fit2 <- glm(y ~ width + color, family=binomial, data=Crabs)
# 너비(양적변수)와 색깔 어두운색(이항변수)을 예측변수로
fit3 <- glm(y ~ width+c4, family=binomial, data=Crabs)

# 1. 분류표
prop <- sum(Crabs$y)/nrow(Crabs) # Y=1에 대한 표본비율(pi0)
prop
# hat{pi}_i > pi_0 = 0.6416185 이면 Y=1로 예측
predicted <- as.numeric(fitted(fit) > prop) 
xtabs(~Crabs$y + predicted) # 관측값과 예측값의 분류표

# hat{pi}_i > pi_0 = 0.5 이면 Y=1로 예측
predicted2 <- as.numeric(fitted(fit) > 0.5) 
xtabs(~Crabs$y + predicted2) # 관측값과 예측값의 분류표

# 2. ROC 곡선
library(pROC)
rocplot <- roc(y ~ fitted(fit), data=Crabs) # 색깔(범주형변수)
rocplot2 <- roc(y ~ fitted(fit2), data=Crabs) # 색깔(양적변수)
rocplot3 <- roc(y ~ fitted(fit3), data=Crabs) # 색깔 어두운색(이항변수)

plot.roc(rocplot, legacy.axes=TRUE) # legacy.axes=FALSE인 경우 x축에 특이도 사용
plot.roc(rocplot2, legacy.axex=TRUE, lty=2, col="red", lwd=2, add=TRUE)
plot.roc(rocplot3, legacy.axex=TRUE, lty=3, col="blue", lwd=2, add=TRUE)
legend(0.3, 0.7, col=c("black", "red","blue"), lty=c(1,2,3), lwd=2,
       legend=c("Color(factor)","Color(quantatative)", "Color(dark indicator)"))

# 일치성지수
auc(rocplot)
auc(rocplot2)
auc(rocplot3)