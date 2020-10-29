# 작업공간 설정
setwd("C:/R/Categorical Data Analysis")

# CH 03. 일반화선형모형

## Section 3.2. 이항자료에 대한 일반화선형모형
  # 코골이와 심장 자료: 분할표 형태의 자료
Heart <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Heart.dat", header=TRUE)

# 코골이의 정도가 심장병 발병에 어떤 영향을 미치는지 로지스틱 회귀모형 적합:
 # 코골이 정도(snoring):설명변수 / 심장병유무(yes, no):반응변수
library(dplyr)
  #각 범주에 수치를 적용한 설명변수 x를 Heart 데이터에 생성
Heart$x <- recode(Heart$snoring, never=0, occasional=2, 
                  nearly_every_night=4, every_night=5) 
  #각 코골이 정도에 대한 표본수 변수 n 생성
Heart$n <- Heart$yes + Heart$no
#분할표 형태의 자료에 로지스틱 회귀모형 적용시 "weights=n" 옵션 반드시 필요
#"weights" 옵션으로 표본비율이 yes/n 이라는 것을 지정
fit <- glm(yes/n ~ x, family=binomial(link=logit), weights=n, data=Heart)
summary(fit)
fitted(fit)

#선형회귀모형 적합
  #선형회귀모형에서 family=quasi(link="identity") : f(x)=x
  #  variance="mu(1-mu)" : 성공의 확률 yes/n = mu 이므로 이항분포의 분산 mu(1-mu)로 지정
fit2 <- glm(yes/n ~ x, family=quasi(link="identity", variance="mu(1-mu)"),
            weights=n, data=Heart) 
  # 검정통계량의 표준오차(std.Error) 계산시 dispersion=1로 지정하지 않으면 
  # 추정된 dispersion parameter값 사용. 
  # 그러나 위의 코드에서 분산을 이미 지정하였으므로 지정된 분산 사용시 dispersion=1으로
summary(fit2, dispersion=1) 

  # 일반자료 : 개인별 자료
Heart2 <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Heart2.dat", header=TRUE)
  # 개인별 자료에 로지스틱 모형을 적합
fitt <- glm(y ~ x, family=binomial(link=logit), data=Heart2)
summary(fitt)

#########################################################################
## Section 3.3. 도수와 비율에 대한 일반화선형모형
   # 암참게와 부수체 자료
Crabs <-  read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=TRUE)
   # 로그선형모형 적합
fit <- glm(sat ~ width, family=poisson(link="log"), data=Crabs)
summary(fit)
   # 항등연결함수를 이용한 포아송 회귀모형
fit2 <- glm(sat ~ width, family=quasipoisson(link="identity"), start=c(0.5,0.5), data=Crabs)
summary(fit2)
  #일반화가법모형 적합
  # s( )는 일반화가법모형의 평활함수 
install.packages("gam")
library(gam)
gam.fit <- gam(sat ~ s(width), family=poisson, data=Crabs)
summary(gam.fit)

   # 등딱지 너비(width)(x축)에 대한 부수체 수 (satellite)(y축) 그림
plot(sat ~ width, xlab="width", ylab="Number of satellites", data=Crabs)
   # 로그선형모형 적합 곡선
curve(predict(fit, data.frame(width=x), type="resp"), add=TRUE, col="red", lty=2, lwd=2) 
   # 일반화가법 모형 적합곡선
curve(predict(gam.fit, data.frame(width=x), type="resp"), add=TRUE, col="blue", lty=3, lwd=3) 
# 항등연결함수를 이용한 포아송 회귀곡선
curve(predict(fit2, data.frame(width=x), type="resp"), add=TRUE, col="violet", lty=4, lwd=3) 

#########################################################################
## Section 3.4. 통계적 추론과 모형진단
  # 정치성향과 진화에 대한 믿음 자료 : ideology(정치성향: 설명변수 x)
Evo <-  read.table("http://www.stat.ufl.edu/~aa/cat/data/Evolution.dat", header=TRUE)
  #이항분포 자료의 표본크기 변수 생성 
Evo$n <- Evo$true + Evo$false 
  # 로지스틱 회귀모형 적합 : 진화에 대한 믿음 여부가 반응변수 (Ture/False)
fit <- glm(true/n ~ ideology, family=binomial, weights=n, data=Evo)
summary(fit)

confint(fit) #프로파일 가능도 신뢰구간
confint.default(fit) #왈드 신뢰구간

  # 로지스틱 회귀모형에서 정치성향 변수(x변수)의 유의성 가능도비 검정
  # 설명변수가 1개인 로지스틱 모형에서는 적합도 검정으로 사용
library(car)
Anova(fit)
  # 스코어 검정통계량 제곱값 = 스코어 카이제곱 통계량 (df=1)
  # beta=0인 모형 적합
library(statmod)
fit1 <- glm(true/n ~ 1, family=binomial, weights=n, data=Evo) #null model 
glm.scoretest(fit1, Evo$ideology)^2 


attach(Evo)
cbind(ideology, true, false, n, 
      ml=true/n, # 포화모형의 추정값(최대우도추정량) 
      fitted=fitted(fit), # 모형적합값 
      pear.res = residuals(fit, type="pearson"), # 피어슨 잔차 
      dev.res=residuals(fit), # 이탈도 잔차
      std.res=rstandard(fit, type="pearson"))
      # rstandard(fit, type="pearson") : 피어슨 표준화 잔차