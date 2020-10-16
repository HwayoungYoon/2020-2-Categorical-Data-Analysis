### CH 02. 분할표

## Section 2.2. 2x2 분할표의 비율 비교
# p1-p2의 왈도 신뢰구간
prop.test(c(189,104), c(11034,11037), conf.level=0.95, correct=FALSE)
  # 심근경색증 환자수(위약, 아스피린), 총인원수(위약, 아스피린) 순서로 x와 n 입력 
  # correct=FALSE : 연속성 보정은 사용하지 않음.

# p1-p2의 스코어 신뢰구간
library(PropCIs)
diffscoreci(189,11034,104,11037, conf.level=0.95)
  # 위약그룹 심근경색증 환자수, 위약그룹 총 환자수, 아스피린그룹 심근경색증 환자수, 아스피린그룹 총인원수
  # 순서로 x1, n1, x2, n2 입력 

# 상대위험도 p1/p2 신뢰구간 
riskscoreci(189,11034,104,11037, conf.level=0.95)
  # x1, n1, x2, n2 입력 


## Section 2.3. 오즈비
# 오즈비의 왈도 신뢰구간
install.packages("epitools")
library(epitools)
oddsratio(c(189,10845,104,10933), method="wald", conf=0.95, correct=FALSE)
  # 위약그룹 심근경색증 환자수, 심근경색증 아닌 환자수, 아스피린그룹 심근경색증 환자수, 심근경색 아닌 환자수
  # x1, n1-x1, x2, n2-x2 순서로 
  # correct=FALSE : 연속성 보정은 사용하지 않음.

# 오즈비의 스코어 신뢰구간
library(PropCIs)
orscoreci(189,11034,104,11037, conf.level=0.95)
  # x1, n1, x2, n2 입력 


## Section 2.4. 카이제곱검정법 
Political <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Political.dat", header=TRUE)
  # web자료 불러들일 때 
str(Political)
# factor : 문자형 혹은 숫자형 변수를 범주형으로 변경
Political$Party <- factor(Political$party, levels=c("Dem", "Rep", "Ind"))
# xtabs : 분할표 생성
# ()로 묶은 이유 : GenderGap에 할당과 동시에 출력 위해
(GenderGap <- xtabs(~gender+Party, data=Political))
# GenderGap <- matrix(c(495,272,590,330,265,498), ncol=3, byrow=TRUE)
(chi.GG <- chisq.test(GenderGap)) # Peason 카이제곱 검정
chi.GG$stdres #표준화잔차

# 칸도수의 상대적 크기로 그린 모자이크 그림 
mosaicplot(GenderGap, main="Mosaic Plot", xlab="Gender", color=TRUE) 
#칸도수와 표준화잔차의 상대적인 크기로 그린 모자이크 그림 
install.packages("vcd")
library(vcd)
mosaic(GenderGap, gp=shading_Friendly, residuals=chi.GG$stdres, residuals_type="Std\nresiduals",
       labeling=labeling_residuals)

# 가능도비 카이제곱 검정
#### rJava install 참고 : https://r-pyomega.tistory.com/6
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_261')
install.packages("rJava")
library(rJava)
install.packages("Deducer")
library(Deducer)
likelihood.test(GenderGap)

## Section 2.5. 순서형 자료의 독립성 검정 
Malform <- matrix(c(17066, 14464, 788, 126, 37, 48, 38, 5, 1, 1), ncol=2)
install.packages("vcdExtra")
library(vcdExtra)
CMHtest(Malform, rscores = c(0, 0.5, 1.5, 4.0, 7.0)) #수준점수(rscores) 
  # Mantel의 스코어 검정 
  # 대립가설 : 양의 상관관계를 갖는다. (단측검정. rho>0)
(M <- sqrt(6.5699))
(p.val <- 1- pnorm(M))


## Section 2.6. 소표본 정확추론 
# Fisher의 정확검정 
tea <- matrix(c(3,1,1,3), ncol=2)
fisher.test(tea)  # 양측검정
fisher.test(tea, alternative="greater") # 대립가설 : theta >1 

# 중앙 p-값
library(epitools)
ormidp.test(3,1,1,3, or=1) #독립성 검정에 대한 중앙 p-값

# 오즈비에 대한 소표본 신뢰구간
library(epitools)
or.midp(c(3,1,1,3), conf.level=0.95)