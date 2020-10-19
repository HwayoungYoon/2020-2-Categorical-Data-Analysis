# 2.3.
# p1-p2의 스코어 신뢰구간
library(PropCIs)
diffscoreci(624000,1000000,13000,1000000, conf.level=0.95)

# 상대위험도 p1/p2 신뢰구간 
riskscoreci(624000,1000000,13000,1000000, conf.level=0.95)


# 2.17.
Gap <- matrix(c(871,821,336,347,42,83), ncol=3, byrow=TRUE)
(chi.GG <- chisq.test(Gap)) # Peason 카이제곱 검정
chi.GG$stdres #표준화잔차

# 가능도비 카이제곱 검정
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_261')
library(rJava)
library(Deducer)

Gap.1 <- matrix(c(871,821,347,42), ncol=2, byrow=TRUE)
likelihood.test(Gap.1)

Gap.2 <- matrix(c(1692,336,389,83), ncol=2, byrow=TRUE)
likelihood.test(Gap.2)


# 2.23.b.
library(epitools)
ormidp.test(21,2,15,3, or=1)
or.midp(c(21,2,15,3), conf.level=0.95)