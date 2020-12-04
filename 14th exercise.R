# 작업공간 설정
setwd("C:/R/Categorical Data Analysis")

# 자료 입력
Students <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Students.dat", header=TRUE)
variable.names(Students)


##########5.4.a.###########################################################
# 여러 로지스틱 회귀모형의 적합도 : residual deviance & AIC
m1 <- glm(abor~1, family=binomial, data=Students) #M1
summary(m1)
m2.1 <- glm(abor~factor(ideol), family=binomial, data=Students) #M2.1
summary(m2.1)
m2.2 <- glm(abor~ideol, family=binomial, data=Students) #M2.2
summary(m2.2)
1-pchisq(62.719-45.464, 59-58) #검정
m3.1 <- glm(abor~factor(relig), family=binomial, data=Students) #M3.1
summary(m3.1)
m3.2 <- glm(abor~relig, family=binomial, data=Students) #M3.2
summary(m3.2)
1-pchisq(62.719-48.262, 59-58) #검정
m4 <- glm(abor~news, family=binomial, data=Students) #M4
summary(m4)
1-pchisq(62.719-55.389, 59-58) #검정
m5 <- glm(abor~hsgpa, family=binomial, data=Students) #M5
summary(m5)
1-pchisq(62.719-62.645, 59-58) #검정
m6 <- glm(abor~gender, family=binomial, data=Students) #M6
summary(m6)
1-pchisq(62.719-61.554, 59-58) #검정

m7 <- glm(abor~ideol + relig, family=binomial, data=Students) #M7
summary(m7)
1-pchisq(45.464-42.522, 58-57) #검정
1-pchisq(48.262-45.464, 58-57) #검정
m8 <- glm(abor~ideol + news, family=binomial, data=Students) #M8
summary(m8)
1-pchisq(45.464-32.014, 58-57) #검정
1-pchisq(55.389-32.014, 58-57) #검정
m9 <- glm(abor~relig + news, family=binomial, data=Students) #M9
summary(m9)
1-pchisq(48.262-41.697, 58-57) #검정
1-pchisq(55.389-41.697, 58-57) #검정
m10 <- glm(abor~ideol + relig + news, family=binomial, data=Students) #M10
summary(m10)
1-pchisq(45.464-29.791, 57-56) #검정
1-pchisq(48.262-29.791, 57-56) #검정
1-pchisq(55.389-29.791, 57-56) #검정

m11 <- glm(abor~ideol + relig + news + hsgpa, family=binomial, data=Students) #M11
summary(m11)
1-pchisq(29.791-25.767, 56-55) #검정
m12 <- glm(abor~ideol + relig + news + gender, family=binomial, data=Students) #M12
summary(m12)
1-pchisq(29.791-29.740, 56-55) #검정
m13 <- glm(abor~ideol + relig + news + hsgpa + gender, family=binomial, data=Students) #M13
summary(m13)
1-pchisq(25.767-25.188, 55-54) #검정
1-pchisq(29.740-25.188, 55-54) #검정

m14 <- glm(abor~ideol + relig + news + hsgpa + ideol*relig, family=binomial, data=Students) #M14
summary(m14)
1-pchisq(25.767-23.093, 55-54) #검정
m15 <- glm(abor~ideol + relig + news + hsgpa + ideol*news, family=binomial, data=Students) #M15
summary(m15)
1-pchisq(25.767-25.608, 55-54) #검정
m16 <- glm(abor~ideol + relig + news + hsgpa + ideol*hsgpa, family=binomial, data=Students) #M16
summary(m16)
1-pchisq(25.767-24.305, 55-54) #검정
m17 <- glm(abor~ideol + relig + news + hsgpa + relig*news, family=binomial, data=Students) #M17
summary(m17)
1-pchisq(25.767-23.195, 55-54) #검정
m18 <- glm(abor~ideol + relig + news + hsgpa + relig*hsgpa, family=binomial, data=Students) #M18
summary(m18)
1-pchisq(25.767-25.725, 55-54) #검정
m19 <- glm(abor~ideol + relig + news + hsgpa + news*hsgpa, family=binomial, data=Students) #M19
summary(m19)
1-pchisq(25.767-25.631, 55-54) #검정


# 잔차 진단
fit <- glm(abor~ideol + relig + news + hsgpa, family=binomial, data=Students)
res <- cbind(rstandard(fit, type="pearson"), residuals(fit, type="pearson"),
             rstandard(fit, type="deviance"), residuals(fit, type="deviance"))
colnames(res) <- c("stand.Pearson.res", "Pearson.res",
                   "stand.deviance.res", "deviance.res")
res


##########5.4.b.###########################################################
# stepAIC
fit <- glm(abor~gender+age+hsgpa+cogpa+dhome+dres+tv+sport+news+aids+
             veg+factor(affil)+ideol+relig+affirm+factor(life), 
           family=binomial, data=Students)
library(MASS)
stepAIC(fit)

# bestglm
attach(Students)
Students2 <- data.frame(gender,age,hsgpa,cogpa,dhome,dres,tv,sport,news,
                        aids,veg,affil,relig,affirm,life,abor) # response variable in last column
library(leaps)
library(bestglm)
bestglm(Students2, family=binomial, IC="AIC") # color and width as integer(numerical)


##########5.4.c.###########################################################
fit <- glm(veg~gender+age+hsgpa+cogpa+dhome+dres+tv+sport+news+aids+
             factor(affil)+ideol+relig+abor+affirm+factor(life), 
           family=binomial, data=Students)
summary(fit)
1-pchisq(50.725-17.989, 59-41)