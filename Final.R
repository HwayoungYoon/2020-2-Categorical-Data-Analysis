# �۾����� ����
setwd("C:/R/Categorical Data Analysis/data")

#######
#  1  #
#######
LowBirth <- read.csv(file='LowBirth.csv', header=TRUE)

fit1 <- glm(y ~ x1+x2+factor(x3)+x4, family=binomial, data=LowBirth)
summary(fit1)
73.304-58.235
1-pchisq(73.304-58.235, 59-54)
library(car)
Anova(fit1)

fit1.1 <- glm(y ~ x1+x2+x4, family=binomial, data=LowBirth)
summary(fit1.1)
58.364-58.235
1-pchisq(58.364-58.235, 56-54)
Anova(fit1.1)

fit1.2 <- glm(y ~ x2+x4, family=binomial, data=LowBirth)
summary(fit1.2)
58.503-58.364
1-pchisq(58.503-58.364, 57-56)
Anova(fit1.2)

fit1.3 <- glm(y ~ x2, family=binomial, data=LowBirth)
summary(fit1.3)
58.857-58.503
1-pchisq(58.857-58.503, 58-57)
Anova(fit1.3)

library(MASS)
stepAIC(fit1)

attach(LowBirth)
LowBirth.df <- data.frame(x1, x2, x3, x4, y)
library(leaps)
library(bestglm)
bestglm(LowBirth.df, family=binomial, IC="AIC")
bestglm(LowBirth.df, family=binomial, IC="BIC")

exp(-0.05175241)

#######
#  2  #
#######
Carcin <- read.csv(file='Carcinoma.csv', header=TRUE)
# �׷�ȭ�� �ڷ��̹Ƿ� ���� ������ �� weights=count �ɼ� ���
# chapter 6.1.5, 5.2 ����
library(VGAM)
fit2 <- vglm(y~x1+x2+x3, family=multinomial, weights=count, data=Carcin)
coef(fit2, matrix=TRUE)
summary(fit2)

# �쵵�� ���� : x1�� ���Ǽ� ����
fit2.x1 <- vglm(y~x2+x3, family=multinomial, weights=count, data=Carcin)
summary(fit2.x1)
lrtest(fit2, fit2.x1)

# �쵵�� ���� : x2�� ���Ǽ� ����
fit2.x2 <- vglm(y~x1+x3, family=multinomial, weights=count, data=Carcin)
summary(fit2.x2)
lrtest(fit2, fit2.x2)

# �쵵�� ���� : x3�� ���Ǽ� ����
fit2.x3 <- vglm(y~x1+x2, family=multinomial, weights=count, data=Carcin)
summary(fit2.x3)
lrtest(fit2, fit2.x3)

1.2302+0.9338
-0.2266-0.7437
1.8150-2.6465

df <- data.frame(Carcin$x1, Carcin$x3, round(fitted(fit2.x2),3))
unique(df)

#######
#  3  #
#######
Disorder <- read.csv(file='Disorder.csv', header=TRUE)
library(VGAM)
fit3 <- vglm(Y~X1+X2, family=cumulative(parallel=TRUE), data=Disorder)
summary(fit3)

# ���ɵ��� ���� : x1�� ���Ǽ� ����
fit3.x1 <- vglm(Y~X2, family=cumulative(parallel=TRUE), data=Disorder)
lrtest(fit3, fit3.x1)
summary(fit3.x1)

# ���ɵ��� ���� : x2�� ���Ǽ� ����
fit3.x2 <- vglm(Y~X1, family=cumulative(parallel=TRUE), data=Disorder)
lrtest(fit3, fit3.x2)
summary(fit3.x2)

# �������� �ŷڱ���
confint(fit3)

# ����Ȯ���� ����
attach(Disorder)
data.frame(X1, X2, fitted(fit3))


#######
#  4  #
#######
hospital <- c("A","A","A","A","B","B","B","B")
status <- c("mild","mild","severe","severe","mild","mild","severe","severe")
live <- c("death","live","death","live","death","live","death","live")
count <- c(6,594,57,1443,8,592,8,192)
Healing <- data.frame(hospital, status, live, count)
H <- Healing$hospital; S <- Healing$status; L <- Healing$live

##### ����
# ��ȭ����(Saturated Model) : (HSL)
fit.full <- glm(count ~ H*S*L, family=poisson, data=Healing)
summary(fit.full)

# ��������������(Homogeneous Association Model) : (HS, HL, SL)
fit.asso <- glm(count ~ H + S + L + H:S + H:L + S:L, family=poisson, data=Healing)
summary(fit.asso)

# ���ǿ���������(Conditional Association Model) 
# 1. (HL, SL)
fit.HS.L <- glm(count ~ H + S + L + H:L + S:L, family=poisson, data=Healing)
summary(fit.HS.L)
# 2. (HS, HL)
fit.SL.H <- glm(count ~ H + S + L + H:S + H:L, family=poisson, data=Healing)
summary(fit.SL.H)
# 3. (HS, SL)
fit.HL.S <- glm(count ~ H + S + L + H:S + S:L, family=poisson, data=Healing)
summary(fit.HL.S)

# ���տ���������(Joint Association Model)
# 1. (HS, L)
fit.HS.L2 <- glm(count ~ H + S + L + H:S, family=poisson, data=Healing)
summary(fit.HS.L2)
# 2. (SL, H)
fit.SL.H2 <- glm(count ~ H + S + L + S:L, family=poisson, data=Healing)
summary(fit.SL.H2)
## 3. (HL, S)
#fit.HL.S2 <- glm(count ~ H + S + L + H:L, family=poisson, data=Healing)
#summary(fit.HL.S2)

# ��ȣ��������(Mutual Independence Model) : (H, S, L)
fit.ind <- glm(count ~ H + S + L , family=poisson, data=Healing)
summary(fit.ind)

##### ���� ���ռ� ����
library(epiDisplay)
# ��������������
## 1)H_0: (HS, HL, SL) vs H_a: (HSL)
lrtest(fit.full, fit.asso)

# ���ǿ���������
## 2) H_0: (HL, SL) vs H_a: (HS, HL, SL)
lrtest(fit.asso, fit.HS.L)
## 3) H_0: (HS, HL) vs H_a: (HS, HL, SL)
lrtest(fit.asso, fit.SL.H)
## 4) H_0: (HS,  SL) vs H_a: (HS, HL, SL)
lrtest(fit.asso, fit.HL.S)

# ���տ���������
## 5) H_0: (HS, L) vs H_a: (HS, HL, SL)
lrtest(fit.HL.S, fit.HS.L2)
## 6) H_0: (SL, H) vs H_a: (HS, HL, SL)
lrtest(fit.HL.S, fit.SL.H2)
### 7) H_0: (HL, S) vs H_a: (HS, HL, SL)
#lrtest(fit.asso, fit.HL.S2)

# ��ȣ��������
## 7) H_0: (H, S, L) vs H_a: (HS, HL, SL)
lrtest(fit.HL.S, fit.ind)

#####
# ���� ���� ���
data.frame(H, S, L, Healing$count, Ind=fitted(fit.ind), SL.H=fitted(fit.SL.H2), 
           HS.L=fitted(fit.HS.L2), HS.SL=fitted(fit.HL.S), HS.HL=fitted(fit.SL.H), 
           HL.SL=fitted(fit.HS.L), HS.HL.SL=fitted(fit.asso), Full=fitted(fit.full))

##### ĭ ǥ��ȭ ����
fit4 <- glm(count ~ H + S + L + H:S + S:L, family=poisson, data=Healing)
summary(fit4)
deviance(fit4)
res <- rstandard(fit, type="pearson")
data.frame(H, S, L, Healing$count, fitted(fit4), res)

# ���Ǻο������� ���� ���Ǽ�����
library(car)
Anova(fit4)

# ���Ǻ� ������� �ŷڱ��� : �������� �ŷڱ���
exp(confint(fit4))