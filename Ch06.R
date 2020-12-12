# �۾����� ����
setwd("C:/R/Categorical Data Analysis")

### Ch6. �ٹ��� ��������

#########################################################################
## Section 6.1. ���ع��� ���� ���� (������ ��������)
Gators <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Alligators.dat", header=TRUE)
str(Gators)
Gators$y.fac <- factor(Gators$y, levels=c("F","I","O"))
 # vglm : fitting vector generalized models
 # ���ع��ִ� Others (��Ÿ)
install.packages("VGAM")
library(VGAM) # package for multivariate GLMs, such as multinomial models
fit <- vglm(y.fac ~ x, family=multinomial, data=Gators)
 # ������������ �� ������� ��Ľ����� ���
coef(fit, matrix=TRUE)
 # hde.NA = FALSE : Wald test ����� NA�� ��µ��� �ʵ���.
summary(fit, hde.NA = FALSE) 

 # ��ü���� ���ع��ַ� ����� ���ع��� ���� ����
fit2 <- vglm(y.fac~x, family=multinomial(refLevel="I"), data=Gators)
summary(fit2, hde.NA = FALSE)
confint(fit2) # Wald confidence interval
confintvglm(fit2, method="profile") # profile likelihood CI 

fit0 <- vglm(y.fac~1, family=multinomial, data=Gators)
summary(fit0) # deviance for working model is 98.3412
 # �쵵�� ���� : H0: beta_1=beta_2=0, H1: ��� �ϳ��� beta_j =!=0 (j=1,2)
lrtest(fit, fit0) # lrtest function of VGAM package

## 6.1.3. ����Ȯ���� ����
 # estimated response probabilities for outcome categories
fitted(fit2)
 # ������ ����Ȯ���� ����
cbind(fitted(fit2),Gators$x)

# ����Ȯ�� ��ǥ
beta.hat<-coef(fit, matrix=TRUE)
# Plot each pi_j
 # ��ǥ�� �׸��� ���� �׸� Ʋ
curve(exp(beta.hat[1,2] + beta.hat[2,2]*x)/
        (1 + exp(beta.hat[1,1] + beta.hat[2,1]*x) + exp(beta.hat[1,2] + beta.hat[2,2]*x)-2),
      ylab = expression(hat(pi)), xlab = "Length", type="n", 
      xlim = c(min(Gators$x), max(Gators$x)), col = "black", lty = "solid", lwd = 2, 
      n = 1000, panel.first = grid(col = "gray", lty = "dotted"))
 # ��Ÿ�� ���� ���� Ȯ�� �
curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[2,1]*x) + exp(beta.hat[1,2] + beta.hat[2,2]*x)),
      col = "red", lty = 1, lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(Gators$x), max(Gators$x)))
 # ��ü���� ���� ���� Ȯ�� �
curve(exp(beta.hat[1,2] + beta.hat[2,2]*x)/
              (1 + exp(beta.hat[1,1] + beta.hat[2,1]*x) + exp(beta.hat[1,2] + beta.hat[2,2]*x)),
      col = "blue", lty =2, lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(Gators$x), max(Gators$x)))
 # ����� ���� ���� Ȯ�� �
curve(expr = exp(beta.hat[1,1] + beta.hat[2,1]*x)/
              (1 + exp(beta.hat[1,1] + beta.hat[2,1]*x) + exp(beta.hat[1,2] + beta.hat[2,2]*x)),
      col = "green", lty = 3, lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(Gators$x), max(Gators$x)))
legend(x = 3.0, y = 0.7, legend=c("��Ÿ", "��ü��","���"), lty=c(1,2,3),
       col=c("red","blue","green"), bty="n", lwd = c(2,2,2), seg.len = 4, cex=1.2)

#######
## 6.1.5. ���������� ��� �������� ���׸���
Afterlife <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Afterlife.dat", header=TRUE)
Afterlife
fit <- vglm(cbind(yes,undecided,no)~gender+race, family=multinomial, data=Afterlife)
summary(fit)

  # ������ ���������� ����� ���׸���
fit.race <- vglm(cbind(yes,undecided,no)~race, family=multinomial, data=Afterlife)
summary(fit.race)
 # �쵵�� ���� : ���������� ���Ǽ� ����
 # H0: betaG_1=betaG_2=0, H1: ��� �ϳ��� betaG_j =!=0 (j=1,2)
lrtest(fit, fit.race)

  # ������ ���������� ����� ���׸���
fit.gender <- vglm(cbind(yes,undecided,no)~gender, family=multinomial, data=Afterlife)
summary(fit.gender)
 # �쵵�� ���� : ���������� ���Ǽ� ����
 # H0: betaR_1=betaR_2=0, H1: ��� �ϳ��� betaR_j =!=0 (j=1,2)
lrtest(fit, fit.gender) 

# ����Ȯ���� ����
# estimated response probabilities for outcome categories
data.frame(Afterlife$race, Afterlife$gender, round(fitted(fit),3))

##########################################################
## Section 6.2. ���� ���� ���� (������ ��������)
  # grouped data (����ǥ�ڷ�)
Polviews <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Polviews.dat", header=TRUE)
library(VGAM)
fit <- vglm(cbind(y1,y2,y3,y4,y5)~party+gender, 
            family=cumulative(parallel=TRUE), data=Polviews)
summary(fit)
# ����Ȯ���� ����
attach(Polviews)
data.frame(gender, party, fitted(fit))

####### ����Ȯ��
 # ��� ���� ������ ���� ���� ���� ���� : 
 # �� ���ֺ� ����� ������ ���� ���Ⱑ �ٸ� ����
fit2 <- vglm(cbind(y1,y2,y3,y4,y5)~party+gender, 
             family=cumulative, data=Polviews)
summary(fit2)

 # H0: ��� ���� ���� ���� ������ �����ϴ�
 # Ha: ��� ���� ������ ���� ���� ���� ������ �����ϴ�.
lrtest(fit2, fit)
#######

### ��� �߷� : H0: beta1=0 (���簡�� ���δ� ��ġ���⿡ ������ ����)
fit.g <- vglm(cbind(y1,y2,y3,y4,y5)~gender, 
              family=cumulative(parallel=TRUE), data=Polviews)
summary(fit.g)
lrtest(fit, fit.g)

 # �������� ��ġ���� ���������� ����Ȯ�� P(Y>j)�� ���ڷ� ������ 
 # ��ȭ���(x1=1)�� ���ִ��(x1=0)��  ���� �������
 # �ŷڱ����� (exp(3.218), exp(4.072))
confint(fit, method="profile")

############################################
## 6.2.5. �� ���� ���԰� �ູ����
Happy <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Happy.dat", header=TRUE)

library(VGAM)
 # ��� ���� �������� ���� ���� : income���� ������
fit <- vglm(cbind(y1,y2,y3)~income, family=cumulative(parallel=TRUE), data=Happy)
summary(fit)

fit0 <- vglm(cbind(y1,y2,y3)~1, family=cumulative, data=Happy)
 # income�� ���� ���Ǽ� ����
lrtest(fit, fit0)

 # �߼�����(2�� ����)
library(vcdExtra)
CMHtest(cbind(Happy$y1,Happy$y2,Happy$y3), rscores=c(1,2,3))

 # ���� ���� ���� ���� ���� : income���� ������
fit2 <- vglm(cbind(y1,y2,y3)~ factor(income), family=multinomial, data=Happy)
summary(fit2)

fit0 <- vglm(cbind(y1,y2,y3)~ 1, family=multinomial, data=Happy)
 # income�� ���� ���Ǽ� ����
lrtest(fit2, fit0)

##########################################################
## Section 6.4. �̿����� ���� (������ ��������)
library(VGAM)
fit <- vglm(cbind(y1,y2,y3,y4,y5)~party+gender, 
            family=acat(parallel=TRUE, reverse=TRUE), data=Polviews)
summary(fit)