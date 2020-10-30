# ������ �ڷ�м� �߰����� �ڵ�

# 1.1. �е� 95% �ŷڱ���
library(binom)
binom.confint(48, 750, conf.level=0.95, method="asymptotic")

# 1.1. ���ھ� 95% �ŷڱ���
prop.test(48, 750, p=0.50, alternative="two.sided", correct=FALSE)
library(binom)
binom.confint(48, 750, conf.level=0.95, method="wilson")

# 1.2. �е� ����
(z = (0.064-0.1)/sqrt(0.064*(1-0.064)/750))
(p.val = 2*(1- pnorm(-z)))

# 1.2. ���ھ� ����
(0.064-0.1)/sqrt(0.1*(1-0.1)/750)
prop.test(48, 750, p=0.10, alternative="two.sided", correct=FALSE)

# 2.1.
39/30000
6/60000
## ������ ���� �е� �ŷڱ���
prop.test(c(39,6), c(30000,60000), conf.level=0.95, correct=FALSE)
## ������ ���� ���ھ� �ŷڱ���
library(PropCIs)
diffscoreci(39,30000,6,60000, conf.level=0.95)
## ������赵 �ŷڱ���
library(PropCIs)
riskscoreci(39,30000,6,60000, conf.level=0.95)
## �����
(tt <- (0.0013*(1-0.0013))/(0.0001*(1-0.0001)))
## ����� �е� �ŷڱ���
library(epitools)
oddsratio(c(39,29961,6,59994), method="wald", conf=0.95, correct=FALSE)
## ����� ���ھ� �ŷڱ���
library(PropCIs)
orscoreci(39,30000,6,60000, conf.level=0.95)

# 2.2.
table2.2 <- array(data=c(39,6,29961,59994), dim=c(2,2), dimnames=list(Smoking=c("Yes","No"), LungCaner=c("Yes","No")))

## ������ ����
(chi.RG <- chisq.test(table2.2)) # Pearson ī������ ����
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_261')
library(rJava)
library(Deducer)
likelihood.test(table2.2) # ���ɵ��� ī������ ����

## ǥ��ȭ����
chi.RG$stdres
## ĭ������ ǥ��ȭ������ ������� ũ��� �׸� ������ũ �׸�
library(vcd)
mosaic(table2.2, gp=shading_Friendly, residuals=chi.RG$stdres, residuals_type="Std\nresiduals",
       labeling=labeling_residuals)

# 3.������ �ڷ��� ������ ���� 
Malform <- matrix(c(7,55,489,475,293,38,61,129,570,431,154,12), ncol=2)
library(vcdExtra)
CMHtest(Malform, rscores = c(0, 3, 9.5, 19.5, 37, 55))
(M <- sqrt(111.67))
(p.val <- 1- pnorm(M))

# 4.1. ���Ǻ� �����
(19*52)/(132*11)
(0*97)/(9*6)
19/151
11/63
6/103
(0.1258278/(1-0.1258278))/(0.1746032/(1-0.1746032))
# 4.2. �ֺ� �����
19/160
17/166
(0.11875/(1-0.11875))/(0.1024096/(1-0.1024096))

# 5.1.
table5 <- data.frame(no=c(17066,14464,788,126,37), yes=c(48,38,5,1,1), x=c(0,0.5,1.0,4.0,7.0))
table5 <- cbind(table5, n=table5[,1] + table5[,2])

## ������ƽ ȸ�͸���
fit <- glm(yes/n ~ x, family=binomial(link=logit), weights=n, data=table5)
summary(fit)

## ����ȸ�͸��� ����
fit2 <- glm(yes/n ~ x, family=quasi(link="identity", variance="mu(1-mu)"),
            weights=n, data=table5) 
summary(fit2, dispersion=1)

# 5.2.
confint(fit) #�������� ���ɵ� �ŷڱ���
confint.default(fit) #�е� �ŷڱ���

# 5.3.
## ������ƽ ȸ�͸������� ���ɵ��� ����
library(car)
Anova(fit)
## ���ھ� ����
library(statmod)
fit1 <- glm(yes/n ~ 1, family=binomial, weights=n, data=table5) #null model 
(XX <- glm.scoretest(fit1, table5$x)^2)
(pp <- 1- pchisq(XX, 1))

# 5.4.
attach(table5)
x5x5 <- cbind(x, yes, no, n, 
              ml=yes/n, # ��ȭ������ ������(�ִ�쵵������) 
              fitted=fitted(fit), # �������հ� 
              pear.res = residuals(fit, type="pearson"), # �Ǿ ���� 
              dev.res=residuals(fit), # ��Ż�� ����
              std.res=rstandard(fit, type="pearson"))
x5x5
x5x5 <- as.data.frame(x5x5)
library(writexl)
write_xlsx(x5x5, path = "C:/R/Categorical Data Analysis/x5x5.xlsx")