# 2.3.
# p1-p2�� ���ھ� �ŷڱ���
library(PropCIs)
diffscoreci(624000,1000000,13000,1000000, conf.level=0.95)

# ������赵 p1/p2 �ŷڱ��� 
riskscoreci(624000,1000000,13000,1000000, conf.level=0.95)


# 2.17.
Gap <- matrix(c(871,821,336,347,42,83), ncol=3, byrow=TRUE)
(chi.GG <- chisq.test(Gap)) # Peason ī������ ����
chi.GG$stdres #ǥ��ȭ����

# ���ɵ��� ī������ ����
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