# 1.9.a.
(0.6-0.5)/sqrt(0.6*0.4/100)
2*(1-pnorm(2.041241))
# 1.9.b
sqrt(0.6*0.4/100)
0.6-1.96*0.04898979
0.6+1.96*0.04898979

library(binom)
# 1.12.a
sqrt(0*1/25)
(0-0.5)/0
2*pnorm((0-0.5)/sqrt(0*1/25))
# 1.12.b
binom.confint(0, 25, conf.level = 0.95, methods = "asymptotic")
# 1.12.c
prop.test(0, 25, p=0.5, alternative = "two.sided", correct = FALSE)
2*pnorm((0-0.5)/sqrt(0.5*0.5/25))
1-pchisq(25, 1)
# 1.12.d
binom.confint(0, 25, conf.level = 0.95, methods = "wilson")

# 1.13.b
2*log(1.0/(0.5^25))
1-pchisq(34.65736, 1)

# 1.13.c
2*log(1.0/(0.926^25))
1-pchisq(3.844052, 1)