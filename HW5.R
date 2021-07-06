######## q5
#### c)
chisq.test(c(75, 55, 15, 5), p=c(0.5, 0.3, 0.1, 0.1))

######## q7

#### b)
chisq.test(c(125, 300-125), p=c(1/24, 23/24))


######## q8
library(MASS)
table(survey$Smoke, survey$Exer)
chisq.test(table(survey$Smoke, survey$Exer))
table(survey$Smoke == 'Never', survey$Exer)
chisq.test(table(survey$Smoke == 'Never', survey$Exer))
