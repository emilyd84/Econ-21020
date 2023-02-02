
library(readxl)
caschool <- read_excel("caschool.xlsx")

#defining income variable
income <- (caschool$avginc*1000)

#mean and standard deviation calculations
mean(caschool$avginc)
sd(caschool$avginc)
mean(income)
sd(income)
mean(caschool$math_scr)

#districts with class size <= 20
fewer20 <- caschool$str<=20
sum(fewer20)
fewer20math <- caschool[fewer20==TRUE,]$math_scr
mean(fewer20math)

#districts with class size > 20
greater20 <- caschool$str>20
sum(greater20)
greater20math <- caschool[greater20==TRUE,]$math_scr
mean(greater20math)

#standard error calculation
se1 <- sd(fewer20math)/sqrt(sum(fewer20))
se2 <- sd(greater20math)/sqrt(sum(greater20))
se <- sqrt(se1**2+se2**2)

#calculating test statistic
t <- (mean(fewer20math)-mean(greater20math))/se
t

#finding p-value
pnorm(t)
2*(1-pnorm(t))

#covariance calculations
cov(caschool$avginc, caschool$math_scr)
cov(income, caschool$math_scr)

#correlation calculations
cor(caschool$avginc, caschool$math_scr)
cor(income, caschool$math_scr)