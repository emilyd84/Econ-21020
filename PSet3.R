
library(readxl)
caschool <- read_excel("caschool.xlsx")
income <- (caschool$avginc*1000)
mean(caschool$avginc)
sd(caschool$avginc)
mean(income)
sd(income)
mean(caschool$math_scr)
fewer20 <- caschool$str<=20
sum(fewer20)
fewer20math <- caschool[fewer20==TRUE,]$math_scr
mean(fewer20math)
greater20 <- caschool$str>20
sum(greater20)
greater20math <- caschool[greater20==TRUE,]$math_scr
mean(greater20math)
se1 <- sd(fewer20math)/sqrt(sum(fewer20))
se2 <- sd(greater20math)/sqrt(sum(greater20))
se <- sqrt(se1**2+se2**2)
t <- (mean(fewer20math)-mean(greater20math))/se
t
pnorm(t)
2*(1-pnorm(t))
cov(caschool$avginc, caschool$math_scr)
cov(income, caschool$math_scr)
cor(caschool$avginc, caschool$math_scr)
cor(income, caschool$math_scr)