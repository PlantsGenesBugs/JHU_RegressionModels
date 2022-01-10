install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)

devtools::install_github("jhudsl/matahari")
library(matahari)

dance_start(value = FALSE, contents = FALSE)

head(college)
college$major_category <- as.factor(college$major_category)

#There is a lot of detail in the information. Better to use category of major than individual subject. 
#The sample sizes vary a lot.
#Initial analysis to see effect of major regardless anything else

fit0 <- lm(median ~ major_category-1, data=college)
summary(fit0)

#Analyse if sample size will affect median income
y <- college$median
x <- college$sample_size
plot(x, y)
points(x[1 : (nrow(college)/2)], y[1 : (nrow(college)/2)], pch = 21, col = "black", bg = "lightblue")
points(x[(nrow(college)/2 + 1) : nrow(college)], y[(nrow(college)/2 + 1) : nrow(college)], pch = 21, col = "black", bg = "salmon")

#sample size does not predict median income, so can be excluded from modeling

#Investigate if income is linked to gender
fit1 <- lm(median~perc_men, data=college)
summary(fit1)

fit2 <- lm(median~perc_women, data=college)
summary(fit2)

#Mean income for any student is $42287, with men on average making $4087 more than women, regardless major

#Investigate the mean income for every major, and the mean difference for women
fit3 <- lm(median ~ major_category + perc_women-1, data=college)
summary(fit3)
max(summary(fit3)$coef[1:16,1])
min(summary(fit3)$coef[1:16,1])

#The highest income ($52069)is received by Business graduates and lowest ($29904) is received by Interdisciplinary grads
#with women making on average $5046 less for any given major

#It is possible that choice of major will be heavily influenced by gender, and so it is worth investigating the interaction
#of gender and major on median income

fit4 <- lm(median~major_category + major_category*perc_women-1, data=college)

#check if interaction is significant
anova(fit3, fit4)

#ANOVA indicates significant difference when interaction term is added

summary(fit4)
#Analysis of values shows that income is affected more in some majors than others when including gender as a interacting factor
#interesting to note that the mean income for women who graduate with journalism as a major is no different to that received by men

dance_save("~/Desktop/college_major_analysis.rds")





