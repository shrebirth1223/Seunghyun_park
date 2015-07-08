pnorm(2)-pnorm(-2)

control <- c(91, 87, 99, 77, 88, 91)
treat <- c(101, 110, 103, 93, 99, 104)

t.test(control, treat, alternative = "less", var.equal = TRUE)


#---------------------          Assignment 2                --------------------
#                                                             Seunghyun Park
#################################   Q 1    #####################################
#1. (5 pts) The data below are the number of points scored in 30 games by the Portland Trailblazers.
#Scores: 90,95,89,71,73,96,87,95 107,89,96,80,97,95 102,97,93 101,82,83,74,91,83,98,95 111,99 120,93,84
score <- c(90, 95, 89, 71, 73, 96, 87, 95, 107, 89, 96, 80, 97, 95, 102, 97, 93, 101,82,83,74,91,83,98,95, 111,99, 120,93,84)

#a. Estimate the sample mean score. What does the quantity estimate?
Port_mean <- mean(score)
Port_mean

#b. Is the estimate in part(a) likely to equal the population parameter? Why or why not
#It's not big number of samples, So This will going to be close but not exactly same.

#c. Calculate the standard error for your sample estimate.
sd_score <- sd(score)
standard_error <- sd_score/sqrt(length(score))
standard_error

#d. What does the quantity in part(c) measure?
#How much close the sample is likely to the population.if n is small, than error are going big. 

#e. Calculate a 95% confidence interval for the population mean.
co_interval_right <- Port_mean + sd(score)*1.96/sqrt(length(score))
co_interval_left <- Port_mean - sd(score)*1.96/sqrt(length(score))
co_interval <- co_interval_right - co_interval_left
co_interval
#or, t.test(score) also show the range.

#f. Provide an interpretation for the interval you calculated in part (e).
#the result is likely to in this range. 
#Using hint from Jevin: most plausible value the population mean, In roughly 95% of the random sample from the population, the interval will include the true poplation mean.






#################################   Q 2    #####################################
#2. (5 pts) Using the following data, test the null hypothesis that male and females have the same mean cholesterol concentrations. Include descriptive statistics, hypothesis testing (e.g., t-test) and 95% confidence intervals.
#Male: 220.1, 218.6, 229.6, 228.8,222.0,224.1, 226.5 Female: 223.4,221.5,230.2,224.3,223.8,230.8
Male <- c(220.1, 218.6, 229.6, 228.8,222.0,224.1, 226.5 )
Female <- c(223.4,221.5,230.2,224.3,223.8,230.8)
t.test(Male,Female,alternative = "two.sided",var.equal = TRUE)

#Using hint from Jevin
layout(matrix(2:1, ncal =1))
hist(Male)
hist(Female)
mean(Male)
mean(Female)
sd(Male)
sd(Female)
t.test(Male, Female)
#95% CI: -6.4 < u < 3.5 which contains zero so can't reject null hypothesis






#################################   Q 3    #####################################
#3. (5 pts) A clinical trail was carried out to test whether a new treatment has an effect on the rate of recovery of patients. The null hypothesis “H0: the treatment has no effect” was rejected with a P-value of 0.04. The researchers used a significance level of 5%. State whether the following conclusions is correct. If not, explain why.

#a. The treatment has only a small effect.
#false.We can not know the amount of effect from it.

#b. The treatment has some effect.
#true Yes, it has effect but we cannot measure the amount of the effect

#c. The probability of committing a Type I error is 0.04.
#false a is 0.05

#d. The probability of committing a Type II error is 0.04.
#false We cannot get the answer. The information is not enough

#e. The null hypothesis would not have been rejected if the significance level was α=0.01.
#true. It is rejected if it is more than 0.05






#################################   Q 4    #####################################
#4. (5 pts) The data below are volumes of red blood cells from two individuals. Test the hypothesis (using the Mann-Whitney test) that the red blood cells of person B are 1.5 times the volume of person A.
#person A: 248, 236, 269, 254, 249, 251, 260, 245, 239, 255 person B: 380, 391, 377, 392, 398, 374
A=c(248,236,269,254,249,251,260,245,239,255)
B=c(380,391,377,392,398,374)
wilcox.test(A,1.5*B)

#Using hint from Jevin
pA2 <- 1.5*A
layout(matrix(2:1,ncol=1))
hist(pA2,xlim=c(250,410))
hist(B,xlim=c(250,410))
#H0 = pA cells have volume 1.5*pB
#Ha = pA cells don't have volume 1.5*pB
t.test(pA2,B)






#################################   Q 5    #####################################
#5. (5 pts) What is the difference between the standard error of mean and the standard deviation? Provide example data that illustrates their difference.
#standard error: tells us how confident you are, how confident sample means to the population means. As we get more data, error goes to 0
#standard derivation: add more n, Than we go to the actual derivatin. It shows the the dispersion from the given sample.