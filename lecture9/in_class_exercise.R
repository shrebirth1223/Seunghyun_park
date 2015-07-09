#1. A store keeps track of purchase history. The manager wonders if there is an association between the amount of money a customer spends on their first visit to the store and their second visit. Below is data collected on 10 customers. Each column corresponds to one customer. For example, the first customer spend $20 on the first visit and $23 on the second visit. The second customer spend $32 on first visit and $34 on second, etc.
#Money spent on first visit (in dollars): 20,32,35,34,40,51,52,56,57,68 Money spent on second visit (in dollars): 23,34,36,44,42,51,54,57,54,62
first_visit <- c(20,32,35,34,40,51,52,56,57,68)
second_visit <- c(23,34,36,44,42,51,54,57,54,62)
data=cbind(first_visit,second_visit)
#a. Display the relationship between first and second visit dollar amounts?
plot(first_visit,second_visit)
#b. Describe the pattern in part (a) briefly. Is there a relationship? Is it positive or negative? Is it linear or non-linear? Is it weak or strong?
#It shows the positive linear relation ship betweeen first_visit and second_visit, It's quite strong.
#c. Calculate the correlation coefficient between the amount of money spent on the first visit and the second visit.
corelation <- cor(first_visit,second_visit)
standard_error <- sqrt((1-corelation^2)/(length(first_visit)-2))
#d. What does the standard error in part (c) refer to?
#e. Calculate an approximate 95% confidence interval for ρ.
interval_right <- cor(data)-2*standard_error#first answer
interval_left <- cor(data)+2*standard_error#first answer
rt<-cor.test(first_visit,second_visit)#Second answer
rt$conf.int#Second answer


#2. Answer the following question using the data from question (2).
#a. Adding $30 to each of the observations for the second visit. How is the correlation coefficient between first and second visits affected? What can you conclude about the effects on the correlation coefficient of adding a constant to one or both of the variables?
c1_B <- first_visit + 30
c2_B <- second_visit + 30
cor(first_visit,c2_B)
#b. Convert the first visit to cents (i.e., multiply by 100). How does this affect the correlation between the first and second visits? What can you conclude about the effects on the correlation coefficient of multiplying one or both of the variables by a constant?
c1_C <- first_visit*100
cor(c1_C,second_visit)


#3. Some species seem to thrive in captivity, whereas others are prone to health and behavior difficulties when caged. Maternal care problems in some captive species, for example, lead to high infant mortality. Can these differences be predicted? The following data are measurements of the infant mortality (percentage of births) of 20 carnivore species in captivity along with the log (based-10) of the minimal home range sizes (in km2) of the same species in the wild (Clubb and Mason 2003). For example, -1.3 is the home range and 4 is the captive infant mortality percentage.
#Log home range size: -1.3 (4),-0.5 (22),-0.3 (0),0.2 (0),0.1 (11),0.5 (13),1.0 (17),0.3 (25),0.4 (24),0.5 (27),0.1 (29),0.2 (33),0.4 (33),1.3 (42),1.2 (33),1.4 (20),1.6 (19),1.6 (19),1.8 (25),3.1 (65)
#a. Draw a scatter plot of these data, with log of home range size as the explanatory variable. Describe the association between the two variables in words.
b1 <- c(-1.3,-0.5,-0.3,0.2,0.1,0.5,1.0,0.3,0.4,0.5,0.1,0.2,0.4,1.3,1.2,1.4,1.6,1.6,1.8,3.1)
b2 <- c(4,22,0,0,11,13,17,25,24,27,29,33,33,42,33,20,19,19,25,65)
plot(b1,b2)
#b. Estimate the slope and intercept of the least squares regression line, with the log of home range size as the explanatory variable. Add this line to your plot.
m <- lm(b2~b1)
abline(m)
#c. Does home range size in the wild predict the mortality of captive carnivores? Carry out a formal test. Assume that the species data are independent.
# Ho: home range size does not predict infant mortality (beta = 0)
# Ha: home range size does predict infant mortality (beta != 0)
# answer: b = 9.955, a=16.280, SE=2.766, t=3.6, df=18, P=0.002
# reject null Ho
summary(m)
a <- m$coefficients[1]
b <- m$coefficients[2]
a = 16.28047
b = 9.955187
# predicted values vs actual values
b3 <- mat.or.vec(20,1)
for (i in 1:length(b3)) {
  b3[i] = b*b1[i] + a
}
points(b1,b3,col="red")
#d. Outliers should be investigated because they might have a substantial effect on the estimate so of the slope and intercept. Recalculate the slope and intercept of the regression line from part (c) after excluding the outlier at large home range size (which correspond to the polar bear). Add the new line to your plot. By how much did it change the slope?
b1_p <- c(-1.3,-0.5,-0.3,0.2,0.1,0.5,1.0,0.3,0.4,0.5,0.1,0.2,0.4,1.3,1.2,1.4,1.6,1.6,1.8)
b2_p <- c(4,22,0,0,11,13,17,25,24,27,29,33,33,42,33,20,19,25,25)
m_p <- lm(b2_p~b1_p)
abline(m_p)
summary(m_p)
# answer: b = 6.600, a=17.510, SE = 3.074, t=2.147, df=17, P = 0.0465
# still reject Ho but not as strongly, the slope changes from 9.955 to 6.600

