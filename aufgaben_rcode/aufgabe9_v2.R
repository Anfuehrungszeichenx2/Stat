#1)
m1 <-  c(5.46, 5.34, 4.34, 4.82, 4.4, 5.12, 5.69, 5.53, 4.77, 5.82)
m2 <- c(5.45, 5.31, 4.11, 4.69, 4.18, 5.05, 5.72, 5.54, 4.62, 5.89, 5.6, 5.19, 3.31, 4.43, 5.3, 4.09)
sd1 <- 0.5
sd2 <- 0.6
n1 <- length(m1)
n2 <- length(m2)
#test the H0: mu1 >= mu2
alpha <- 0.05
#rejection Region
r <- qnorm(alpha)
#[ , -1.644854]
#teststistic
t <- (mean(m1) - mean(m2)) / 
  sqrt((sd1^2 / n1) + (sd2^2 / n2))
#1.027782
p_value <- 1-pnorm(t)  #hier wegen 1- fragen
p_value
#0.1520261
#we fail to reject H0 since we are outsice of the rejection area


#2
x <- c(7.06, 11.84, 9.28, 7.92, 13.5, 3.98, 3.82, 7.34, 8.7, 9.24, 4.86, 3.32,
        12.78, 12, 5.24, 11.4, 6.56, 9.04, 7.72, 9.26, 7.88, 8.6, 9.3, 8.42, 8.54)
y <- c(8.68, 6, 6.3, 10.24, 10.88, 5.36, 7.82, 4.7, 9.02, 9.78, 6.9, 5.8, 13.56,
        10.32, 13.3, 11.38, 7.94, 10.74, 13.68, 14.92, 7.42, 10.36, 10.54,
        5.22, 13.74, 12.98, 10.34, 10.02, 17.8, 13.04, 5.2, 9.4, 11.18, 12.68,
        12.36)
#H0: X >= Y, H1 x < y
##################################################################
#case: equal variances:
t.test(x, y, alternative = 'less', paired = F, var.equal = T, conf.level = 0.95)
#p-value = 0.0181
#we can reject the h0 since we are under 0.05

#################################################################
#case: unequal variances:
t.test(x, y, alternative = 'less', paired = F, var.equal = F, conf.level = 0.95)
#p-value = 0.01596
#we can reject the h0 since we are under 0.05



#3
x <- c(16, 15, 11, 20, 19, 14, 13, 15, 14, 16)
y <- c(13, 13, 10, 18, 17, 11, 10, 15, 11, 16)
t.test(x = x, y = y, alternative = 'two.sided', paired = T, var.equal = F, conf.level = 0.95, mu = 0)
t.test(x = x, y = y, alternative = 'two.sided', paired = T, var.equal = T, conf.level = 0.95, mu = 0)
#0.0007205


#4)
x <- c(102.4, 101.3, 97.6, 98.2, 102.3, 99.1, 97.8, 103.9, 101.6, 100.1)
y <- c(98.4, 101.7, 100.5, 99.3, 100.6, 99.6, 102.2, 101.1, 99.9, 101.0)
#H0: sigma_a <= sigma_b, H1: sigma_a > sigma_b
alpha <- 0.05
var.test(x = x, y = y, alternative = 'greater', conf.level = 1-alpha)
#p-value = 0.03404
#we reject the H0

#5
#a)
x <- c(7.2, 4.1, 5.5, 4.5, 5.7, 3.8, 4.6, 6.0, 5.2, 5.4)
y <- c(5.3, 4.4, 5.0, 3.5, 3.9, 4.9, 5.6, 2.5, 4.0, 3.6)
alpha <- 0.1
#H0: sigma_x = sigma_y, H1: sigma_x != sigma_y
var.test(x = x, y = y, alternative = 'two.sided', conf.level = 1-alpha)
#p-value = 0.8814
#we fail to reject H0

warum ist in der B nicht paired lol
var(x)
var(y)
#b)
#H0: mu_x <= mu_y, H1: mu_x > mu_y
0.02374
alpha <- 0.025
t.test(x = x, y = y, alternative = 'greater', paired = F, var.equal = TRUE, conf.level = 1-alpha)
#p-value = 0.03581
#we reject H0