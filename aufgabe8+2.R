#aufgabe 4
#a
#80% immunity rate
#b
#H0: p <= 80, H1: p > 80
p0 <- 0.8
n <- 200
x <- 172
phut <- x/n
alpha <- 0.05
#rejection area [pnorm(a-alpha), ]
r <- pnorm(1 - alpha)
#r <- [0.8289439, ]
#teststatistic
t <- (phut - p0) / sqrt((p0 * (1 - p0)) / n)
#2.12132
#we reject h0 since its in the rejection region
t > r
#based on this sample, the new vaccine should be more effective than the current one
p_value <- 1 - pnorm(t)
p_value
#0.01694743
#c
#we are at risk of making a type 1 error, -> rejecting a true h0
#exact
binom.test(172, p = 0.8, n = n, alternative = 'greater', conf.level = 1-alpha)
#0.01793
#aufgabe5
#its a two tailed test, since it can go in both direction
#b
n <- 30
x <- 1- pbinom(22, n, 0.5) + pbinom(7, n, 0.5)
x
#0.005222879
#f[r die c jerecten wir weil 0.005224 kleiner als 0.01 ist also reject both sides

#aufgabe6
n <- 82
sample_mu <- 248
sample_sd <- 5
alpha <- 0.05
#a h0: mu >= 250, h1: < 250
mu0 <- 250
#da wir keinen mu und sd haben, benutyten wir den t-test
#rejection region
r <- -qt(1-alpha, n-1)
r
#[ , -1.663884]
#teststatistics
t <- (sample_mu - mu0) / ((sample_sd)/sqrt(n))
t
#-3.622154
t < r
#we reject h0, since we are in the rejection area
p_value <- pt(t,n-1)
p_value
#0.0002540167
#p value is smaller than 0.05



#b)
#h0: sd >= 7, h1: sd <7
#we are using the test 3 since we are testing over variance
n <- 82
sample_mu <- 248
sample_sd <- 5
alpha <- 0.05
sd0 <- 7
#given from h0
#rejection region 
r <- qchisq(alpha, n-1)
r
#[ ,61.26148
#teststatistics
t <- ((n - 1) * sample_sd)/sd0
t
#57.85714
t < r
#we reject h0, since we are in the rejection area
p_value <- pchisq(t, n-1)
p_value
#0.02419782
p_value > alpha


#aufgabe 7
n <- 100
sd <- 0.3
sample_mean <- 10.1
alpha <- 0.1
#mean is unbekannt, aber wir haben sample mean und sd
#H0: mu = 10, H1: mu != 10
mu0 <- 10
#Rejection region
ru <- qnorm(1-(alpha/2))
rl <- -qnorm(1-(alpha/2))
rl
#[-1.644854, 1.644854]
#teststatistic
t <- (sample_mean - mu0) / (sd / sqrt(n))
t > ru
#3.333333
#we reject h0 because we are in the rejection region
p_value <- 1 - pnorm(t)
p_value
#0.0004290603



#8
n <- 500
x <- 302
phut <- x/n
alpha <- 0.05
#H0: p = 0.5
p0 <- 0.5
#rejection area
ru <- qnorm(1-(alpha/2))
rl <- -qnorm(1-(alpha/2))
ru
#[-1.959964, 1.959964]
#teststatistic
t <- (phut - p0)/ (sqrt((p0 * (1 - p0))/n))
t > ru
#wir rejecten h0
#die coin is unfair vermutlich
p_value <- 1-pnorm(t)
p_value < 0.025

#exact
binom.test(x = 300, p = 0.5, n = n, alternative = 'two.sided', conf.level = 1-alpha)
