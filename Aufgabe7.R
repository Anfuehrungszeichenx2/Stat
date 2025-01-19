library(TeachingDemos) #fuer diese ganzen tests

#2
#A population is known to be normally distributed with a standard
#deviation of 2.8.
#(a) Compute the 95% confidence interval on the mean based on the
#following sample of nine: 8, 9, 10, 13, 14, 16, 17, 20, 21.
sample <- c(8, 9, 10, 13, 14, 16, 17, 20, 21)
sample_mean <- mean(sample)
pop_sd <- 2.8
alpha <- 0.05
z.test(x = sample, stdev = pop_sd, alternative = "two.sided",
       conf.level = 1-alpha)$conf.int
#der confidence interval ist [13.59270, 14.85175]
#jetzt ohne z.test
q <- qnorm(1-(alpha/2))
n <- length(sample)
L <- sample_mean - q* (pop_sd / sqrt(n))
U <- sample_mean + q * (pop_sd / sqrt(n))
L
U

#(b) Now compute the 99% confidence interval using the same data.
sample <- c(8, 9, 10, 13, 14, 16, 17, 20, 21)
sample_mean <- mean(sample)
pop_sd <- 2.8
alpha <- 0.01
z.test(x = sample, stdev = pop_sd, alternative = "two.sided",
       conf.level = 1-alpha)$conf.int

#3 3. You take a sample of 22 from a population of test scores, and the mean
#of your sample is 60
#a) You know the standard deviation of the population is 10. What
#is the 99% confidence interval on the population mean?
pop_sd <- 10
alpha <- 0.01
n <- 22
sample_mean <- 60
#wir haben keine sample, also die pure formel
q <- qnorm(1-(alpha/2))
L <- sample_mean - q * (pop_sd/sqrt(n))
U <- sample_mean + q * (pop_sd/sqrt(n))
L
U
#[54.50831, 65.49169]
#(b) Now assume that you do not know the population standard devi-
#ation, but the standard deviation in your sample is 10. What is
#the 99% confidence interval on the mean now?
#wir suchen den confidence interfal fuer mean, aber haben kein sd
sample_sd <- 10
n <- 22
alpha <- 0.01
sample_mean <- 60
t <- qt(p = 1-(alpha/2),df= n-1)
L <- sample_mean - t *(sample_sd/sqrt(n))
U <- sample_mean + t *(sample_sd/sqrt(n))
L
U
#[53.96352, 66.03648]


4. Calculate for the below given sample from a normally distributed pop-
  ulation the 95% confidence intervals
(a) for the mean, if the standard deviation is 2
(b) for the mean, if the standard deviation is unknown
(c) for the variance, if the mean is 250
(d) for the variance, if the mean is unknown
xi <- c(247.4, 249.0, 248.5, 247.5, 250.6, 252.2, 253.4, 248.3, 251.4, 246.9,
249.8, 250.6, 252.7, 250.6, 250.6, 252.5, 249.4, 250.6, 247.0, 249.4)
#a
sample_mean <- mean(xi)
sd <- 2
n <- length(xi)
alpha <- 0.05
#wir suchen ci fuer mu und haben sd, also erste formel
#da wir einen sample haben, koennen wir z.test nutzen
z.test(x = xi, stdev = sd,conf.level = 1-alpha, alternative = "two.sided")$conf.int
#[249.0435, 250.7965]

#b hier haben wir das selbe, aber keinen sd
#hier kann ich den t test anwerden, da ich sample habe
t.test(x = xi, alternative = "two.sided", conf.level = 1-alpha)$conf.int
#[249.0084, 250.8316]
#jetzt mit der formel
xmit hut = sample mean
t <- qt(1-(alpha/2),n - 1)
L <- sample_mean - t * (sd(xi)/sqrt(n))
U <- sample_mean + t * (sd(xi)/sqrt(n))
L
U
#[249.0084, 250.8316]




#c jetzt suchen wir fuer die var, und haben den mean. also die 3te formel
qn <- sum((xi - sample_mean)^2)
L <-  qn/qchisq(1-(alpha/2), n)
U <- qn/qchisq(alpha/2, n)
L
U
#[ 2.109828, 7.516805]
sigma.test(x = xi, alternative = "two.sided", conf.level = 1-alpha)$conf.int
2.194426 8.094298
e
#d das selbe, aber wir haben kein mean]
d_1 <- ((n-1)*sd(xi)^2)
L <- d_1 / qchisq(1-(alpha/2), n-1) 
U <- d_1 / qchisq((alpha/2), n-1)
L
U
#[2.194426, 8.094298]


At a telemarketing firm, the length of a telephone solicitation (in sec-
onds) is a normally distributed random variable with mean μ and stan-
dard deviation σ, both unknown. A sample of 51 calls has mean length
300 and standard deviation 60.

sample_sd <- 60
n <- 51
sample_mean <- 300
alpha <- 0.05
#construct the 95 confidence upper bound for mu
#kein sample, also per formel
t <- qt(1-alpha, n-1)
U <- sample_mean + t * sample_sd/sqrt(n)
U
#314.0804

#construct the 95% confidence lower bound for sdd
y <- (n-1)*sample_sd^2
L <- sqrt(y/qchisq(p = 1-alpha,n-1))
L          
#51.63794        


At a certain farm the weight of a peach (in ounces) at harvest time
is a normally distributed random variable with standard deviation 0.5.
How many peaches must be sampled to estimate the mean weight with
a margin of error ±0.2 and with 95% confidence

sample_sd <- 0.5
alpha <- 0.05
moe <- 0.2
n <- (qnorm(1-(alpha/2))/moe)^2


7. You read about a survey in a newspaper and find that 70% of the 250
people sampled prefer candidate A.
(a) Compute the 95% confidence interval.

alpha <- 0.05
n <- 250
p_mit_hut <- (0.7*n)/n
q <- qnorm(1-(alpha/2))
L <- p_mit_hut - q * sqrt((p_mit_hut * (1-p_mit_hut))/n)
U <- p_mit_hut + q * sqrt((p_mit_hut * (1-p_mit_hut))/n)
L
U
#[0.6431948, 0.7568052]
binom.test(x=0.7*n, n=250, conf.level = 1-alpha, alternative = "two.sided")$conf.int

(b) You are surprised by this survey because you thought that more
like 50% of the population preferred this candidate. Based on this
sample, is 50% a possible population proportion?
#possible, but not in the confident, intervall, so verrz unlilezshsuidf
  
  
  
  8. A researcher was interested in knowing how many people in the city
supported a new tax. He sampled 100 people from the city and found
that 40% of these people supported the tax. What is the upper limit of
#jthe 95% (one-side) confidence interval on the population proportion?
  
n <- 100
alpha <- 0.05 
p_hut <- (0.4*100)/100   #wichtig hier nicht teilen

q <- qnorm(1-alpha)
U <- p_hut + q * sqrt((p_hut*(1-p_hut))/n)
#mit formel
binom.test(x = 0.4*100, n = n, conf.level = 1 - alpha, alternative = "less")$conf.int
#[,0.4870242]


9)An advertising agency wants to construct a 99% confidence lower bound
for the proportion of dentists who recommend a certain brand of tooth-
  paste. The margin of error is to be 0.02. How large should the sample
#be?
alpha <- 0.01
moe <- 0.02
z <- qnorm(1-alpha)
f <- seq(0,1,length = 101)
n <- max(ceiling((z^2 * f *(1-f))/moe^2))
#hier wieder mit der moe
#hier nachfragen warum wir <= 0.2 machen
f <- seq(0,0.2,by = 0.01)
n <- max(ceiling((z^2 * f *(1-f))/moe^2))



10. The interval [45.6, 47.8] is a symmetric 99% confidence interval for the
unknown parameter μ based on a sample x1, . . . , x10 from a normal
distribution N (μ, σ2) with unknown σ. Calculate the sample mean ¯x
and the sample standard deviation s.
#
L <- 45.6
U <- 47.8
alpha <- 0.01
n <- 10

sample_mean <- (U+L)/2
sample_sd <- ((sample_mean - L)*sqrt(n))/qt(1-(alpha/2),n-1)


The waiting time at the pay desk of a certain supermarket is normally
distributed with mean waiting time μ and known standard deviation
σ = 1, 8 minutes. A confidence interval for the mean waiting time
(in minutes) for this supermarket is [5.12; 8.32]. If the sample size is
#n = 10, what is then the confidence level?
#sdfsdf
sample_sd <- 1.8
L <- 5.12
U <- 8.32
n <- 10
leange <- 8.32-5.12
z <- (3.2/2)/(1.8/sqrt(n))
z <- (leange * sqrt(n)) / 2 * sampel_sd
alpha <- 2*(1-pnorm(z))
confidence_level <- 1-alpha


intervallleange <- 2 * qnorm(1-(alpha/2)) * (sample_sd/sqrt(n))
