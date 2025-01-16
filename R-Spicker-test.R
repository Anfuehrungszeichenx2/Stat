#mu mit pop sd
sample <- c(8, 9, 10, 13, 14, 16, 17, 20, 21)
sample_mean <- mean(sample)
pop_sd <- 2.8
alpha <- 0.05
q <- qnorm(1-(alpha/2))
n <- length(sample)
L <- sample_mean - q* (pop_sd / sqrt(n))
U <- sample_mean + q * (pop_sd / sqrt(n))
z.test(x = sample, stdev = pop_sd, alternative = "two.sided",
       conf.level = 1-alpha)$conf.int

L <- 12.39292
U <- 16.05152

sample_mean_umgestellt <- (L+U)/2
sample_mean_umgestellt

q_umgestellt <- (U + sample_mean)/(pop_sd/sqrt(n))
q_umgestellt <- (sample_mean - L)/(pop_sd/sqrt(n))

pop_sd_umgestellt <- ((U-sample_mean)*sqrt(n))/qnorm(1-(alpha/2))
pop_sd_umgestellt <- ((sample_mean-L)*sqrt(n))/qnorm(1-(alpha/2))

n_umgestellt <- round(((pop_sd*q)/(U-sample_mean))^2)
n_umgestellt <- round(((pop_sd*q)/(sample_mean-L))^2)

moe <- qnorm(1-alpha/2) * (pop_sd/sqrt(n))
n_aus_moe <- ((q*pop_sd)/moe)^2

alpha_umgestellt <- 2*(1-pnorm(qnorm(1-(alpha/2))))
               


#mu ohne population sd
sample <- c(8, 9, 10, 13, 14, 16, 17, 20, 21)
n <- length(sample)
sample_mean <- mean(sample)
sample_sd <- sd(sample)
alpha <- 0.05
t <- qt(1 - (alpha / 2), n-1)
L <- sample_mean - t * (sample_sd/sqrt(n))
U <- sample_mean + t * (sample_sd / sqrt(n))
t.test(x = sample, conf.level = 1-alpha, alternative = 'two.sided')

#sample mean
sample_mean_umgestellt <- (U+L)/2

#t
t <- ((U - L) * sqrt(n)) / (2 * sample_sd)
t_2 <- (U - sample_mean)/(sample_sd/sqrt(n))
t_3 <- (sample_mean -L)/(sample_sd/sqrt(n))

#s
sample_sd_umgestellt_1 <- (U - sample_mean) * (sqrt(n)) / t
sample_sd_umgestellt_2 <- (sample_mean - L) * (sqrt(n)) / t


#n
n_umgestellt_1 <- round(((sample_sd * qt(1-(alpha/2), df = n-1)) / (U - sample_mean))^2)
n_umgestellt_2 <- round(((sample_sd * qt(1-(alpha/2), df = n-1)) / (sample_mean - L)^2)

#moe 
moe <- qt(1-(alpha/2), n-1) * (sample_sd/sqrt(n))
moe <- (U-L)/2

#n mit moe
n <- round(((qt(1-(alpha/2), n-1)*sample_sd)/moe)^2)

#alpha
alpha_umgestellt <- 2* (1-pt(t, n-1))

#intervalll'nge

leange <- 2 * moe
leange_2 <- 2 * qt(1-(alpha/2), n-1)*(sample_sd/sqrt(n))

#moe mit leanve
moe <- leange/2

#wenn 95 conf aber nur upper oder nur lower

L_alleine <- sample_mean - qt(1-alpha, n-1) * (sample_sd/sqrt(n))
U_alleine <- sample_mean + qt(1-alpha, n-1) * (sample_sd/sqrt(n))
