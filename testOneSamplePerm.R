library(fastR2)
library(tidyr)

samp_size <- 15
true_mean <- 6.6
true_sd <- 0.8
coffee <- rnorm(samp_size,true_mean,true_sd)

hyp_mean <- 7

t_test_res <- t.test(coffee,mu=hyp_mean,alternative = "less")
t_test_p <- t_test_res$p.value

coffee_minus <- coffee - hyp_mean

(obs_neg_prop <- sum(coffee_minus < 0))

N <- 10^5 - 1
sign_results <- numeric(N)
for (i in 1:N){
  sign_vals <- sample(c(-1,1),length(coffee),replace = T)
  sign_results[i] <- sum(sign_vals*coffee_minus < 0)
}

gf_bar(~sign_results) %>% gf_vline(xintercept = obs_neg_prop,color="red")

perm_test_p <- (sum(sign_results >= obs_neg_prop) + 1)/(N+1)

binom_test_p <- 1-pbinom(obs_neg_prop-1,length(coffee),0.5)

n_coffee <- length(coffee)
mu_coffee <- mean(coffee)
obs_t <- (mu_coffee - hyp_mean)/sqrt(var(coffee)/n_coffee)
t_vals <- numeric(N)
for (i in 1:N){
  bootx <- sample(coffee,replace = T)
  t_vals[i] <- (mean(bootx) - mu_coffee)/sqrt(var(bootx)/n_coffee)
}

gf_histogram(~t_vals) %>% gf_vline(xintercept = obs_t,color="red")

boot_test_p <- (sum(t_vals <= obs_t) + 1)/(N+1)

results <- data.frame(test=c("t","perm","binom","boot"),p_value=c(t_test_p,perm_test_p,binom_test_p,boot_test_p))

results

t_test_res$conf.int

