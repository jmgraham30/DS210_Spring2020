library(fastR2)
library(tidyr)


set.seed(1234)
coffee <- rnorm(30,7,0.25)

t.test(coffee,mu=7,alternative = "less")$p.value

coffee_minus_seven <- coffee - 7

(obs_neg_prop <- sum(coffee_minus_seven < 0))

N <- 10^5 - 1
sign_results <- numeric(N)
for (i in 1:N){
  sign_vals <- sample(c(-1,1),length(coffee),replace = T)
  sign_results[i] <- sum(sign_vals*coffee_minus_seven < 0)
}

gf_bar(~sign_results) %>% gf_vline(xintercept = obs_neg_prop,color="red")

(sum(sign_results >= obs_neg_prop) + 1)/(N+1)

1 - pbinom(obs_neg_prop-1,length(coffee),0.5)

n_coffee <- length(coffee)
mu_coffee <- mean(coffee)
obs_t <- (mu_coffee - 7)/sqrt(var(coffee)/n_coffee)
t_vals <- numeric(N)
for (i in 1:N){
  bootx <- sample(coffee,replace = T)
  t_vals[i] <- (mean(bootx) - mu_coffee)/sqrt(var(bootx)/n_coffee)
}

gf_histogram(~t_vals) %>% gf_vline(xintercept = obs_t,color="red")

(sum(t_vals <= obs_t) + 1)/(N+1)


