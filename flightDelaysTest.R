library(fastR2)
library(resampledata)

delay_times <- FlightDelays$Delay

table(FlightDelays$Carrier)

num_AA <- 2906
num_UA <- 1123


FlightDelays %>% group_by(Carrier) %>%
  summarise(mean_delay = mean(Delay))


mean_AA <- 10.1
mean_UA <- 16.0

obs_mean_diff <- mean_AA - mean_UA

N <- 10^5 - 1

mean_diffs <- numeric(N)


for (i in 1:N){
  perm_data <- sample(delay_times)
  AA_data <- perm_data[1:num_AA]
  UA_data <- perm_data[-(1:num_AA)]
  mean_diffs[i] <- mean(AA_data) - mean(UA_data)
}

gf_histogram(~mean_diffs) %>% gf_vline(xintercept = obs_mean_diff)


(sum(mean_diffs <= obs_mean_diff) + 1)/(N + 1)


table(FlightDelays$Month)

num_May <- 1999
num_June <- 2030


FlightDelays %>% group_by(Month) %>%
  summarise(mean_delay = mean(Delay))


mean_May <- 8.88
mean_June <- 14.5

obs_mean_diff <- mean_May - mean_June

N <- 10^5 - 1

mean_diffs <- numeric(N)


for (i in 1:N){
  perm_data <- sample(delay_times)
  May_data <- perm_data[1:num_May]
  June_data <- perm_data[-(1:num_June)]
  mean_diffs[i] <- mean(May_data) - mean(June_data)
}

gf_histogram(~mean_diffs) %>% gf_vline(xintercept = obs_mean_diff)


(sum(mean_diffs <= obs_mean_diff) + 1)/(N + 1)

t.test(Delay~Month,data=FlightDelays)
