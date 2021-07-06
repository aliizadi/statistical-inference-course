######## q3
chick_m = Chick$weight[Chick$feed=="meatmeal"]
chick_c = Chick$weight[Chick$feed=="casein"]
#### a) 

bootstrap_diff_means <- function(a, b, n_bootstrap=1000){
  n_a <- length(a)
  n_b <- length(b)
  
  bootstrap_samples <- c()
  
  for(i in 1:n_bootstrap){
    bootstrap_a <- sample(1:n_a, n_a, replace=TRUE) 
    bootstrap_b <- sample(1:n_b, n_b, replace=TRUE)
    diff <- mean(a[bootstrap_a])-mean(b[bootstrap_b])
    bootstrap_samples <- c(bootstrap_samples, diff) 
  }
  return(bootstrap_samples)
}

bootstrap_samples = bootstrap_diff_means(chick_c, chick_m)

#### b)
p_value_diff_means_t_test <- function(a, b) {
  null = 0 
  observation = mean(a) - mean(b)
  n_a = length(a)
  n_b = length(b)
  se = sqrt((sd(a) ^ 2) / n_a + (sd(b) ^ 2) / n_b)
  t = (observation - null) / se
  df = min(n_a, n_b)
  p_value = (1 - pt(t, df=df)) * 2
  return (p_value)
}

p_value_original_sample = p_value_diff_means_t_test(chick_c, chick_m)

p_value_bootstrap_diff_means_t_test <- function(a, b, bootstrap_samples) {
  null = 0
  n_bootstrap_samples = length(bootstrap_samples)
  observation = mean(a) - mean(b)
  se = sd(bootstrap_samples)/sqrt(n_bootstrap_samples)
  t = (observation - null) / se
  df = n_bootstrap_samples - 1
  p_value = (1 - pt(t, df=df)) * 2
  return (p_value)
}

p_value_bootstrap_sample = p_value_bootstrap_diff_means_t_test(chick_c, chick_m,
                                                               bootstrap_samples
                                                               )

#### c) 

confidence_interval_diff_means_t <- function(a, b, p=0.95) {
  observation = mean(a) - mean(b)
  n_a = length(a)
  n_b = length(b)
  se = sqrt((sd(a) ^ 2) / n_a + (sd(b) ^ 2) / n_b)
  df = min(n_a, n_b)
  t_star = qt((1-p)/2, df=df) * (-1)
  lower = observation - t_star * se
  upper = observation + t_star * se
  
  return (c(lower, upper))
}

confidence_interval_original_sample = confidence_interval_diff_means_t(chick_c,
                                                                       chick_m)

confidence_interval_bootstrap_diff_means_t <- function(a, b, bootstrap_samples
                                                       , p=0.95) {
  observation = mean(a) - mean(b)
  n_bootstrap_samples = length(bootstrap_samples)
  se = sd(bootstrap_samples)/sqrt(n_bootstrap_samples)
  df = n_bootstrap_samples - 1
  t_star = qt((1-p)/2, df=df) * (-1)
  lower = observation - t_star * se
  upper = observation + t_star * se
  
  return (c(lower, upper))
}

confidence_interval_bootstrap_sample = confidence_interval_bootstrap_diff_means_t(chick_c, chick_m, bootstrap_samples)


######## q8

#### a) 

Diet = Diet["pre.weight"] - Diet["weight6weeks"]
Diet$Diet=as.factor(Diet$Diet)


ggplot(Diet, aes(x = Diet, y=loss)) +
  geom_boxplot() + 
  ggtitle("box plot") + 
  theme(plot.title = element_text(hjust = 0.5))

#### b) 

result = aov(loss ~ Diet, data = Diet)
summary(result)

#### d) 
TukeyHSD(result)
