######## q5
men <- c(128.35, 160.34, 133.74, 138.12, 91.00, 97.43, 128.58, 148.78,
                 150.65, 110.96, 135.7, 118.77, 147.1, 107.2, 122.46, 129.36,
                 158.14, 102.72, 136.59, 146.02, 105.88, 111.24, 131.22, 124.6,
                 137.85, 136.46, 145.31, 166.71, 158.66, 108.63, 103.11, 149.29)

women <- c(116.62, 137.15, 106.07, 172.58, 151.33, 98.73, 136.11, 149.9, 140.8,
           98.58, 158.4, 97.97, 117.99, 126.53, 128.67, 126.57, 124.3, 120.39,
           150.08, 143.05, 130.18, 108.04, 136.39, 124.94, 136.86, 143.03,
           128.58, 142.51, 151.68, 120.94)

#### a)
n_men = length(men)
n_women = length(women)
diff_mean <- mean(men) - mean(women)
diff_standard_error <- sqrt((sd(men) ^ 2) / n_men + (sd(women) ^ 2) / n_women)
z = -qnorm((1-0.95)/2, 0, 1)

upper = diff_mean + z * diff_standard_error
lower = diff_mean - z * diff_standard_error

######## q7

## a)

pwr <- function(ua, n=50, alpha=0.05) {
  se = 5.6 / sqrt(n)
  z_alpha = qnorm(alpha, 0, 1)
  y = z_alpha * se + 28
  z = (y - ua) / se
  return(pnorm(z, 0, 1))
}
us = 22:27
pwrs_1 = c(pwr(22), pwr(23), pwr(24), pwr(25), pwr(26), pwr(27))
plot(us, pwrs_1, type='l')

## b)
pwrs_2 = c(pwr(22, alpha=0.01), pwr(23, alpha=0.01), pwr(24, alpha=0.01),
           pwr(25, alpha=0.01), pwr(26, alpha=0.01), pwr(27, alpha=0.01))

plot(us, pwrs_1, type="o", col="blue", pch="o", ylab="y" )

points(us, pwrs_2, col="red", pch="*")
lines(us, pwrs_2, col="red")

## c)
pwrs_3 = c(pwr(22, n=20), pwr(23, n=20), pwr(24, n=20),
           pwr(25, n=20), pwr(26, n=20), pwr(27, n=20))

plot(us, pwrs_1, type="o", col="blue", pch="o", ylab="y" )

points(us, pwrs_3, col="red", pch="*")
lines(us, pwrs_3, col="red")
