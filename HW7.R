######## q7
### a)

df_total = data.frame(
  'Surgery' = c('Survived', 'Survived', 'Died', 'Died'),
  'Hospital' = c('A', 'B', 'A', 'B'),
  'Freq' = c(2037, 784, 63, 16)
)

fit1 <- glm(Surgery ~ Hospital, weights = Freq, data = df_total, family = binomial(logit))
summary(fit1)

odds_ratio = exp(fit1$coefficient[2])
odds_ratio_se = summary(fit1)$coefficient[2,2]
or_lower <- exp( fit1$coefficient[2] - qnorm(.975) *  odds_ratio_se)
or_upper <- exp( fit1$coefficient[2] + qnorm(.975) * odds_ratio_se )

### b)

df_partition = data.frame(
  'Surgery' = c('Survived', 'Survived', 'Died', 'Died', 'Survived', 'Survived', 'Died', 'Died'),
  'Hospital' = c('A', 'B', 'A', 'B', 'A', 'B', 'A', 'B'),
  'Condition' = c('Good', 'Good', 'Good', 'Good', 'Poor', 'Poor', 'Poor', 'Poor'),
  'Freq' = c(594, 592, 6, 8, 1443, 192, 57, 8)
)

fit2 <- glm(Surgery ~ Hospital + Condition, weights = Freq, data = df_partition, family = binomial(logit))
summary(fit2)

odds_ratio = exp(fit2$coefficient[2])
odds_ratio_se = summary(fit2)$coefficient[2,2]
or_lower <- exp( fit2$coefficient[2] - qnorm(.975) *  odds_ratio_se)
or_upper <- exp( fit2$coefficient[2] + qnorm(.975) * odds_ratio_se )

######## q8
### a)
df = Data
sample_size <- floor(2/3 * nrow(df))
train_index <- sample(seq_len(nrow(df)), size = sample_size)

train <- df[train_index, ]
test <- df[-train_index, ]

model <- glm(Response ~., family=binomial, data=train)
summary(model)

### c)


### d)
library(pROC)
prob=predict(model, type=c("response"))
train$prob=prob
g <- roc(Response ~ prob, data = train)
plot(g) 

### e)
qqnorm(model$residuals)
qqline(model$residuals)

plot(model$residuals ~ model$fitted)

plot(model$residuals)
