require('ggplot2')
library(dplyr)
library(ggExtra)
library(GGally)
library(ggcorrplot)
library("scatterplot3d")

######## q0
### b)
any(is.na(StudentsPerformance))

######## q1
### a)
ggplot(StudentsPerformance, aes(x = G1)) +
  geom_histogram(aes(y =..density..), binwidth = 1, fill="yellow", color="black") +
  geom_density() + 
  ggtitle("G1 histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

### b)
quantile(StudentsPerformance$G1)

ggplot(StudentsPerformance, aes(sample = G1)) +
  stat_qq() +
  stat_qq_line() + 
  ggtitle("G1 qq plot comparing with normal distribution") + 
  theme(plot.title = element_text(hjust = 0.5))

### c)
(mean(StudentsPerformance$G1) - median(StudentsPerformance$G1)) / 
  sd(StudentsPerformance$G1)

### d)
ggplot(StudentsPerformance, aes(x = G1)) +
  geom_boxplot() + 
  ggtitle("box plot G1") + 
  theme(plot.title = element_text(hjust = 0.5))

### e)
mean(StudentsPerformance$G1)
median(StudentsPerformance$G1)
var(StudentsPerformance$G1)
sd(StudentsPerformance$G1)

### f)
ggplot(StudentsPerformance, aes(x = G1)) +
  geom_density() + 
  geom_vline(aes(xintercept=mean(G1)), color="blue", size=1) +
  geom_vline(aes(xintercept=median(G1)), color="red", size=1) + 
  ggtitle("G1 density plot with blue mean and red median") + 
  theme(plot.title = element_text(hjust = 0.5))

### g)
mean = mean(StudentsPerformance$G1)
G1 = StudentsPerformance$G1
frequencies = c(sum((mean - mean/2) > G1),
               sum(((mean - mean/2) <= G1) & (G1 < mean)),
               sum(((mean + mean/2) > G1) & (G1 >= mean)),
               sum((mean + mean/2) <= G1))


categories_names = c('1', '2', '3', '4')

data = data.frame(
  value=frequencies,
  names=categories_names
)

data <- data %>% 
  arrange(desc(names)) %>%
  mutate(prop = round(value / sum(data$value) *100, digits=1)) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(data, aes(x = "", y = prop, fill = names)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = ypos, label = prop), color = "white", size=6) + 
  theme_void() 

### h)
bp = boxplot(G1)
bp

######## q2
### a)
sex = StudentsPerformance$sex
freq = table(sex)
percentage = table(sex) / sum(table(sex))

### b)
df  = as.data.frame(percentage) 
ggplot(data=df, aes(x=sex, y=Freq, fill=sex)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)


### c)
df = as.data.frame(freq) 
df = df[order(df$Freq),]

ggplot(data=df, aes(x=sex, y=Freq, fill=sex)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Freq), vjust=1.6, color="black", size=3.5) + 
  coord_flip()


### d)
ggplot(StudentsPerformance, aes(x=sex, y=G1, fill=sex)) +
  geom_violin()


######## q3
### b)
ggplot(data = StudentsPerformance, aes(x = G1, y = G2)) +
  geom_point()

### c)
cor(StudentsPerformance$G1, StudentsPerformance$G2)


### e)
cor.test(StudentsPerformance$G1, StudentsPerformance$G2)

### f)
ggplot(data = StudentsPerformance, aes(x = G1, y = G2, color=sex)) +
  geom_point()

### g)
p = ggplot(StudentsPerformance, aes(G1, G2)) +
  geom_point() + 
  geom_hex(bins = 10) + 
  geom_smooth() + 
  scale_fill_viridis_c() + 
  ggtitle("G1 vs G2 Hexbin plot") + 
  theme(plot.title = element_text(hjust = 0.5))

ggMarginal(p, type="histogram", size=5)
  
### h)
ggplot(StudentsPerformance, aes(x=G1, y=G2) ) +
  geom_density_2d() + 
  ggtitle("G1 vs G2 2d density plot") + 
  theme(plot.title = element_text(hjust = 0.5))

######## q4
### a)
numerical = c('age', 'goout', 'studytime', 'failures', 'health', 'absences', 'G1', 'G2', 'G3')
ggpairs(StudentsPerformance[, numerical],
        upper = list(continuous = "points", combo = "dot_no_facet"))

ggpairs(StudentsPerformance[, c('G1', 'G2', 'G3', 'sex', 'Fjob', 'Mjob', 'internet', 'romantic', 'school')])

### b)
corr = round(cor(StudentsPerformance[, numerical]), 1)
p_values = cor_pmat(StudentsPerformance[, numerical])
ggcorrplot(corr, hc.order = TRUE, outline.col = "white", type = "lower", lab = TRUE, p.mat = p_values)

### c)
colors <- c("#999999", "#E69F00")
colors <- colors[as.numeric(StudentsPerformance$sex)]
sp = scatterplot3d(StudentsPerformance[,c('G1', 'G2', 'G3')], pch = 16, color=colors)
legend(sp$xyz.convert(24, 10, 4.5), legend = levels(StudentsPerformance$sex),
      col =  colors, pch = 16)
######## q5
### a)
table(StudentsPerformance[,c('sex', 'romantic')])

### b)
ggplot(StudentsPerformance, aes(x = sex, fill = romantic)) + 
  geom_bar(position = "dodge")

### c)
ggplot(StudentsPerformance, aes(x = sex, fill = romantic)) + 
  geom_bar(position = "stack") + 

### d)
ggplot(data = StudentsPerformance, aes(x = sex, fill = romantic)) +
  geom_bar(position = "fill") + ylab("proportion") +
  stat_count(geom = "text", 
             aes(label = stat(round((..count..)/sum(..count..), digits = 2))),
             position=position_fill(vjust=0.5), colour="black")



######## q6
n_samples = 35
population = StudentsPerformance$G1
samples = sample_n(StudentsPerformance, n_samples)$G2

population_mean = mean(population)
population_sd = sd(population)

samples_mean = mean(samples)
samples_sd = sd(samples)


### a)
upper = samples_mean + qnorm((1-0.95)/2, lower.tail = FALSE) * population_sd / sqrt(n_samples)
lower = samples_mean - qnorm((1-0.95)/2, lower.tail = FALSE) * population_sd / sqrt(n_samples)


### c)
df = data.frame(
  G1 = samples
)
ggplot(df, aes(x = G1)) +
  geom_histogram(aes(y =..density..), binwidth = 2, fill="yellow", color="black") +
  stat_function(fun = dnorm, args = list(mean = samples_mean,
                                         sd = samples_sd)) + 
  ggtitle("histogram 95% mean confidence interval") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(aes(xintercept=lower),
             linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=upper),
             linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=samples_mean),
             linetype="dashed", size=1)

### d)
se = population_sd / sqrt(n_samples)
z = (samples_mean - population_mean) / se
p_value = pnorm(z, lower.tail = FALSE)

######## q7
### a)

n_samples = 25
samples = sample_n(StudentsPerformance, n_samples)

G1 = samples$G1
G2 = samples$G2

diff = G1 - G2
u_diff = mean(diff)

population_sd = sd(StudentsPerformance$G1 - StudentsPerformance$G2)
se = population_sd / sqrt(n_samples)

t = u_diff / se
df = n_samples - 1

p_value = 2 * pt(t, df=df)


### b)
n_samples = 100

G1 = sample_n(StudentsPerformance, n_samples)$G1
G2 = sample_n(StudentsPerformance, n_samples)$G2
observation = mean(G1) - mean(G2)

n_a = length(G1)
n_b = length(G2)
se = sqrt((sd(StudentsPerformance$G1) ^ 2) / n_a + (sd(StudentsPerformance$G1) ^ 2) / n_b)
t = (observation - 0) / se
df = min(n_a, n_b)
p_value = pt(t, df=df) * 2


upper = observation + qt((1-0.95)/2, df=df, lower.tail = FALSE) * se
lower = observation - qt((1-0.95)/2, df=df, lower.tail = FALSE) * se

######## q8
### a)
n_samples = 20
samples = sample_n(StudentsPerformance, n_samples)$absences

n_bootstrap = 200
bootstrap_samples <- c()

for(i in 1:n_bootstrap){
  bootstrap_a <- sample(1:n_samples, n_samples, replace=TRUE) 
  mean <- mean(samples[bootstrap_a])
  bootstrap_samples <- c(bootstrap_samples, mean) 
}

quantile(bootstrap_samples, probs=c(0.025, 0.975))

### b)

samples_mean = mean(samples)
bootstrap_sd = sd(boostrap_samples)
upper = samples_mean + qnorm((1-0.95)/2, lower.tail = FALSE) * bootstrap_sd / sqrt(n_bootstrap)
lower = samples_mean - qnorm((1-0.95)/2, lower.tail = FALSE) * bootstrap_sd / sqrt(n_bootstrap)

hist(bootstrap_samples)

######## q9

StudentsPerformance$GPA = (StudentsPerformance$G1 + StudentsPerformance$G2 +
                             StudentsPerformance$G3) / 3 

StudentsPerformance$failures=as.factor(StudentsPerformance$failures)

ggplot(StudentsPerformance, aes(x = failures, y=GPA)) +
  geom_boxplot() + 
  ggtitle("box plot") + 
  theme(plot.title = element_text(hjust = 0.5))

result = aov(GPA ~ failures, data = StudentsPerformance)
summary(result)
