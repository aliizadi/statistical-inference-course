######## q5
## c)
dbinom(100, 100, 0.995)
sum(dbinom(0:98, 100, 0.995))

######## q7
## a)
exams_mean <- 462
exams_std <- 119
projects_mean <- 584
projects_std <- 151

student_exam <- 620
student_project <- 670

## b)
z_score_exam <- (student_exam - exams_mean) / exams_std
z_score_project <- (student_project - projects_mean) / projects_std

## c)
x <- seq(-3, 3, by = 0.01)
y <- dnorm(x, mean = 0, sd = 1.0)
plot(x, y, main = "Standard Normal Distribution", col = "blue")

norm_z_score_exam = dnorm(z_score_exam, 0, 1)
norm_z_score_project = dnorm(z_score_project, 0, 1)

text(
  x = c(z_score_exam, z_score_project),
  y = c(norm_z_score_exam, norm_z_score_project),
  labels = c("exam", "project")
)

abline(v = z_score_exam, h = norm_z_score_exam, lty = 2)
abline(v = z_score_project, h = norm_z_score_project, lty = 2)

## f)
exam_percentile_score <- pnorm(z_score_exam, 0, 1)
project_percentile_score <- pnorm(z_score_project, 0, 1)
## g)
percentage_of_students_better_in_exam <- 1 - exam_percentile_score
## h)
percentage_of_students_better_in_project <-
  1 - project_percentile_score

## i)
exam_scores <-
  c(57,
    66,
    69,
    71,
    72,
    73,
    74,
    77,
    78,
    78,
    79,
    79,
    81,
    81,
    82,
    83,
    83,
    88,
    89,
    94)

boxplot(exam_scores)
qqnorm(exam_scores)



######## q9
## b)


first_vpn_connection_likelihood = 0.1
second_vpn_connection_likelihood = 0.3

trials_first = c()
trials_second = c()

for (m in 1:100000) {
  first_vpn_belief <- 0.5
  second_vpn_belief <- 0.5
  for (i in 1:100) {
    if (runif(1) < 0.5) {
      first_vpn_belief = (first_vpn_connection_likelihood * first_vpn_belief) / (
        first_vpn_connection_likelihood * first_vpn_belief + second_vpn_connection_likelihood * second_vpn_belief
      )
    } else{
      second_vpn_belief = (second_vpn_connection_likelihood * second_vpn_belief) / (
        first_vpn_connection_likelihood * first_vpn_belief + second_vpn_connection_likelihood * second_vpn_belief
      )
    }
    
    if (first_vpn_belief >= 0.9 | second_vpn_belief >= 0.9) {
      if (first_vpn_belief >= 0.9)
        trials_first = c(trials_first, i)
      if (second_vpn_belief >= 0.9)
        trials_second = c(trials_second, i)
      break
    }
  }
}

mean(trials_second)

######## q10
library(ggplot2)
df = StudentsPerformance
## a)
ggplot(df, aes(x = writing_score, color=gender)) +
  geom_histogram(binwidth = 2, fill="white") + 
  geom_vline(aes(xintercept=mean(df$writing_score[gender == "male"])), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(df$writing_score[gender == "female"])), linetype="dashed", size=1) + 
  ggtitle("writing_score histogram plot") + 
  theme(plot.title = element_text(hjust = 0.5))

## b)
ggplot(df, aes(sample = reading_score, color=gender)) +
  stat_qq() +
  stat_qq_line() + 
  ggtitle("reading_score qq plot") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(sample = writing_score, color=gender)) +
  stat_qq() +
  stat_qq_line() + 
  ggtitle("writing_score qq plot") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(sample = math_score, color=gender)) +
  stat_qq() +
  stat_qq_line() + 
  ggtitle("math_score qq plot") + 
  theme(plot.title = element_text(hjust = 0.5))

## c)
reading_score_mu_hat = mean(df$reading_score)
reading_score_std_hat = sd(df$reading_score)
writing_score_mu_hat = mean(df$writing_score)
writing_score_std_hat = sd(df$writing_score)
math_score_mu_hat = mean(df$math_score)
math_score_std_hat = sd(df$math_score)

ggplot(df, aes(x = reading_score)) +
  geom_histogram(aes(y =..density..), binwidth = 2, fill="yellow", color="black") +
  stat_function(fun = dnorm, args = list(mean = reading_score_mu_hat,
                                         sd = reading_score_std_hat)) + 
  ggtitle("histogram with 2.5% and 97.5% percentile") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(aes(xintercept=qnorm(0.025, reading_score_mu_hat,
                                  reading_score_std_hat)),
             linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=qnorm(0.975, reading_score_mu_hat,
                                  reading_score_std_hat)),
             linetype="dashed", size=1)

ggplot(df, aes(x = writing_score)) +
  geom_histogram(aes(y =..density..), binwidth = 2, fill="yellow", color="black") +
  stat_function(fun = dnorm, args = list(mean = writing_score_mu_hat,
                                         sd = writing_score_std_hat)) + 
  ggtitle("histogram with 2.5% and 97.5% percentile") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(aes(xintercept=qnorm(0.025, writing_score_mu_hat,
                                  writing_score_std_hat)),
             linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=qnorm(0.975, writing_score_mu_hat,
                                  writing_score_std_hat)),
             linetype="dashed", size=1)

ggplot(df, aes(x = math_score)) +
  geom_histogram(aes(y =..density..), binwidth = 2, fill="yellow", color="black") +
  stat_function(fun = dnorm, args = list(mean = math_score_mu_hat,
                                         sd = math_score_std_hat)) + 
  ggtitle("histogram with 2.5% and 97.5% percentile") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(aes(xintercept=qnorm(0.025, math_score_mu_hat,
                                  math_score_std_hat)),
             linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=qnorm(0.975, math_score_mu_hat,
                                  math_score_std_hat)),
             linetype="dashed", size=1)

## d)
ggplot(df, aes(x = race, y=math_score , color=gender)) +
  geom_boxplot() + 
  ggtitle("box plot math_score for each race group seperated by gender") + 
    theme(plot.title = element_text(hjust = 0.5))

## e) 
df$mean = rowMeans(df[, c("reading_score", "math_score", "writing_score")], 
                   na.rm=TRUE) 

ggplot(df, aes(x = parents_education, y = mean, fill=parents_education)) + 
  geom_bar(stat = "summary", fun = "mean") + 
  ggtitle("average score of studets score grouping by their parents education ") + 
  theme(plot.title = element_text(hjust = 0.5))
