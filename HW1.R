## install.packages('hexbin', dependencies=TRUE, repos='http://cran.rstudio.com/')
################### a)
positions_type <- c("UI Developers", "Backend Developers", "management", "HR",
                   "HSE")
population <- c(8, 12, 4, 3, 3)

################### b) bar plot
barplot(population, names.arg=positions_type ,xlab="type",ylab="# of employees",
        main="distribution of position types in a software company")

################### c) box plots
ui_developers <- c(75000, 25000, 48000, 42000, 35200, 45000, 23000, 45500)
back_end_developers <- c(20000, 80000, 36000, 46300, 41000, 43000, 22000, 37000,
                        39000, 43500, 69000, 5000)
management <- c(80000, 67000, 56000, 82000)
HR <- c(45000, 39000, 30000)
HSE <- c(12000, 25000, 31500)
boxplot(ui_developers, back_end_developers, management, HR, HSE, names = positions_type)

################### quantile and outliers
find_iqr <- function(data) {
    return(quantile(data, 3/4) - quantile(data, 1/4))
}

find_outliers <- function(data) {
  upper_whisker <- min(max(data), quantile(data, 3/4) + 1.5 * find_iqr(data))
  lower_whisker <- max(min(data), quantile(data, 1/4) - 1.5 * find_iqr(data))
  return(data[data < lower_whisker | data > upper_whisker])
}

quantile(ui_developers)
find_iqr(ui_developers)
find_outliers(ui_developers)

quantile(back_end_developers)
find_iqr(back_end_developers)
find_outliers(back_end_developers)

quantile(management)
find_iqr(management)
find_outliers(management)

quantile(HR)
find_iqr(HR)
find_outliers(HR)

quantile(HSE)
find_iqr(HSE)
find_outliers(HSE)

################### e) Histogram
hist(ui_developers, breaks=seq(2000,80000,l=8), prob=TRUE, main = lines(density(ui_developers)))
hist(back_end_developers, breaks=seq(0,80000,l=8), prob=TRUE, main = lines(density(back_end_developers)))
hist(management, breaks=seq(50000,90000,l=6), prob=TRUE, main = lines(density(management)))
hist(HR, breaks=seq(20000,60000,l=4), prob=TRUE, main = lines(density(HR)))
hist(HSE, breaks=seq(0,40000,l=8), prob=TRUE, main = lines(density(HSE)))

################### f) Pie Chart
very_high = length(c(back_end_developers[ back_end_developers > 50000],
                     ui_developers[ ui_developers > 50000],
                     management[ management > 50000],
                     HR[ HR > 50000],
                     HSE[ HSE > 50000]))

high = length(c(back_end_developers[ back_end_developers > 40000 &  back_end_developers <= 50000],
                ui_developers[ ui_developers > 40000 &  ui_developers <= 50000],
                management[ management > 40000 & management <= 50000],
                HR[ HR > 40000 & HR <= 50000],
                HSE[ HSE > 40000 & HSE <= 50000]))

middle = length(c(back_end_developers[ back_end_developers > 30000 &  back_end_developers <= 40000],
                ui_developers[ ui_developers > 30000 &  ui_developers <= 40000],
                management[ management > 30000 & management <= 40000],
                HR[ HR > 30000 & HR <= 40000],
                HSE[ HSE > 30000 & HSE <= 40000]))

low = length(c(back_end_developers[ back_end_developers > 20000 &  back_end_developers <= 30000],
                  ui_developers[ ui_developers > 20000 &  ui_developers <= 30000],
                  management[ management > 20000 & management <= 30000],
                  HR[ HR > 20000 & HR <= 30000],
                  HSE[ HSE > 20000 & HSE <= 30000]))

very_low = length(c(back_end_developers[ back_end_developers <= 20000],
                     ui_developers[ ui_developers <= 20000],
                     management[ management <= 20000],
                     HR[ HR <= 20000],
                     HSE[ HSE <= 20000]))

salary_count = c(very_high, high, middle, low, very_low)
salary_type = c("very high", "high", "middle", "low", "very low")
pct <- round(salary_count/sum(salary_count)*100)
lbls <- paste(salary_type, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(salary_count,labels = lbls, col=rainbow(length(salary_type)),
    main="Pie Chart of Saleries") 

################### g) mean, median, variance, standard deviation of back-end developers
mean(back_end_developers)
median(back_end_developers)
var(back_end_developers)
sd(back_end_developers)
