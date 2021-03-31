library(tidyverse)
library(ggplot2)
library(mosaic)
library(cowplot)

##challenge 1

library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-2021-datasets/master/IMDB-movies.csv"
d <- read_csv(f, col_names = TRUE) # creates a "tibble"
head(d)


#Use a one-line statement to filter the dataset to include just movies from 1920 to 1979 and movies that are between 1 and 3 hours long (runtimeMinutes < 240), and add a new column that codes the startYear into a new variable, decade (“20s,” “30s,” …“70s”). If you do this correctly, there should be 5651 movies remaining in the dataset.
d_filtered <- d %>% 
  filter(startYear >= 1920 & startYear <= 1979 & runtimeMinutes >= 60 & runtimeMinutes <= 180) %>%
  mutate(decade = paste0(substr(startYear,3,3),"0s"))
view(d_filtered)

#plot histograms of the distribution of runtimeMinutes for each decade.
ggplot(d_filtered, aes(x = runtimeMinutes)) + geom_histogram() + facet_wrap(vars(decade))

#calculate the population mean and population standard deviation in runtimeMinutes for each decade and save the results in a new dataframe, results.
results <- d_filtered %>%
  group_by(decade) %>%
  summarize(mean = mean(runtimeMinutes), sd = sd(runtimeMinutes), name = "population") 
view(results)

#Draw a single sample of 100 movies, without replacement, from each decade, calculate this single sample mean and sample standard deviation in runtimeMinutes, and estimate the SE around the population mean runtimeMinutes for each decade based on the standard deviation and sample size from these samples.
set.seed(100)
sample_results <- d_filtered %>% 
  group_by(decade) %>% 
  sample_n(100, replace = FALSE) %>% 
  summarize(mean = mean(runtimeMinutes), sd = sd(runtimeMinutes), name = "sample")
view(sample_results)

#Compare these estimates to the actual population mean runtimeMinutes for each decade and to the calculated SE in the population mean for samples of size 100 based on the population standard deviation for each decade.
p1 <- rbind(sample_results, results) %>% 
  ggplot(aes(x = name, y = mean)) + 
  geom_point() + 
  facet_wrap(~decade)
p2 <- rbind(sample_results, results) %>% 
  ggplot(aes(x = name, y = sd)) + 
  geom_point() + 
  facet_wrap(~decade)
plot_grid(p1, p2)

#Generate a sampling distribution of mean runtimeMinutes for each decade by [a] drawing 1000 samples of 100 movies from each decade and, for each sample, [b] calculating the mean runtimeMinutes and the standard deviation in runtimeMinutes
s <- data.frame()
for (i in 1:1000) {
  sample_d <- d_filtered %>% 
    group_by(decade) %>% 
    sample_n(100) %>% 
    summarize(mean = mean(runtimeMinutes), sd = sd(runtimeMinutes), sample_num = i)
  s <- rbind(s, sample_d)
}
view(s)

#Then, calculate the MEAN and the STANDARD DEVIATION of the sampling distribution of sample means for each decade (the former should be a very good estimate of the population mean, while the latter is another estimate of the standard error in the population mean for a particular sample size)
distribution_results <- s %>% 
  group_by(decade) %>% 
  summarize(mean = mean(mean), sd = sd(sd), name = "distribution")

#plot a histogram of the sampling distribution. What shape does it have?
ggplot(s, aes(x = mean)) + geom_histogram() + facet_wrap(~ decade)
#Anwser:normally distribution

#Finally, compare the standard error in runtimeMinutes for samples of size 100 from each decade [1] as estimated from your first sample of 100 movies, [2] as calculated from the known population standard deviations for each decade, and [3] as estimated from the sampling distribution of sample means.
standard_errors <- rbind(sample_results, results, distribution_results)
ggplot(standard_errors, aes(x = name, y = sd)) + 
  geom_point() + facet_wrap(~ decade)


##challenge 2

library(manipulate)
library(mosaic)
library(cowplot)

#What is the probability that she will see 9 or fewer bees arrive during any given session?
ppois(9, lambda=12)

#What is the probability that she will see no bees arrive in a session?
ppois(0, lambda=12)

#What is the probability that she will see exactly 5 bees arrive in a session?
ppois(5, lambda=12)-ppois(4, lambda=12)

#What is the probability that she will see more than 18 bees arrive in a session?
1-ppois(18, lambda=12)

#Plot the relevant Poisson mass function over the values in range 0 ≤ x ≤ 24.
data.frame(list(x = 0:24, prob = dpois(0:24, 12)))
ggplot(data.frame(list(x = 0:24, prob = dpois(0:24, 12))), aes(x, prob)) + 
  geom_bar(stat = "identity") + labs(x = "x", y = "prob")

#Using the rpois() function, simulate 1460 results from this distribution (i.e., 4 full years of morning monitoring sessions).
simulation = data.frame(num_of_bees = rpois(1460, 12))

#Plot the simulated results using the histogram() function from the {mosaic} package and use xlim() to set the horizontal limits to be from 0 to 24. How do your simulated results compare to the shape of the probability mass function you plotted above?
histogram(~num_of_bees, data = simulation, xlim = c(0, 24))


##challenge 3
df = read.csv("https://raw.githubusercontent.com/difiore/ada-2021-datasets/master/zombies.csv")

#Calculate the population mean and standard deviation for each quantitative random variable (height, weight, age, number of zombies killed, and years of education).
mean(df$height)
mean(df$weight)
mean(df$age)
mean(df$zombies_killed)
mean(df$years_of_education)

sd.p = function(x){
  sd_p <- sd(x)*sqrt((length(x)-1)/length(x))
  return(sd_p)
}

sd.p(df$height)
sd.p(df$weight)
sd.p(df$age)
sd.p(df$zombies_killed)
sd.p(df$years_of_education)

#Use {ggplot} and make boxplots of each of these variables by gender.
d_pivot <- pivot_longer(df,
                        c("height", "weight", "age", "zombies_killed", "years_of_education"),
                        names_to = "Variable",
                        values_to = "Value"
)

ggplot(data = d_pivot, aes(x = factor(gender), y = Value)) +
  geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
  facet_wrap(. ~ Variable, scales = "free") +
  labs(
    title = paste0("Variables by gender")) +
  xlab("Gender") +
  ylab("Frequency")

#Use {ggplot2} and make scatterplots of height and weight in relation to age, using different colored points for males versus females. Do these variables seem to be related? In what way?
plot.h <- ggplot(data = df, aes(x = age, y = height, color = factor(gender))) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method='lm', formula= y~x) 

plot.w <- ggplot(data = df, aes(x = age, y = weight, color = factor(gender))) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method='lm', formula= y~x) 

plot_grid(plot.h, plot.w, nrow = 2, rel_widths = c(1,1))


#Using histograms and Q-Q plots, check whether the quantitative variables seem to be drawn from a normal distribution. Which seem to be and which do not?
ggplot(d_pivot, aes(x = Value)) + 
  facet_wrap(. ~ Variable, scales = "free") +
  geom_histogram()

ggplot(d_pivot, aes(sample = Value)) + 
  facet_wrap(. ~ Variable, scales = "free") +
  stat_qq() 

#Now use the sample_n() function from {dplyr} to sample ONE subset of 50 zombie apocalypse survivors (without replacement) from this population and calculate the mean and sample standard deviation for each variable. Also estimate the standard error for each variable based on this sample and use that to construct a 95% confidence interval for each mean. You can use either the standard normal or a Student’s t distribution to derive the critical values needed to calculate the lower and upper limits of the CI.
zombie.rd.dist <- df %>% 
  select(height, weight, age, zombies_killed, years_of_education) %>% 
  sample_n(size = 50, replace = FALSE)

se = function(x, type="normal") {
  if (type == "normal") {
    se <- sd(x)/sqrt(length(x))
  }
  if (type == "poisson"){
    SE<- sqrt(mean(x)/length(x))
  }
  return(se)
}

zombie.sp <- zombie.rd.dist %>% 
  summarize_each(funs(mean, sd, se))


CI = function(x, y, na.rm = TRUE) {
  upper <- x + qnorm(1 - 0.05 / 2) * y
  lower <- x + qnorm(0.05 / 2) * y
  return(c(lower,upper))
}

sample_CI <- list("height_sample" = CI(zombie.sp$height_mean, zombie.sp$height_se),
                  "weight_sample" = CI(zombie.sp$weight_mean, zombie.sp$weight_se),
                  "age_sample" = CI(zombie.sp$weight_mean, zombie.sp$weight_se),
                  "zk_sample" = CI(zombie.sp$zombies_killed_mean, zombie.sp$zombies_killed_se),
                  "years_edu_sample" = CI(zombie.sp$years_of_education_mean, zombie.sp$years_of_education_se))

zombie.rd.dist <- zombie.rd.dist %>% summarize(mean_height = mean(height),
                                               mean_weight = mean(weight), 
                                               mean_age = mean(age), 
                                               mean_zk = mean(zombies_killed),
                                               mean_edu = mean(years_of_education))

#Then draw another 99 random samples of 50 zombie apocalypse survivors out and calculate the mean for each of the these samples. Together with the first sample you drew out, you now have a set of 100 means for each variable (each based on 50 observations), which constitutes a sampling distribution for each variable. 
for (i in 1:99){
  rep <- df %>% select(height, weight, age, zombies_killed, years_of_education) %>%
    sample_n(size = 50, replace = FALSE) %>% 
    summarize(mean_height = mean(height), mean_weight = mean(weight), 
              mean_age = mean(age), mean_zk = mean(zombies_killed), mean_edu = mean(years_of_education))
  zombie.rd.dist <- rbind(zombie.rd.dist, rep)
}

#What are the means and standard deviations of the sampling distribution for each variable?
(zombie.stats <- zombie.sp %>% summarize_each(funs(mean, sd)))

#Finally, construct an 95% confidence interval for each mean directly from the sampling distribution of sample means using the central 95% that distribution (i.e., by setting the lower and upper CI bounds to 2.5% and 97.5% of the way through that distribution).
ci_mean <- list("height_sdist" = CI(mean(zombie.rd.dist$mean_height), sd.p(zombie.rd.dist$mean_height)),
                "weight_sdist" = CI(mean(zombie.rd.dist$mean_weight), sd.p(zombie.rd.dist$mean_weight)),
                "age_sdist" = CI(mean(zombie.rd.dist$mean_age), sd.p(zombie.rd.dist$mean_age)),
                "zk_sdist" = CI(mean(zombie.rd.dist$mean_zk), sd.p(zombie.rd.dist$mean_zk)),
                "years_edu_sdist" = CI(mean(zombie.rd.dist$mean_zk), sd.p(zombie.rd.dist$mean_zk)))
ci_mean

#How do the standard deviations of the sampling distribution for each variable compare to the standard errors estimated from your first sample of size 50? What do sampling distributions for each variable mean look like? Are they normally distributed? What about for those variables that you concluded were not originally drawn from a normal distribution? How do the two 95% CIs you estimated compare to one another (i.e., the CI based on one sample and its estimated standard deviation versus the CI based on simulation)?

Samp.sd <- c(zombie.stats$mean_height_sd, zombie.stats$mean_weight_sd, zombie.stats$mean_age_sd, zombie.stats$mean_zk_sd, zombie.stats$mean_edu_sd)
Sample.se <- c(zombie.sp$height_se, zombie.sp$weight_se, zombie.sp$age_se, zombie.sp$zombies_killed_se, zombie.sp$years_of_education_se)

#answer: the standard deviations of the sampling distribution for each variable are similar to the standard errors estimated from the first sample of size 50

z <- pivot_longer(zombie.rd.dist,
                  c("mean_height", "mean_weight", "mean_age", "mean_zk", "mean_edu"),
                  names_to = "Variable",
                  values_to = "Value")

ggplot(z, aes(x = Value)) + 
  facet_wrap(~ Variable, scales = "free") +
  theme_bw(base_size = 12) +
  geom_histogram()

ggplot(z, aes(sample = Value)) + 
  facet_wrap(~ Variable, scales = "free") +
  stat_qq() 

