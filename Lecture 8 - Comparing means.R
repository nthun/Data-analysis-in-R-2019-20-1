# Lecture 9 - Comparing means: t-test and ANOVA

install.packages("sjstats")
install.packages("psych")

library(tidyverse)
library(broom)
library(psych)

# Doing t-test in R
# Single sample t-test
# Using the attitude dataset
?attitude

ggplot(attitude) +
    aes(x = rating) +
    geom_histogram(binwidth = 5) +
    geom_vline(aes(xintercept = 50), size = 1.5, color = "red", linetype = "dashed")

# Check if there is a significant difference from 50 to any direction
single_result <- t.test(attitude$rating, mu = 50)
tidy(single_result)

# Check if rating is significantly higher than 50
t.test(attitude$rating, mu = 50, alternative = "greater")

# Check if rating is significantly lower than 50
t.test(attitude$rating, mu = 50, alternative = "less")


# Calculate effect size
# Calculate r
psych::t2r(6.584486, 29)
# Calculate Cohen's d
psych::t2d(6.584486, 29)

# Paired t-test
# We are using the sleep dataset
?sleep

# Plot the data
ggplot(sleep) +
    aes(x = extra, fill = group) +
    geom_density(alpha = .6)

ggplot(sleep) +
    aes(x = group, y = extra, fill = group) +
    geom_boxplot()

# Create table
sleep %>% 
    group_by(group) %>% 
    summarise(extra_mean = mean(extra),
              extra_sd = sd(extra),
              n = n(),
              extra_se = extra_sd/sqrt(n))

# Running the paired t-test
paired_result <- t.test(extra ~ group, paired = TRUE, data = sleep)

# You can tidy up the results as usual using the broom::tidy(), and calculate effect size metrics at the same time
paired_result %>% 
    tidy() %>% 
    summarise(d = t2d(t = statistic, n = parameter + 1),
              r = t2r(t = statistic, df = parameter))

# Independent samples t-test
# Using the ToothGrowth dataset
?ToothGrowth

ggplot(ToothGrowth) +
    aes(x = supp, y = len, fill = supp, group = supp) +
    geom_rug(sides = "lr", aes(color = supp), size = 1.2) +
    geom_boxplot() +
    geom_jitter()


# Check the assumption of variance homogeneity (F-test)
# Non-significant effect shows that the variances are not different in the two groups
var.test(len ~ supp, data = ToothGrowth)

independent_result <- t.test(len ~ supp, var.equal = TRUE, data = ToothGrowth)
tidy(independent_result)

# Welch two sample t-test
independent_result <- t.test(len ~ supp, var.equal = TRUE, data = ToothGrowth)

# Get means and sd-s 
ToothGrowth %>% 
    group_by(supp) %>% 
    summarise(len_mean = mean(len) %>% round(2),
              len_sd = sd(len) %>% round(2),
              len_se = (len_sd/sqrt(n())) %>% round(2),
              n = n())

# Calculating effect sizes
# Get an r value
psych::t2r(t = 1.9153, df = 58)
# Get a Cohen's d value
psych::t2d(t = 1.9153, n = 59)


# Using ANOVA to compare three groups
# Using the plantgrowth dataset
?PlantGrowth
ggplot(PlantGrowth) +
    aes(x = weight, fill = group) +
    geom_density(alpha = .6)

ggplot(PlantGrowth) +
    aes(x = group, y = weight, fill = group) +
    geom_boxplot()

# Checking hoogeneity of variance
bartlett.test(weight ~ group, data = PlantGrowth)

# Run the ANOVA
anova_result <- aov(weight ~ group, data = PlantGrowth)
summary(anova_result)

# But of course, tidy() is better
tidy(anova_result)

PlantGrowth %>% 
    group_by(group) %>% 
    summarise(weight_mean = mean(weight),
              weight_sd = sd(weight))

sjstats::eta_sq(anova_result)

## Doing the same using linear regression
lm_result <- lm(weight ~ group, data = PlantGrowth)
summary(lm_result)
tidy(lm_result)

# To get the group effect without the pairwise comparisons, use anova()
# As you can see, anova can lead to false assumptions because it can answer to a different question
anova(lm_result)


# Post-hoc tests for one categorical predictor
ggplot(PlantGrowth) +
  aes(x = group, y = weight, fill = group) +
  geom_boxplot()

anova_model <- aov(weight ~ group, data = PlantGrowth)
TukeyHSD(anova_model)

# Btw, you can add significance to your plots using ggsignif::geom_signif()
install.packages("ggsignif")
library(ggsignif)

ggplot(PlantGrowth) +
  aes(x = group, y = weight, fill = group) +
  geom_boxplot() +
  geom_signif(comparisons = list(c("trt1", "trt2")), 
              map_signif_level = TRUE)

# Use another post-hoc test using the multcomp package
library(multcomp)
post1 <- multcomp::glht(anova_model, linfct = mcp(group = "Dunnett"), base = 1)
summary(post1)
confint(post1)

# Use build in contrasts
# This one is used for comparing the control condition to treatment conditions
contrasts(PlantGrowth$group) <- contr.treatment(3, base = 1)

# But maybe it is better use weights to define the contrasts.
# This will compare the control condition to the treatment conditions
contrast1 <- c(-2,1,1)

# This compares the two treatment conditions
contrast2 <- c(0,-1,1)

contrasts(PlantGrowth$group) <- cbind(contrast1, contrast2)

# You can check that the contrasts are now tied to the variable
PlantGrowth$group

# Let's run the ANOVA
plant_model <- aov(weight ~ group, data = PlantGrowth)

# Test the significance of the contasts by using summary.lm()
# this reveals that there is not sig. difference between the treatments and the control conditions, but there is a difference between treatments
summary.lm(plant_model)

# Using polynomial contrasts (trend analysis)
contrasts(PlantGrowth$group) <- contr.poly(3)
plant_poly <- aov(weight ~ group, data = PlantGrowth)

# the quadratic trend is significant, because the category in the middle is smaller than the ones in the center
summary.lm(plant_poly)

# Lecture 10: Factorial ANOVA, contrasts, post-hoc tests, ANCOVA, repeated-measures ANOVA

library(ggfortify)

# Factorial ANOVA
# We have several categorical predictors
# Note that you can use the unteraction() function in the group to make subgroups
ggplot(ToothGrowth) +
  aes(x = dose, y = len, fill = supp, group = interaction(supp, dose)) +
  geom_boxplot()

ToothGrowth <-
  ToothGrowth %>% 
  as_tibble() %>% 
  mutate(dose = as.factor(dose))

# Post-hoc tests
tooth_model <- aov(len ~ dose*supp, data = ToothGrowth)
summary(tooth_model)

# Investigate post hoc-compaisons for all levels, and tidy-up
TukeyHSD(tooth_model) %>% tidy()

# Check residual diagnostics
autoplot(tooth_model, which = 1:6)


# Repeated measures ANOVA
# We are going to use the ez package for the repeated-measures ANOVA
install.packages("ez")
library(ez)

mtept
?mtept
# Prepare data (tidy up)
df <- 
  multcomp::mtept %>% 
  rownames_to_column("id") %>% 
  gather(time, value, -treatment, -id) %>% 
  mutate(time = str_sub(time, 2)) %>% 
  as_tibble()


# Just plot data for each person
ggplot(df) + 
  aes(x = time, y = value) + 
  geom_point() +
  facet_wrap(~id, scales = "free_y")

# Perform repeated measures ANOVA
repeated_anova <-
  ezANOVA(
    dv = .(value),
    wid = .(id),
    within = .(time),
    data = df
  )

repeated_anova

# We can also include between subject factors, so it will be a mixed ANOVA
mixed_anova <-
  ezANOVA(
    dv = .(value),
    wid = .(id),
    within = .(time),
    between = .(treatment) ,
    data = df,
    type = 3
  )

mixed_anova



### PERMUTATION TESTING
# Check if the difference between 
point_diff <- lm(len ~ supp, data = ToothGrowth)

summary(point_diff)

install.packages("infer")
library(infer)

obs_diff <- 
  ToothGrowth %>% 
  group_by(supp) %>% 
  summarise(len = mean(len)) %>% 
  spread(supp, len) %>% 
  mutate(diff = OJ - VC) %>% 
  pull(diff)

null_sim_distn <-
  ToothGrowth %>% 
  mutate(supp = as.factor(supp)) %>% 
  specify(len ~ supp) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("VC", "OJ"))

null_sim_distn %>% 
  visualize() +
  shade_p_value(obs_stat = obs_diff, direction = "greater")

get_p_value(null_sim_distn, obs_stat = obs_diff, direction = "greater")

get_confidence_interval(null_sim_distn)



# PLOTS FOR THE SLIDES --------------------------------------------------------------

# Plots for the slides
# Generate some random data for example plots
set.seed(1) # Set random seed for reproducibility
stud <- tibble(x = rt(500, 6))

single_sample <- tibble(x = rnorm(500, 1, 1))

paired_sample <- 
  tibble(sample = "x", 
         value = rnorm(500, 2, 1)) %>% 
  bind_rows(tibble(sample = "y", 
                   value = rnorm(500, 0, 1)))

independent_sample <- 
  tibble(sample = "x", 
         value = rnorm(500, 2, 1)) %>% 
  bind_rows(tibble(sample = "y", 
                   value = rnorm(250, 0, 1)))

# Showing the t-distribution (histogram) against normal distribution (density)
ggplot(stud) +
  aes(x = x) +
  geom_histogram(aes(y = ..density..), binwidth = .5) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(stud$x), sd = sd(stud$x)),
                lwd = 1.5, 
                col = "red") +
  labs(x = NULL, y = NULL)

# Plot different types of t-tests
# Single sample t-test
ggplot(single_sample) +
  aes(x = x) +
  geom_histogram(fill = "lightblue") +
  geom_vline(aes(xintercept = 0), size = 1.5, color = "red", linetype = "dashed") +
  labs(x = NULL, y = NULL)

# Paired sample t-test
ggplot(paired_sample) +
  aes(x = value, fill = sample) +
  geom_histogram(data = paired_sample %>% filter(sample == "x"),fill = "salmon", alpha = .8) +
  geom_histogram(data = paired_sample %>% filter(sample == "y"),fill = "skyblue", alpha = .8) +
  labs(x = NULL, y = NULL)

# Independent samples t-test
ggplot(independent_sample) +
  aes(x = value, fill = sample) +
  geom_histogram(data = independent_sample %>% filter(sample == "x"),fill = "salmon", alpha = .8) +
  geom_histogram(data = independent_sample %>% filter(sample == "y"),fill = "skyblue", alpha = .8) +
  labs(x = NULL, y = NULL)


# ANOVA
# Creating plots to show how ANOVA works
anova_df <- 
  bind_rows(tibble(group = "x", value = rnorm(5, 5, 3)),
            tibble(group = "y", value = rnorm(5, 10, 2)),
            tibble(group = "z", value = rnorm(5, 17, 4))) %>% 
  mutate(id = row_number(),
         grandmean = mean(value)) %>% 
  group_by(group) %>% 
  mutate(groupmean = mean(value),
         xmin = min(id),
         xmax = max(id))

# SSt
anova_df %>% 
  ggplot() +
  aes(x = id, y = value) +
  geom_hline(aes(yintercept = groupmean), size = 1.5) +
  geom_hline(aes(yintercept = grandmean), linetype = "dashed", size = 1.5, color = "red") +
  geom_segment(aes(yend = value, xend = id, y = grandmean), color = "blue", size = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~group, scales = "free_x") +
  ggtitle("Difference between the grand mean (baseline model)\nand the observed values", subtitle = "Total variance") +
  theme(plot.title=element_text(size = 18, face = "bold"),
        plot.subtitle=element_text(size = 16))

# SSr
anova_df %>% 
  ggplot() +
  aes(x = id, y = value) +
  geom_hline(aes(yintercept = groupmean), size = 1.5) +
  geom_hline(aes(yintercept = grandmean), linetype = "dashed", size = 1.5, color = "red") +
  geom_segment(aes(yend = value, xend = id, y = groupmean), color = "red", size = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~group, scales = "free_x") +
  ggtitle("Difference between the model (group means)\nand the observed values", subtitle = "Unexplained variance") +
  theme(plot.title=element_text(size = 18, face = "bold"),
        plot.subtitle=element_text(size = 16))

# SSm
anova_df %>% 
  ggplot() +
  aes(x = id, y = value) +
  geom_hline(aes(yintercept = groupmean), size = 1.5) +
  geom_hline(aes(yintercept = grandmean), linetype = "dashed", size = 1.5, color = "red") +
  geom_segment(aes(yend = groupmean, xend = id, y = grandmean), color = "purple", size = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~group, scales = "free_x") +
  ggtitle("Difference between the model (group means)\nand the baseline (grand mean)", subtitle = "Model improvement") +
  theme(plot.title=element_text(size = 18, face = "bold"),
        plot.subtitle=element_text(size = 16))
