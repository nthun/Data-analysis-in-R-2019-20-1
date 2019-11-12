## Lecture 7 - Other types of regression
install.packages("tidyverse")
install.packages("titanic")
install.packages("AER")

library(tidyverse)
library(broom)
library(titanic)

theme_set(theme_light())

# Create plot to show why linear regression is not good for binomial data
df_logit <- 
    tibble( y = seq(.0001,.9999,.0001),
            x = psych::logit(y),
    )

df <- 
    tibble( x = c(rnorm(500, -5, 3) , rnorm(500, 5, 3)),
            y = c(rep(0, 500), rep(1,500))
    )

ggplot(df) + 
    aes(x = x, y = y) + 
    geom_point(alpha = .2) +
    geom_point(data = df_logit, size = .1, color = "blue") +
    # geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
    coord_cartesian(ylim = c(-.25, 1.25)) +
    labs(x = "Predictor", y = "Outcome")


# Use case for logistic regression ----------------------------------------
# We will use the titanic dataset
# Make the table printing neat, transform variable names to lowercase
titanic <- 
    titanic_train %>% 
    rename_all(str_to_lower) %>% 
    as_tibble()
    
# Fit logistic binomial regression
surv_fit <- glm(survived ~ fare * sex + sibsp + parch, family = "binomial", data = titanic)

summary(surv_fit)
tidy(surv_fit)
glance(surv_fit)

# To get the odds ratio, use the exp() function on the coefficients
exp(surv_fit$coefficients)
# Calculate confidence intervals for the ORs
exp(confint(surv_fit))

# But instead of the previous, do yourself a favor and use tidy with the following parameters to get ORs and conf int. 
tidy(surv_fit, conf.int = TRUE, exponentiate = TRUE)

# Let's plot the data. Please mind that you need to tweek the arguments for geom_smooth() to fit a binomial logistic function.
ggplot(titanic) +
    aes(y = survived, x = fare, group = sex, color = sex) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(labels = scales::percent_format())

# Reporting logistic regression
library(sjPlot)

tab_model(surv_fit, show.aic = TRUE, show.loglik = TRUE, collapse.se = TRUE)

# To save it to html, do:
# Coefficients are automatically transformed to Odds Ratios
surv_fit_table_html <-
  tab_model(surv_fit, show.aic = TRUE, show.loglik = TRUE, collapse.se = TRUE)

# You can save the results using the write_lines() function
write_lines(surv_fit_table_html, "surv_fit_table.html")

## Poisson regression
# Use poisson regression to predict a count-type variable (integer values, and totally left-skewed)
# We are predicting the number of family members on board, by age

titanic <-
    titanic %>% 
    mutate(family = sibsp + parch)

# Check the distribution of family variable
titanic %>% 
    ggplot() +
    aes(x = family) +
    geom_histogram(bins = 10)

# Yep, definitely poisson distribution
# Fitting a poisson regression is not difficult, just use the family = "poisson" parameter
family_fit_pois <- glm(family ~ age, family = "poisson", data = titanic)
# Check the results. They look very much like the output of logistic regression, only the model summary statistics are different
summary(family_fit_pois)
tidy(family_fit_pois, exponentiate = TRUE, conf.int = TRUE)
glance(family_fit_pois)

# However the poisson regression is not apropriate for data that has a large dispersion
# Dispersion shoul not be significantly larger than 1
# We can test the dispersion like this:
AER::dispersiontest(family_fit_pois)

# We have to run a negative binomial regression, since dispersion is 1.9 (variance is more than 2x the mean). This parameter was calculated using quasipoisson family.
family_fit_nb <- MASS::glm.nb(family ~ age, data = titanic)

# Check the results
summary(family_fit_nb)
tidy(family_fit_nb, exponentiate = TRUE, conf.int = TRUE)
glance(family_fit_nb)

# You can create all the diagnostic values as for linear regression
augment(family_fit_nb)

# Let's plot this. Mind the geom_smooth() parameters!
titanic %>% 
    ggplot() +
    aes(y = family, x = age) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family = "poisson"))

# When reporting poisson/negative binomial regression, you have to report the same things as in logistic regression

tab_model(family_fit_nb)

#Cumulative Link Model for Ordinal data

install.packages("ordinal")
install.packages("janitor")

library(ordinal)
library(janitor)

# We will use a dataset about the ratings of NYC restaurants from A to C
restaurants <- read_csv("https://data.cityofnewyork.us/api/views/43nn-pn8j/rows.csv")

# we drop some irrelevant variables, filter a few values, tidy variable names
rest_clean <-
  restaurants %>% 
  janitor::clean_names() %>% 
  select(boro, cuisine_description, critical_flag, score, grade) %>% 
  drop_na() %>% 
  filter(grade %in% c("A", "B", "C")) %>% 
  filter(cuisine_description %in% c("African", "American", "Asian", "Latin", "Middle Eastern")) %>% 
  filter(boro %in% c ("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"))

view(rest_clean)

# dependent variable needs to be a factor
rest_clean <- 
  rest_clean %>% 
  mutate(grade = as.factor(grade),
         cuisine_description = fct_relevel(cuisine_description, "American"))

#building the cumulative link model
# Comparing to American cousine, and the BRONX
clm1 <- clm(grade ~ cuisine_description + boro, data = rest_clean)
summary(clm1)

#running post-hoc tests
emmeans::emmeans(clm1, "cuisine_description", "boro")


# testing the model assumption, the proportional odd's ratio 
#with either the nominal_test or scale_test function
nominal_test(clm1)
scale_test(clm1)

#let's plot our data
ggplot(rest_clean, aes(x = cuisine_description, y = grade)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_wrap("boro") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
