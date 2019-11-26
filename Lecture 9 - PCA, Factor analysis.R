# PCA, Factor analysis, CFA
library(tidyverse)
library(GGally)
library(psych)
library(broom)

# Principal component analysis
# Using the Eyesenk Personality Inventory + Big5 inventory 
eb <-
    epi.bfi %>% 
    select(epiE:bfopen) %>% 
    as_tibble()

# Explore the 
ggpairs(eb)

# Do pca with default settings
eb_pca <- pca(eb)
eb_pca

# Get communality for each variable
eb_pca$communality

# Estimate the optimal number of factors
nfactors(eb)
# Parallel analysis to estimate the optimal number of factors
fa.parallel(eb, fa = "pc")

# Do the pca with 3 factors
pca(eb, nfactors = 3)

# Let's try rotating the factors
eb_pca_rot <- pca(eb, nfactors = 3, rotate = "varimax")
eb_pca_rot



# Show loadings properly
eb_loadings <- 
    loadings(eb_pca_rot) %>% 
    unclass() %>% 
    tidy() %>% 
    mutate_if(is_numeric, round, 2) 

# Create a plot to show component loadings
eb_loadings %>% 
    gather(component, loading, RC1:RC3) %>% 
    mutate(sign = if_else(loading >= 0, "positive", "negative")) %>% 
    ggplot() +
        aes(y = loading %>% abs(), x = .rownames %>% fct_rev(), fill = sign, label = loading) +
        geom_col(position = "dodge") +
        coord_flip() +
        geom_text() +
        facet_wrap(~component) +
        labs(y = "Loading strength", x = "Variable") +
        theme_minimal()

# Ok, so how are we using this information?
# Extract component scores into a tibble
scores <- 
    eb_pca_rot$score %>% 
    as_tibble()

# You can use these loadings from further on as any other varable. 
# E.g. check the correlations with the diagnostic scales from the original dataset
bind_cols(epi.bfi %>% select(bdi:stateanx), scores) %>% 
    ggpairs()

# we can also save the factor loadings as keys for creating scales
factor2cluster(eb_pca_rot, cut = .7)

# Assumptions to check
# KMO test should be 
KMO(eb)
# Bartlett test should be non-significant
cortest.bartlett(eb)


# Exploratory factor analysis
# Using the Big Five Inventory to find the five factors
bfi <- 
    psych::bfi %>% 
    as_tibble() %>% 
    select(A1:O5)

# Finding the optimal number of factors
nfactors(bfi, rotate = "varimax")
fa.parallel(bfi, fa = "fa", fm = "minres")

bfi_efa <- psych::fa(bfi, nfactors = 6, scores = "regression", rotate = "varimax")

# Let's visualize the loadings.
# Unfortunatelly, the psych package provides rather inconsistent output, so we need to do some ugly data transformation first

bfi_efa$loadings %>% 
    matrix(ncol = ncol(bfi_efa$loadings)) %>% 
    as_tibble() %>% 
    mutate(variable = bfi_efa$loadings %>% rownames()) %>% 
    gather(factor, loading, -variable) %>% 
    mutate(sign = if_else(loading >= 0, "positive", "negative")) %>% 
    ggplot() +
    aes(y = loading %>% abs(), x = variable %>% fct_rev(), fill = sign, label = round(loading, 2)) +
    geom_col(position = "dodge") +
    coord_flip() +
    geom_text() +
    facet_wrap(~factor) +
    labs(y = "Loading strength", x = "Variable")

# It turns out that our 6th factor is not that mmeaningful, so get rid of it
bfi_efa <- psych::fa(bfi, nfactors = 5, scores = "regression", rotate = "varimax")

# The visualizations shows that the loadings are much better now    
bfi_efa$loadings %>% 
    matrix(ncol = ncol(bfi_efa$loadings)) %>% 
    as_tibble() %>% 
    mutate(variable = bfi_efa$loadings %>% rownames()) %>% 
    gather(factor, loading, -variable) %>% 
    mutate(sign = if_else(loading >= 0, "positive", "negative")) %>% 
    ggplot() +
    aes(y = loading %>% abs(), x = variable %>% fct_rev(), fill = sign, label = round(loading, 2)) +
    geom_col(position = "dodge") +
    coord_flip() +
    geom_text() +
    facet_wrap(~factor) +
    labs(y = "Loading strength", x = "Variable")

# Show which variables load to which factor the most, and what is the direction
psych::factor2cluster(bfi_efa)



# CFA -------------------------------------------------------------------------------
# Confirmatory factor analysis using the lavaan package
# Check out the hompage: http://lavaan.ugent.be/

install.packages("lavaan")
library(tidyverse)
library(lavaan)

hs_data <- 
  lavaan::HolzingerSwineford1939 %>% 
  as_tibble()

# Defining the CFA model
hs_model <- " visual  =~ x1 + x2 + x3 
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
"

fit <- cfa(hs_model, data = hs_data)

# If you want to get the fit measures, you have to set this parameter in summary()
# This returns the most important information about the model
summary(fit, fit.measures = TRUE)

# Plotting the models is easy with semPlot, but parameters require tweaking to make it informative and pretty
install.packages("semPlot")
library(semPlot)

# This will give you the clean model structure
semPaths(fit)
# If you want to get the parameter estimates use
semPaths(fit, "est")

# You can also get standardized estimates
semPaths(fit, "std")

# This does not tell us much

# Fixing intercepts
# "
# # three-factor model
# visual =~ x1 + x2 + x3
# textual =~ x4 + x5 + x6
# speed   =~ x7 + x8 + x9
# 
# # intercepts
# x1 ~ 1
# x2 ~ 1
# x3 ~ 1
# x4 ~ 1
# x5 ~ 1
# x6 ~ 1
# x7 ~ 1
# x8 ~ 1
# x9 ~ 1
# "

# Lets define the model in a separate file, and read it (this may be a good way to keep your model)
# You can find this file next to the script in a separate directory (CFA models)

hs_model_fi <- read_lines("CFA models/fs_model_fi.txt")

fit2 <- cfa(hs_model_fi, hs_data)
summary(fit2, fit.measures = TRUE)

hs_model_ort <- read_lines("CFA models/hs_model_orthogonal.txt")

fit3 <- cfa(hs_model_ort, hs_data)
summary(fit3, fit.measures = TRUE)
semPaths(fit3, "est")


# We can also regard all latent variables to be independent (orthogonal) by using the orthogonal = TRUE parameter on our original model

fit4 <- cfa(hs_model, data = hs_data, orthogonal = TRUE)
summary(fit4, fit.measures = TRUE)
semPaths(fit4, "est")

# If the factor loadings of the first indicator of each latent variable will no longer be fixed to 1 if you use the std.lev = TRUE parameter
fit5 <- cfa(hs_model, hs_data, std.lev = TRUE)
summary(fit5, fit.measures = TRUE)
semPaths(fit5, "est")

# You can build 2-level structurese (see file)
hs_model_2fac <- read_lines("CFA models/fs_model_2fac.txt")

fit6 <- cfa(hs_model_2fac, hs_data, std.lev = TRUE)
summary(fit6, fit.measures = TRUE)
semPaths(fit6, "est")

# You can do multi group analysis

fit7 <- cfa(hs_model, data = hs_data, group = "school")
summary(fit7, fit.measures = TRUE)
semPaths(fit7, "est")




