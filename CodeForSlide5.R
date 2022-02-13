
# Environment ----

library(tidyverse) # Data import/Hundling/Visualization
library(estimatr) # Summary statistics
library(rpart) # Tree
library(rpart.plot) # Tree visualization

# Data ----

raw <- read_csv("example.csv") # Import data

# Adaptive tree ----

fit <- rpart(exptot ~ agehead + sexhead + educhead + dmmfd + dfmfd,
  data = raw,
  control = rpart.control(
    maxdepth = 2, # Depth 2
    cp = 0, # No simplification
    minbucket = 150
  ) # Minimum subsample size
) # Adaptive tree

rpart.plot(fit) # Visualization


# Honest tree ----
set.seed(111) # Fix seed value

group <- sample(1:2, nrow(raw), replace = TRUE) # Sample split

fit <- rpart(exptot ~ agehead + sexhead + educhead + dmmfd + dfmfd,
  data = raw[group == 1, ],
  control = rpart.control(
    maxdepth = 2,
    cp = 0,
    minbucket = 150
  )
) # Honest tree

rpart.plot(fit) # Visualization

raw <- mutate(raw,
  prediction = predict(
    fit,
    raw
  ),
  prediction = round(prediction)
) # Save prediction

refit <- lm_robust(exptot ~ 0 + factor(prediction),
  data = raw[group == 2, ]
) # Refiting

refit <- tidy(refit)

ggplot(
  refit,
  aes(
    y = term,
    x = estimate,
    xmin = conf.low,
    xmax = conf.high
  )
) +
  geom_pointrange() +
  theme_bw() # Visualization with confidence interval
