
# Environment ----

library(tidyverse) # Data import/Hundling/Visualization
library(estimatr) # Summary statistics

# Data ----

raw <- read_csv("example.csv") # Import data

# Single variable OLS ----

ggplot(raw,
       aes(x = agehead,
           y = exptot)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

# Multi variables OLS -----

fit <- lm_robust(
  exptot ~ dfmfd + dmmfd + agehead,
  raw
) # Fit OLS

result <- tidy(fit) # Convert data.frame

result

# Show coefficients ----

ggplot(
  result,
  aes(
    y = term,
    x = estimate,
    xmin = conf.low,
    xmax = conf.high
  )
) +
  geom_pointrange() +
  theme_bw() # Visualization

