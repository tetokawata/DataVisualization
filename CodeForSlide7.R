
# Environment ----

library(tidyverse) # Data import/Hundling/Visualization
library(estimatr) # Summary statistics
library(MatchIt) # Tree
library(gtsummary)

# Data ----

raw <- read_csv("example.csv") # Import data

# Matching on Average treatment effect ----

m.out <- matchit(dfmfd ~ agehead + dmmfd + educhead + sexhead,
  data = raw,
  method = "cem") # CEM

m.out # Check matching results

plot(summary(m.out), xlim = c(0, 0.4))

matched.data <- match.data(m.out,
                           drop.unmatched = FALSE)

matched.data <- mutate(matched.data,
                       drop = if_else(is.na(subclass),"drop","not"))

tbl_summary(select(matched.data,
                   agehead,
                   dmmfd,
                   educhead,
                   sexhead,
                   drop),
            by = drop)

## Regression after matching ----



fit <- lm_robust(exptot ~ dfmfd + agehead + dmmfd + educhead + sexhead,
  data = match.data(m.out),
  weights = weights,
  clusters = subclass
)

fit <- tidy(fit)
fit <- filter(fit, term == "dfmfd")
fit <- mutate(fit, method = "Matching")

ggplot(fit, aes(
  x = estimate,
  xmin = conf.low,
  xmax = conf.high,
  y = method
)) +
  geom_pointrange()


# PS match on Average treatment effect ----

m.out <- matchit(dfmfd ~ agehead + dmmfd + educhead + sexhead,
                 data = raw,
                 method = "subclass") # CEM

m.out # Check matching results

plot(summary(m.out), xlim = c(0, 0.4))


## Regression after matching ----

fit <- lm_robust(exptot ~ dfmfd + agehead + dmmfd + educhead + sexhead,
                 data = match.data(m.out),
                 weights = weights,
                 clusters = subclass
)

fit <- tidy(fit)
fit <- filter(fit, term == "dfmfd")
fit <- mutate(fit, method = "Matching")

ggplot(fit, aes(
  x = estimate,
  xmin = conf.low,
  xmax = conf.high,
  y = method
)) +
  geom_pointrange()

