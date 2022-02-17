
# Environment ----

library(tidyverse) # Data import/Hundling/Visualization
library(estimatr) # Summary statistics
library(MatchIt) # Tree

# Data ----

raw <- read_csv("example.csv") # Import data

# Matching ----

m.out <- matchit(dfmfd ~ agehead + dmmfd + educhead + sexhead,
  data = raw,
  method = "cem"
) # CEM

m.out # Check matching results

plot(summary(m.out), xlim = c(0, 0.4))

# Regression after matching ----

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
