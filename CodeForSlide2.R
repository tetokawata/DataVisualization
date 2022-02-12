
# Environment ----

library(tidyverse) # Data import/Hundling/Visualization
library(gtsummary) # Summary statistics

# Data ----

raw <- read_csv("example.csv") # Import data

temp <- select(raw, exptot, agehead, dfmfd, dmmfd) # Select interest variables

# Summary statistics ----

tbl_summary(temp) # Show summary statistics:

## Median (25% and 75% quantile) for continuous variable

## Number of sample with = 1 (their share) for an indicator variable

## Share of each category for categorical variable


tbl_summary(temp,
  by = dfmfd
) # Show summary statistics by dfmfd


# Cumulative distribution function ----

ggplot(raw, aes(x = exptot)) +
  stat_ecdf() +
  theme_bw() +
  ylab("ECDF") +
  xlab("Total expenditure") # Draw ECDF of total expenditure

ggplot(raw, aes(
  x = exptot,
  color = factor(dfmfd)
)) +
  stat_ecdf() +
  theme_bw() +
  ylab("ECDF") +
  xlab("Total expenditure") # Draw ECDF by micro-finance status

# Histogram ----

ggplot(raw, aes(x = exptot)) +
  geom_histogram() +
  theme_bw() +
  xlab("Total expenditure") # Draw histogram of total expenditure

ggplot(raw, aes(x = exptot)) +
  geom_histogram() +
  theme_bw() +
  ylab("ECDF") +
  xlab("Total expenditure") +
  facet_wrap(~dfmfd) # Draw histogram by micro-finance status

# Boxplot ----

ggplot(raw, aes(
  x = exptot,
  y = factor(dfmfd)
)) +
  geom_boxplot() +
  theme_bw() # Draw boxplot


# Scatter and heatmap ----

ggplot(raw, aes(
  x = agehead,
  y = exptot
)) +
  geom_point() +
  theme_bw() # Draw scatter


ggplot(raw, aes(
  x = agehead,
  y = exptot
)) +
  geom_bin2d() +
  theme_bw() # Draw heatmap
