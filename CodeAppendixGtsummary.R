
# Environment ----

library(tidyverse) # Data import/Hundling/Visualization
library(gtsummary) # Summary table

# Data ----

raw <- read_csv("example.csv") # Import data

df <- select(raw, -nh,-weight) # Remove nh and weight

# Adaptive tree ----

tbl_summary(df, by = dfmfd) # Show table comparing dfmfd == 1 and 0
