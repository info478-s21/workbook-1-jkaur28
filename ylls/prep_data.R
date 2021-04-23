# Prep self-harm data

# IMPORTANT: You **do not** need to run this script, it has already been done for you

# Set up
library(dplyr)

# Prep self-harm data
self_harm_deaths <- read.csv("data/raw/self-harm-deaths.csv")
self_harm_ylls <- read.csv("data/raw/self-harm-ylls.csv")

self_harm_data <- rbind(self_harm_deaths, self_harm_ylls) %>%
  filter(Location == "Colombia")

write.csv(self_harm_data, "data/prepped/self-harm.csv", row.names = FALSE)
