# A brief look at self harm data, downloaded from http://ihmeuw.org/3zn8
# Make sure to *clearly label* all charts and files you create

# Set up
# Set working directory using the Session menu
library(dplyr)
library(tidyverse)
library(ggplot2)

home.dir<-"/homes/katburk/repos/iSchool/wb-1-katburk/ylls/"

getwd()
# Load data
self_harm_data <- read.csv("data/prepped/self-harm.csv")
View(self_harm_data)

# Deal with string age-groups
self_harm_data <- self_harm_data %>%
  filter(Value > 0) %>%
  mutate(age_group = substr(Age, 1, 2))

###########################################################################
### Create a plot of the death rate (deaths per 100K) in each age-group ###
###########################################################################

# Filter the data (to only the death rate rows)
death_rate <- self_harm_data %>% 
  filter(Measure == "Deaths per 100,000")
# Create the chart (age on X axis, death rate on Y axis)
 ggplot(data = death_rate, aes(x = Age, y = Value)) +
  geom_point(stat = "identity")
# Save you chart to the `charts/` folder for grading

#######################################################################
### Create a plot of the yll rate (ylls per 100K) in each age-group ###
#######################################################################

# Filter the data (to only the ylls rate rows)
yll_rate <- self_harm_data %>% 
  filter(Measure == "YLLs per 100,000")
# Create the chart (age on X axis, death rate on Y axis)
ggplot(data = yll_rate, aes(x = Age, y = Value)) +
  geom_point(stat = "identity")
  
# Save you chart to the `charts/` folder for grading

###########################################################################
### Create a plot of comparing the YLL rate to the Death Rate (scatter) ###
###########################################################################

# Reshape (e.g., `spread()`) your data to have separate column for each metric
df <- spread(self_harm_data[-c(8,9)], Measure, Value)

# Create a scatter plot of the YLL rate v.s. the death rate
# Label the age of each point
ggplot(df, aes(x=`YLLs per 100,000`, y=`Deaths per 100,000`)) +
  geom_label(label=df$age_group)
  
# Save you chart to the `charts/` folder for grading
