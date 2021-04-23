# A brief look at self harm data, downloaded from http://ihmeuw.org/3zn8
# Make sure to *clearly label* all charts and files you create

# Set up
# Set working directory using the Session menu
library(dplyr)
library(ggplot2)

home.dir<-"/homes/katburk/repos/iSchool/wb-1-katburk/ylls/"


# Load data
self_harm_data <- read.csv(paste0(home.dir,("data/prepped/self-harm.csv")))
View(self_harm_data)

# Deal with string age-groups
self_harm_data <- self_harm_data %>%
  filter(Value > 0) %>%
  mutate(age_group = substr(Age, 1, 2))

###########################################################################
### Create a plot of the death rate (deaths per 100K) in each age-group ###
###########################################################################

# Filter the data (to only the death rate rows)
  
# Create the chart (age on X axis, death rate on Y axis)

# Save you chart to the `charts/` folder for grading

#######################################################################
### Create a plot of the yll rate (ylls per 100K) in each age-group ###
#######################################################################

# Filter the data (to only the ylls rate rows)

# Create the chart (age on X axis, death rate on Y axis)

# Save you chart to the `charts/` folder for grading

###########################################################################
### Create a plot of comparing the YLL rate to the Death Rate (scatter) ###
###########################################################################

# Reshape (e.g., `spread()`) your data to have separate column for each metric

# Create a scatter plot of the YLL rate v.s. the death rate
# Label the age of each point

# Save you chart to the `charts/` folder for grading
