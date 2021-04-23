# Build a life table in R

# Set up
# Set your working directory using the Session menu
library(readxl)

# Load life table
life_table <- read_excel("./life-table.xlsx")

# Fix column names
colnames(life_table) <- sapply(strsplit(colnames(life_table), " "), `[`, 1)

# Fill in first row of life table as described in the README.md file

# Write a function `increment_lt` to increment through the life table.

# Fill in the table (except life_expetancy column)
# using a `while()` loop and your `increment_lt` function

# Fill in the life expectancy column using a `while` loop

# Write lifetable to a .csv file for grading. 
