# Build a life table in R

# Set up
# Set your working directory using the Session menu
library(readxl)

setwd("/Users/jasmeenkaur/Desktop/workbook-1-jkaur28/life-expectancy")
# Load life table
life_table <- read_excel("life-table.xlsx")

# Fix column names
colnames(life_table) <- sapply(strsplit(colnames(life_table), " "), `[`, 1)

# Fill in first row of life table as described in the README.md file
life_table$num_survived[1] <- life_table$num_living[1] * (1 - life_table$prob_dying[1])

life_table$num_died[1] <- life_table$num_living[1] - life_table$num_survived[1]

life_table$years_lived[1] <- (2 * life_table$num_survived[1] * life_table$avg_length[1]) + (life_table$num_died[1] * life_table$avg_length[1])

life_table$life_expectancy[1] <- sum(life_table$years_lived)/life_table$num_living[1]

# Write a function `increment_lt` to increment through the life table.
increment_lt <- function(row_num) {
  return(row_num + 1)
}

# Fill in the table (except life_expetancy column)
# using a `while()` loop and your `increment_lt` function
i = 1
while(i < nrow(life_table)) {
  life_table$num_survived[i] <- life_table$num_living[i] * (1 - life_table$prob_dying[i])
  life_table$num_died[i] <- life_table$num_living[i] - life_table$num_survived[i]
  life_table$years_lived[i] <- (2 * life_table$num_survived[i] * life_table$avg_length[i]) + (life_table$num_died[i] * life_table$avg_length[i])
  life_table$num_living[increment_lt(i)] <- life_table$num_survived[i]
  i = increment_lt(i)
}
life_table$num_survived[nrow(life_table)] <- life_table$num_living[nrow(life_table)] * (1 - life_table$prob_dying[nrow(life_table)])
life_table$num_died[nrow(life_table)] <- life_table$num_living[nrow(life_table)] - life_table$num_survived[nrow(life_table)]
life_table$years_lived[nrow(life_table)] <- (2 * life_table$num_survived[nrow(life_table)] * life_table$avg_length[nrow(life_table)]) + (life_table$num_died[nrow(life_table)] * life_table$avg_length[nrow(life_table)])

# Fill in the life expectancy column using a `while` loop
for (i in 1:24) {
  life_table$life_expectancy[i] <- sum(tail(life_table$years_lived, 24-(i-1))) / life_table$num_living[i] + life_table$age[i]
}

# Write lifetable to a .csv file for grading. 
write.csv(life_table,file="final-life-table_JK.csv", row.names = FALSE)

