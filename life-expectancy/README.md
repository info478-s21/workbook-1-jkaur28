# Life Expectancy Calculation

## Overview

This repository contains a **simplified** example of calculating life expectancy in R. Data was downloaded [here](http://ghdx.healthdata.org/record/global-burden-disease-study-2015-gbd-2015-reference-life-table) from the [Institute for Health Metrics and Evaluation](https://healthdata.org).

Here is some information from the data source's README file:

> **Global Burden of Disease Study 2015 (GBD 2015) Reference Life Table**: This reference life table is used in GBD to calculate Years of Life Lost (YLLs). It was constructed based on the lowest estimated age-specific mortality rates from all locations with populations over 5 million in the 2013 iteration of GBD. YLLs are computed by multiplying the number of estimated deaths by the reference life tables life expectancy at age of death. The table includes estimates for the probability of death within an age range, the proportion of the hypothetical cohort still alive at age x, and life expectancy at age x for ages 0 to 110+ at five-year intervals. 

## Instructions

To compute the life expectancy, you should load the **life-table.xlxs** file into R, which contains the probabilities of deaths from the IHME study. Then, do the following:

First, let's do the following for the **first row** only:
1. Compute the `num_survived` column by multiplying the number of people living at the start of the interval (`num_living`) times the probability of survival (one minus `pr_death`).

2. Compute the `num_died` column by subtracting the _number survived_ from the number of people living at the _start of the interval_.

3. Compute the _years lived during the interval_ (`years_lived`) by adding the following values:
    - Years lived by those who survived: number of people who survived the interval times the length of the interval.
    - Years lived by those who _did not_ survive: number of people who died times the average length of the interval

4. Fill in the **second row** of the `num_living` column using the number of people at the _end of the previous interval_ (i.e., `num_survived`).

Because we want to repeat the steps above for every row, you should wrap the above step **in a function** (`increment_lt`). Then, you'll need to use iteration (e.g., a `for()` or `while()` loop) to iterate through each row of the table and fill in the values. 

Working with the first row only, lets compute the life expectancy by dividing the **total** number of years lived (across _all age intervals_: `years_lived`) by the number of people living at the start of each interval (`num_living`). And, make sure to add the _start_ age to the life expectancy so that it expresses the _total_ life expectancy. 

Then, using another iterative technique (e.g., `for()` or `while()` loop), iterate through each row of the table and compute the life expectancy (at birth) for each row.

Write your values to a `.csv` file for grading. 

Compare your computed values to the `life-table-complete.xlxs` file for the completed life expectancies.
