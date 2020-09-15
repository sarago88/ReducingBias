### Practice your R skills: Analyzng candy ratings ###
### Sara Gottlieb-Cohen, StatLab Manager           ###

## Load packages

library(tidyverse)
library(rstatix)

## Load the data

# Pretest (T0)
Implicit_T0 <- read_csv("https://raw.githubusercontent.com/sarago88/ReducingBias/master/Result-pretest.csv")

# Immediate post-test (T1)
Implicit_T1 <- read_csv("https://raw.githubusercontent.com/sarago88/ReducingBias/master/Result-posttest-after-1st-training.csv")

# Short-term post-test (T2)
Implicit_T2 <- read_csv("https://raw.githubusercontent.com/sarago88/ReducingBias/master/Result_1-week-posttest.csv")

# Supplementary training immediate post-test (T3)
Implicit_T3 <- read_csv("https://raw.githubusercontent.com/sarago88/ReducingBias/master/Result_post-test-after-2nd-training.csv")

# Long-term post-test (T4)
Implicit_T4 <- read_csv("https://raw.githubusercontent.com/sarago88/ReducingBias/master/Results-posttest-after-2-month.csv")

# Load the results from the training data, just so that we can identify which subjects
# were in which training condition.

Training <- read_csv("https://raw.githubusercontent.com/sarago88/ReducingBias/master/1st-training.csv")

## Data manipulation

# Select only the columns you need from each data set, and join them so that 
# we have Subject, Race, and D scores from all time points in one data frame.

# First rename "SubNumber" in the training data so that it can be joined with data
# from other time points.



# We ultimately want a summary table that summarizes the average D score for 
# each condition. This will be helpful for graphing the data.
# We need to first transform the data from wide to long format in order to summarize it.
# Don't forget to include the standard error in your summary table! We will need these
# values to plot error bars.



# Replicate the Figure 4. 
# Plot the data as a line graph with Time on the x-axis and average D score on the
# y-axis. Plot separate lines for different conditions.



## Analyses

# 1. Was there an immediate effect of initial training?



# 1a. Was there a significant effect of time (T0 vs. T1) within the Black training group?



# 1b. Was there a significant effect of training group at T1?



# We can perform post-hoc tests to test for differences among the three groups
# at T1. Note that Tukey HSD only works for a between-subjects variable, and takes 
# an aov object as input.



# You could also subset the data, perform individual t-tests, and correct for multiple
# comparisons.

# 2. Was there a lasting effect of Black individuation training at T2? 



# 3. Was there an effect of the second training? 



# 3b. Was there a significant effect of time within the Black training group? 



# 3b. Was there a significant effect of training group at T3?



# 4. Was there a long term effect?



# 4a. Was there a significant effect of time within the Black training group?



# 4b. Was there a significant effect of race training group at T4?


