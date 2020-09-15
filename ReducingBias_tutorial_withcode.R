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

Training_clean <- Training %>%
  rename(___ = ___)

Implicit_T0_small <- Implicit_T0 %>%
  select(Subject, D) %>%
  left_join(select(Training_clean, Race, Subject), by = "___") %>%
  rename(D_T0 = D)

Implicit_T1_small <- Implicit_T1 %>%
  select(___, ___) %>%
  rename(D_T1 = ___)

Implicit_T2_small <- Implicit_T2 %>%
  select(___, ___) %>%
  rename(___ = ___)

Implicit_T3_small <- Implicit_T3 %>%
  select(___, ___) %>%
  rename(___ = ___)

Implicit_T4_small <- Implicit_T4 %>%
  select(___, ___) %>%
  rename(___ = ___)

Implicit_D_all <- Implicit_T0_small %>%
  left_join(___, by = "___") %>%
  left_join(___, by = "___") %>%
  left_join(___, by = "___") %>%
  left_join(___, by = "___") 

# We ultimately want a summary table that summarizes the average D score for 
# each condition. This will be helpful for graphing the data.
# We need to first transform the data from wide to long format in order to summarize it.
# Don't forget to include the standard error in your summary table! We will need these
# values to plot error bars.

Implicit_all_long <- Implicit_D_all %>%
  gather(___ = "Time", ___ = "D", -___, -___)

Implicit_all_summary <- Implicit_all_long %>%
  group_by(___, ___) %>%
  summarize(mean = ___(___, na.rm = T),
            n = n(),
            se = ___(___, na.rm = T)/sqrt(n)) %>%
  arrange(Time)

# Replicate the Figure 4. 
# Plot the data as a line graph with Time on the x-axis and average D score on the
# y-axis. Plot separate lines for different conditions.

ggplot(Implicit_all_summary, aes(x = ___, y = ___, group = ___, color = ___)) +
  geom_line() +
  geom_errorbar(aes(ymin = ___, ymax = ___), width = .1)

## Analyses

# 1. Was there an immediate effect of initial training?

# There are multiple ways to conduct an ANOVA with a within-subjects factor; I have 
# demonstrated only how to do this using anova_test() from the rstatix package.

Implicit_all_long  %>%
  na.omit() %>%
  filter(Time == "___" | Time == "___") %>%
  anova_test(dv = ___, 
             wid = ___,
             between = ___,
             within = ___)

# 1a. Was there a significant effect of time (T0 vs. T1) within the Black training group?

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "___" | Time == "___",
         Race == "___") %>%
  anova_test(dv = ___, 
             wid = ___,
             within = ___)

# 1b. Was there a significant effect of training group at T1?

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "___") %>%
  anova_test(dv = ___, 
             between = ___)

# We can perform post-hoc tests to test for differences among the three groups
# at T1. Note that Tukey HSD only works for a between-subjects variable, and takes 
# an aov object as input.

T1_anova <- aov(D ~ Race, data = subset(Implicit_all_long, Time == "D_T1"))
TukeyHSD(T1_anova)

# You could also subset the data, perform individual t-tests, and correct for multiple
# comparisons.

# 2. Was there a lasting effect of Black individuation training at T2? 

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "___" | Time == "___",
         Race == "___") %>%
  anova_test(dv = ___, 
             wid = ___,
             within = ___)

# 3. Was there an effect of the second training? 

Implicit_all_long  %>%
  na.omit() %>%
  filter(Time == "___" | Time == "___") %>%
  anova_test(dv = ___, 
             wid = ___,
             between = ___,
             within = ___)

# 3b. Was there a significant effect of time within the Black training group? 

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "___" | Time == "___",
         Race == "___") %>%
  anova_test(dv = ___, 
             wid = ___,
             within = ___)

# 3b. Was there a significant effect of training group at T3?

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "___") %>%
  anova_test(dv = ___, 
             between = ___)

# 4. Was there a long term effect?

Implicit_all_long  %>%
  na.omit() %>%
  filter(Time == "___" | Time == "___") %>%
  anova_test(dv = ___, 
             wid = ___,
             between = ___,
             within = Time)

# 4a. Was there a significant effect of time within the Black training group?

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "___" | Time == "___",
         Race == "___") %>%
  anova_test(dv = ___, 
             wid = ___,
             within = ___)

# 4b. Was there a significant effect of race training group at T4?

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "___") %>%
  anova_test(dv = ___, 
             between = ___)