################################################################################
###
### Data analysis in R: Week 5
###
### Sara Gottlieb-Cohen, Manager of Statistical Support Services
### Marx Library
### Yale University
###
################################################################################

# Pretest (T0)
Implicit_T0 <- read.csv("/Users/sgc/Documents/Workshops/Data analysis in R/Week5/Result-pretest.csv")

# Immediate post-test (T1)
Implicit_T1 <- read.csv("/Users/sgc/Documents/Workshops/Data analysis in R/Week5/Result-posttest-after-1st-training.csv")

# Short-term post-test (T2)
Implicit_T2 <- read.csv("/Users/sgc/Documents/Workshops/Data analysis in R/Week5/Result_1-week-posttest.csv")

# Supplementary training immediate post-test (T3)
Implicit_T3 <- read.csv("/Users/sgc/Documents/Workshops/Data analysis in R/Week5/Result_post-test-after-2nd-training.csv")

# Long-term post-test (T4)
Implicit_T4 <- read.csv("/Users/sgc/Documents/Workshops/Data analysis in R/Week5/Results-posttest-after-2-month.csv")

# Load the results from the training data, just so that we can identify which subjects
# were in which training condition.
Training <- read.csv("/Users/sgc/Documents/Workshops/Data analysis in R/Week5/1st-training.csv")

## Load packages

library(tidyverse)
library(rstatix)

## Clean the data. Select only the columns you need from each data set, and join them
## so that we have Subject, Race, and D scores from all time points in one data frame.

View(Training)

Training1 <- Training %>%
  rename(Subject = SubNumber)

Implicit_T0_small <- Implicit_T0 %>%
  select(Subject, D) %>%
  left_join(select(Training1, Race, Subject), by = "Subject") %>%
  rename(D_T0 = D)

Implicit_T1_small <- Implicit_T1 %>%
  select(Subject, D) %>%
  rename(D_T1 = D)

Implicit_T2_small <- Implicit_T2 %>%
  select(Subject, D) %>%
  rename(D_T2 = D)

Implicit_T3_small <- Implicit_T3 %>%
  select(Subject, D) %>%
  rename(D_T3 = D)

Implicit_T4_small <- Implicit_T4 %>%
  select(Subject, D) %>%
  rename(D_T4 = D)

Implicit_D_all <- Implicit_T0_small %>%
  left_join(Implicit_T1_small, by = "Subject") %>%
  left_join(Implicit_T2_small, by = "Subject") %>%
  left_join(Implicit_T3_small, by = "Subject") %>%
  left_join(Implicit_T4_small, by = "Subject") 

## Transform the data from wide to long format 
  
Implicit_all_long <- Implicit_D_all %>%
  gather(key = "Time", value = "D", -Subject, -Race)

## Create a summary table and reproduce Figure 4. Don't forget error bars!

# Summary table:

Implicit_all_summary <- Implicit_all_long %>%
  group_by(Race, Time) %>%
  summarize(mean = mean(D, na.rm = T),
            n = n(),
            se = sd(D, na.rm = T)/sqrt(n)) %>%
  arrange(Time)

# Plot the data:

ggplot(Implicit_all_summary, aes(x = Time, y = mean, group = Race, color = Race)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1)

## Analyses

# 1. Was there an immediate effect of initial training?

Implicit_all_long  %>%
  na.omit() %>%
  filter(Time == "D_T0" | Time == "D_T1") %>%
  anova_test(dv = D, 
             wid = Subject,
             between = Race,
             within = Time)

# Or: 

anova1_data <- Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "D_T0" | Time == "D_T1") %>%
  mutate(Subject = as.factor(Subject))

anova1 <- aov(D ~ Race*Time + Error(Subject/Time), data = anova1_data)
summary(anova1)

# 1a. Was there a significant effect of time (T0 vs. T1) within the Black training group?

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "D_T0" | Time == "D_T1",
         Race == "Black") %>%
  anova_test(dv = D, 
           wid = Subject,
           within = Time)

# We can also perform a paired t-test, since there is only one variable 
# and only two levels.

ttest1_data <- Implicit_D_all %>%
  select(D_T0, D_T1, Race) %>%
  filter(Race == "Black") %>%
  na.omit()

t.test(ttest1_data$D_T0, ttest1_data$D_T1, paired = TRUE)

# Does F = t^2?

2.5141^2

# 1b. Was there a significant effect of training group at T1?

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "D_T1") %>%
  anova_test(dv = D, 
             between = Race)

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
  filter(Time == "D_T0" | Time == "D_T2",
         Race == "Black") %>%
  anova_test(dv = D, 
             wid = Subject,
             within = Time)

# 3. Was there an effect of the second training? 

Implicit_all_long  %>%
  na.omit() %>%
  filter(Time == "D_T0" | Time == "D_T3") %>%
  anova_test(dv = D, 
             wid = Subject,
             between = Race,
             within = Time)

# 3b. Was there a significant effect of time within the Black training group? 

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "D_T0" | Time == "D_T3",
         Race == "Black") %>%
  anova_test(dv = D, 
             wid = Subject,
             within = Time)

# 3b. Was there a significant effect of training group at T3?

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "D_T3") %>%
  anova_test(dv = D, 
             between = Race)

# 4. Was there a long term effect?

Implicit_all_long  %>%
  na.omit() %>%
  filter(Time == "D_T0" | Time == "D_T4") %>%
  anova_test(dv = D, 
             wid = Subject,
             between = Race,
             within = Time)

# 4a. Was there a significant effect of time within the Black training group?

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "D_T0" | Time == "D_T4",
         Race == "Black") %>%
  anova_test(dv = D, 
             wid = Subject,
             within = Time)

# 4b. Was there a significant effect of race training group at T4?

Implicit_all_long %>%
  na.omit() %>%
  filter(Time == "D_T4") %>%
  anova_test(dv = D, 
             between = Race)