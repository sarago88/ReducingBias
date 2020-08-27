################################################################################
###
### Data analysis in R: Week 5
###
### Sara Gottlieb-Cohen, Manager of Statistical Support Services
### Marx Library
### Yale University
###
################################################################################

## Read in the data files:

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

Training1 <- Training %>%
  rename(Subject = SubNumber) %>%
  select(Subject, Race)

Implicit_T0_small <- Implicit_T0 %>%
  select(Subject, D) %>%
  left_join(Training1, by = "Subject") %>%
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

Implicit_all <- Implicit_T0_small %>%
  left_join(Implicit_T1_small, by = "Subject") %>%
  left_join(Implicit_T2_small, by = "Subject") %>%
  left_join(Implicit_T3_small, by = "Subject") %>%
  left_join(Implicit_T4_small, by = "Subject") %>%
  rename(Condition = Race)

## Transform the data from wide to long format 

Implicit_long <- Implicit_all %>%
  gather(key = "Time", value = "D", -Subject, -Condition)

Implicit_long_Victor <- Implicit_all %>%
  gather(key = "Time", value = "D", -Subject, -Condition) %>%
  mutate(D = case_when(is.na(D) ~ 0))

Implicit_long_nona <- Implicit_long %>%
  na.omit()

## Create a summary table and reproduce Figure 4. Don't forget error bars!

# Summary table:

Implicit_summary <- Implicit_long_nona %>%
  group_by(Time, Condition) %>%
  summarize(mean = mean(D),
            n = n(),
            se = sd(D)/sqrt(n))


# Plot the data:

ggplot(Implicit_summary, aes(x = Time, y = mean, group = Condition, color = Condition)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1)

## Analyses

# 1. Was there an immediate effect of initial training?

Implicit_long %>% 
  na.omit() %>%
  filter(Time == "D_T1") %>%
  ezANOVA(dv = D,
          wid = Subject,
          between = Condition)

library(ez)
ezANOVA(data = anova)  

anova1 <- lmer(D ~ Condition*Time + (1|Subject), data = anova_1_data)
anova(anova1)

anova3 <- aov(D ~ (Condition*Time) + Error(Subject/(Time)), data = anova_1_data)
summary(anova3)
TukeyHSD(anova3)

Implicit_long_nona %>% 
  filter(Time == "D_T0" | Time == "D_T1") %>%
  anova_test(dv = D,
             wid = Subject,
             within = c(Time, Condition))

# 1a. Was there a significant effect of time (T0 vs. T1) within the Black training group?

Implicit_long_nona %>%
  filter(Condition == "Black",
         Time == "D_T0" | Time == "D_T1") %>%
  anova_test(dv = D,
             wid = Subject,
             within = Time)

ttest_data <- Implicit_long %>%
  filter(Condition == "Black",
         Time == "D_T0" | Time == "D_T1")
  
t.test(ttest_data$D ~ ttest_data$Time, paired = TRUE)


# 1b. Was there a significant effect of training group at T0 or T1?

test_data <- Implicit_long_nona %>% 
  filter(Time == "D_T1") 

anova2 <- lm(D ~ Condition, data = test_data)
tukey_hsd(anova2)




# 2. Was there a lasting effect of Black individuation training at T2? 


  
  

# 3. Was there an effect of the second training? 





# 3b. Was there a significant effect of time within the Black training group? 





# 3b. Was there a significant effect of training group at T3?





# 4. Was there a long term effect?





# 4a. Was there a significant effect of time within the Black training group?





# 4b. Was there a significant effect of race training group at T4?




