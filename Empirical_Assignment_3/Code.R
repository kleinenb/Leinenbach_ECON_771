#Empirical Assignment 3

#Libraries
library(haven)
library(dplyr)
library(tidyr)
library(rdrobust)
library(ggplot2)
library(rddensity)
library(fixest)

#Load Data
setwd("/Users/katieleinenbach/Desktop/OneDrive - Emory University/ECON 771/Data/Ericson 2014/DataFiles")
data <- read_dta('Data_main.dta')
subsidy <- read_dta('Data_subsidyinfo.dta')

df <- data %>% left_join(subsidy,by="PDPregion")

#Question 1
df <- df %>%
  group_by(uniqueID) %>% 
  mutate(cohort = min(year)) # Add cohort (year of plan introduction)

sum_stats_mean <- df %>%
  filter(year == cohort) %>% 
  select(uniqueID, year, premium, deductible) %>%
  pivot_longer(c(premium, deductible), names_to="variable", values_to="value") %>%
  group_by(variable, year) %>%
  summarize(Mean = round(mean(value)), SD = round(sd(value))) %>% 
  pivot_longer(c(Mean, SD), names_to="stat", values_to="value") %>%
  pivot_wider(names_from=year, values_from=value)

sum_stats_benefit <- df %>%
  filter(year == cohort) %>% 
  select(uniqueID, year, benefit) %>%
  mutate(enhanced = ifelse(benefit=="E", 1, 0)) %>% 
  group_by(year) %>% 
  summarize(enhanced = round(mean(enhanced), 2)) %>%
  pivot_wider(names_from=year, values_from=enhanced)

sum_stats_us = df %>%
  filter(year == cohort) %>% 
  group_by(orgParentCode) %>% 
  mutate(cohort_state = min(year), state_offered = ifelse(year > cohort_state, 1, 0)) %>%
  group_by(year) %>% 
  summarize(state_offered = round(mean(state_offered), 2))

sum_stats_state = df %>%
  filter(year == cohort) %>% 
  group_by(orgParentCode, state) %>% 
  mutate(cohort_state = min(year), state_offered = ifelse(year > cohort_state, 1, 0)) %>%
  group_by(year) %>% 
  summarize(state_offered = round(mean(state_offered), 2))

unique_firms <- df %>%
  group_by(cohort) %>%
  summarize(firms = length(unique(orgParentCode)))

plan <- df %>%
  group_by(cohort, year) %>%
  summarize(plans = length(planName))
plan <- plan[plan$cohort == plan$year,]



#Question 2
df_2006 <- df %>% 
  filter(year == 2006 & benefit == "B") %>% 
  group_by(state) %>% 
  mutate(enroll_share = log(enrollmentImpute / sum(enrollmentImpute)), 
         rel_premium = premium - s2006) %>% 
  filter(abs(rel_premium) <= 10)

rd_plot <- rdplot(df_2006$enroll_share, df_2006$rel_premium, 
                 h = 10, p = 4, nbins = 20,
                 title="Figure 3", 
                 x.label="Monthly premium - LIS Subsidy, 2006", 
                 y.label="log enrollment share, 2006")

#Question 3
rd_plot_10 <- rdplot(df_2006$enroll_share, df_2006$rel_premium, 
                  h = 10, p = 4, nbins = 10,
                  title="Figure 3: 10 bins", 
                  x.label="Monthly premium - LIS Subsidy, 2006", 
                  y.label="log enrollment share, 2006")

rd_plot_30 <- rdplot(df_2006$enroll_share, df_2006$rel_premium, 
                     h = 10, p = 4, nbins = 30,
                     title="Figure 3: 30 bins", 
                     x.label="Monthly premium - LIS Subsidy, 2006", 
                     y.label="log enrollment share, 2006")


#Question 4
rdr <- rdrobust(df_2006$enroll_share, df_2006$rel_premium)
summary(rdr)
rdplot <- rdplot(df_2006$enroll_share, df_2006$rel_premium,
       title="", 
       x.label="Monthly premium − LIS subsidy, 2006", 
       y.label="log enrollment share, 2006")


#Question 5
plot <- rdplotdensity(rddensity(df_2006$rel_premium), df_2006$rel_premium)

#Question 6
summary(rdrobust(df_2006$enroll_share, df_2006$rel_premium, h = 4))

#Question 7
summary(rdrobust(df_2006$enroll_share, df_2006$rel_premium))

#Question 8
mod <- feols(log(premium) ~  1 | state + year | enroll_share ~ LIS , data = df_2006, cluster="orgParentCode")
mod$iv_residuals







