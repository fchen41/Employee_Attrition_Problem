# BUSINESS UNDERSTANDING----

#load library
library(tidyverse)
library(tidyquant)
library(forcats)
library(stringr)
library(readxl)

#load data
train_raw_tbl <- read_excel("./00_Data/telco_train.xlsx", sheet = 1)

#data subset
dept_job_role_tbl <- 
  train_raw_tbl %>% 
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

#1. Business Science Problem Framework----

#1A. View Business as Machine----

# Department and Job Role
# Define Objectives: Retrain Hign performers
# Assess Outcomes: TBD
dept_job_role_tbl %>% 
  group_by(Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pct = n / sum(n))

#1B. Understand the Drivers----

# Investigate Objectices: 16.1% Percent Attrition
# Synthesize Outcomes: High counts and high percentage
# Hypothesize Drivers: Job Role and Departments

#By department----
dept_job_role_tbl %>% 
  group_by(Department, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(Department) %>% 
  mutate(pct = n / sum(n))

#By job role----
dept_job_role_tbl_1 <- 
  dept_job_role_tbl %>% 
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(Department, JobRole) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  filter(Attrition %in% "Yes")
#It must exist some problems relates to job roles and department, e.g. sales representative

# 1C. Measuring the Drivers----

# Collect Information on Employee Attrition----

# Develop KPI's: Industry KPIs: 8.8%----
## 8.8% as a consevative KPI compared to Bureau of Labor Statistics

# glimpse(train_raw_tbl): see all info
# features(descriptive/employment/compensation/survey/performance/work_life/training&education/time_based) 
# Breakdown data collection into strategic areas. 

dept_job_role_tbl_2 <- 
  dept_job_role_tbl_1 %>% 
  arrange(desc(pct)) %>% 
  mutate(
    above_industry_avg = ifelse(pct > 0.088, "Yes", "No")
  )

# 1D. Uncover Problems and Opportunities----

calculate_attrition_cost <- function(
  # Employee from excel
  n                    = 1,
  salary               = 80000,
  
  # Direct Costs
  separation_cost      = 500,
  vacancy_cost         = 10000,
  acquisition_cost     = 4900,
  placement_cost       = 3500,
  
  # Productivity Costs
  net_revenue_per_employee = 250000,
  workdays_per_year        = 240,
  workdays_position_open   = 40,
  workdays_onboarding      = 60,
  onboarding_efficiency    = 0.50
) {
  # Direct Costs
  direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  # Lost Productivity Costs
  productivity_cost <- net_revenue_per_employee / workdays_per_year * 
    (workdays_position_open + workdays_onboarding * onboarding_efficiency) 
  
  # Savings of Salary & Benefits (Cost Reduction)
  salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
  
  # Estimated Turnover Per Employee
  cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
  
  # Total Cost of Employee Turnover
  total_cost <- n * cost_per_employee
  
  return(total_cost)
}

# Calculate Cost by Job Role----
dept_job_role_tbl_2 %>% 
  mutate(cost_of_attrition = calculate_attrition_cost(n = n))
