# Projections Script: Will split up projections by multicampus buildings. 

# # Projections Sheet 
library(RODBC)
library(tidyverse)
library(googlesheets4)

# Projections Upload 

panda <- odbcConnectAccess2007('G:/Shared drives/Planning and Analysis/1 - operations management/data/mega files/Planning & Analysis.accdb')
projections <- sqlQuery(panda, 'SELECT* FROM projections WHERE fiscal_year = 2027')
multicampus <- sqlQuery(panda, 'SELECT* FROM multicampus')
names <- sqlQuery(panda, 'SELECT* FROM school_names')
odbcCloseAll()

# Prep multicampus 
multicampus_improv <- multicampus %>% 
  
  janitor::clean_names() %>% 
  
  filter(is.na(final_year)) %>% 
  
  mutate(
    expo = last_grade - first_grade + 1
  ) %>% 
  
  uncount(expo) %>% 
  
  group_by(id) %>% 
  
  mutate(
    gradenum = first_grade + row_number()-1, 
    campus_name = case_when(
      sch == 4381 ~ paste("Eliot", campus_name), 
      TRUE ~ campus_name
    )
  ) %>% 
  
  select(
    grouping, sch, name, campus_name, gradenum
  ) 


projection_data <- projections %>% 
  
  filter(budget_group == "core") %>% 
  
  group_by(sch, gradenum, projection_group) %>% 
  
  filter(version == max(version)) %>% 
  
  left_join(multicampus_improv, by = c("sch", "gradenum")) %>% 
  
  left_join(names, by = "sch") %>% 
  
  mutate(
    school_name = case_when(
      is.na(campus_name) ~ schname, 
      TRUE ~ campus_name
    )
  ) %>% 
  
  select(
    sch, 
    sch_year, 
    school_name, 
    grade, 
    gradenum, 
    projection_group, 
    projection
  ) %>% 
  
  arrange(sch_year, school_name, grade)

projections_summary <- projection_data %>% 
  
  group_by(sch, school_name) %>% 
  
  summarize(
    count = sum(projection, na.rm = TRUE)
  )