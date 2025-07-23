
# Perparation
library(tidyverse)
library(openxlsx)
library(purrr)
library(httr)
library(jsonlite)
library(openai)

Sys.setenv(OPENAI_API_KEY = "x")

####################################################################
## Initial processing of the ILO data 
####################################################################

structure <- read.xlsx("Input//ISCO-08 EN Structure and definitions.xlsx") %>%
  janitor::clean_names () 


definition_l1 <- structure %>%
  # filter only level-2 for taxonomies
  filter (level == "1") %>%
  mutate(
    tasks_include = str_remove(tasks_include, paste0("Tasks performed by ", str_to_lower(title_en)," usually include: \\s*")),
    task_list = str_split(tasks_include, ";\\s*")
  ) %>%
  select(isco_08_code, task_list) %>%
  unnest(task_list) %>%
  mutate(
    task = str_trim(task_list),
    task = str_remove(task, "\\.$"),  # Remove final period
    task = str_to_sentence(task)
  ) %>%
  select(isco_08_code, task)

definition_l2 <- structure %>%
  # filter only level-2 for taxonomies
  filter (level == "2") %>%
  select (isco_08_code, tasks_include) %>%
  mutate(
    tasks_include = str_remove(tasks_include, "Tasks performed by workers in this sub-major group usually include: \\s*"),
    task_list = str_split(tasks_include, ";\\s*")
  ) %>%
  select(isco_08_code, task_list) %>%
  unnest(task_list) %>%
  mutate(
    task = str_trim(task_list),
    task = str_remove(task, "\\.$"),  # Remove final period
    task = str_to_sentence(task)
  ) %>%
  select(isco_08_code, task)

definition_l3 <- structure %>%
  # filter only level-3 for taxonomies
  filter (level == "3") %>%
  select (isco_08_code, tasks_include) %>%
  mutate(
    tasks_include = str_remove(tasks_include, "Tasks performed usually include:\\s*"),
    task_list = str_split(tasks_include, ";\\s*")
  ) %>%
  select(isco_08_code, task_list) %>%
  unnest(task_list) %>%
  mutate(
    task = str_trim(task_list),
    task = str_remove(task, "\\.$"),  # Remove final period
    task = str_to_sentence(task)
  ) %>%
  select(isco_08_code, task)

definition_l4 <- structure %>%
  # filter only level-4 for taxonomies
  filter (level == "4") %>%
  select (isco_08_code, tasks_include) %>%
  mutate(
    tasks_include = str_remove(tasks_include, "Tasks include -\\s*"),  # Remove lead-in
    task_list = str_extract_all(tasks_include, "\\(\\w\\)\\s[^\\(\\)]+(?:;|\\.)")  # Extract bullet points
  ) %>%
  select(isco_08_code, task_list) %>%
  unnest(task_list) %>%
  mutate(
    task = str_remove(task_list, "^\\(\\w\\)\\s*"),  # Remove (a), (b), etc.
    task = str_trim(task),
    task = str_remove(task, ";$|\\.$"),  # Remove trailing punctuation
    task = str_to_sentence(task)
  ) %>%
  select(isco_08_code, task) 

definition <- definition_l1 %>%
  rbind (definition_l2) %>%
  rbind (definition_l3) %>%
  rbind (definition_l4) %>%
  arrange (isco_08_code) %>% drop_na (task)

remove (definition_l1) 
remove (definition_l2) 
remove (definition_l3) 
remove (definition_l4) 

####################################################################
## (NLP-based grouping )
# Option 1: Embedding + K-means (Best balance of speed + meaning)
# Concatenate ISCO Job Title or Family to the Task Text Before Embedding (Soft Bias)
####################################################################

library(text2vec)

definition_augmented <- definition %>%
  # Match with code
  mutate (l1 = str_sub(isco_08_code, 1, 1),
          l2 = str_sub(isco_08_code, 1, 2),
          l3 = str_sub(isco_08_code, 1, 3),
          l4 = str_sub(isco_08_code, 1, 4)) %>%
  # Match with title
  mutate (t1 = structure$title_en[match(l1, structure$isco_08_code)],
          t2 = structure$title_en[match(l2, structure$isco_08_code)],
          t3 = structure$title_en[match(l3, structure$isco_08_code)],
          t4 = structure$title_en[match(l4, structure$isco_08_code)]) %>%
  # Clean up title
  mutate (t4 = ifelse(l4 == l3, "", t4),
          t3 = ifelse(l3 == l2, "", t3),
          t2 = ifelse(l2 == l1, "", t2)) %>%
  # Concat text with titles and families
  mutate (task_augmented = paste0(t1, ">", t2, ">", t3, ">", t4, "-", task)) %>%
  select (isco_08_code, task, task_augmented)

write.xlsx (definition_augmented, "Temp//ISCO task list augmented.xlsx")
