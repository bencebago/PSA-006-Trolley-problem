# Data cleaning and analysis

library(tidyverse)
library(qualtRics)

# Read the API key from a file. The file should not be on github.
# This is just a text file with one line, that is the API key.
qualtrics_api_key <- read_lines("meta_data/qualtrics_api_key.txt")

# Read survey_id data
qualtrics_survey_ids <- 
  read_csv("meta_data/qualtrics_surveys.csv") %>% 
  filter(survey_name != "PSA006_master")    

# Correct answers for attention check
correct_answers <- read_csv("meta_data//correct_answers.csv")

qualtrics_api_credentials(api_key = qualtrics_api_key,
                          base_url = "https://lapsyde.eu.qualtrics.com")

# Read data from all Qualtrics surveys and merge them into one tibble
trolley_raw <- 
  qualtrics_survey_ids %>% 
  mutate(data = map(survey_id ,~fetch_survey(surveyID = .x,
                                 include_display_order = TRUE,
                                 force_request = TRUE))) %>% 
  filter(str_detect(survey_name, "Southern")) %>% 
  unnest(data)
  
glimpse(trolley_raw)
  
# TODO: 
# Only Native speakers should be kept

trolley <-
  trolley_raw %>% 
  # Rename randomization variables
  # rename(scenario1 = FL_24_DO, 
  #        scenario2 = FL_22_DO) %>%
  # Remove those who didn't finish the questionnaire
  filter(Progress == 100) %>% 
  # Remove underage participants
  filter(age_1 >= 18) %>% 
  # Exclude careless responders
  filter_at(vars(starts_with("careless_")), all_vars(. != "Yes")) %>%
  # Remove confused participants 
  filter(confusion != "By the time I answered the questions on the preceding page, I was still somewhat confused by the material.  I do not think I understood this material well enough to give reasonable answers to the questions I was asked.") %>%
  # Exclude those with familiarity of the topic
  filter(familiarity <= 3) %>% 
  # Exclude those who wrote letter to the technical prolems field. This shold be improved after we see the data (e.g. answers such as "No" shouldn't be excluded)
  filter(is.na(str_match(technical_problems, "[a-zA-Z ]+"))) %>% 
  # Remove those who can't tell which scenarios they saw
  left_join(correct_answers, by = "scenario1") %>%
  filter(trolley_attention == trolley_answer &
         speedboat_attention == speedboat_answer) %>% 
  select(-trolley_answer, -speedboat_answer)

  

  
