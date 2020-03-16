# Data cleaning and analysis

library(tidyverse)
library(qualtRics)

# Read the API key from a file. The file should not be on github.
# qualtrics_api_key <- read_lines("qualtrics_api_key.txt")
# 
# qualtrics_api_credentials(api_key = qualtrics_api_key, 
#                           base_url = "https://lapsyde.eu.qualtrics.com")
# 
# qualtRics::fetch_survey(surveyID = "SV_0N8LUtb9tTQHJTn",
#                         include_display_order = TRUE
#                         )

# Read file manually for being able to do the cleaning script

trolley_raw <- qualtRics::read_survey("data/PSA006_Southern_March+7,+2020_15.32.csv")
correct_answers <- read_csv("data/correct_answers.csv")

glimpse(trolley_raw)

# TODO: 
# Only Native speakers should be kept


trolley <-
  trolley_raw %>% 
  # Rename randomization variables
  rename(scenario1 = FL_24_DO, 
         scenario2 = FL_22_DO) %>%
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

  

  
