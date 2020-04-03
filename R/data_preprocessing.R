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
  unnest(data)
  
glimpse(trolley_raw)
  
# TODO: 
# Only Native speakers should be kept

trolley <-
  trolley_raw %>% 
  # Aggregate randomization variables
  mutate(scenario1 = case_when(FL_24_DO_Footbridgepole == 1 ~ "Pole",
                               FL_24_DO_Footbridgeswitch == 1 ~ "Switch",
                               TRUE ~ NA_character_),
         scenario2 = case_when(FL_22_DO_Standardswitch == 1 ~ "Standardswitch",
                               FL_22_DO_Standardfootbridge == 1 ~ "Standardfootbridge",
                               FL_22_DO_Loop == 1 ~ "Loop",
                               FL_22_DO_Obstaclecollide == 1 ~ "Obstaclecollide",
                               TRUE ~ NA_character_)) %>% 
  # Remove those who didn't finish the questionnaire
  filter(Progress == 100) %>% 
  # Exclude careless responders
  filter_at(vars(careless_1, careless_2), all_vars(. != "Yes")) %>%
  filter(careless_3 != "No") %>%
  # Remove confused participants 
  filter(confusion != "By the time I answered the questions on the preceding page, I was still somewhat confused by the material.  I do not think I understood this material well enough to give reasonable answers to the questions I was asked.") %>%
  # Exclude those with familiarity of the topic
  filter(familiarity <= 3) %>% 
  # Exclude those who wrote letter to the technical prolems field. This shold be improved after we see the data (e.g. answers such as "No" shouldn't be excluded)
  filter(is.na(str_match(technical_problems, "[a-zA-Z ]+"))) %>% 
  # Remove those who can't tell which scenarios they saw
  left_join(correct_answers, by = "scenario1") %>%
  filter(trolley_attention == trolley_answer) %>% 
  select(-trolley_answer, -speedboat_answer)



  
