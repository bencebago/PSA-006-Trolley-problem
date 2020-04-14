# Data cleaning and analysis

library(tidyverse)
library(qualtRics)
library(jsonlite)

# Read the API key from a file. The file should not be on github.
# This is just a text file with one line, that is the API key.
qualtrics_api_key <- read_lines("meta_data/qualtrics_api_key.txt")

# Read survey_id data
qualtrics_survey_ids <- 
  read_csv("meta_data/qualtrics_surveys.csv") %>% 
  filter(survey_name != "PSA006_master")    

# Correct answers for attention check for the tasks
correct_answers <- read_csv("meta_data/correct_answers.csv")

qualtrics_api_credentials(api_key = qualtrics_api_key,
                          base_url = "https://lapsyde.eu.qualtrics.com")

# Read data from all Qualtrics surveys and merge them into one tibble
trolley_raw <- 
  qualtrics_survey_ids %>% 
  mutate(data = map(survey_id ,~fetch_survey( surveyID = .x,
                                              include_display_order = TRUE,
                                              force_request = TRUE, 
                                              label = FALSE,
                                              convert = FALSE))) %>% 
  unnest(data)
  
glimpse(trolley_raw)
  
# Proces data
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
  filter(Progress >= 98) %>% 
  # Remove all practice runs
  filter(!practice) %>% 
  # Exclude careless responders
  filter_at(vars(careless_1, careless_2), all_vars(. != 1)) %>%
  filter(careless_3 != 2) %>%
  # Remove confused participants 
  filter(confusion != 3) %>%
  # Exclude those with familiarity of the topic
  filter(familiarity <= 3) %>% 
  # Technical problems is not in the master questionnaire!
  filter(technical_problems != 2) %>% 
  # Exclude those who did not fill the questionnaire on their native language
  filter(native_language != 2) %>% 
  # Remove those who can't tell which scenarios they saw
  left_join(correct_answers, by = "scenario1") %>% 
  # Use the correct answer descriptions as attention check for the tasks
  # Only those are kept who answered to either of the tasks correctly
  filter(trolley_attention == trolley_answer) %>%
  select(-trolley_answer, -speedboat_answer)


# Questionnaire structure processing ------------------------------------------------
# This creates a codebook for the answer, based on the master questionnaire
# Read the master 
master <- read_json("questionnaire_structure/PSA006_master.qsf")

answer_options <-
  tibble(q = master$SurveyElements) %>% 
  # Rectagle list data
  hoist(q, 
        qid = c("Payload", "DataExportTag"),
        question_text = c("Payload", "QuestionText"),
        answer = c("Payload", "Choices")) %>% 
  unnest_longer(answer) %>% 
  hoist(answer, answer_label = "Display") %>% 
  drop_na(qid) %>% 
  # Take out the html from question text
  mutate(question_text = str_remove_all(question_text, "<[^>]*>")) %>%
  select(-q, -answer, answer_value = answer_id) %>%
  # Take out the consent questions
  # filter(!str_detect(qid, "consent")) %>% 
  arrange(qid)
  
# Print all questions and answer options
print(answer_options, n = 500)
  