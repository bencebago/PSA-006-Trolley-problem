# Download raw data and data structure (questions)
# This version is required for fetching the survey. Newer versions doesn't work.
remotes::install_version("qualtRics", version = "3.1.2")

library(tidyverse)
library(qualtRics)
library(jsonlite)
library(lubridate)

# Read the API key from a file. The file should not be on github.
# This is just a text file with one line, that is the API key.
qualtrics_api_key <- read_lines("meta_data/qualtrics_api_key.txt")

# Read survey_id data
qualtrics_survey_ids <- 
  read_csv("meta_data/qualtrics_surveys.csv") %>% 
  filter(survey_name != "PSA006_master")

qualtrics_api_credentials(api_key = qualtrics_api_key,
                          base_url = "https://lapsyde.eu.qualtrics.com")

# Read data from all Qualtrics surveys and merge them into one tibble
# WARNING, only runs with qualtRics 3.1.2!
trolley_raw <- 
  qualtrics_survey_ids %>% 
  mutate(data = map(survey_id ,~fetch_survey( surveyID = .x,
                                              include_display_order = TRUE,
                                              force_request = TRUE, 
                                              label = FALSE,
                                              convert = FALSE) %>% 
                      mutate(practice = as.character(practice)))) %>% 
  unnest(data)

glimpse(trolley_raw)
write_csv(trolley_raw, "data/trolley_raw.csv")

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
