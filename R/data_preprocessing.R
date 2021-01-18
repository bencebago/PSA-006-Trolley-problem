# Pre-process raw data to analyzable data
library(tidyverse)
library(jsonlite)
library(lubridate)
library(vroom)

# Read raw data
trolley_raw <- vroom("data/trolley_raw.csv")

# Correct answers for attention check for the tasks
correct_answers_1 <- read_csv("meta_data/correct_answers_1.csv")
correct_answers_2 <- read_csv("meta_data/correct_answers_2.csv")

# Remove all trial runs and incomplete surveys
trolley_proc <-
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
  filter(str_detect(str_to_lower(practice), "false")) %>% 
  # Remove the answers for a particular lab that has unflagged practice data
  filter(!(lab == "TUR_021" & StartDate < date("2020-04-22")),
         !(lab == "AUT_003" & StartDate < date("2020-06-18")),
         !(lab == "USA_095"))

# write_csv(trolley_proc, "data/trolley_preprocessed.csv")

# Exclude participants specified in the preregistration
trolley <-
  trolley_proc %>% 
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
  filter(native_language != 2)

# Remove those who can't tell which scenarios they saw
# Use the correct answer descriptions as attention check for the tasks
# Only those are kept who answered either of the tasks correctly

study1a <- 
  trolley %>%
  left_join(correct_answers_1, by = "scenario1") %>% 
  filter(trolley_attention == trolley_answer) %>%
  select(-trolley_answer, -speedboat_answer)

study1b <- 
  trolley %>% 
  left_join(correct_answers_1, by = "scenario1") %>% 
  filter(speedboat_attention == speedboat_answer) %>%
  select(-trolley_answer, -speedboat_answer)

study2a <-
  trolley %>% 
  left_join(correct_answers_2, by = "scenario2") %>% 
  filter(trolley_attention == trolley_answer) %>%
  select(-trolley_answer, -speedboat_answer)

study2b <-
  trolley %>% 
  left_join(correct_answers_2, by = "scenario2") %>% 
  filter(speedboat_attention == speedboat_answer) %>%
  select(-trolley_answer, -speedboat_answer)

write_csv(study1a, "data/study1a.csv")
write_csv(study1b, "data/study1b.csv")
write_csv(study2a, "data/study2a.csv")
write_csv(study2a, "data/study2b.csv")

