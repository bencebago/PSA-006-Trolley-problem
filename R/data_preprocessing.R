# Pre-process raw data to analyzable data
library(tidyverse)
library(jsonlite)
library(lubridate)
library(vroom)

# Read raw data
trolley_raw <- vroom("data/trolley_raw.csv")

# Correct answers for attention check for the tasks
correct_answers <- read_csv("meta_data/correct_answers.csv")

# Some country codes are not in standard iso3 format, we need those for recoding
custom_countries <- 
  c("LEB" = "LBN", 
    "BUL" = "BGR", 
    "SPA" = "ESP", 
    "SWT" = "CHE")

# Remove all trial runs and incomplete surveys and remove unnecessary variables
trolley_proc <-
  trolley_raw %>% 
  # Remove those who didn't finish the questionnaire
  filter(Progress >= 98) %>% 
  # Remove all practice runs
  filter(str_detect(str_to_lower(practice), "false")) %>% 
  # Remove the answers for a particular lab that has unflagged practice data
  filter(!(lab == "TUR_021" & StartDate < date("2020-04-22")),
         !(lab == "AUT_003" & StartDate < date("2020-06-18")),
         !(lab == "USA_095")) %>% 
  # Remove all variables that are not needed
  select(
         -c(Status:RecordedDate,
            RecipientLastName:IND_006,
            second:LS,
            JPN_003_debrief,
            image1:image6,
            PHL_004_consent:PHL_003_consent,
            confirmCode,
            Q_URL,
            imgset,
            practice))

write_csv(trolley_proc, "data/trolley_preprocessed.csv")

# Flag participants for inclusion based on the preregistration
trolley <-
  trolley_proc %>% 
  # Aggregate randomization variables
  mutate(scenario1 = case_when(FL_24_DO_Footbridgepole == 1 ~ "Pole",
                               FL_24_DO_Footbridgeswitch == 1 ~ "Switch",
                               TRUE ~ NA_character_),
         scenario2 = case_when(FL_22_DO_Standardswitch == 1 ~ "Standardswitch",
                               FL_22_DO_Standardfootbridge == 1 ~ "Standardfootbridge",
                               FL_22_DO_Loop == 1 ~ "Loop",
                               FL_22_DO_Obstaclecollide == 1 ~ "Obstaclecollide",
                               TRUE ~ NA_character_),
  # Flag careless responders
         include_nocareless = if_else(careless_1 != 1 &
                                      careless_2 != 1 &
                                      careless_3 != 2,
                                      TRUE, FALSE),
   # Flag confused participants
         include_noconfusion = if_else(confusion != 3, TRUE, FALSE),
   # Flag those with familiarity of the topic
         include_nofamiliarity = if_else(familiarity <= 3, TRUE, FALSE),
   # Flag those with technical problems
         include_notechproblem = if_else(technical_problems != 2, TRUE, FALSE),
   # Flag those who did not fill the questionnaire on their native language
        include_nonativelang = if_else(native_language != 2, TRUE, FALSE),
   # Flag those without any exclusion criterion
        include_noproblem = if_else(include_nocareless & 
                                    include_noconfusion & 
                                    include_nofamiliarity & 
                                    include_notechproblem &
                                    include_nonativelang, 
                                    TRUE, FALSE)) %>%
   # Flag those that are eligible to analysis in each study
  left_join(select(correct_answers, -scenario2) %>% drop_na(scenario1), 
            by = c("scenario1")) %>%
  mutate(include_study1a = if_else((trolley_attention == trolley_answer) & 
                                   include_noproblem, 
                                   TRUE, FALSE),
         include_study1b = if_else((speedboat_attention == speedboat_answer) & 
                                   include_noproblem, 
                                   TRUE, FALSE)) %>% 
  select(-trolley_answer, -speedboat_answer) %>% 
  left_join(select(correct_answers, -scenario1) %>% drop_na(scenario2), 
            by = c("scenario2")) %>%   
  mutate(include_study2a = if_else((trolley_attention == trolley_answer) & 
                                   include_noproblem, 
                                   TRUE, FALSE),
         include_study2b = if_else((speedboat_attention == speedboat_answer) & 
                                   include_noproblem, 
                                   TRUE, FALSE)) %>% 
  select(-trolley_answer, -speedboat_answer) %>% 
  # Add processed variables
  mutate(country3 = str_extract(lab, "[A-Z]+") %>% 
                    # Some country codes are not standard iso3 codes; replace
                    recode(., !!!custom_countries),
         # Some labs collected data in other countries; correct
         country3 = case_when(lab == "GBR_001" ~ "PAK",
                              lab == "GBR_031" ~ "BRA",
                              lab == "GBR_060" ~ "DNK",
                              TRUE ~ country3),
         # Add region, based on the survey name
         Region = str_remove(survey_name, "PSA006_"),
         # Age is a multiple choice question that starts(1) at 18
         Age = age_1 + 17,
         Gender = sex,
         # select those that have a higher education
         edu_high = education_leve > 2,
         # Higher education is recorded differently in Germany and Austria
         edu_high_ger = education_level_germ > 4,
         `Higher education` = case_when(country3 %in% c("AUT", "DEU") ~ edu_high_ger,
                                        TRUE ~ edu_high)) %>% 
  select(-edu_high, -edu_high_ger)

write_csv(trolley, "data/trolley.csv")


