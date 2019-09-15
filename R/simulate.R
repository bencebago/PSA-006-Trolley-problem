library(tidyverse)
library(BayesFactor)
source(here::here("R/draw_values.R"))
source(here::here("R/generate_dataset.R"))
source(here::here("R/get_ttestbf_inference.R"))
source(here::here("R/get_lmbf_inference.R"))

simulate = function(
  bf_thresholds = c(10, 1/10),
  n_per_cell = NULL,
  which_study_to_test = "study_1", #either "study_1", "study_2", or "both"
  prior = .23,
  class_mean = 6,
  class_sd = 2.19,
  mean_difference = 1.01,
  class_names = c("a", "b", "c")
){
    correct_inferences_study_1_culture = rep("replicated", 3) 
    correct_inferences_study_2_culture = rep("replicated", 3)
    incorrect_inferences_study_1_culture = rep("not replicated", 3)
    incorrect_inferences_study_2_culture = rep("not replicated", 3)
    
  if(which_study_to_test == "study_1"){
    correct_inferences = correct_inferences_study_1_culture
    incorrect_inferences = incorrect_inferences_study_1_culture
  }
  if(which_study_to_test == "study_2"){
    correct_inferences = correct_inferences_study_2_culture
    incorrect_inferences = incorrect_inferences_study_2_culture
  }
  if(which_study_to_test == "both"){
    correct_inferences = c(correct_inferences_study_1_culture, 
                           correct_inferences_study_2_culture)
    incorrect_inferences = c(incorrect_inferences_study_1_culture, 
                             incorrect_inferences_study_2_culture)
  }
  
  output_table_list_counter = 0
  output_table_list = list(NA)
  
  #Study 1
  
  if(which_study_to_test == "study_1" | which_study_to_test == "both"){
     # Generate data where all culture replicates
      full_data <- 
        generate_dataset(n_per_cell, class_mean, class_sd, tasks = 2) %>% 
        mutate_at(vars(starts_with("v1_")), 
                  ~draw_values(n_per_cell, class_mean - mean_difference, class_sd))
      
    # Calculate stats and inference
    output_table <-
      full_data %>% 
      gather(code, choice) %>% 
      mutate(condition = str_extract(code, "\\d+") %>% as.integer(),
             class = str_sub(code, -1) %>% factor(),
             study = "study_1") %>% 
      get_ttestbf_inference(., 
                       groups = c("study", "class"), 
                       formula = choice ~ condition,  
                       bf_thresholds = bf_thresholds,
                       prior = prior)
    
    # What does this do and how to remove?
    output_table_list_counter = output_table_list_counter + 1 
    output_table_list[[output_table_list_counter]] = output_table
  }
  
  # Study 2
  if(which_study_to_test == "study_2" | which_study_to_test == "both"){
    # Responses to the v1 task will be diffrent in all cultures
      full_data <- 
        generate_dataset(n_per_cell, class_mean, class_sd) %>% 
        mutate_at(vars(starts_with("v1_")), ~draw_values(n_per_cell, class_mean - mean_difference, class_sd))
      
     output_table <-
       full_data %>% 
       gather(code, choice) %>% 
       mutate(condition = str_extract(code, "\\d+") %>% as.integer(),
              class = str_sub(code, -1) %>% factor(),
              study = "study_2",
              personal_force = if_else(condition %in% c(2,4), 0, 1) %>% 
                               factor(labels = c("no personal force", "personal force")),
              intention =      if_else(condition %in% c(1, 4), 0, 1) %>% 
                               factor(labels = c("no intention", "intention"))) %>% 
      get_lmbf_inference(., 
                         groups = c("study", "class"), 
                         bf_thresholds = bf_thresholds,
                         prior = prior)
    
     # What does this do and how to remove?
    output_table_list_counter = output_table_list_counter + 1 
    output_table_list[[output_table_list_counter]] = output_table
    
  }
  
  output_table_merged <- bind_rows(output_table_list)
  
  all_inference_correct = all(output_table_merged[,"inference"] == correct_inferences)
  any_inference_incorrect = any(output_table_merged[,"inference"] == incorrect_inferences)
  
  
  return(c(n_per_cell, 
           which_study_to_test,
           all_inference_correct, 
           any_inference_incorrect))
}

