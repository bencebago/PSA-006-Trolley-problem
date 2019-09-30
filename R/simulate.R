library(tidyverse)
library(BayesFactor)
source(here::here("R/draw_values.R"))
source(here::here("R/generate_dataset.R"))
source(here::here("R/get_ttestbf_inference.R"))
source(here::here("R/get_lmbf_inference.R"))


simulate = function(
  bf_thresholds = c(10, 1/10),
  n_per_cell = NULL,
  study = "both", #either "study_1", "study_2", or "both"
  true_effect = c("original effect in all cultures", "null effects"), #either  "a culture replicates only", "original effect in all cultures"
  prior = .23,
  class_mean = 6,
  class_sd = 2.19,
  mean_difference = 1.01,
  class_names = c("a", "b", "c")
){
  
  if(true_effect == "original effect in all cultures"){
    correct_inferences_study_1_culture = c("replicated", "replicated", "replicated")
    correct_inferences_study_2_culture = c("replicated", "replicated", "replicated")
    incorrect_inferences_study_1_culture = c("not replicated", "not replicated", "not replicated")
    incorrect_inferences_study_2_culture = c("not replicated", "not replicated", "not replicated")
    
  } else if(true_effect == "null effects"){
    correct_inferences_study_1_culture = c("not replicated", "not replicated", "not replicated")
    correct_inferences_study_2_culture = c("not replicated", "not replicated", "not replicated")
    incorrect_inferences_study_1_culture = c("replicated", "replicated", "replicated")
    incorrect_inferences_study_2_culture = c("replicated", "replicated", "replicated")
    
  } else {print("ERROR: cant determine true effect without valid true_effect")}
  
  if(study == "study_1"){
    correct_inferences = c(correct_inferences_study_1_culture)
    incorrect_inferences = c(incorrect_inferences_study_1_culture)
  }
  if(study == "study_2"){
    correct_inferences = c(correct_inferences_study_2_culture)
    incorrect_inferences = c(incorrect_inferences_study_2_culture)
  }
  
  
  output_table_list_counter = 0
  output_table_list = list(NA)
  
  #Study 1
  
  if(study == "study_1" ){
    if(true_effect == "original effect in all cultures"){
      # Responses to a task will be diffrent in all cultures
      full_data <- 
        generate_dataset(n_per_cell, class_mean, class_sd, tasks = 2) %>% 
        mutate_at(vars(starts_with("v1_")), 
                  ~draw_values(n_per_cell, class_mean - mean_difference, class_sd))
      
    } else if(true_effect == "null effects"){
      # There will be no differences between the variables
      full_data <- generate_dataset(n_per_cell, class_mean, class_sd, tasks = 2)
      
    } else(print("ERROR: No valid effect specified"))
    
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
  if(study == "study_2"){
    if(true_effect == "original effect in all cultures"){
      # Responses to the v1 task will be diffrent in all cultures
      full_data <- 
        generate_dataset(n_per_cell, class_mean, class_sd) %>% 
        mutate_at(vars(starts_with("v1_")), ~draw_values(n_per_cell, class_mean - mean_difference, class_sd))
      
    } else if(true_effect == "null effects"){
      # There will be no differences between the variables
      full_data <- generate_dataset(n_per_cell, class_mean, class_sd)
      
    } else(print("ERROR: No valid effect specified"))
    
    # Calculate stats and inference
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
  
  # output_table_merged = do.call("rbind", output_table_list)
  output_table_merged <- bind_rows(output_table_list)
  
  all_inference_correct = all(output_table_merged[,"inference"] == correct_inferences)
  # any_inference_incorrect = any(output_table_merged[,"inference"] == incorrect_inferences)
  
  
  return(c(
           # true_effect, 
           # n_per_cell, 
           # study,
           all_inference_correct 
           # any_inference_incorrect
           ))
}
