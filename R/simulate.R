library(tidyverse)
library(BayesFactor)
source("R/draw_values.R")
source("R/generate_dataset.R")
source("R/get_bf_inference.R")


simulate = function(
  bf_thresholds = c(5, 1/5),
  true_effect = "original effect in all cultures", #either  "a culture replicates only", "original effect in all cultures"
  n_per_cell = NULL,
  which_study_to_test = "study_1", #either "study_1", "study_2", or "both"
  prior = 3, #either "medium", "wide", "ultrawide", BayesFactor package default is "medium"
  class_mean = 6,
  class_sd = 2.19,
  mean_difference = 1.01,
  class_names = c("a", "b", "c")
){
  
  if(true_effect == "a culture replicates only"){
    correct_inferences_study_1_culture = c("replicated", "not replicated", "not replicated")
    correct_inferences_study_2_culture = c("replicated", "not replicated", "not replicated")
    incorrect_inferences_study_1_culture = c("not replicated", "replicated", "replicated")
    incorrect_inferences_study_2_culture = c("not replicated", "replicated", "replicated")
    
  } else if(true_effect == "original effect in all cultures"){
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

  if(which_study_to_test == "study_1"){
    correct_inferences = c(correct_inferences_study_1_culture)
    incorrect_inferences = c(incorrect_inferences_study_1_culture)
  }
  if(which_study_to_test == "study_2"){
    correct_inferences = c(correct_inferences_study_2_culture)
    incorrect_inferences = c(incorrect_inferences_study_2_culture)
  }
  if(which_study_to_test == "both"){
    correct_inferences = c(correct_inferences_study_1_culture, correct_inferences_study_2_culture)
    incorrect_inferences = c(incorrect_inferences_study_1_culture, incorrect_inferences_study_2_culture)
  }
  
  output_table_list_counter = 0
  output_table_list = list(NA)
  
  #Study 1
  
  if(which_study_to_test == "study_1" | which_study_to_test == "both"){
    if(true_effect == "a culture replicates only"){
      # Responses to a task will be diffrent in one culture
      full_data <-
        generate_dataset(n_per_cell, class_mean, class_sd, tasks = 2) %>% 
        mutate(v1_a = draw_values(n_per_cell, class_mean - mean_difference, class_sd))
      
    } else if(true_effect == "original effect in all cultures"){
      # Responses to a task will be diffrent in all cultures
      full_data <- 
        generate_dataset(n_per_cell, class_mean, class_sd, tasks = 2) %>% 
        mutate_at(vars(starts_with("v1_")), 
                  ~draw_values(n_per_cell, class_mean - mean_difference, class_sd))
      
    } else if(true_effect == "null effects"){
      # There will be no differences between the variables
      full_data <- generate_dataset(n_per_cell, class_mean, class_sd, tasks = 2)
      
    } else(print("ERROR: No valid effect specified"))
    
    output_table <-
      full_data %>% 
      gather(code, choice) %>% 
      mutate(condition = str_extract(code, "\\d+") %>% as.integer(),
             class = str_sub(code, -1) %>% factor(),
             study = "study_1") %>% 
      get_bf_inference(., 
                       groups = c("study", "class"), 
                       formula = choice ~ condition,  
                       bf_thresholds = bf_thresholds,
                       prior = prior)
    
    output_table_list_counter = output_table_list_counter + 1 
    output_table_list[[output_table_list_counter]] = output_table
  }
  
  # Study 2
  if(which_study_to_test == "study_2" | which_study_to_test == "both"){
    if(true_effect == "a culture replicates only"){
      # Responses to the v1 task will be diffrent in one culture
      full_data <-
        generate_dataset(n_per_cell, class_mean, class_sd) %>% 
        mutate(v1_a = draw_values(n_per_cell, class_mean - mean_difference, class_sd))
      
    } else if(true_effect == "original effect in all cultures"){
      # Responses to the v1 task will be diffrent in all cultures
      full_data <- 
        generate_dataset(n_per_cell, class_mean, class_sd) %>% 
        mutate_at(vars(starts_with("v1_")), ~draw_values(n_per_cell, class_mean - mean_difference, class_sd))
      
     } else if(true_effect == "null effects"){
       # There will be no differences between the variables
      full_data <- generate_dataset(n_per_cell, class_mean, class_sd)
      
    } else(print("ERROR: No valid effect specified"))
    
    
    output_table <-
      full_data %>% 
      gather(code, choice) %>% 
      mutate(condition = str_extract(code, "\\d+") %>% as.integer(),
             class = str_sub(code, -1) %>% factor(),
             study = "study_2"
             ) %>% 
      # There are four conditions here, but we only need the first two
      filter(condition %in% 1:2) %>% 
      get_bf_inference(., 
                       groups = c("study", "class"), 
                       formula = choice ~ condition, 
                       bf_thresholds = bf_thresholds,
                       prior = prior)
    
    output_table_list_counter = output_table_list_counter + 1 
    output_table_list[[output_table_list_counter]] = output_table
    
  }
  
  # output_table_merged = do.call("rbind", output_table_list)
  output_table_merged <- bind_rows(output_table_list)
  
  all_inference_correct = all(output_table_merged[,"inference"] == correct_inferences)
  any_inference_incorrect = any(output_table_merged[,"inference"] == incorrect_inferences)
  
  
  return(c(true_effect, 
           n_per_cell, 
           which_study_to_test,
           all_inference_correct, 
           any_inference_incorrect))
}

