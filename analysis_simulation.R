###############PSA - creating variables

####function to create ratings between 1-9
mysamp <- function(n, m, s, lwr, upr, rounding) {
  samp <- round(rnorm(n, m, s), rounding)
  samp[samp < lwr] <- lwr
  samp[samp > upr] <- upr
  samp
}


library(pbapply)
library(reshape2)
library(BayesFactor)

####### Simulation parameters

simulation_func = function(
  bf_thresholds = c(3, 1/3),
  main_inference_method = "method_1", #either "method_1", where we look at whether the original effect replicates in all culture classes separately
  true_effect = "original effect in all cultures", #either  "a culture replicates only", "original effect in all cultures"
  N_pre_condition_per_class = 3000,
  which_study_to_test = "study_1", #either "study_1", "study_2", or "both"
  prior = "medium", #either "medium", "wide", "ultrawide", BayesFactor package default is "medium"
  SMD_Effect_size_study2 = 1.01/2.19, # this is the effect identified in the original study
  SMD_Effect_size_study1 = 1.02/2.19
){
  class_names = c("a", "b", "c")
  if(true_effect == "a culture replicates only"){
    correct_inferences_study_2_culture = c("replicated", "not", "not")
    correct_inferences_study_1_culture = c("replicated", "not", "not")
    incorrect_inferences_study_2_culture = c("not", "replicated", "replicated")
    incorrect_inferences_study_1_culture = c("not", "replicated", "replicated")
    
  } else if(true_effect == "original effect in all cultures"){
    correct_inferences_study_2_culture = c("replicated", "replicated", "replicated")
    correct_inferences_study_1_culture = c("replicated", "replicated", "replicated")
    incorrect_inferences_study_2_culture = c("not", "not", "not")
    incorrect_inferences_study_1_culture = c("not", "not", "not")
    
  } else if(true_effect == "null effects"){
    correct_inferences_study_2_culture = c("not", "not", "not")
    correct_inferences_study_1_culture = c("not", "not", "not")
    incorrect_inferences_study_2_culture = c("replicated", "replicated", "replicated")
    incorrect_inferences_study_1_culture = c("replicated", "replicated", "replicated")
    
  } else {print("ERROR: cant determine true effect without valid true_effect")}
  
  if(which_study_to_test == "study_2"){
    correct_inferences = c(correct_inferences_study_2_culture)
    incorrect_inferences = c(incorrect_inferences_study_2_culture)
  }
  if(which_study_to_test == "study_1"){
    correct_inferences = c(correct_inferences_study_1_culture)
    incorrect_inferences = c(incorrect_inferences_study_1_culture)
  }
  if(which_study_to_test == "both"){
    correct_inferences = c(correct_inferences_study_1_culture, correct_inferences_study_2_culture)
    incorrect_inferences = c(incorrect_inferences_study_1_culture, incorrect_inferences_study_2_culture)
  }
  
  output_table_list_counter = 0
  output_table_list = list(NA)
  
  
  if(which_study_to_test == "study_2" | which_study_to_test == "both"){
    if(true_effect == "a culture replicates only"){
      v1_a = mysamp(N_pre_condition_per_class, m=6-(2.19*SMD_Effect_size_study2), s=2.19, lwr=1, upr=9, rounding=0)
      v2_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      
    } else if(true_effect == "original effect in all cultures"){
      v1_a = mysamp(N_pre_condition_per_class, m=6-(2.19*SMD_Effect_size_study2), s=2.19, lwr=1, upr=9, rounding=0)
      v2_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_b = mysamp(N_pre_condition_per_class, m=6-(2.19*SMD_Effect_size_study2), s=2.19, lwr=1, upr=9, rounding=0)
      v2_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_c = mysamp(N_pre_condition_per_class, m=6-(2.19*SMD_Effect_size_study2), s=2.19, lwr=1, upr=9, rounding=0)
      v2_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
    } else if(true_effect == "null effects"){
      v1_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
    } else(print("ERROR: No valid effect specified"))
    
    
    #############################################STUDY2 - CULTURE1 ANALYSIS
    full_data=data.frame(v1_a, v2_a, v3_a, v4_a, v1_b, v2_b, v3_b, v4_b, v1_c, v2_c, v3_c, v4_c)
    suppressMessages({
    full_data2=melt(full_data, variable.name="code", value.name="choice")
    })
    
    code<-"v([1-9]+)_([a-z]+)"
    full_data2$condition=as.integer(sub(code, "\\1", full_data2$code))
    full_data2$class=factor(sub(code, "\\2", full_data2$code))
    full_data2$side_effect = factor(ifelse(full_data2$condition>2,1,0))
    full_data2$personal_force = factor(ifelse(full_data2$condition==1|full_data2$condition==3,1,0))
    full_data2$condition1 = factor(ifelse(full_data2$condition==1,1,0))
    full_data2$study = "study_2"
    full_data2$class_type = "culture"
    
    output_table = as.data.frame(matrix(NA, nrow = length(class_names), ncol = 4))
    names(output_table) = c("study", "class", "bf", "inference")
    
    for(i in 1:length(class_names)){
      data_to_analyze = full_data2[full_data2[,"class"] == class_names[i],]
      output_table[i,"study"] = data_to_analyze$study[1]
      output_table[i,"class"] = paste(data_to_analyze$class_type[1], "_",class_names[i], sep = "")
      
      model1=ttestBF(formula=choice ~ condition1, data=data_to_analyze, rscale = prior)
      
      bf = round(as.numeric(matrix(model1)), 3)
      if(bf > bf_thresholds[1]){inference = "replicated"} else if(bf < bf_thresholds[2]){inference = "not"} else {inference = "inconclusive"}
      
      
      output_table[i,"bf"] = bf
      output_table[i,"inference"] = inference
    }
    
    output_table_list_counter = output_table_list_counter + 1 
    output_table_list[[output_table_list_counter]] = output_table
    
  }
  
  
  
  ################################Study 1 analysis
  ####################################################################################
  ####################################################################################
  ####################################################################################Cultural effects 
  
  if(which_study_to_test == "study_1" | which_study_to_test == "both"){
    if(true_effect == "a culture replicates only"){
      v1_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_a = mysamp(N_pre_condition_per_class, m=6-(2.19*SMD_Effect_size_study1), s=2.19, lwr=1, upr=9, rounding=0)
      v3_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      
    } else if(true_effect == "original effect in all cultures"){
      v1_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_a = mysamp(N_pre_condition_per_class, m=6-(2.19*SMD_Effect_size_study1), s=2.19, lwr=1, upr=9, rounding=0)
      v3_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_b = mysamp(N_pre_condition_per_class, m=6-(2.19*SMD_Effect_size_study1), s=2.19, lwr=1, upr=9, rounding=0)
      v3_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_c = mysamp(N_pre_condition_per_class, m=6-(2.19*SMD_Effect_size_study1), s=2.19, lwr=1, upr=9, rounding=0)
      v3_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
    } else if(true_effect == "null effects"){
      v1_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_a = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_b = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v2_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v3_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      v4_c = mysamp(N_pre_condition_per_class, m=6, s=2.19, lwr=1, upr=9, rounding=0)
      
    } else(print("ERROR: No valid effect specified"))
    
    
    full_data=data.frame(v1_a, v2_a, v3_a, v4_a, v1_b, v2_b, v3_b, v4_b, v1_c, v2_c, v3_c, v4_c)
    suppressMessages({
      full_data2=melt(full_data, variable.name="code", value.name="choice")
    })
    
    code<-"v([1-9]+)_([a-z]+)"
    full_data2$condition=as.integer(sub(code, "\\1", full_data2$code))
    full_data2$class=factor(sub(code, "\\2", full_data2$code))
    full_data2$condition = factor(full_data2$condition, levels=c(1:4), labels=c("standard", "pole", "switch", "remote"))
    full_data2$study = "study_1"
    full_data2$class_type = "culture"
    
    
    output_table = as.data.frame(matrix(NA, nrow = length(class_names), ncol = 4))
    names(output_table) = c("study", "class", "bf", "inference")
    
    for(i in 1:length(class_names)){
      data_to_analyze = full_data2[full_data2[,"class"] == class_names[i],]
      output_table[i,"study"] = data_to_analyze$study[1]
      output_table[i,"class"] = paste(data_to_analyze$class_type[1], "_",class_names[i], sep = "")
      
      
      
      ###personal force: switch vs pole
      switch_pole_data = subset(data_to_analyze, condition=="pole"|condition=="switch")
      switch_pole_data$condition = factor(switch_pole_data$condition)
      model4=ttestBF(formula = choice ~ condition, data = switch_pole_data, rscale = prior)
      bf3 = round(as.numeric(matrix(model4)), 3)
      bf = bf3
      if(bf > bf_thresholds[1]){inference = "replicated"} else if(bf < bf_thresholds[2]){inference = "not"} else {inference = "inconclusive"}
      
      output_table[i,"bf"] = bf
      output_table[i,"inference"] = inference
    }
    
    output_table_list_counter = output_table_list_counter + 1 
    output_table_list[[output_table_list_counter]] = output_table
    
  }
  
  
  output_table_merged = do.call("rbind", output_table_list)
  
  all_inference_correct = all(output_table_merged[,"inference"] == correct_inferences)
  any_inference_incorrect = any(output_table_merged[,"inference"] == incorrect_inferences)
  
  
  return(c(true_effect, N_pre_condition_per_class, all_inference_correct, any_inference_incorrect))
}




final_output = as.data.frame(matrix(NA, nrow = 3, ncol = 4))
names(final_output) = c("true effect pattern", "N_pre_condition_per_class", "correct inference rate", "incorrect inference rate")



iterations = 10000
effect_size = 0.3 # 0.23 is half of the original effect size. The original effect size in both studies was roughly 0.46. 1.02/2.19 in study 2 and 1.01/2.19 in study 1
sample_size = 900
prior_rscales = 3
bf_threshold_to_use = 5


print("Simulationg scenario 1/3")
out = pbreplicate(iterations, simulation_func(bf_thresholds = c(bf_threshold_to_use, 1/bf_threshold_to_use),
                                            main_inference_method = "method_1", 
                                            true_effect = "original effect in all cultures", 
                                            N_pre_condition_per_class = sample_size,
                                            which_study_to_test = "both", 
                                            prior = prior_rscales,
                                            SMD_Effect_size_study2 = effect_size,
                                            SMD_Effect_size_study1 = effect_size 
))

final_output[1, "true effect pattern"] = out[1,1]
final_output[1, "N_pre_condition_per_class"] = out[2,1]
final_output[1, "correct inference rate"] = mean(as.logical(out[3,])) # power to make correct inference on ALL claims
final_output[1, "incorrect inference rate"] = mean(as.logical(out[4,])) # incorrect inference on any one claim

print("Simulationg scenario 2/3")
out = pbreplicate(iterations, simulation_func(bf_thresholds = c(bf_threshold_to_use, 1/bf_threshold_to_use),
                                            main_inference_method = "method_1", 
                                            true_effect = "a culture replicates only", 
                                            N_pre_condition_per_class = sample_size,
                                            which_study_to_test = "both", 
                                            prior = prior_rscales,
                                            SMD_Effect_size_study2 = effect_size,
                                            SMD_Effect_size_study1 = effect_size 
))


final_output[2, "true effect pattern"] = out[1,1]
final_output[2, "N_pre_condition_per_class"] = out[2,1]
final_output[2, "correct inference rate"] = mean(as.logical(out[3,])) # power to make correct inference on ALL claims
final_output[2, "incorrect inference rate"] = mean(as.logical(out[4,])) # incorrect inference on any one claim


print("Simulationg scenario 3/3")
out = pbreplicate(iterations, simulation_func(bf_thresholds = c(bf_threshold_to_use, 1/bf_threshold_to_use),
                                            main_inference_method = "method_1", 
                                            true_effect = "null effects", 
                                            N_pre_condition_per_class = sample_size,
                                            which_study_to_test = "both", 
                                            prior = prior_rscales,
                                            SMD_Effect_size_study2 = effect_size,
                                            SMD_Effect_size_study1 = effect_size
))


final_output[3, "true effect pattern"] = out[1,1]
final_output[3, "N_pre_condition_per_class"] = out[2,1]
final_output[3, "correct inference rate"] = mean(as.logical(out[3,])) # power to make correct inference on ALL claims
final_output[3, "incorrect inference rate"] = mean(as.logical(out[4,])) # incorrect inference on any one claim

#########N_pre_condition_per_class has to be multiplied by 24.

final_output
