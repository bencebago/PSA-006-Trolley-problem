###############PSA - creating variables

####function to create ratings between 1-9
mysamp <- function(n, m, s, lwr, upr, rounding) {
  samp <- round(rnorm(n, m, s), rounding)
  samp[samp < lwr] <- lwr
  samp[samp > upr] <- upr
  samp
}



library(reshape2)
library(BayesFactor)

####### Simulation parameters

simulation_func = function(
bf_thresholds = c(3, 1/3),
main_inference_method = "method_1", #either "method_1", where we look at whether the original effect replicates in all culture classes separately
true_effect = "original effect in all cultures", #either  "a culture replicates only", "original effect in all cultures"
N_pre_condition_per_class = 3000,
which_study_to_test = "study_1", #either "study_1", "study_2", or "both"
prior = "medium" #either "medium", "wide", "ultrawide", BayesFactor package default is "medium"
){
  class_names = c("a", "b")
   if(true_effect == "a culture replicates only"){
    correct_inferences_study_2_culture1 = c("replicated", "not")
    correct_inferences_study_2_culture2 = c("replicated", "not")
    correct_inferences_study_1_culture1 = c("replicated", "not")
    correct_inferences_study_1_culture2 = c("replicated", "not")
    
  } else if(true_effect == "original effect in all cultures"){
    correct_inferences_study_2_culture1 = c("replicated", "replicated")
    correct_inferences_study_2_culture2 = c("replicated", "replicated")
    correct_inferences_study_1_culture1 = c("replicated", "replicated")
    correct_inferences_study_1_culture2 = c("replicated", "replicated")

 
  } else {print("ERROR: cant determine true effect without valid true_effect")}
  
  if(which_study_to_test == "study_2"){
    correct_inferences = c(correct_inferences_study_2_culture1, correct_inferences_study_2_culture2)
  }
  if(which_study_to_test == "study_1"){
    correct_inferences = c(correct_inferences_study_1_culture1, correct_inferences_study_1_culture2)
  }
  if(which_study_to_test == "both"){
    correct_inferences = c(correct_inferences_study_1_culture1, correct_inferences_study_1_culture2, correct_inferences_study_2_culture1, correct_inferences_study_2_culture2)
  }
  
  output_table_list_counter = 0
  output_table_list = list(NA)
  
  
  if(which_study_to_test == "study_2" | which_study_to_test == "both"){
 if(true_effect == "a culture replicates only"){
      v1_a = mysamp(N_pre_condition_per_class, m=4.98, s=2.19, lwr=1, upr=9, rounding=0)
      v2_a = mysamp(N_pre_condition_per_class, m=5.89, s=2.19, lwr=1, upr=9, rounding=0)
      v3_a = mysamp(N_pre_condition_per_class, m=6.25, s=2.19, lwr=1, upr=9, rounding=0)
      v4_a = mysamp(N_pre_condition_per_class, m=5.85, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
      v2_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
      v3_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
      v4_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
      
      
    } else if(true_effect == "original effect in all cultures"){
      v1_a = mysamp(N_pre_condition_per_class, m=4.98, s=2.19, lwr=1, upr=9, rounding=0)
      v2_a = mysamp(N_pre_condition_per_class, m=5.89, s=2.19, lwr=1, upr=9, rounding=0)
      v3_a = mysamp(N_pre_condition_per_class, m=6.25, s=2.19, lwr=1, upr=9, rounding=0)
      v4_a = mysamp(N_pre_condition_per_class, m=5.85, s=2.19, lwr=1, upr=9, rounding=0)
      
      v1_b = mysamp(N_pre_condition_per_class, m=4.98, s=2.19, lwr=1, upr=9, rounding=0)
      v2_b = mysamp(N_pre_condition_per_class, m=5.89, s=2.19, lwr=1, upr=9, rounding=0)
      v3_b = mysamp(N_pre_condition_per_class, m=6.25, s=2.19, lwr=1, upr=9, rounding=0)
      v4_b = mysamp(N_pre_condition_per_class, m=5.85, s=2.19, lwr=1, upr=9, rounding=0)
      
    } else(print("ERROR: No valid effect specified"))
    
    
    #############################################STUDY2 - CULTURE1 ANALYSIS
    full_data=data.frame(v1_a, v2_a, v3_a, v4_a, v1_b, v2_b, v3_b, v4_b)
    full_data2=melt(full_data, variable.name="code", value.name="choice")
    
    code<-"v([1-9]+)_([a-z]+)"
    full_data2$condition=as.integer(sub(code, "\\1", full_data2$code))
    full_data2$class=factor(sub(code, "\\2", full_data2$code))
    full_data2$side_effect = factor(ifelse(full_data2$condition>2,1,0))
    full_data2$personal_force = factor(ifelse(full_data2$condition==1|full_data2$condition==3,1,0))
    full_data2$condition1 = factor(ifelse(full_data2$condition==1,1,0))
    full_data2$study = "study_2"
    full_data2$class_type = "culture1"
    
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
    

    #############################################STUDY2 - culture2 analysis
      if(true_effect == "a culture replicates only"){
        v1_a = mysamp(N_pre_condition_per_class, m=4.98, s=2.19, lwr=1, upr=9, rounding=0)
        v2_a = mysamp(N_pre_condition_per_class, m=5.89, s=2.19, lwr=1, upr=9, rounding=0)
        v3_a = mysamp(N_pre_condition_per_class, m=6.25, s=2.19, lwr=1, upr=9, rounding=0)
        v4_a = mysamp(N_pre_condition_per_class, m=5.85, s=2.19, lwr=1, upr=9, rounding=0)
        
        v1_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
        v2_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
        v3_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
        v4_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
        
        
      } else if(true_effect == "original effect in all cultures"){
        v1_a = mysamp(N_pre_condition_per_class, m=4.98, s=2.19, lwr=1, upr=9, rounding=0)
        v2_a = mysamp(N_pre_condition_per_class, m=5.89, s=2.19, lwr=1, upr=9, rounding=0)
        v3_a = mysamp(N_pre_condition_per_class, m=6.25, s=2.19, lwr=1, upr=9, rounding=0)
        v4_a = mysamp(N_pre_condition_per_class, m=5.85, s=2.19, lwr=1, upr=9, rounding=0)
        
        v1_b = mysamp(N_pre_condition_per_class, m=4.98, s=2.19, lwr=1, upr=9, rounding=0)
        v2_b = mysamp(N_pre_condition_per_class, m=5.89, s=2.19, lwr=1, upr=9, rounding=0)
        v3_b = mysamp(N_pre_condition_per_class, m=6.25, s=2.19, lwr=1, upr=9, rounding=0)
        v4_b = mysamp(N_pre_condition_per_class, m=5.85, s=2.19, lwr=1, upr=9, rounding=0)
        
        
      } else(print("ERROR: No valid effect specified"))
    
    full_data=data.frame(v1_a, v2_a, v3_a, v4_a, v1_b, v2_b, v3_b, v4_b)
    full_data2=melt(full_data, variable.name="code", value.name="choice")
    
    code<-"v([1-9]+)_([a-z]+)"
    full_data2$condition=as.integer(sub(code, "\\1", full_data2$code))
    full_data2$class=factor(sub(code, "\\2", full_data2$code))
    full_data2$side_effect = factor(ifelse(full_data2$condition>2,1,0))
    full_data2$personal_force = factor(ifelse(full_data2$condition==1|full_data2$condition==3,1,0))
    full_data2$condition1 = factor(ifelse(full_data2$condition==1,1,0))
    full_data2$study = "study_2"
    full_data2$class_type = "culture2"
    
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
        v1_a = mysamp(N_pre_condition_per_class, m=4.98, s=2.19, lwr=1, upr=9, rounding=0)
        v2_a = mysamp(N_pre_condition_per_class, m=4.15, s=2.19, lwr=1, upr=9, rounding=0)
        v3_a = mysamp(N_pre_condition_per_class, m=5.14, s=2.19, lwr=1, upr=9, rounding=0)
        v4_a = mysamp(N_pre_condition_per_class, m=5.85, s=2.19, lwr=1, upr=9, rounding=0)
        
        v1_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
        v2_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
        v3_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
        v4_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
        
        
      } else if(true_effect == "original effect in all cultures"){
        v1_a = mysamp(N_pre_condition_per_class, m=4.98, s=2.19, lwr=1, upr=9, rounding=0)
        v2_a = mysamp(N_pre_condition_per_class, m=4.15, s=2.19, lwr=1, upr=9, rounding=0)
        v3_a = mysamp(N_pre_condition_per_class, m=5.14, s=2.19, lwr=1, upr=9, rounding=0)
        v4_a = mysamp(N_pre_condition_per_class, m=5.85, s=2.19, lwr=1, upr=9, rounding=0)
        
        v1_b = mysamp(N_pre_condition_per_class, m=4.98, s=2.19, lwr=1, upr=9, rounding=0)
        v2_b = mysamp(N_pre_condition_per_class, m=4.15, s=2.19, lwr=1, upr=9, rounding=0)
        v3_b = mysamp(N_pre_condition_per_class, m=5.14, s=2.19, lwr=1, upr=9, rounding=0)
        v4_b = mysamp(N_pre_condition_per_class, m=5.85, s=2.19, lwr=1, upr=9, rounding=0)
        
  } else(print("ERROR: No valid effect specified"))
    
  
    full_data=data.frame(v1_a, v2_a, v3_a, v4_a, v1_b, v2_b, v3_b, v4_b)
    full_data2=melt(full_data, variable.name="code", value.name="choice")
    
    code<-"v([1-9]+)_([a-z]+)"
    full_data2$condition=as.integer(sub(code, "\\1", full_data2$code))
    full_data2$class=factor(sub(code, "\\2", full_data2$code))
    full_data2$condition = factor(full_data2$condition, levels=c(1:4), labels=c("standard", "pole", "switch", "remote"))
    full_data2$study = "study_1"
    full_data2$class_type = "culture1"
    

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
        if(bf3 > bf_thresholds[1]){inference = "replicated"} else {inference = "not"}
      
            output_table[i,"bf"] = bf
      output_table[i,"inference"] = inference
    }
    
    output_table_list_counter = output_table_list_counter + 1 
    output_table_list[[output_table_list_counter]] = output_table
    
    
    ##############################################culture2

       if(true_effect == "a culture replicates only"){
        v1_a = mysamp(N_pre_condition_per_class, m=4.98, s=2.19, lwr=1, upr=9, rounding=0)
        v2_a = mysamp(N_pre_condition_per_class, m=4.15, s=2.19, lwr=1, upr=9, rounding=0)
        v3_a = mysamp(N_pre_condition_per_class, m=5.14, s=2.19, lwr=1, upr=9, rounding=0)
        v4_a = mysamp(N_pre_condition_per_class, m=5.85, s=2.19, lwr=1, upr=9, rounding=0)
        
        v1_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
        v2_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
        v3_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
        v4_b = mysamp(N_pre_condition_per_class, m=5, s=2.19, lwr=1, upr=9, rounding=0)
        
        
      } else if(true_effect == "original effect in all cultures"){
        v1_a = mysamp(N_pre_condition_per_class, m=4.98, s=2.19, lwr=1, upr=9, rounding=0)
        v2_a = mysamp(N_pre_condition_per_class, m=4.15, s=2.19, lwr=1, upr=9, rounding=0)
        v3_a = mysamp(N_pre_condition_per_class, m=5.14, s=2.19, lwr=1, upr=9, rounding=0)
        v4_a = mysamp(N_pre_condition_per_class, m=5.85, s=2.19, lwr=1, upr=9, rounding=0)
        
        v1_b = mysamp(N_pre_condition_per_class, m=4.98, s=2.19, lwr=1, upr=9, rounding=0)
        v2_b = mysamp(N_pre_condition_per_class, m=4.15, s=2.19, lwr=1, upr=9, rounding=0)
        v3_b = mysamp(N_pre_condition_per_class, m=5.14, s=2.19, lwr=1, upr=9, rounding=0)
        v4_b = mysamp(N_pre_condition_per_class, m=5.85, s=2.19, lwr=1, upr=9, rounding=0)
        
      } else(print("ERROR: No valid effect specified"))
    
    full_data=data.frame(v1_a, v2_a, v3_a, v4_a, v1_b, v2_b, v3_b, v4_b)
    full_data2=melt(full_data, variable.name="code", value.name="choice")
    
    code<-"v([1-9]+)_([a-z]+)"
    full_data2$condition=as.integer(sub(code, "\\1", full_data2$code))
    full_data2$class=factor(sub(code, "\\2", full_data2$code))
    full_data2$condition = factor(full_data2$condition, levels=c(1:4), labels=c("standard", "pole", "switch", "remote"))
    full_data2$study = "study_1"
    full_data2$class_type = "culture2"
    
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
        if(bf3 > bf_thresholds[1]){inference = "replicated"} else {inference = "not"}
      
      
      output_table[i,"bf"] = bf
      output_table[i,"inference"] = inference
    }
    
    
    output_table_list_counter = output_table_list_counter + 1 
    output_table_list[[output_table_list_counter]] = output_table
  }
  
  
  output_table_merged = do.call("rbind", output_table_list)
  
  all_inference_correct = all(output_table_merged[,"inference"] == correct_inferences)
  
  return(all_inference_correct)
}


iterations = 5000

out = replicate(iterations, simulation_func(bf_thresholds = c(3, 1/3),
                            main_inference_method = "method_1", 
                            true_effect = "a culture replicates only", 
                            N_pre_condition_per_class = 850,
                            which_study_to_test = "both", 
                            prior = "medium" 
                            ))

mean(out) # power to make correct inference on ALL claims

#########N_pre_condition_per_class has to be multiplied by 16.


iterations = 5000

out = replicate(iterations, simulation_func(bf_thresholds = c(3, 1/3),
                                            main_inference_method = "method_1", 
                                            true_effect = "original effect in all cultures", 
                                            N_pre_condition_per_class = 250,
                                            which_study_to_test = "both", 
                                            prior = "medium" 
))

mean(out) # power to make correct inference on ALL claims
