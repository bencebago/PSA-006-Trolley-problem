# Calculate interaction stats for study2 interaction terms
# INPUT: data frame, one of "study2" datasets
# OUTPUT: Bayesian and frequentist stats for study2 3rd order interactions

calculate_interaction_stats <- function(df = NULL){
  
  # Create a tibble that contains the labels for the cultural variables
  cultural_vars <-
    tibble(var = c("Collectivism", "ver_ind", "hor_ind", "ver_col", "hor_col"),
           variable = c("Country-level collectivism", "Vertical Individualism", "Horizontal Individualism", "Vertical Collectivism", "Horizontal Collectivism"))
  
  # Create a nested tibble that can be mapped through
  df %>%
    select(country3, trolley_3_rate, trolley_4_rate, 
           trolley_5_rate, trolley_6_rate,
           all_of(cultural_vars$var)) %>% 
    # Collapse conditions
    pivot_longer(cols = c(trolley_3_rate, trolley_4_rate, 
                          trolley_5_rate, trolley_6_rate),
                 names_to = "condition", 
                 values_to = "rate",
                 values_drop_na = TRUE) %>% 
    # Creating predictors based on the condition
    mutate(personal_force = if_else(str_detect(condition, "3|5"), 0, 1),
           intention = if_else(str_detect(condition, "4|5"), 1, 0),
    # What is this??? lmBF doesn't run without it?!?!?
           country0 = paste0("0", country3)) %>% 
    drop_na() %>% 
    # Create separate nested datasets to all cultural variables
    pivot_longer(cols = cultural_vars$var, names_to = "var") %>% 
    group_by(var) %>%
    nest() %>% 
    ungroup() %>% 
    # Map through the datasets and create stat models using a specific cultural variable
    mutate(datt = map2(data, var,
                       ~lmBF(rate ~ personal_force*intention*value + country0,
                             whichRandom = "country0",
                             data = as.data.frame(.x),
    # Set prior dynamically, based on the interaction with a specific variable
                             rscaleCont = set_names(0.19, str_glue("personal_force:intention:{.y}")))),
           datt2 = map(data,
                       ~lmBF(rate ~ personal_force + intention + value +
                               personal_force:value +
                               personal_force:intention + intention:value +
                               country0,
                             whichRandom = "country0",
                             data = as.data.frame(.x))),
           datt3 = map2(datt, datt2, ~recompute((.x/.y), iterations = 50000) %>% 
                          as_tibble()),
           frequentist = map(data, 
                             ~lmer(rate ~ personal_force*intention*value + (1|country3),
                                   data = .x) %>% 
                               tidy(conf.int = TRUE))) %>% 
    # 
    left_join(cultural_vars, by = "var") %>% 
    transmute(variable, 
              BF = map_dbl(datt3, ~slice(.x) %>% 
                             pull(bf) %>% 
                             round(4)),
              b = map_dbl(frequentist, 
                          ~filter(.x, term == "personal_force:intention:value") %>% 
                            pull(estimate) %>% 
                            round(4)),
              lower = map_dbl(frequentist, 
                              ~filter(.x, term == "personal_force:intention:value") %>% 
                                pull(conf.low) %>% 
                                round(4)),
              higher = map_dbl(frequentist, 
                               ~filter(.x, term == "personal_force:intention:value") %>% 
                                 pull(conf.high) %>% 
                                 round(4)),
              p = map_dbl(frequentist, 
                          ~filter(.x, term == "personal_force:intention:value") %>% 
                            pull(p.value) %>% 
                            round(4))
              )

}
