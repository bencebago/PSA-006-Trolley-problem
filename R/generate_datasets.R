# Generate dataset
# 


# TODO: add parameters (vars, smd) for variables that should be different
# TODO: Generate an already tidy dataset

generate_dataset <- function(n, 
                             mean, 
                             sd, 
                             floor = 1, 
                             ceiling = 9,
                             tasks = 4, 
                             groups = c("a", "b", "c")){
  # Gerate  the variable names. Values are added as a list of NA values to be later unnested
  tidyr::crossing(task = stringr::str_c("v",1:tasks), 
                  group = groups, 
                  value = list(rep(NA_real_, n))) %>% 
    tidyr::unite(key, task, group) %>% 
    tidyr::spread(key, value) %>% 
    tidyr::unnest() %>% 
  # Add the random values
    dplyr::mutate_all(~draw_values(n, mean, sd, floor, ceiling)) %>% 
    # Gather data into long format
    gather(code, choice)

}

# Test if the function generates the dataset
# 
# generate_dataset(1000, 5, 2)
# 
# Using several groups and tasks. Check how many variables are  different from mean
# 
# df_test <- generate_dataset(n = 3000, mean = 5.2, sd = 2.19, tasks = 5, groups = c("x", "y", "w", "z"))
# 
# df_test %>% 
#   purrr::map(~t.test(.x, mu = 5.2) %>% 
#               broom::tidy()) %>% 
#   dplyr::bind_rows()

 
  
 