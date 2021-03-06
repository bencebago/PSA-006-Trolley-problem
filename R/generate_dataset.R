# Generate a dataset of integer values with specified distribution paramaters
# OUTPUT: A tibble in long format
# EXAMPLE: generate_dataset(n = 1000, mean = 5, sd = 2, tasks = 4, groups = c("a", "b"))

# TODO: add optional parameter to manually specify variable names
# TODO: add parameters (vars, smd) for variables that should be different

if (!require(purrr)) install.packages("purrr")
if (!require(dplyr)) install.packages("dplyr")
source(here::here("R/draw_values.R"))

generate_dataset <- function(n, 
                             mean, 
                             sd, 
                             floor = 1, 
                             ceiling = 9,
                             tasks = 4, 
                             groups = c("a", "b", "c")){
  # Gerate  the variable names
  variables <-
    paste(
      paste("v", 1:tasks, sep = ""),
      rep(groups, each = tasks),
      sep = "_"
    )
  
  # Create a list of values and cast it into a tibble
  purrr::map(variables, 
             ~draw_values(n, mean, sd, floor, ceiling)) %>% 
        purrr::set_names(variables) %>% 
        dplyr::as_tibble()

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
#   tidyr::gather(code, value) %>% 
#   dplyr::group_by(code) %>%
#   tidyr::nest() %>%
#   dplyr::mutate(result = purrr::map(data, ~t.test(pull(.x), mu = 5.2) %>%
#                                            broom::tidy())) %>%
#   tidyr::unnest(result)
