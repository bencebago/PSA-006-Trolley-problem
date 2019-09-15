# Create summary table
# Summarise the result of several simulations
# INPUT: a list of replications, created by the simulation function
# OUTPUT: a tibble that shows correct and incorrect inference rates for different scenarios
# EXAMPLE: aggregate_inferences(out)

if (!require(purrr)) install.packages("purrr")
if (!require(furrr)) install.packages("furrr") # Faster for large (10k+) samples
if (!require(dplyr)) install.packages("dplyr")
if (!require(tibble)) install.packages("tibble")

aggregate_inferences <- function(ls){

  ls %>% 
    furrr::future_map_dfr(., 
                          ~tibble::enframe(.x, name = NULL) %>% 
                           t() %>% 
                           tibble::as_tibble()) %>%
    dplyr::rename(
                   sample_size_per_class = 1, 
                   study = 2,
                   correct_inference_rate = 3,
                   incorrect_inference_rate = 4) %>% 
    dplyr::mutate(sample_size_per_class = as.numeric(sample_size_per_class)) %>%
    dplyr::group_by(sample_size_per_class, study) %>% 
    dplyr::summarise(correct_inference_rate = mean(as.logical(correct_inference_rate)),
                     incorrect_inference_rate = mean(as.logical(incorrect_inference_rate))
                     ) %>% 
    dplyr::ungroup()
}