# Create summary table
# Summarise the result of several simulations
# INPUT: a list of replications, created by the simulation function
# OUTPUT: a tibble that shows correct and incorrect inference rates for different scenarios
# EXAMPLE: aggregate_inferences(out)

if (!require(purrr)) install.packages("purrr")
if (!require(furrr)) install.packages("furrr") # Faster for large (10k+) samples
if (!require(dplyr)) install.packages("dplyr")
if (!require(tibble)) install.packages("tibble")

aggregate_inferences <- function(combinations, out_list){

  out_list %>% 
    furrr::future_map_dfr(., 
                          ~tibble::enframe(.x, name = NULL) %>% 
                           t() %>% 
                           tibble::as_tibble()) %>%
    dplyr::rename(correct_inference_rate = 1,
                  incorrect_inference_rate = 2) %>% 
    dplyr::bind_cols(combinations, .) %>% 
    dplyr::group_by(!!!syms(names(select(combinations, -sample)))) %>% 
    dplyr::summarise(correct_inference_rate = mean(as.logical(correct_inference_rate)),
                     incorrect_inference_rate = mean(as.logical(incorrect_inference_rate))
                     ) %>% 
    dplyr::ungroup()
}