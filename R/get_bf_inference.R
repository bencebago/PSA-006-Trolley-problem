# Evaluate results
# Run a Bayesian t-test using 
# INPUT: df: A data frame with the raw data
#        groups: the grouping variables
#        formula: t.test formula (e.g. value ~ group)
#        prior: to be forwarded to the test
#        bf_tresholds: the inference will be based on these
# OUTPUT: A tibble with the grouping variables, a calculated BF, and the inference
# EXAMPLE:
# evaluate_results(full_data2, groups = c("study", "class"), formula = choice ~ condition, prior = "medium")

# TODO: Getting the BF from the object is clumsy

get_bf_inference <- function(df, 
                             groups = c("study", "class"),
                             formula = NULL,
                             prior = prior,
                             bf_thresholds = c(3, 1/3)
                             ){

  df %>% 
    group_nest(!!!syms(groups)) %>%
    transmute(group_cols(), 
              bf = map_dbl(data, ~ttestBF(formula = formula, 
                                          rscale = prior, 
                                          data = as.data.frame(.x)) %>% 
                             matrix() %>% 
                             as.numeric()),
              inference = case_when(bf > bf_thresholds[1] ~ "replicated",
                                    bf < bf_thresholds[2] ~ "not replicated",
                                    TRUE ~ "inconclusive"))
}

