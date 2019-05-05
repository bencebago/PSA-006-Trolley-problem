# Evaluate results
# Run a Bayesian t-test using 
# INPUT: df: A data frame with the raw data
#        groups: the grouping variables
#        formula: t.test formula (e.g. value ~ group)
#        prior: to be forwarded to the test
#        bf_tresholds: the inference will be based on these
# OUTPUT: A tibble with the grouping variables, a calculated BF, and the inference
# EXAMPLE:
# get_bf_inference(full_data2, groups = c("study", "class"), formula = choice ~ condition, prior = "medium")

# TODO: Getting the BF from the object is clumsy, is there a more straightforward way?

if (!require(dplyr)) install.packages("dplyr")
if (!require(purrr)) install.packages("purrr")
if (!require(rlang)) install.packages("rlang")

get_bf_inference <- function(df, 
                             groups = c("study", "class"),
                             formula = NULL,
                             prior = prior,
                             # Evidence for the alternative and null, respectively
                             bf_thresholds = c(5, 1/5) 
                             ){

  df %>% 
    dplyr::group_nest(!!!syms(groups)) %>%
    dplyr::transmute(!!!syms(groups), 
                     bf = purrr::map_dbl(data, 
                                         ~BayesFactor::ttestBF(formula = formula, 
                                                               rscale = prior, 
                                                               data = as.data.frame(.x)) %>% 
                                         matrix() %>% 
                                         as.numeric()),
                     inference = dplyr::case_when(bf > bf_thresholds[1] ~ "replicated",
                                                  bf < bf_thresholds[2] ~ "not replicated",
                                                  TRUE ~ "inconclusive"))
}

