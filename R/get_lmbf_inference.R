# Evaluate results
# Run a Bayesian ANOVA and return the BF for the 
# INPUT: df: A data frame with the raw data
#        groups: the grouping variables
#        formula: t.test formula (e.g. value ~ group)
#        prior: to be forwarded to the test
#        bf_tresholds: the inference will be based on these
# OUTPUT: A tibble with the grouping variables, a calculated BF, and the inference
# EXAMPLE:


if (!require(dplyr)) install.packages("dplyr")
if (!require(purrr)) install.packages("purrr")
if (!require(rlang)) install.packages("rlang")
if (!require(BayesFactor)) install.packages("BayesFactor")


get_lmbf_inference <- function(df, 
                               groups = c("study", "class"),
                               prior = prior,
                               # Evidence for the alternative and null, respectively
                               bf_thresholds = c(10, 1/10) 
){
  suppressMessages(
    df %>% 
      dplyr::group_nest(!!!syms(groups)) %>%
      dplyr::transmute(!!!syms(groups), 
                       bf = purrr::map_dbl(data, 
                                           ~(BayesFactor::lmBF(formula = choice ~ personal_force * intention, 
                                                               rscaleFixed = prior, 
                                                               data = as.data.frame(.x),
                                                               progress = FALSE) /
                                               BayesFactor::lmBF(formula = choice ~ personal_force + intention, 
                                                                 rscaleFixed = prior, 
                                                                 data = as.data.frame(.x),
                                                                 progress = FALSE)) %>% 
                                             BayesFactor::extractBF(onlybf = TRUE)),
                       inference = dplyr::case_when(bf > bf_thresholds[1] ~ "replicated",
                                                    bf < bf_thresholds[2] ~ "not replicated",
                                                    TRUE ~ "inconclusive")
      )
  )
}

# PROTOTYPE
# temp <-
#   df %>% 
#   dplyr::group_nest(!!!syms(groups)) %>%
#   dplyr::transmute(!!!syms(groups), 
#                    bf = purrr::map_dbl(data, 
#                                        ~(BayesFactor::lmBF(formula = choice ~ personal_force * intention, 
#                                                             rscaleFixed = prior, 
#                                                             data = as.data.frame(.x),
#                                                             progress = FALSE) /
#                                         BayesFactor::lmBF(formula = choice ~ personal_force + intention, 
#                                                             rscaleFixed = prior, 
#                                                             data = as.data.frame(.x),
#                                                             progress = FALSE)) %>% 
#                                         BayesFactor::extractBF(onlybf = TRUE)),
#                 inference = dplyr::case_when(bf > bf_thresholds[1] ~ "replicated",
#                                              bf < bf_thresholds[2] ~ "not replicated",
#                                              TRUE ~ "inconclusive")
#                 )
# 