# Calculates the statistics for study 1 
# INPUT: data: data frame or tibble that contains Region and specified variables
#        vars: the variables to be collapsed
#        label: what data label should be atteched to the output
# OUTPUT: a tibble that contains the selected statistics in a format that can be printed

calculate_study1_stat <- function(data = NULL, 
                                  vars = NULL,
                                  label = NULL){

  data %>% 
    as_tibble() %>%
    select(Region, {{vars}}) %>% 
    group_by(Region) %>% 
    nest() %>% 
    arrange(Region) %>% 
    mutate(data_long = map(data, 
                           ~pivot_longer(.x, 
                                         cols = everything(),
                                         names_to = "condition", 
                                         values_to = "rate",
                                         values_drop_na = TRUE) %>% 
                             as.data.frame()),
           bttest = map(data_long, 
                        ~ttestBF(formula = rate ~ condition, data = .x,
                                 rscale = rscale, nullInterval = c(0, Inf)) %>% 
                          as_tibble()),
           fttest = map(data_long,
                        ~t.test(rate ~ condition, data = .x) %>% 
                          broom::tidy())) %>%
    ungroup() %>% 
    transmute(
              Exclusion = label,
              Cluster = Region,
              BF = map_chr(bttest, 
                           ~slice(.x, 2) %>% 
                             pull(bf) %>% 
                             scales::scientific()),
              RR = NA_character_,
              t = map_dbl(fttest, 
                          ~pull(.x, statistic) %>% 
                            round(2)),
              df = map_dbl(fttest, 
                           ~pull(.x, parameter) %>% 
                             round(2)),
              p = map_chr(fttest, 
                          ~pull(.x, p.value) %>% 
                            scales::scientific()),
              `Cohen's d` = map_dbl(data_long,
                                    ~effsize::cohen.d(rate ~ condition, 
                                                      data = .x)$estimate %>% 
                                      round(2))) 

  
}
