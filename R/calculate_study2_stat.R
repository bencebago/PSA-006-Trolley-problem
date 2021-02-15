# Calculates the statistics for study 2 
# INPUT: data: data frame or tibble that contains Region and specified variables
#        vars: the variables to be collapsed
#        label: what data label should be atteched to the output
# OUTPUT: a tibble that contains the selected statistics in a format that can be printed

calculate_study2_stat <- function(data = NULL, 
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
                             mutate(personal_force = if_else(str_detect(condition, "3|5"), 1, 0),
                                    intention = if_else(str_detect(condition, "4|5"), 1, 0)) %>% 
                             as.data.frame()),
           bmod_1 = map(data_long, 
                        ~lmBF(rate ~ personal_force * intention, 
                              data = .x, 
                              rscaleFixed = rscaleFixed)),
           
           bmod_2 = map(data_long, 
                        ~lmBF(rate ~ personal_force + intention, 
                              data = .x, 
                              rscaleFixed = rscaleFixed)),
           bmod = map2(bmod_1, bmod_2,
                       ~recompute(.x / .y, iterations = 50000) %>% 
                        as_tibble()),
           fmod = map(data_long, 
                      ~aov(rate ~ personal_force * intention, data=.x) %>% 
                       broom::tidy())) %>%
    ungroup() %>% 
    transmute(
      Exclusion = label,
      Cluster = Region,
      BF = map_chr(bmod,
                   ~slice(.x, 1) %>%
                     pull(bf) %>%
                     scales::scientific()),
      RR = NA_character_,
      `F` = map_dbl(fmod, 
                ~filter(.x, term == "personal_force:intention") %>% 
                 pull(statistic) %>% 
                 round(3)),
      df = map_chr(fmod,
                   ~filter(.x, term == "Residuals") %>% 
                    pull(df) %>% 
                    paste0("1, ", .)),
      p = map_chr(fmod,
                  ~filter(.x, term == "personal_force:intention") %>%                   
                   pull(p.value) %>%
                   scales::scientific()),
      `Eta squared` = map_dbl(data_long, 
                          ~aov(rate ~ personal_force*intention,
                               data=.x) %>%  
                           effectsize::eta_squared() %>% 
                           as_tibble() %>% 
                           filter(Parameter == "personal_force:intention") %>% 
                           pull(Eta2_partial) %>% 
                           round(3)),
      `Raw effect` = map_dbl(data_long,
                             ~group_by(.x, personal_force, intention) %>%
                              summarise(avg_rate = mean(rate, na.rm = TRUE), 
                                        .groups = "drop") %>% 
                              pivot_wider(names_from = c(personal_force, intention),
                                          values_from = avg_rate) %>% 
                              mutate(raw_diff = (`0_0`-`1_0`) - (`0_1` - `1_1`)) %>%
                              pull(raw_diff) %>% 
                              round(2))

      ) 
  
}