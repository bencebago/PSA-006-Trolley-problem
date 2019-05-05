# For testing
library(tidyverse)
library(furrr)
library(BayesFactor)

plan(multisession(workers = availableCores()-1))

samples <- 100
sample_sizes_to_test <- seq(200, 4000, 200)
true_effect <- c("a culture replicates only", "original effect in all cultures", "null effects")
which_study_to_test <- c("study_1", "study_2", "both")
bf_threshold_to_use <- 5
prior <- 3

out <-
  crossing(sample = 1:samples, sample_sizes_to_test, true_effect, which_study_to_test) %>% 
  as.list() %>% 
  future_pmap(~simulate( 
                        n_per_cell = ..2,
                        true_effect = ..3,
                        which_study_to_test = ..4, 
                        bf_thresholds = c(bf_threshold_to_use, 
                                           1/bf_threshold_to_use),
                         prior = 3,  
                         class_mean = 6,
                         class_sd = 2.19,
                         mean_difference = 1.01,
                         class_names = c("a", "b", "c")),
              .progress = TRUE)




# Get result table
results <-
  out %>% 
  aggregate_inferences() %>% 
  gather(inference, p, -true_effect, -sample_size_per_class, -study)

# Visualize

theme_set(theme_light())

results %>% 
  ggplot() +
  aes(x = sample_size_per_class, y = p, color = inference) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_hline(yintercept = c(.05, .95), linetype = "dashed") +
  facet_grid(study~true_effect) +
  labs(title = str_glue("Correct and incorrect inference rate for different sample sizes for {samples} samples"),
       subtitle = "Dashed lines represent 5% and 95% probability",
       x = "Sample size per class",
       y = NULL) +
  theme(legend.position = "bottom")
