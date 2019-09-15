# For testing
library(tidyverse)
library(furrr)
 
source(here::here("R/simulate.R"))
source(here::here("R/aggregate_inferences.R"))

# Set up multicore processing
plan(multisession(workers = availableCores() - 1))

samples <- 100
sample_sizes_to_test <- seq(200, 2000, 200)
which_study_to_test <- c("study_1", "study_2", "both")
bf_threshold_to_use <- 10
effect_size <- c(0.23, 0.3, 0.46)
# The prior should be equal to the es
class_names = c("a", "b", "c")
class_mean = 6
class_sd = 2.19

combinations <-
                crossing(sample = 1:samples, 
                         sample_sizes_to_test, 
                         which_study_to_test,
                         effect_size
                )

out <-
  combinations %>% 
  as.list() %>% 
  future_pmap(~simulate( 
                        n_per_cell = ..2,
                        which_study_to_test = ..3, 
                        bf_thresholds = c(bf_threshold_to_use, 
                                          1/bf_threshold_to_use),
                        class_mean = class_mean,
                        class_sd = class_sd,
                        mean_difference = ..4 * class_sd, 
                        class_names = c("a", "b", "c"),
                        # Prior should be the same as the ES
                        prior = ..4),
              .progress = TRUE)

results <-
  bind_cols(combinations, 
            map_dfr(out, 
                    ~enframe(.x, name = NULL) %>% 
                     t() %>% 
                     as_tibble())) %>% 
  dplyr::select(sample:effect_size,
                correct_inference_rate = V3,
                incorrect_inference_rate = V4) %>% 
  # dplyr::mutate(sample_size_per_class = as.numeric(sample_size_per_class)) %>%
  dplyr::group_by(sample_sizes_to_test, which_study_to_test, effect_size) %>% 
  dplyr::summarise(correct_inference_rate = mean(as.logical(correct_inference_rate)),
                   incorrect_inference_rate = mean(as.logical(incorrect_inference_rate))
  ) %>% 
  dplyr::ungroup() %>% 
  gather(inference, p, -sample_sizes_to_test, -which_study_to_test, -effect_size)


# Get result table
results <-
  out %>% 
  aggregate_inferences() %>% 
  gather(inference, p, -sample_size_per_class, -study)

# Visualize

theme_set(theme_light())

results %>% 
  ggplot() +
  aes(x = sample_sizes_to_test, y = p, color = inference) +
  geom_line(size = 1.1) +
  # geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_hline(yintercept = c(.05, .95), linetype = "dashed") +
  facet_grid(which_study_to_test~effect_size, labeller = "label_both") +
  labs(title = str_glue("Correct and incorrect inference rate for different sample sizes for {samples} samples"),
       subtitle = "Dashed lines represent 5% and 95% probability",
       x = "Sample size per class",
       y = NULL) +
  theme(legend.position = "bottom") +
  # coord_cartesian(ylim = c(.90, 1)) +
  NULL

# write_csv(results, "simulation_10k.csv")
