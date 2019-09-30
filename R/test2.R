# For testing
library(tidyverse)
library(furrr)
library(tictoc)
 
source(here::here("R/simulate.R"))
source(here::here("R/aggregate_inferences.R"))

# Set up multicore processing
plan(multisession(workers = availableCores()))

samples <- 100
true_effect <- c("original effect in all cultures", "null effects")
bf_threshold_to_use <- 10
effect_size1 <- c(0.23, 0.30) 
effect_size2 <- c(0.28, 0.37) 
sample_sizes_to_test1 <- seq(1500, 2500, 250)
sample_sizes_to_test2 <- seq(1500, 2000, 100)
# The prior should be equal to the es
class_names = c("a", "b", "c")
class_mean = 6
class_sd = 2.19

combinations1 <-
  crossing(
    study = "study_1",
    true_effect,
    effect_size = effect_size1,
    sample_size = sample_sizes_to_test1,
    sample = 1:samples
  )

combinations2 <-
  crossing(study = "study_2",
           true_effect,
           effect_size = effect_size2,
           sample_size = sample_sizes_to_test2,
           sample = 1:samples
  )

combinations <- bind_rows(combinations1, combinations2)

tic()
out <-
  combinations %>% 
  as.list() %>% 
  future_pmap(~simulate(
                        study = ..1,
                        true_effect = ..2,
                        mean_difference = ..3 * class_sd,
                        n_per_cell = ..4,
                        bf_thresholds = c(bf_threshold_to_use, 
                                          1/bf_threshold_to_use),
                        class_mean = class_mean,
                        class_sd = class_sd,
                        class_names = c("a", "b", "c"),
                        # Prior should be the same as the ES
                        prior = ..3),
              .progress = TRUE)
toc()

results <-
  aggregate_inferences(combinations = combinations, out_list = out) %>% 
  gather(inference, p, -study, -true_effect, -sample_size, -effect_size)


# Visualize

theme_set(theme_light())

results %>% 
  filter(study == "study_2") %>% 
  ggplot() +
  aes(x = sample_size, y = p, color = inference) +
  geom_line(size = 1.1, alpha = .7) +
  # geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_hline(yintercept = c(.05, .95), linetype = "dashed") +
  facet_grid(true_effect ~ effect_size, labeller = "label_both") +
  labs(title = str_glue("Correct and incorrect inference rate for different sample sizes for {samples} samples"),
       subtitle = "Dashed lines represent 5% and 95% probability",
       x = "Sample size per class",
       y = NULL) +
  theme(legend.position = "bottom") +
  NULL



# write_csv(results_table, "simulation_10k.csv")

