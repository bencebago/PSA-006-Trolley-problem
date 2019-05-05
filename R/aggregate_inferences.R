# Create summary table

aggregate_inferences <- function(ls){

  ls %>% 
  map_dfr(out, ~enframe(.x, name = NULL) %>% 
            t() %>% 
            as_tibble()) %>%
  rename(true_effect = 1, 
         sample_size_per_class = 2, 
         correct_inference_rate = 3,
         incorrect_inference_rate = 4) %>% 
  group_by(true_effect, sample_size_per_class) %>% 
  summarise(correct_inference_rate = mean(as.logical(correct_inference_rate)),
            incorrect_inference_rate = mean(as.logical(incorrect_inference_rate)))
}