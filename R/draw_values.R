# Generate random values based on provided parameters
# This function samples n integer values between a floor and a ceiling
# The probability of the values are calculated based on a normal distribution of a known
# mean and standard deviation

draw_values <- function(n, # Number of samples 
                        mean, # population mean
                        sd, # population sd
                        floor, # Minimum value
                        ceiling # Maximum value
){
  
  base::sample(floor:ceiling,
               size = n, 
               replace = TRUE,
               prob = msm::dtnorm(floor:ceiling, mean, sd, lower = floor, upper = ceiling)
  )
}
