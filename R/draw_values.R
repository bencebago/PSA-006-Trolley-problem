# Generate random values based on provided parameters
# This function samples n integer values between a floor and a ceiling
# The probability of the values are calculated based on a truncated normal distribution of a known
# mean and standard deviation
# OUTPUT: a vector of n random values
# EXAMPLE: draw_values(n = 10000, mean = 5, sd = 2, floor = 1, ceiling = 9)

if(!require(msm)) install.packages("msm")

draw_values <- function(n, # Number of samples 
                        mean, # population mean
                        sd, # population sd
                        floor = 1, # Minimum value
                        ceiling = 9 # Maximum value
){
  
  base::sample.int(floor:ceiling,
               size = n, 
               replace = TRUE,
               # Use truncated normal distribution
               prob = msm::dtnorm(floor:ceiling, mean, sd, floor, ceiling)
  )
}

# Test if the values are truly centered around the desired mean based on 10k values
# x <- draw_values(n = 10000, mean = 5, sd = 2, floor = 1, ceiling = 9)
# t.test(x, mu = 5)