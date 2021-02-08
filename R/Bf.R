#' Function to caluclate Bayes factors
#' 
## The function is retrieved from 
## https://link.springer.com/article/10.3758/s13423-017-1266-z

Bf <- function(sd, obtained, dfdata, meanoftheory, sdtheory, dftheory, tail = 2)
  
{
  
  area <- 0
  
  normarea <- 0
  
  theta <- meanoftheory - 10 * sdtheory
  
  incr <- sdtheory/200
  
  for (A in -2000:2000){
    
    theta <- theta + incr
    
    dist_theta <- dt((theta-meanoftheory)/sdtheory, df=dftheory)
    
    if(identical(tail, 1)){
      
      if (theta <= 0){
        
        dist_theta <- 0
        
      } else {
        
        dist_theta <- dist_theta * 2
        
      }
      
    }
    
    height <- dist_theta * dt((obtained-theta)/sd, df = dfdata)
    
    area <- area + height * incr
    
    normarea <- normarea + dist_theta*incr
    
  }
  
  LikelihoodTheory <- area/normarea
  
  Likelihoodnull <- dt(obtained/sd, df = dfdata)
  
  BayesFactor <- LikelihoodTheory/Likelihoodnull
  
  BayesFactor
  
}
