get_primes <- function(max_prime){
  
  potential_primes <- as_tibble(seq(2, max_prime))
  potential_primes$PossiblePrime <- TRUE
  
  index <- 1
  
  while (index < dim(potential_primes)[1] / 2) {
    
    if(potential_primes[index,][["PossiblePrime"]]) {
      
      current_possible_prime <- potential_primes[index,][["value"]]
      sequence_to_flag <- seq(index + current_possible_prime,max_prime,current_possible_prime)
      potential_primes[sequence_to_flag,]$PossiblePrime <- FALSE
      
    }
    
    index <-index +1 
  }
  
  primes <- subset(potential_primes, PossiblePrime == TRUE)["value"]
  
  return(primes)  
  
}

get_Factors <- function(n, max_prime) {
  
  prelim_factors <- get_primes(max_prime)
  prelim_factors$mod <- n %% prelim_factors$value
  factor_list <- subset(prelim_factors, mod ==0)
  
  return(factor_list$value)
}


create_primes_database <- function(max_prime) {
  
  primes <- get_primes(max_prime)
  
  write_csv(primes,"primes.csv")
  
}

create_primes_database(1000000)
