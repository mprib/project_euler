
fibonacci_rec <- function(x){
  if(x == 1){
    return(1)
  }  
  
  if(x == 2){
    return(2)
  }  
  
  return(fibonacci_rec(x-1)+ fibonacci_rec(x-2))
}

f_vec <- map_dbl(1:10, fibonacci) 


# definitely seems like this recursive way is slow. Let's try something else
# Build a list of fibonacci numbers that you can later reference 

fib_list <- list()
fib_list[1] <- 1
fib_list[2] <- 2

index <- 3
cutoff <- 4000000
while (fib_list[index -1] < cutoff)
  {
  # check number to make sure it's not above limit before assigning it to the list
  tmp_fib <- fib_list[[index - 1]] + fib_list[[index - 2]]
  
  if(tmp_fib > cutoff)
  {
    break
  }
  
  fib_list[index] <- tmp_fib
  index <- index + 1
}

# filter out the list to only include the evens
filter_out_odds <- function(x)
{
  case_when(x %% 2 == 0 ~ x,
            TRUE ~ 0)
  
}

even_fibs_sum <- fib_list %>% 
  map(filter_out_odds) %>% 
  as_vector() %>% 
  sum()



