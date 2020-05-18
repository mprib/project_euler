# 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
# 
# What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?


divisible_by_all <- function(test_number, max_divisor) {
  
  df_nums <- tibble(divisor = 1:max_divisor)
  sum_mods <- df_nums %>% 
    mutate(mod = test_number %% divisor) %>% 
    summarize(sum = sum(mod))
  
  result <- sum_mods["sum"][[1]] == 0
  
  return(result)
  
}


# Ok. I don't think that is approach is going to work.
variables <- list(numerators = 1:500000, divisors = 1:20)
grid <- expand.grid(variables)
grid$mod <- grid$numerators %% grid$divisors
grid_grouped <- grid %>% group_by(numerators) %>% summarize(sum_mod = sum(mod)) 



# start <- Sys.time()
# 
# test_num <- 1
# while (!divisible_by_all(test_num, 20)) {
#   test_num <- test_num +1
# 
# }
# 
# print(test_num)
# 
# stop <- Sys.time()
# 
# print(paste("Elapsed Time is ", round(stop - start,2), "seconds"))

# start <- Sys.time()
# # here I will try to implement an alternate approach using a vectorized solution
# max_attempted_value <- 2520
# df_possible_numerators <- tibble(Possible_Numerator = 1:max_attempted_value)
# 
# df_possible_numerators <- df_possible_numerators %>% 
#   mutate(
#     div_all = purrr::map2(Possible_Numerator, 10, divisible_by_all)
#     )
# 
# stop <- Sys.time()
# 
# print(paste("Elapsed Time is ", round(stop - start,2), "seconds"))
