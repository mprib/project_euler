library(tidyverse)

# Trying to do this with functions  
filter_only_3_5_mult <- function(x){
  
  case_when(x %% 3 == 0 ~ x,
            x %% 5 == 0 ~ x,
            TRUE ~ as.integer(0))
  
}

1:999 %>% 
  filter_only_3_5_mult() %>% 
  sum()