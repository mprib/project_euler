library(stringr)

# is_palindrome <- function(n) {
#   
#   
#   
# }


n <- 9009

is_palindrome <- function(n){
  
  digits <- str_split(n,"")[[1]]
  palindrome_check <- digits == rev(digits)
  
  result <- all(palindrome_check)  
  
  return(result)
  
}


