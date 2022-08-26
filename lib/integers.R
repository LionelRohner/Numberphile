
# Helpers / Aux -----------------------------------------------------------

#' Get number of digits of a number
#' log10 of a number gives us the approximate number of digits - 1. By using the floor operation we cancel the decimal noise
#' @param x 
#'
#' @return
#'
#' @examples

ndigits <- function(x){
  assertthat::assert_that(min(x) >= 0, msg = "x should be positive!")
  out <- floor(log10(x)+1)
  return(out)
}


# By taking 10 to the power of the number of digits of a number and dividing
# by 10 we get the geometric sequence 1, 10 , 100 , 1000 etc

# TODO: ndigits is a vector, rethink the loop

get_powers_of_ten <- function(ndigits){
  pwrs <- c()
  for (i in 1:ndigits){
    print(i)
    pwr <- 10**ndigits/10
    pwrs <- c(pwrs,pwr)
    ndigits <- ndigits - 1
  }
  # Multiply by 10 to get the upper and not the lower range.
  pwrs <- pwrs * 10
  return(pwrs)
}

# Reverses the deconstruction of integers in digits.https://mathworld.wolfram.com/Concatenation.html

#' Concatenate
#'
#' @param x 
#' @param y 
#' @param base 
#'
#' @return
#' @export
#'
#' @examples
concatenate_math <- function(x,y,base=10){
  concant <- x*base**(ndigits(y))+y
  return(concat)
}

# Main functions ------------------------------------------------------

# Combination of the function above plus one more step. We first get the number
# of digits the integer x followed by its exponent. Then we reapply the previous
# function but with another base, which is the integer itself

# TODO create a function for x%%exps%/%(exps%/%10) and make a wrapper later

# https://stackoverflow.com/questions/19764244/how-can-we-split-an-integer-number-into-a-vector-of-its-constituent-digits-in-r

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
get_digits_vec <- function(x){
  exps <- get_powers_of_ten(ndigits = ndigits(x))
  return(x%%exps%/%(exps%/%10))
}

  