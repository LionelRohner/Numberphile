
# Helpers / Aux -----------------------------------------------------------

# Internal functions

# Get number of digits ----------------------------------------------------

# log10 of a number gives us the appromixate number of digits - 1. By using the
# floor operation we cancel the decimal noise

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples

ndigits <- function(x){
  return(floor(log10(x)+1))
}


# Extract exponents -------------------------------------------------------
# CAVE: depends on ndigtits()

# by taking the 10 to the power of the number of digits in a number and dividing
# by 10 we get the geometric sequence 1, 10 , 100 , 1000 etc

get_int_exps <- function(ndigits){
  pwrs <- c()
  for (i in 1:ndigits){
    pwr <- 10**ndigits/10
    pwrs <- c(pwrs,pwr)
    ndigits <- ndigits - 1
  }
  # shift identity by one 10er exp, since we need the upper range and not the
  # lower one. Think about 1234, if we decconstruct it by digits using mudolo
  # we get 2, 3, 4, and 0, but we need the full circle. To get 1 from 1234, we 
  # need mudolo 1 + max exp, right?
  pwrs <- pwrs * 10
  return(pwrs)
}

# concatenate -------------------------------------------------------------

# reverses the deconstruction of ints in digits. This is the formula for mathematical
# concatenation of integers found on wolfram https://mathworld.wolfram.com/Concatenation.html

#' Title
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
  return(x*base**(ndigits(y))+y)
}

# Exported functions ------------------------------------------------------


# deconstruct an integer into digits --------------------------------------

# Combination of the function above plus one more step. We first get the number
# of digits the integer x followed by its exponent. Then we reapply the previous
# function but with another base, which is the integer itself

# TOOD: Rethink get_int_exps() to directly output the correct numbers! The same
# formula was used twice...


#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
get_digits_vec <- function(x){
  exps <- get_int_exps(ndigits = ndigits(x))
  return(x%%exps%/%(exps%/%10))
}

  