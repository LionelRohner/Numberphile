
# Helpers / Aux -----------------------------------------------------------

# Internal functions

# Get number of digits ----------------------------------------------------

ndigits <- function(x){
  return(floor(log10(x)+1))
}


# Extract exponents -------------------------------------------------------
# CAVE: depends on ndigtits()

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

# reverses the deconstruction of ints in digits

concatenate_math <- function(x,y,base=10){
  return(x*base**(ndigits(y))+y)
}

# Exported functions ------------------------------------------------------

get_digits_vec <- function(x){
  return(get_int_exps(ndigits = ndigits(x)))
}
