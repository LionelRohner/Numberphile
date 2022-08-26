
# Erathostenes ------------------------------------------------------------

#' Sieve of Erathostenes
#'
#' @param to upper bound of primes to return
#'
#' @return Vector of primes up to to-argument
#' @export
#'
#' @examples primes_to_11 <- sieve_ov_Erathostenes(11)
sieve_ov_Erathostenes <- function(to){
  
  # exceptions
  if (to == 2){
    return(2)
  } else if (to == 3){
    return(3)
  }
  
  # vars
  upperBound = round(sqrt(to))
  
  # generate vec of boolean of length "to"
  vecNat = rep(T, to)
  
  # set 1 to false, edge case
  vecNat[1] <- F
  
  # sieve of erathostenes
  # start with 2
  lastPrime = 2
  
  for (i in lastPrime:upperBound){
    # skip indeces that are already F
    if (vecNat[i] == F){
      lastPrime = lastPrime + 1
      next
    }
    
    # find all multiples of primes as these must be composite
    multiples <- seq(lastPrime*2, to, lastPrime)
    vecNat[multiples] <- F
    
    # redefine new lastPrime
    lastPrime = lastPrime + 1
  }
  
  # return primes, i.e. those that are still TRUE
  return(which(vecNat))
}

# Cyclic Primes -----------------------------------------------------------

#' Get cyclic primes - E.g. 113, 131, and 311, i.e. cyclic permutatios that
#' are also primes
#'
#' @param primes 
#'
#' @return
#' @export
#'
#' @examples primes_to_11 <- sieve_ov_Erathostenes(11)
#' get_cyclic_primes(primes_to_11)
get_cyclic_primes <- function(primes){
  
  # Cache vector for primes - reduces the number of iteration if primes are already in
  # this vectior. E.g. a cyclic prime with 3 digits will appear 3 times and should only
  # be evaluated once.
  rotVecCache <- c()
  
  # empty vector for output$
  cyclicPrimes <- c()
  
  cnt = 0
  
  # nenad'sche Heuristik
  primes = primes[!grepl("0|[0-9]+2|4|6|8", primes)]
  
  # go through all rotations in all primes of the slice primesRot
  for(potCyclicPrimes in primes){
    
    # transform prime to character vector to evaluate rotation
    rot <- as.character(potCyclicPrimes)
    rotVec <- unlist(strsplit(rot,""))
    
    # rotation
    check = 0
    
    rotation = length(rotVec)
    
    # evaluate if all rotations are also primes 
    for (i in 1:rotation){
      
      lastIdx <- length(rotVec)
      
      # puts the last index at the first position and pushes the OG-vec to position 2
      rotVec <- rotVec[c(lastIdx,1:lastIdx-1)]
      
      # if rotVec not in cache check whether rotations are primes
      rotVecNum <- as.numeric(paste(rotVec, collapse = "",sep = ""))
      cnt = cnt + 1
      
      # check if rotation is prime
      if(rotVecNum %in% primes){
        # if rotation is in primes, add +1 to check counter  
        check = check + 1
      } else {
        break
      }
    }
    # if the number of checks is equal to the number of rotation all rotation are primes
    if (check == rotation){
      cyclicPrimes <- append(cyclicPrimes, potCyclicPrimes)
    }
  }
  return(cyclicPrimes)
}

# Coin change algo --------------------------------------------------------

#' Title
#'
#' @param coins 
#' @param N 
#'
#' @return
#' @export
#'
#' @examples
coin_change_algo <- function(coins, N){
  
  # create vars
  ways = rep(0,N+1)
  lenCoils = length(coins)
  lenWays = N+1
  
  # Initialize Algo
  ways[1] = 1
  
  # do dynamic programming
  for (i in 1:lenCoils){
    for (j in 1:length(ways)){
      if (coins[i] < j){
        ways[j] <- ways[j] + ways[(j - coins[i])]
      }
    }
  }
  return(ways[N])
}

# TODO: shouldnt this be 3? 1,1,1; 1,1,2; 2,2
coin_change_algo(c(1,2),4)

