
# Erathosthenes -----------------------------------------------------------

#' Title
#'
#' @param to 
#'
#' @return
#' @export
#'
#' @examples
primeEvalErathostenes <- function(to){
  
  # exceptions
  if (to == 2){
    return(2)
  } else if (to == 3){
    return(3)
  }
  
  # vars
  upperBound = round(sqrt(to))
  # print(upperBound)
  vecNat = rep(T, to)
  
  # expection from 1
  vecNat[1] <- F
  
  # sieve of erathostenes
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
    
    # redefine new prime
    lastPrime = lastPrime + 1
    
  }
  
  # return primes, i.e. those that are still TRUE
  return(which(vecNat))
}

# Cyclic Primes -----------------------------------------------------------

#' Title
#'
#' @param primes 
#'
#' @return
#' @export
#'
#' @examples
getCyclicPrimes <- function(primes){
  
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
