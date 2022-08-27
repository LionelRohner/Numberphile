
# Libs --------------------------------------------------------------------

library(tidyverse)
library(tictoc)

# Source Funcs ------------------------------------------------------------

source("lib/primes.R", encoding = "UTF-8")
source("lib/integers.R", encoding = "UTF-8")
source("lib/plot.R", encoding = "UTF-8")

# Benchmarking ------------------------------------------------------------

some_primes <- sieve_ov_Erathostenes(1e6)

# Loop
tic()
x <- extract_digits(some_primes) 
toc()

# Matrix
tic()
y <- extract_digits_matrix(some_primes) 
toc()

identical(x,y)

# Distribution ------------------------------------------------------------

# Check distribution of digits of primes
plot_all_digits(some_primes)

# Check distribution of the last digits of the series of 5
seq_5 <- 4*seq(1:1000)

# Check distribution of digits of primes
plot_last_digits(seq_5)
