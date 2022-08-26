
# Libs --------------------------------------------------------------------

library(tidyverse)

# Source Funcs ------------------------------------------------------------

source("lib/primes.R", encoding = "UTF-8")
source("lib/integers.R", encoding = "UTF-8")

# Testing -----------------------------------------------------------------

n = 5000

somePrimes <- sieve_ov_Erathostenes(10)

x <- somePrimes
rnd_x <-somePrimes[sample(1:length(x),n)]
  
get_digits_vec(x)

get_digits_vec(x) %>%
  tibble::as_tibble_col(column_name = "x") %>% 
  ggplot() + 
  geom_bar(aes(x=x)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(11))
