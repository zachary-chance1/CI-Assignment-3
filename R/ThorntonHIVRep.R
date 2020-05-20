library(tidyverse)
library(haven)

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

hiv <- read_data("thornton_hiv.dta")


# creating the permutations

tb <- NULL

permuteHIV <- function(df, random = TRUE){
  tb <- df
  first_half <- ceiling(nrow(tb)/2)
  second_half <- nrow(tb) - first_half
  
  if(random == TRUE){
    tb <- tb %>%
      sample_frac(1) %>%
      mutate(any = c(rep(1, first_half), rep(0, second_half)))
  }
  
  lm = lm(got~any+male, data = tb)
  
  deltacoef = lm$coefficients
  deltacoef = deltacoef[2]
  return(deltacoef)
  
  # te1 <- tb %>%
  #   filter(any == 1) %>%
  #   pull(got) %>%
  #   mean(na.rm = TRUE)
  # 
  # te0 <- tb %>%
  #   filter(any == 0) %>%
  #   pull(got) %>% 
  #   mean(na.rm = TRUE)
  # 
  # ate <-  te1 - te0
  
  #return(ate)
}

true_effect = permuteHIV(hiv, random = FALSE)

iterations <- 100

permutation <- tibble(
  iteration = c(seq(iterations)), 
  delta = as.numeric(
    c(permuteHIV(hiv, random = FALSE), map(seq(iterations-1), ~permuteHIV(hiv, random = TRUE)))
  )
)

#calculating the p-value

permutation <- permutation %>% 
  arrange(-delta) %>% 
  mutate(rank = seq(iterations))

p_value_100 <- permutation %>% 
  filter(iteration == 1) %>% 
  pull(rank)/iterations

iterations <- 1000

permutation <- tibble(
  iteration = c(seq(iterations)), 
  delta = as.numeric(
    c(permuteHIV(hiv, random = FALSE), map(seq(iterations-1), ~permuteHIV(hiv, random = TRUE)))
  )
)

#calculating the p-value

permutation <- permutation %>% 
  arrange(-delta) %>% 
  mutate(rank = seq(iterations))

p_value_1000 <- permutation %>% 
  filter(iteration == 1) %>% 
  pull(rank)/iterations

iterations <- 10000

permutation <- tibble(
  iteration = c(seq(iterations)), 
  delta = as.numeric(
    c(permuteHIV(hiv, random = FALSE), map(seq(iterations-1), ~permuteHIV(hiv, random = TRUE)))
  )
)

#calculating the p-value

permutation <- permutation %>% 
  arrange(-delta) %>% 
  mutate(rank = seq(iterations))

p_value_10000 <- permutation %>% 
  filter(iteration == 1) %>% 
  pull(rank)/iterations


hist(permutation$delta, freq = FALSE, breaks = 200, main = "Placebo Distribution and True Effect", xlab = "Coefficient on ANY")
abline(v = true_effect)
text(0.4, 10, "True Effect")
text(0.4, 9, as.character(round(true_effect, digits = 4)))

