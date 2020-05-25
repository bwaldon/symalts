library(tidyverse)
library(jsonlite)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df = read.csv("pilot_data.csv", header = TRUE)

df = df %>%
  select(primeStrength, responseChoice)

exp_data = toJSON(df)

exp_data

# beginning of output
[{"primeStrength":3,"responseChoice":1},{"primeStrength":3,"responseChoice":1},{"primeStrength":1,"responseChoice":1}

####################################################################
#weak prime - for testing toymodels  
weak_prime = df %>%
  filter(primeStrength == 0) %>%
  select(responseChoice)

weak_prime = as.array(weak_prime$responseChoice)
weak_data = toJSON(weak_prime)
weak_data


weak_prime = df %>%
  filter(primeStrength == 0) %>%
  filter(responseChoice==1) %>%
  select(responseChoice)

#strong prime - for testing toymodels  
strong_prime = df %>%
  filter(primeStrength == 1) %>%
  select(responseChoice) %>%

strong_prime = as.array(strong_prime$responseChoice)
strong_data = toJSON(strong_prime)
strong_data

#delete this
strong_prime = strong_prime %>%
  filter(primeStrength == 1) %>%
  filter(responseChoice == 1) %>%
  select(responseChoice)




