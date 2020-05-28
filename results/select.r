library(tidyverse)
library(brms)
args = commandArgs(trailingOnly=TRUE)

options(mc.cores = 4)

d <- read_csv("../data/main/data.csv")

d$primeType = factor(d$primeType)
levels(d$primeType) = c("Some", "Number", "Ad-hoc")
d$primeStrength = relevel(factor(d$primeStrength), ref = "3")

# HOW TO OPERATIONALIZE "ITEM"

d <- d %>%
  mutate(itemvec = str_replace(responseSymbols, "\\]", "")) %>%
  mutate(itemvec = str_replace(itemvec, "\\[", "")) %>%
  mutate(itemvec = strsplit(itemvec, split=","))

d$item <- as.numeric(lapply(d$itemvec, FUN = function(x) x[[1]][[1]]))

# MODEL

bayesmodel = function(condname){
  m <- brm(
    responseChoice ~ primeStrength * primeType + (1 + primeStrength*primeType|item) + (1 + primeStrength*primeType|workerid), 
    data = (d %>% filter(btwncondition == condname)),
    # control = list(adapt_delta = 0.9, max_treedepth = 15),
    family = "bernoulli",
    seed = 123,
    iter = 3000,
    # inits = 0
  )
  return(m)
}

model_out <- bayesmodel(args[1])

fixed_effects <- data.frame(fixef(model_out))

fixed_effects <- cbind(rownames(fixed_effects), data.frame(fixed_effects, row.names=NULL))

save.image(file = paste("/home/groups/jdegen/pm_results/",args[1],"select.RData",sep=""))

write_csv(fixed_effects,paste("/home/groups/jdegen/pm_results/",args[1],"select.csv",sep=""))
