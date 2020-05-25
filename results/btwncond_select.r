library(tidyverse)
library(brms)
args = commandArgs(trailingOnly=TRUE)

options(mc.cores = 4)

d <- read_csv("../data/main/data.csv")

d$primeType = factor(d$primeType)
levels(d$primeType) = c("Some", "Number", "Ad-hoc")
d$primeStrength = relevel(factor(d$primeStrength), ref = "3")

d$btwncondition = factor(d$btwncondition)
d$btwncondition = factor(d$btwncondition,levels(d$btwncondition)[c(1,3,4,2)])
d$btwncondition = relevel(d$btwncondition, ref = "alternative")

# HOW TO OPERATIONALIZE "ITEM"

d <- d %>%
  mutate(itemvec = str_replace(responseSymbols, "\\]", "")) %>%
  mutate(itemvec = str_replace(itemvec, "\\[", "")) %>%
  mutate(itemvec = strsplit(itemvec, split=","))

d$item <- as.numeric(lapply(d$itemvec, FUN = function(x) x[[1]][[1]]))

bayesmodel = function(type) {
  subset <- d %>% filter(primeType == type)
  m <- brm(
    responseChoice ~ btwncondition + (1 + btwncondition | item) + (1 | workerid), 
    data = subset %>% filter(primeStrength %in% c("0","1","3")),
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    family = "bernoulli",
    seed = 123,
    iter = 4000,
    # inits = 0
  )
  return(m)
}

model_out <- bayesmodel(args[1])

fixed_effects <- data.frame(fixef(model_out))

fixed_effects <- cbind(rownames(fixed_effects), data.frame(fixed_effects, row.names=NULL))

save.image(file = paste("/home/groups/jdegen/pm_results/",args[1],"btwncondselect.RData",sep=""))

write_csv(fixed_effects,paste("/home/groups/jdegen/pm_results/",args[1],"btwncondselect.csv",sep=""))