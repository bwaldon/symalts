library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

c1_b1 <- read_csv("../data/main/c1-batch1-trials.csv")
c2_b1 <- read_csv("../data/main/c2-batch1-trials.csv")
c3_b1 <- read_csv("../data/main/c3-batch1-trials.csv")
c4_b1 <- read_csv("../data/main/c4-batch1-trials.csv")
c1_b2 <- read_csv("../data/main/c1-batch2-trials.csv")
c2_b2 <- read_csv("../data/main/c2-batch2-trials.csv")
c3_b2 <- read_csv("../data/main/c3-batch2-trials.csv")
c4_b2 <- read_csv("../data/main/c4-batch2-trials.csv")
c1_b3 <- read_csv("../data/main/c1-batch3-trials.csv")
c2_b3 <- read_csv("../data/main/c2-batch3-trials.csv")
c3_b3 <- read_csv("../data/main/c3-batch3-trials.csv")
c4_b3 <- read_csv("../data/main/c4-batch3-trials.csv")

c1_b1$list <- "c1_b1"
c2_b1$list <- "c2_b1"
c3_b1$list <- "c3_b1"
c4_b1$list <- "c4_b1"

c1_b2$list <- "c1_b2"
c2_b2$list <- "c2_b2"
c3_b2$list <- "c3_b2"
c4_b2$list <- "c4_b2"

c1_b3$list <- "c1_b3"
c2_b3$list <- "c2_b3"
c3_b3$list <- "c3_b3"
c4_b3$list <- "c4_b3"


d_noex <- rbind(c1_b1,c2_b1,c3_b1,c4_b1,
                c1_b2,c2_b2,c3_b2,c4_b2,
                c1_b3,c2_b3,c3_b3,c4_b3)

rm(c1_b1,c2_b1,c3_b1,c4_b1,
   c1_b2,c2_b2,c3_b2,c4_b2,
   c1_b3,c2_b3,c3_b3,c4_b3)

# DATA WRANGLING 

# RUN EXCLUSIONS, SUBSET TO CRITICAL

##  MULTIPLE PARTICIPATION EXCLUSIONS

excludes_mult <- read_csv("excludes_mult.csv")

d_noex <- anti_join(x=d_noex, y=excludes_mult, by=c("workerid", "list")) 

## NATIVE LANGUAGE EXCLUSIONS

excludes_lang <- read_csv("excludes_lang.csv")

d_noex <- anti_join(x=d_noex, y=excludes_lang, by=c("workerid", "list")) 

## PERFORMANCE EXCLUSIONS, SUBSET TO CRITICAL

d <- d_noex %>%
  filter(trial_type == "response") %>%
  filter((correctPChoices == TRUE & primeStrength != 3) | primeStrength == 3)

# HOW MUCH OF THE DATA DO WE EXCLUDE DUE TO PERFORMANCE?

(nrow(d_noex %>% filter(trial_type == 'response')) - nrow(d))/nrow(d_noex %>% filter(trial_type == 'response'))

meanat = mean(d$Answer.time_in_minutes) 
meansd = sd(d$Answer.time_in_minutes) 

# UNIQUE WORKER ROW

d$workerid_notunique = d$workerid
d$workerid <- cumsum(c(0,as.numeric(diff(d$workerid_notunique))!=0))

# SAVE DATA

write_csv(d, "../data/main/data.csv")
