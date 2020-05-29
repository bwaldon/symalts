library(tidyverse)
library(bootstrap)
library(brms)
library(jsonlite)
library(viridis)
library(grid)
library(cowplot)

mc.cores = parallel::detectCores()

base = 6
expand = 3

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d <- read_csv("../data/main/data.csv")

# HELPER SCRIPTS

dodge = position_dodge(.9)

theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}

ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

d$primeStrength <- factor(d$primeStrength)

d$primeType = factor(d$primeType)
levels(d$primeType) = c("Some", "Number", "Ad-hoc")

alternative = d %>%
  filter(btwncondition == "alternative")

exhclausal = d %>%
  filter(btwncondition == "exh-clausal")

exhsubclausal = d %>%
  filter(btwncondition == "exh-subclausal")

exhonly= d %>%
  filter(btwncondition == "exh-only")

prod_graphs = function(condname) {
  if(condname == "Canonical") {
    subset <- alternative
  } else if (condname == "Exhaustive\n-clausal") {
    subset <- exhclausal
  } else if (condname == "Exhaustive\n-only") {
    subset <- exhonly
  } else if (condname == "Exhaustive\n-subclausal") {
    subset <- exhsubclausal
  }
  
  # RELABELING
  
  levels(subset$primeStrength) <- c("Weak", "Strong", "Alternative", "Baseline")
  
  # MEANS AND CONFINTS BY EXPRESSION + CONDITION
  
  byConditionExpression <- subset %>%
    group_by(primeStrength,primeType) %>%
    summarize(Mean = mean(responseChoice), 
              CILow =ci.low(responseChoice),
              CIHigh =ci.high(responseChoice)) %>%
    ungroup() %>%
    mutate(YMin = Mean - CILow,
           YMax = Mean + CIHigh)
  
  # VISUALIZE - CONDITION x EXPRESSION
  
  base = 6
  expand = 3
  
  plot <- ggplot(byConditionExpression, aes(x=primeType, y=Mean, fill = primeStrength)) +
    geom_bar(stat="identity",position = "dodge") +
    theme_bw() +
    scale_fill_viridis(discrete = TRUE) +
    theme(text = element_text(size = base * expand / 2, face = "bold")) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +  
    labs(fill = "Prime type") +
    xlab(element_blank()) +
    ylim(0,0.71) +
    ggtitle(condname)
  
  if(!(condname == "Canonical")) {
    plot <- plot + labs(y = NULL)
  } else {
    plot <- plot + labs(y = "Proportion of\n'Better Picture?' chosen")
  }
  
  return(plot)
  
}

alt_graph <- prod_graphs("Canonical") + theme(legend.position = "none")
exhonly_graph <- prod_graphs("Exhaustive\n-only") + theme(legend.position = "none")
exhclausal_graph <- prod_graphs("Exhaustive\n-clausal") + theme(legend.position = "none")
exhsubclausal_graph <- prod_graphs("Exhaustive\n-subclausal") + theme(legend.position = "none")

graphs <- plot_grid(alt_graph, exhonly_graph, exhsubclausal_graph, exhclausal_graph,
                    nrow = 1, align = "hv") + theme(panel.spacing = unit(1,"pt"))

legend <- plot_grid(get_legend(prod_graphs("Canonical") + theme(legend.position = "bottom")))

plot_grid(graphs,legend,nrow=2,rel_heights = c(6,1))

ggsave(paste("selections.pdf",sep=""), width = 8, height = 2.5, units = "in", dpi = 1000)

# BY EXPR TYPE 

d$btwncondition = factor(d$btwncondition)

d$btwncondition = factor(d$btwncondition,levels(d$btwncondition)[c(1,3,4,2)])

byConditionType <- d %>%
  group_by(btwncondition,primeType) %>%
  summarize(Mean = mean(responseChoice), 
            CILow =ci.low(responseChoice),
            CIHigh =ci.high(responseChoice)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow,
         YMax = Mean + CIHigh)

levels(byConditionType$btwncondition) = c("Canonical","Exhaustive-\nonly","Exhaustive-\nsubclausal","Exhaustive-\nclausal")

ggplot(byConditionType, aes(x=btwncondition, y=Mean, fill = primeType)) +
  scale_fill_grey() +
  theme_bw() +
  geom_bar(stat="identity",position = "dodge") +
  theme(text = element_text(size = base * expand / 1.25, face = "bold"),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +
  labs(x = "Condition", y = "Proportion of \n'Better Picture?'\n chosen", fill = "Expression")

ggsave(paste("btwncond.pdf",sep=""), width = 6, height = 4, units = "in", dpi = 1000)

# OTHER GRAPHS (NOT IN PAPER)

# REACTION TIME PLOTS

rt_mean = mean(d$response_rt)
rt_sd = sd(d$response_rt)

d_rt <- d %>%
  filter((rt_mean - 2 * rt_sd) < response_rt) %>%
  filter((rt_mean + 2 * rt_sd) > response_rt)

alternative_rt = d_rt %>%
  filter(btwncondition == "alternative")

exhclausal_rt = d_rt %>%
  filter(btwncondition == "exh-clausal")

exhsubclausal_rt = d_rt %>%
  filter(btwncondition == "exh-subclausal")

exhonly_rt = d_rt %>%
  filter(btwncondition == "exh-only")

# REACTION TIME GRAPHS

prod_rtgraphs = function(condname) {
  if(condname == "Alternative") {
    subset <- alternative_rt
  } else if (condname == "Exhaustive-clausal") {
    subset <- exhclausal_rt
  } else if (condname == "Exhaustive-only") {
    subset <- exhonly_rt
  } else if (condname == "Exhaustive-subclausal") {
    subset <- exhsubclausal_rt
  }
  
  # RELABELING
  
  levels(subset$primeStrength) <- c("Weak", "Strong", "Alternative; Exh-only;\nExh-subclausal; Exh-clausal", "Baseline")
  
  # MEANS AND CONFINTS BY EXPRESSION + CONDITION
  
  byConditionExpression <- subset %>%
    group_by(primeStrength,primeType) %>%
    summarize(Mean = mean(response_rt), 
              CILow =ci.low(response_rt),
              CIHigh =ci.high(response_rt)) %>%
    ungroup() %>%
    mutate(YMin = Mean - CILow,
           YMax = Mean + CIHigh)
  
  
  # VISUALIZE - CONDITION x EXPRESSION
  
  base = 6
  expand = 3
  
  # axis.ticks.x=element_blank()
  
  plot <- ggplot(byConditionExpression, aes(x=primeType, y=Mean, fill = primeStrength)) +
    geom_bar(stat="identity",position = "dodge") +
    theme_bw() +
    scale_fill_viridis(discrete = TRUE) +
    theme(text = element_text(size = base * expand / 1.834646, face = "bold")) +
    geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +  
    labs(fill = "Prime type") +
    xlab(element_blank()) +
    ylim(0,3000) +
    ggtitle(condname)
  
  if(!(condname == "Alternative")) {
    plot <- plot + labs(y = NULL)
  } else {
    plot <- plot + labs(y = "Mean response time (ms)")
  }
  
  # ggsave(paste(condname,".jpg",sep=""), width = 4, height = 2, units = "in", dpi = 1000)
  return(plot)
  
}

alt_rtgraph <- prod_rtgraphs("Alternative") + theme(legend.position = "none")
exhonly_rtgraph <- prod_rtgraphs("Exhaustive-only") + theme(legend.position = "none")
exhclausal_rtgraph <- prod_rtgraphs("Exhaustive-clausal") + theme(legend.position = "none")
exhsubclausal_rtgraph <- prod_rtgraphs("Exhaustive-subclausal") + theme(legend.position = "none")

rtgraphs <- plot_grid(alt_rtgraph, exhonly_rtgraph, exhsubclausal_rtgraph, exhclausal_rtgraph,nrow=1,vjust = -2)

plot_grid(graphs,legend,rtgraphs,nrow=3,rel_heights = c(6, 1, 6))

ggsave(paste("selectionandresponsetimes.jpg",sep=""), width = 8, height = 4, units = "in", dpi = 1000)

# COMPARING AGGREGATE BEHAVIOR BETWEEN CONDITIONS

d$btwncondition = factor(d$btwncondition)

d$btwncondition = factor(d$btwncondition,levels(d$btwncondition)[c(1,3,4,2)])

# VISUALIZE - SELECTIONS

byCondition <- d %>%
  group_by(btwncondition) %>%
  summarize(Mean = mean(responseChoice), 
            CILow =ci.low(responseChoice),
            CIHigh =ci.high(responseChoice)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow,
         YMax = Mean + CIHigh)

levels(byCondition$btwncondition) = c("Canonical","Exhaustive-only","Exhaustive-\nsubclausal","Exhaustive-\nclausal")

ggplot(byCondition, aes(x=btwncondition, y=Mean, fill = btwncondition)) +
  scale_fill_grey() +
  theme_bw() +
  geom_bar(stat="identity",position = "dodge") +
  theme(text = element_text(size = base * expand / 2.834646, face = "bold"),
        legend.position = "none") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +
  labs(x = "Condition", y = "Proportion of\n'Better Picture?' chosen")



# SAME THING, FOR REACTION TIMES

d_rt$btwncondition = factor(d_rt$btwncondition)

d_rt$btwncondition = factor(d_rt$btwncondition,levels(d_rt$btwncondition)[c(1,3,4,2)])

byCondition_rt <- d_rt %>%
  filter(primeStrength %in% c(0,1,3)) %>%
  group_by(btwncondition) %>%
  summarize(Mean = mean(response_rt), 
            CILow =ci.low(response_rt),
            CIHigh =ci.high(response_rt)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow,
         YMax = Mean + CIHigh)

levels(byCondition_rt$btwncondition) <- c("Alternative","Exhaustive-only","Exhaustive-\nsubclausal","Exhaustive-\nclausal")

ggplot(byCondition_rt, aes(x=btwncondition, y=Mean, fill = btwncondition)) +
  scale_fill_grey() +
  theme_bw() +
  geom_bar(stat="identity",position = "dodge") +
  theme(text = element_text(size = base * expand / 2.834646, face = "bold"),
        legend.position = "none") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +
  labs(x = "Condition", y = "Mean response time (ms)")

ggsave(paste("btwncond_rt.jpg",sep=""), width = 4, height = 3, units = "in", dpi = 1000)

# CHANGE IN RT BASED ON SELECTION? 

bySelection_rt <- d_rt %>%
  group_by(responseChoice,btwncondition) %>%
  summarize(Mean = mean(response_rt), 
            CILow =ci.low(response_rt),
            CIHigh =ci.high(response_rt)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow,
         YMax = Mean + CIHigh)

bySelection_rt$responseChoice <- factor(bySelection_rt$responseChoice)
levels(bySelection_rt$responseChoice) <- c("Weak image",'"Better picture?"')
levels(bySelection_rt$btwncondition) <- c("Alternative","Exhaustive-only","Exhaustive-\nsubclausal","Exhaustive-\nclausal")

ggplot(bySelection_rt, aes(x=btwncondition, y=Mean, fill = responseChoice)) +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  geom_bar(stat="identity",position = "dodge") +
  theme(text = element_text(size = base * expand / 2.834646, face = "bold"),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +
  labs(x = "Condition", y = "Mean response time (ms)", fill = "Response choice")

ggsave(paste("byselect_rt.jpg",sep=""), width = 3, height = 2, units = "in", dpi = 1000)

# BAYESIAN LOGISTIC REGRESSION

levels(d$primeStrength) <- c("Weak", "Strong", "Alternative", "Baseline")

d$primeStrength = relevel(d$primeStrength, ref = "Baseline")

bayesmodel = function(condname, pstrength){
  if(condname == "Alternative") {
    subset <- alternative
  } else if (condname == "Exhaustive-clausal") {
    subset <- exhclausal
  } else if (condname == "Exhaustive-only") {
    subset <- exhonly
  } else if (condname == "Exhaustive-subclausal") {
    subset <- exhsubclausal
  }
  m <- brm(
    responseChoice ~ primeStrength * primeType + (1 + primeStrength*primeType|responseSymbols) + (1 + primeStrength*primeType|workerid), 
    data = subset %>% filter(primeStrength %in% c(pstrength, "3")),
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    family = "bernoulli",
    seed = 123
  )
  return(m)
}

## MAXIMAL RANDOM EFFECTS STRUCTURE, USING DEFAULT UNINFORMATIVE OR WEAKLY INFORMATIVE PRIORS

m <- brm(
  responseChoice ~ primeStrength * primeType + (1 + primeStrength*primeType|responseSymbols) + (1 + primeStrength*primeType|workerid), 
  data = alternative,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  family = "bernoulli",
  seed = 123
)

## TAKING OUT BY-ITEM TERM

m <- brm(
  responseChoice ~ primeStrength * primeType + (1 + responseSymbols) + (1 + primeStrength*primeType|workerid), 
  data = d,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  family = "bernoulli",
  seed = 123
)

### FIXED EFFECTS TABLE

fixed_effects <- data.frame(fixef(m_max_pw_alternative))

fixed_effects$X1 <- c("Intercept", "Condition = Weak", "Condition = Strong", 
                      "Condition = Exhaustive", "Expression = Number", "Expression = Ad-hoc",
                      "Condition = Weak : Expression = Number", "Condition = Strong : Expression = Number",
                      "Condition = Exhaustive : Expression = Number", "Condition = Weak : Expression = Ad-hoc",
                      "Condition = Strong : Expression = Ad-hoc", "Condition = Exhaustive : Expression = Ad-hoc")

fixed_effects_table <- 
  fixed_effects %>%
  rename(Predictors = "X1", `2.5%` = "Q2.5", `97.5%` = "Q97.5") %>%
  select(-Est.Error) %>%
  mutate(Evidence=sign(`2.5%`)==sign(`97.5%`)) %>%
  mutate(Evidence=replace(Evidence,Evidence==TRUE,"*")) %>%
  mutate(Evidence=replace(Evidence,Evidence==FALSE,""))

# ERRATTA (NOT PART OF PREREGISTRATION)

## NO RANDOM-EFFECTS IN BRM MODEL (FOR TESTING)

m <- brm(
  responseChoice ~ primeStrength * primeType, 
  data = d,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  family = "bernoulli",
  seed = 123
)

## NUMBER OF CRITICAL RESPONSES PER CONDITION...

summmary_noex <- d_noex %>%
  filter(trial_type == "response") %>%
  group_by(primeStrength) %>%
  summarize(n = n())

summmary <- d %>%
  filter(trial_type == "response") %>%
  group_by(primeStrength) %>%
  summarize(n = n())

## EXCLUDE DATA IF 'RIGHT' MATH ANSWER AND 'WRONG' MATH ANSWER MATCH (SANITY CHECK)

t2 <- d_noex %>%
  filter(trial_type == "response" & primeStrength == 3) %>% 
  select(pOSymbols,pOwrongAnswer,pTSymbols,pTwrongAnswer) %>%
  mutate(pTnumbers = gsub("\\]","",gsub("\\[","",pTSymbols))) %>%
  mutate(rightAT = sapply(strsplit(as.character(pTnumbers), ","), function(x) x[[1]][1])) %>%
  mutate(wrongAT = gsub("\\]","",gsub("\\[","",pTwrongAnswer))) %>%
  mutate(wrongAT = sapply(strsplit(as.character(wrongAT), ","), function(x) x[[1]][1])) %>%
  mutate(pOnumbers = gsub("\\]","",gsub("\\[","",pOSymbols))) %>%
  mutate(rightAO = sapply(strsplit(as.character(pOnumbers), ","), function(x) x[[1]][1])) %>%
  mutate(wrongAO = gsub("\\]","",gsub("\\[","",pOwrongAnswer))) %>%
  mutate(wrongAO = sapply(strsplit(as.character(wrongAO), ","), function(x) x[[1]][1])) %>%
  filter(rightAO == wrongAO | rightAT == wrongAT)

# wrongA = fromJSON(pOwrongAnswer)[1],

## INDIVIDUAL-LEVEL ANALYSES

d_ind <- d %>% 
  group_by(workerid, primeType) %>% 
  summarize(number_of_switches = sum(diff(responseChoice)!=0)) 

d_ind %>%
  group_by(primeType) %>%
  summarize(more_than_two = sum(number_of_switches >= 2)/n())

ggplot(d_ind, aes(x=number_of_switches, fill = primeType)) +
  geom_histogram(position = "dodge") +
  theme(axis.text.x=element_text(angle=20,hjust=1,vjust=1)) +
  scale_x_continuous(breaks=seq(0,16,1)) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +  
  labs(x = "Times response changed", y = "Number of people", fill = "Expression") 

ggsave("~/Desktop/indvar.png", dpi = 600)

# MEANS AND CONFINTS BY EXPRESSION + CONDITION

byConditionExpressionPrime <- d_rt %>%
  group_by(primeStrength,btwncondition) %>%
  mutate(prime_rt = block_rt - response_rt) %>%
  summarize(Mean = mean(prime_rt), 
            CILow =ci.low(prime_rt),
            CIHigh =ci.high(prime_rt)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow,
         YMax = Mean + CIHigh)

### ERRATA

# # MEANS AND CONFINTS BY CONDITION
# 
# byCondition <- subset %>%
#   group_by(primeStrength) %>%
#   summarize(Mean = mean(responseChoice), 
#             CILow =ci.low(responseChoice),
#             CIHigh =ci.high(responseChoice)) %>%
#   ungroup() %>%
#   mutate(YMin = Mean - CILow,
#          YMax = Mean + CIHigh)
# 
# # MEANS AND CONFINTS BY EXPRESSION
# 
# byExpression <- subset %>%
#   group_by(primeType) %>%
#   summarize(Mean = mean(responseChoice), 
#             CILow =ci.low(responseChoice),
#             CIHigh =ci.high(responseChoice)) %>%
#   ungroup() %>%
#   mutate(YMin = Mean - CILow,
#          YMax = Mean + CIHigh)

# # VISUALIZE - BY CONDITION
# 
# ggplot(byCondition, aes(x=primeStrength, y=Mean)) +
#   geom_bar(stat="identity",position = "dodge") +
#   theme(axis.text.x=element_text(angle=20,hjust=1,vjust=1)) +
#   geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +  
#   labs(x = "Condition", y = "Proportion of\n'Better Picture?' chosen") 

# # VISUALIZE - BY EXPRESSION
# 
# ggplot(byExpression, aes(x=primeType, y=Mean)) +
#   geom_bar(stat="identity",position = "dodge") +
#   theme(axis.text.x=element_text(angle=20,hjust=1,vjust=1)) +
#   geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +  
#   labs(x = "Condition", y = "Proportion of\n'Better Picture?' chosen") 

# byBtwnConditionType <- d %>%
#   group_by(primeType,btwncondition) %>%
#   summarize(Mean = mean(responseChoice), 
#             CILow =ci.low(responseChoice),
#             CIHigh =ci.high(responseChoice)) %>%
#   ungroup() %>%
#   mutate(YMin = Mean - CILow,
#          YMax = Mean + CIHigh)
