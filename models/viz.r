library(tidyverse)
library(bootstrap)
library(brms)
library(jsonlite)
library(viridis)
library(grid)
library(cowplot)
library(jsonlite)
library(plyr)

mc.cores = parallel::detectCores()

base = 6
expand = 3
dodge = position_dodge(.9)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
alt <- data.frame(fromJSON("results_alt.json"))
exh <- data.frame(fromJSON("results_exh.json"))
strong <- data.frame(fromJSON("results_strong.json"))
weak <- data.frame(fromJSON("results_weak.json"))

exh$condition = "exh"
strong$condition = "strong"
weak$condition = "weak"
alt$condition = "alt"

d <- rbind(strong,weak,alt,exh)
d$condition <- factor(d$condition,levels(factor(d$condition))[c(1,2,4,3)])

prod_graph = function(data) {
  
  levels(data$condition) = c("Before parameter change","Increase prior\nexpectation of\nexhaustive alternative","Increase prior\nexpectation that lexicon\ncontains non-exhaustive 'some'", "Increase prior\nexpectation that lexicon\ncontains exhaustive 'some'")
  data$state = factor(data$state)
  levels(data$state) = c("6/9 symbols\nare X", "9/9 symbols\nare X")

  plot <- ggplot(data, aes(x=factor(state), y=probs, fill = condition)) +
    geom_bar(stat="identity",position = "dodge") +
    theme_bw() +
    scale_fill_grey() +
    theme(text = element_text(size = base * expand / 1.1, face = "bold"),
          legend.position = "right") +
          #axis.text.x = element_text(angle = 25, hjust = 1)) +
    labs(fill = element_blank(), y = "Simulated L1 probability") +
    xlab(element_blank()) +
    ylim(0,0.9) +
    guides(fill=guide_legend(
      keywidth=0.1,
      keyheight=0.75,
      default.unit="inch")
    )
  
  return(plot)
  
}

prod_graph(d)

ggsave("plot.pdf",width = 7, height = 4,units = "in",dpi = 1000)

exh_graph <- prod_graphs(exh) + ggtitle("Higher expectation of exhaustive alternative")
weak_graph <- prod_graphs(weak) + ggtitle("Higher expectation of inclusive 'some'")
strong_graph <- prod_graphs(strong) + ggtitle("Higher expectation of exhaustive 'some'")

graphs <- plot_grid(exh_graph, weak_graph, strong_graph,
                    nrow = 1, align = "hv")

legend <- plot_grid(get_legend(exh_graph + theme(legend.position = "bottom")))

plot_grid(graphs,legend,nrow=2,rel_heights = c(6,1))

ggsave(paste("predictions.jpg",sep=""), width = 8, height = 2, units = "in", dpi = 1000)

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
    theme(text = element_text(size = base * expand / 2.834646, face = "bold")) +
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

# rtlegend <- plot_grid(get_legend(prod_rtgraphs("Alternative") + theme(legend.position = "bottom")))

plot_grid(graphs,legend,rtgraphs,nrow=3,rel_heights = c(6, 1, 6))

ggsave(paste("selectionandresponsetimes.jpg",sep=""), width = 8, height = 4, units = "in", dpi = 1000)

# COMPARING AGGREGATE BEHAVIOR BETWEEN CONDITIONS

d$btwncondition = factor(d$btwncondition)

d$btwncondition = factor(d$btwncondition,levels(d$btwncondition)[c(1,3,4,2)])

# VISUALIZE - SELECTIONS

byCondition <- d %>%
  filter(primeStrength %in% c(0,1,3)) %>%
  group_by(btwncondition) %>%
  summarize(Mean = mean(responseChoice), 
            CILow =ci.low(responseChoice),
            CIHigh =ci.high(responseChoice)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow,
         YMax = Mean + CIHigh)

levels(byCondition$btwncondition) = c("Alternative","Exhaustive-only","Exhaustive-\nsubclausal","Exhaustive-\nclausal")

ggplot(byCondition, aes(x=btwncondition, y=Mean, fill = btwncondition)) +
  scale_fill_grey() +
  theme_bw() +
  geom_bar(stat="identity",position = "dodge") +
  theme(text = element_text(size = base * expand / 2.834646, face = "bold"),
        legend.position = "none") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +
  labs(x = "Condition", y = "Proportion of\n'Better Picture?' chosen")

## BY EXPR TYPE 

byConditionType <- d %>%
  filter(primeStrength %in% c(0,1,3)) %>%
  group_by(btwncondition,primeType) %>%
  summarize(Mean = mean(responseChoice), 
            CILow =ci.low(responseChoice),
            CIHigh =ci.high(responseChoice)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow,
         YMax = Mean + CIHigh)

levels(byCondition$btwncondition) = c("Alternative","Exhaustive-only","Exhaustive-\nsubclausal","Exhaustive-\nclausal")

ggplot(byConditionType, aes(x=btwncondition, y=Mean, fill = primeType)) +
  scale_fill_grey() +
  theme_bw() +
  geom_bar(stat="identity",position = "dodge") +
  theme(text = element_text(size = base * expand / 2.834646, face = "bold"),
        legend.position = "bottom") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge) +
  labs(x = "Condition", y = "Proportion of\n'Better Picture?' chosen")


ggsave(paste("btwncond.jpg",sep=""), width = 3, height = 1.5, units = "in", dpi = 1000)

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
 