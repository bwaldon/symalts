library(tidyverse)
library(viridis)

# INDIVIDUAL VARIATION

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d <- read_csv("../data/main/data.csv") 

d$primeType = factor(d$primeType)
levels(d$primeType) = c("Some", "Number", "Ad-hoc")

d$primeStrength = factor(d$primeStrength)
levels(d$primeStrength) <- c("Weak","Strong","Alternative","Baseline")

# EXHAUSTIVE ALTERNATIVE

summarystats <- d %>%
  filter(btwncondition != "alternative") %>%
  filter(primeStrength == "Alternative" | primeStrength == "Baseline") %>%
  group_by(workerid, primeStrength, primeType) %>%
  summarize(n = n (), nPragmatic = sum(responseChoice == 1)) %>%
  filter(n == 4) %>%
  ungroup() %>%
  # group_by(workerid) %>%
  spread(key = primeStrength, value = nPragmatic) %>%
  drop_na(Alternative, Baseline) %>%
  mutate(switch = Alternative - Baseline) %>%
  summarize(morePrag = sum(switch > 0) / n(), lessPrag = sum(switch < 0) / n(), noChange = sum(switch == 0) / n())
# gather(key = "behavior", value = "count") 

toplot <- d %>%
  filter(btwncondition != "alternative") %>%
  filter(primeStrength == "Alternative" | primeStrength == "Baseline") %>%
  group_by(workerid, primeStrength,primeType) %>%
  summarize(n = n (), nPragmatic = sum(responseChoice == 1)) %>%
  filter(n == 4) %>%
  ungroup() %>%
  group_by(workerid)  %>%
  spread(key = primeStrength, value = nPragmatic) %>%
  drop_na(Alternative, Baseline) %>%
  mutate(switch = Alternative - Baseline) %>%
  group_by(primeType) %>%
  summarize(morePrag = sum(switch > 0) / n(), lessPrag = sum(switch < 0) / n(), noChange = sum(switch == 0) / n())%>%
  gather(key = "behavior", value = "count", -primeType) 

toplot$behavior <- factor(toplot$behavior)
levels(toplot$behavior) <- c("Less pragmatic",
                             "More pragmatic",
                             "No change")

base = 6
expand = 3

toplot %>%
  ggplot(aes(x = primeType, y = count, fill = behavior)) +
  geom_bar(stat="identity",position = "dodge") +
  scale_fill_viridis(discrete = TRUE) + 
  xlab(element_blank()) +
  theme_bw() +
  labs(y = "Number of participants", fill = "Change in behavior", x = "Expression") +
  theme(text = element_text(size = base * expand / 2, face = "bold")) +
  ggtitle("Exhaustive alternative priming")


#### STRONG

summarystats2 <- d %>%
  # filter(btwncondition != "alternative") %>%
  filter(primeStrength == "Strong" | primeStrength == "Baseline") %>%
  group_by(workerid, primeStrength, primeType) %>%
  summarize(n = n (), nPragmatic = sum(responseChoice == 1)) %>%
  filter(n == 4) %>%
  ungroup() %>%
  # group_by(workerid) %>%
  spread(key = primeStrength, value = nPragmatic) %>%
  drop_na(Strong, Baseline) %>%
  mutate(switch = Strong - Baseline) %>%
  summarize(morePrag = sum(switch > 0) / n(), lessPrag = sum(switch < 0) / n(), noChange = sum(switch == 0) / n())
# gather(key = "behavior", value = "count") 


toplot2 <- d %>%
  # filter(btwncondition != "alternative") %>%
  filter(primeStrength == "Strong" | primeStrength == "Baseline") %>%
  group_by(workerid, primeStrength,primeType) %>%
  summarize(n = n (), nPragmatic = sum(responseChoice == 1)) %>%
  filter(n == 4) %>%
  ungroup() %>%
  group_by(workerid)  %>%
  spread(key = primeStrength, value = nPragmatic) %>%
  drop_na(Strong, Baseline) %>%
  mutate(switch = Strong - Baseline) %>%
  group_by(primeType) %>%
  summarize(morePrag = sum(switch > 0) / n(), lessPrag = sum(switch < 0) / n(), noChange = sum(switch == 0) / n())%>%
  gather(key = "behavior", value = "count", -primeType)

toplot2$behavior <- factor(toplot2$behavior)
levels(toplot2$behavior) <- c("Less pragmatic",
                              "More pragmatic",
                              "No change")

base = 6
expand = 3

toplot2 %>%
  ggplot(aes(x = primeType, y = count, fill = behavior)) +
  geom_bar(stat="identity",position = "dodge") +
  scale_fill_viridis(discrete = TRUE) + 
  xlab(element_blank()) +
  theme_bw() +
  labs(y = "Number of participants", fill = "Change in behavior", x = "Expression") +
  ggtitle("strong") +
  theme(text = element_text(size = base * expand / 2, face = "bold")) 

### WEAK

summarystats3 <- d %>%
  # filter(btwncondition != "alternative") %>%
  filter(primeStrength == "Weak" | primeStrength == "Baseline") %>%
  group_by(workerid, primeStrength, primeType) %>%
  summarize(n = n (), nPragmatic = sum(responseChoice == 1)) %>%
  filter(n == 4) %>%
  ungroup() %>%
  # group_by(workerid) %>%
  spread(key = primeStrength, value = nPragmatic) %>%
  drop_na(Weak, Baseline) %>%
  mutate(switch = Weak - Baseline) %>%
  summarize(morePrag = sum(switch > 0) / n(), lessPrag = sum(switch < 0) / n(), noChange = sum(switch == 0) / n())
# gather(key = "behavior", value = "count") 

toplot3 <- d %>%
  # filter(btwncondition != "alternative") %>%
  filter(primeStrength == "Weak" | primeStrength == "Baseline") %>%
  group_by(workerid, primeStrength,primeType) %>%
  summarize(n = n (), nPragmatic = sum(responseChoice == 1)) %>%
  filter(n == 4) %>%
  ungroup() %>%
  group_by(workerid)  %>%
  spread(key = primeStrength, value = nPragmatic) %>%
  drop_na(Weak, Baseline) %>%
  mutate(switch = Weak - Baseline) %>%
  group_by(primeType) %>%
  summarize(morePrag = sum(switch > 0) / n(), lessPrag = sum(switch < 0) / n(), noChange = sum(switch == 0) / n())%>%
  gather(key = "behavior", value = "count", -primeType) 

toplot3$behavior <- factor(toplot3$behavior)
levels(toplot3$behavior) <- c("Less pragmatic",
                              "More pragmatic",
                              "No change")

base = 6
expand = 3

toplot3 %>%
  ggplot(aes(x = primeType, y = count, fill = behavior)) +
  geom_bar(stat="identity",position = "dodge") +
  scale_fill_viridis(discrete = TRUE) + 
  xlab(element_blank()) +
  theme_bw() +
  labs(y = "Number of participants", fill = "Change in behavior", x = "Expression") +
  theme(text = element_text(size = base * expand / 2, face = "bold")) +
  ggtitle("weak")

## Errata

d_numbet <- d %>%
  filter(btwncondition != "alternative") %>%
  filter(primeStrength == 2 | primeStrength == 3) %>%
  group_by(workerid, primeStrength,primeType) %>%
  summarize(n = sum(responseChoice == 1))

base = 6
expand = 3

d_numbet$primeType <- factor(d_numbet$primeType)
levels(d_numbet$primeType) <- c("Some", "Number", "Ad-hoc")
d_numbet$primeStrength <- factor(d_numbet$primeStrength)
levels(d_numbet$primeStrength) <- c("Alternative","Baseline")

ggplot(d_numbet, aes (n, fill = primeStrength)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~primeType*primeStrength, ncol = 2) +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE) +
  theme(text = element_text(size = base * expand / 2, face = "bold")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none") +
  xlab('Number of "Better Picture?" selections') +
  ylab("Number of participants")

ggsave(paste("byworker_exh.pdf",sep=""), width = 4, height = 5, units = "in", dpi = 1000)

# 3s and 4s are pragmatic 
# 0s and 1s are literal

prod_graphs_bysubject = function(condname) {
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
  
  subset$workerid <- factor(subset$workerid)
  
  # MEANS AND CONFINTS BY EXPRESSION + CONDITION
  
  byConditionExpression <- subset %>%
    group_by(workerid) %>%
    summarize(Mean = mean(responseChoice), 
              CILow =ci.low(responseChoice),
              CIHigh =ci.high(responseChoice), n = n()) %>%
    ungroup() %>%
    arrange(desc(Mean)) %>%
    mutate(YMin = Mean - CILow,
           YMax = Mean + CIHigh)
  
  # VISUALIZE - CONDITION x EXPRESSION
  
  base = 6
  expand = 3
  
  plot <- ggplot(byConditionExpression, aes(x = reorder(workerid, -Mean), y=Mean)) +
    geom_bar(stat="identity",position = "dodge") +
    # facet_wrap(~workerid) +
    theme_bw() +
    scale_fill_viridis(discrete = TRUE) +
    theme(text = element_text(size = base * expand / 2, face = "bold")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge,color = "red") +  
    labs(fill = "Prime type") +
    xlab(element_blank()) +
    ylim(0,1) +
    ggtitle(condname)
  
  if(!(condname == "Canonical")) {
    plot <- plot + labs(y = NULL)
  } else {
    plot <- plot + labs(y = "Proportion of\n'Better Picture?' chosen")
  }
  
  return(plot)
  
}

prod_graphs_bysubject("Canonical")
prod_graphs_bysubject("Exhaustive\n-only")
prod_graphs_bysubject("Exhaustive\n-clausal")
prod_graphs_bysubject("Exhaustive\n-subclausal")

prod_graphs_bySubjectDiff = function(condname) {
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
  
  subset$workerid <- factor(subset$workerid)
  
  # MEANS AND CONFINTS BY EXPRESSION + CONDITION
  
  byConditionExpression <- subset %>%
    filter(primeStrength %in% c("Strong","Baseline")) %>%
    group_by(primeStrength,workerid) %>%
    summarize(Mean = mean(responseChoice)) %>%
    # CILow =ci.low(responseChoice),
    # CIHigh =ci.high(responseChoice), n = n()) %>%
    ungroup() %>%
    arrange(desc(Mean)) %>%
    # mutate(YMin = Mean - CILow,
    #        YMax = Mean + CIHigh) %>%
    spread(primeStrength, Mean) %>%
    mutate(diff = Baseline-Strong)
  
  # VISUALIZE - CONDITION x EXPRESSION
  
  base = 6
  expand = 3
  
  plot <- ggplot(byConditionExpression, aes(x = reorder(workerid, -diff), y=diff)) +
    geom_bar(stat="identity",position = "dodge") +
    # facet_wrap(~primeStrength) +
    theme_bw() +
    scale_fill_viridis(discrete = TRUE) +
    theme(text = element_text(size = base * expand / 2, face = "bold")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    # geom_errorbar(aes(ymin=YMin,ymax=YMax),size = 0.25,width= 0.025,position = dodge,color = "red") +  
    labs(fill = "Prime type") +
    xlab(element_blank()) +
    ylim(-1,1) +
    ggtitle(condname)
  
  if(!(condname == "Canonical")) {
    plot <- plot + labs(y = NULL)
  } else {
    plot <- plot + labs(y = "Proportion of\n'Better Picture?' chosen")
  }
  
  return(plot)
  
}

prod_graphs_bySubjectDiff("Canonical")
prod_graphs_bySubjectDiff("Exhaustive\n-subclausal")
prod_graphs_bySubjectDiff("Exhaustive\n-only")
prod_graphs_bySubjectDiff("Exhaustive\n-clausal")