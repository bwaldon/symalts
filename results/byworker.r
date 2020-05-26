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