require(dplyr)

order_md <- function(INPUT, OUTPUT, CONDITIONAL = NULL, DISCRETE = NULL, M = 25, B = 100, hlevel = .1, LOG = FALSE, ConditionalControl = FALSE){
  #This function comes from the work primarily put forth by Simar, Wilson, and Darario. 
  #For a deeper look into the order-m and conditional order-m functions, read "Conditional nonparametric frontier models for convex and
  #nonconvex technologies: a unifying approach" by Cinzia Daraio and Leopold Simar (2007)
  if(is.null(DISCRETE) == TRUE & is.null(CONDITIONAL) == TRUE){
    probdone = FALSE
  }else{probdone = TRUE}
  
  if(is.null(DISCRETE) == FALSE){
    DISCRETE <- data.frame(DISCRETE)
    DISCRETE <- as.data.frame(lapply(DISCRETE, as.factor))
    DISCRETE <- data.frame(lapply(DISCRETE, as.numeric))
    DISCRETE <- data.matrix(DISCRETE)
  }
  
  N <- nrow(INPUT)
  for (i in 1:N){
    start_time <- Sys.time()
    datatemp <- Quadrant_Split(INPUT = INPUT, OUTPUT = OUTPUT, i = i)
    datatemp$probs <- 1
    if(is.null(CONDITIONAL) == FALSE){
      datatemp$probs <- probconditional(CONDITIONAL, i = i, hlevel = hlevel, LOG = LOG, ConditionalControl = ConditionalControl)*datatemp$probs
    }
    
    if(is.null(DISCRETE) == FALSE){
      datatemp$probs <- probdiscrete(DISCRETE, i=i)*datatemp$probs
    }
    
    dataadjusted <- datatemp %>% filter((xsmaller == TRUE & ylarger == TRUE)|(xlarger == TRUE & ysmaller == TRUE))
    return_test <- order_md_score(dataadjusted, M = M, B = B, probdone = probdone)
    
    if(i == 1){
      scores <- return_test
    }else{
      scores <- rbind(scores, return_test)
    }
    end_time <- Sys.time()
    print(end_time - start_time)
    print(i)
  }
  return <- data.frame(scores)
}
order_m_input <- function(INPUT, OUTPUT, CONDITIONAL = NULL, DISCRETE = NULL, M = 25, B = 100, hlevel = .1, LOG = FALSE, ConditionalControl = FALSE){
  #This function comes from the work primarily put forth by Simar, Wilson, and Darario. 
  #For a deeper look into the order-m and conditional order-m functions, read "Conditional nonparametric frontier models for convex and
  #nonconvex technologies: a unifying approach" by Cinzia Daraio and Leopold Simar (2007)
  if(is.null(DISCRETE) == TRUE & is.null(CONDITIONAL) == TRUE){
    probdone = FALSE
  }else{probdone = TRUE}
  
  if(is.null(DISCRETE) == FALSE){
    DISCRETE <- data.frame(DISCRETE)
    DISCRETE <- as.data.frame(lapply(DISCRETE, as.factor))
    DISCRETE <- data.frame(lapply(DISCRETE, as.numeric))
    DISCRETE <- data.matrix(DISCRETE)
  }
  
  N <- nrow(INPUT)
  for (i in 1:N){
    start_time <- Sys.time()
    datatemp <- Quadrant_Split(INPUT = INPUT, OUTPUT = OUTPUT, i = i)
    datatemp$probs <- 1
    if(is.null(CONDITIONAL) == FALSE){
      datatemp$probs <- probconditional(CONDITIONAL, i = i, hlevel = hlevel, LOG = LOG, ConditionalControl = ConditionalControl)*datatemp$probs
    }
    
    if(is.null(DISCRETE) == FALSE){
      datatemp$probs <- probdiscrete(DISCRETE, i=i)*datatemp$probs
    }
    
    dataadjusted <- datatemp %>% filter(ylarger == TRUE)
    return_test <- order_md_score(dataadjusted, M = M, B = B, probdone = probdone)
    
    if(i == 1){
      scores <- return_test
    }else{
      scores <- rbind(scores, return_test)
    }
    end_time <- Sys.time()
    print(end_time - start_time)
    print(i)
  }
  return <- data.frame(scores)
}
probconditional <- function(CONDITIONAL = CONDITIONAL, i = i, hlevel=.1, LOG = FALSE, ConditionalControl = FALSE){
  Differences <- data.frame(sweep(CONDITIONAL, MARGIN = 2, STATS = CONDITIONAL[i,], '-'))
  if (LOG == TRUE){
    h <- hlevel
    Differences <- Differences/h
  }else{
    Differences <- data.frame(sweep(Differences, MARGIN = 2, STATS = CONDITIONAL[i,]*hlevel, '/'))
  }
  probs <- exp(-(abs(Differences)/2))
  probs <- apply(probs,1, prod)
  if(ConditionalControl == TRUE){
    probs <- probs^(1/ncol(CONDITIONAL))
  }
  returns <- as.data.frame(probs)
}
Quadrant_Split <- function(INPUT = INPUT, OUTPUT = OUTPUT, i = i){
  INPUTnew <- data.frame(sweep(INPUT, MARGIN = 2, STATS = INPUT[i,], '/'))
  xik <- apply(INPUTnew, 1, max)
  xlarger <- apply(INPUTnew >= 1, 1, all)
  xsmaller <- apply(INPUTnew <= 1, 1, all)
  
  OUTPUTnew <- data.frame(sweep(OUTPUT, MARGIN = 2, STATS = OUTPUT[i,], '/'))
  yik <- 1/apply(OUTPUTnew, 1, min)
  ylarger <- apply(OUTPUTnew >= 1, 1, all)
  ysmaller <- apply(OUTPUTnew <= 1, 1, all)
  returns <- data.frame(xik,xlarger,xsmaller,yik,ylarger,ysmaller)
}
probdiscrete <- function(DISCRETE = DISCRETE, i = i){
  Differences <- sweep(DISCRETE, MARGIN = 2, STATS = DISCRETE[i,], '-')
  probs <- as.numeric(apply(Differences == 0, 1, all))
  returns <- as.data.frame(probs)
}
order_md_score <- function(dataadjusted, M = 25, B = 30, probdone = FALSE){
  effinput <- 0
  effoutput <- 0
  effworst <- 0
  effbest <- 0
  for (j in 1:B){
    if(probdone == FALSE){
      MSample = slice_sample(dataadjusted, n = M, replace = TRUE)
    }else{
      MSample = slice_sample(dataadjusted, n = M, weight_by = as.matrix(dataadjusted$probs), replace = TRUE)
    }
    effinput[j] <- min(MSample$xik)
    effoutput[j] <- min(MSample$yik)
    effworst[j] <- min(min(MSample$yik),min(MSample$xik))
    effbest[j] <- max(min(MSample$yik),min(MSample$xik))
  }
  score_input <- mean(effinput)
  sd_input <- sd(effinput)
  score_output <- mean(effoutput)
  sd_output <- sd(effoutput)
  score_best <- mean(effworst)
  sd_best <- sd(effworst)
  score_worst <- mean(effbest)
  sd_worst <- sd(effbest)
  return <- cbind(score_input, sd_input, score_output, sd_output, score_best, sd_best, score_worst, sd_worst)
}
