require(dplyr)


order_md_score <- function(dataadjusted, ninput, noutput, xi, yi, M = 25, B = 30){
  effinput <- 0
  effoutput <- 0
  effworst <- 0
  effbest <- 0
  xik <- 0
  yik <- 0
  for (j in 1:B){
    
    MSample = sample_n(dataadjusted, size = M, replace = TRUE)
    
    #INPUT
    for (k in 1:ninput){
      if (k == 1){
        xik <- MSample[,k]/xi[k]
      }else{
        xik <- cbind(xik, MSample[,k]/xi[k])
      }
    }
    xik <- as.data.frame(xik)
    xik$bestxik <- apply(xik[1:ninput], MARGIN = 1, FUN = max, na.rm = TRUE)
    
    #OUTPUT
    for (k in 1:noutput){
      if (k == 1){
        yik <- yi[k]/MSample[,(ninput+k)]
      }else{
        yik <- cbind(yik, yi[k]/MSample[,(ninput+k)])
      }
    }
    yik <- as.data.frame(yik)
    yik$bestyik <- apply(yik[1:noutput], MARGIN = 1, FUN = max, na.rm = TRUE)
    
    scores <- data.frame(xik,yik)
    
    effoutput[j] <- min(yik$best)
    effinput[j] <- min(xik$best)
    effworst[j] <- min(min(yik$best),min(xik$best))
    effbest[j] <- max(min(yik$best),min(xik$best))
  }
  
  
  score_output <- mean(effoutput)
  score_input <- mean(effinput)
  score_worst <- mean(effworst)
  score_best <- mean(effbest)
  
  return <- data.frame(score_input,score_output,score_worst,score_best)
}
order_md_conditional_score <- function(dataadjusted, ninput, noutput, xi, yi, M = 25, B = 30, probs){
  effinput <- 0
  effoutput <- 0
  effworst <- 0
  effbest <- 0
  xik <- 0
  yik <- 0
  for (j in 1:B){
    
    MSample = sample_n(dataadjusted, size = M, replace = TRUE, weight = as.matrix(probs))
    sample_n
    #INPUT
    for (k in 1:ninput){
      if (k == 1){
        xik <- MSample[,k]/xi[k]
      }else{
        xik <- cbind(xik, MSample[,k]/xi[k])
      }
    }
    xik <- as.data.frame(xik)
    xik$bestxik <- apply(xik[1:ninput], MARGIN = 1, FUN = max, na.rm = TRUE)
    
    #OUTPUT
    for (k in 1:noutput){
      if (k == 1){
        yik <- yi[k]/MSample[,(ninput+k)]
      }else{
        yik <- cbind(yik, yi[k]/MSample[,(ninput+k)])
      }
    }
    yik <- as.data.frame(yik)
    yik$bestyik <- apply(yik[1:noutput], MARGIN = 1, FUN = max, na.rm = TRUE)
    
    scores <- data.frame(xik,yik)
    
    effoutput[j] <- min(yik$best)
    effinput[j] <- min(xik$best)
    effworst[j] <- min(min(yik$best),min(xik$best))
    effbest[j] <- max(min(yik$best),min(xik$best))
  }
  
  
  conditional_score_output <- mean(effoutput)
  conditional_score_input <- mean(effinput)
  conditional_score_worst <- mean(effworst)
  conditional_score_best <- mean(effbest)
  
  return <- data.frame(conditional_score_input,conditional_score_output,conditional_score_worst,conditional_score_best)
}
Quadrant_Split <- function(INPUT = INPUT, OUTPUT = OUTPUT, i = i){
  
  ninput <- ncol(INPUT)
  noutput <- ncol(OUTPUT)
  
  xi <- INPUT[i,]
  yi <- OUTPUT[i,]
  
  allygreater <- 1
  for(j in 1:noutput){
    allygreater <- ifelse(yi[j] <= OUTPUT[,j], allygreater, 0)
  }
  
  allylesser <- 1
  for(j in 1:noutput){
    allylesser <- ifelse(yi[j] >= OUTPUT[,j], allylesser, 0)
  }
  
  allxgreater <- 1
  for(j in 1:noutput){
    allxgreater <- ifelse(xi[j] <= INPUT[,j], allxgreater, 0)
  }
  
  allxlesser <- 1
  for(j in 1:noutput){
    allxlesser <- ifelse(xi[j] >= INPUT[,j], allxlesser, 0)
  }
  
  allygreater <- data.frame(allygreater)
  allylesser <- data.frame(allylesser)
  allxgreater <- data.frame(allxgreater)
  allxlesser <- data.frame(allxlesser)
  
  returns <- cbind(allygreater, allylesser, allxgreater, allxlesser)
}
order_md <- function(ID, INPUT, OUTPUT, CONDITIONAL = NULL, DISCRETE = NULL, M = 25, B = 30, hlevel = .3, LOG = FALSE, onlyscores = TRUE){
  #This function comes from the work primarily put forth by Simar, Wilson, and Darario. 
  #For a deeper look into the order-m and conditional order-m functions, read "Conditional nonparametric frontier models for convex and
  #nonconvex technologies: a unifying approach" by Cinzia Daraio and Leopold Simar (2007)
  
  h <- 0; compareable <- 0
  
  N <- nrow(ID)
  
  ninput <- ncol(INPUT)
  noutput <- ncol(OUTPUT)
  
  data <- cbind(INPUT, OUTPUT)
  
  if(is.null(CONDITIONAL) == FALSE){
    nconditional <- ncol(CONDITIONAL)
    data <- cbind(data, CONDITIONAL)
  }
  if(is.null(DISCRETE) == FALSE){
    ndiscrete <- ncol(DISCRETE)
    data <- cbind(data, DISCRETE)
  }
  
  for (i in 1:N){
    xi <- INPUT[i,]
    yi <- OUTPUT[i,]
    
    SPLIT <- Quadrant_Split(INPUT = INPUT, OUTPUT = OUTPUT, i = i)
    datatemp <- as.data.frame(cbind(data,SPLIT))
    
    datatemp$probs <- 1
    
    
    
    if(is.null(CONDITIONAL) == FALSE){
      zi <- CONDITIONAL[i,]
      probconditional <- probconditional(CONDITIONAL, zi, hlevel=.3, LOG = FALSE)
      datatemp$probs <- datatemp$probs*probconditional
    }
    
    if(is.null(DISCRETE) == FALSE){
      zidiscrete <- DISCRETE[i,]
      probdiscrete <- probdiscrete(DISCRETE, zidiscrete)
      datatemp$probs <- datatemp$probs*probdiscrete
    }
    
    datatemp2 <- datatemp[-i,]
    dataadjusted1 <- subset(datatemp2, allygreater == 1 & allxlesser == 1)
    dataadjusted2 <- subset(datatemp2, allylesser == 1 & allxgreater == 1)
    dataadjusted <- rbind(datatemp[i,],dataadjusted1,dataadjusted2)
    
    return_test <- order_md_score(dataadjusted,ninput,noutput,xi,yi, M, B)
    
    if(is.null(CONDITIONAL) == FALSE || is.null(DISCRETE) == FALSE){
      probability <- dataadjusted$probs
      return_test_2 <- order_md_conditional_score(dataadjusted, ninput, noutput, xi, yi, M, B, probs = probability)
      return_test <- cbind(return_test,return_test_2)
    }
    
    if(i == 1){
      scores <- return_test
    }else{
      scores <- rbind(scores, return_test)
    }
  }
  if(onlyscores == TRUE){
    return <- scores
  }else{
    return <- cbind(ID, data, scores)
  }
  return <- return
}
probconditional <- function(Z, zi, hlevel=.3, LOG = FALSE){
  zi <- Z[i,]
  nconditional <- ncol(as.data.frame(Z))
  
  if(LOG == TRUE){
    for(j in 1:nconditional){
      if(j==1){
        difference <- Z[,j]-zi[j]
        h <- hlevel
        probs <- (1/sqrt(2*pi))*exp(-(abs(difference/h)/2))
      }else{
        difference <- Z[,j]-zi[j]
        h <- hlevel
        probs <- probs*(1/sqrt(2*pi))*exp(-(abs(difference/h)/2))
      }
    }
  }else{
    for(j in 1:nconditional){
      if(j==1){
        difference <- Z[,j]-zi[j]
        h <- zi[j]*hlevel
        probs <- (1/sqrt(2*pi))*exp(-(abs(difference/h)/2))
      }else{
        difference <- Z[,j]-zi[j]
        h <- zi[j]*hlevel
        probs <- probs*(1/sqrt(2*pi))*exp(-(abs(difference/h)/2))
      }
    }
  }
  returns <- as.data.frame(probs)
}
probdiscrete <- function(Zdiscrete, zidiscrete){
  zidiscrete <- Zdiscrete[i,]
  nconditionaldiscrete <- ncol(as.data.frame(Zdiscrete))
  for(j in 1:nconditionaldiscrete){
    if(j==1){
      probs <- ifelse(Zdiscrete[,j] == zidiscrete[j], 1, 0)
    }else{
      probs <- ifelse(Zdiscrete[,j] == zidiscrete[j], probs, 0)
    }
  }
  returns <- as.data.frame(probs)
}
