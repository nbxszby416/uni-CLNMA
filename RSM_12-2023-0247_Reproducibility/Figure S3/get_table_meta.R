
get_table_meta <- function(data, net, iv, correct='NONE'){
  ### treatment order by package netmeta
  order_re <- order(netrank(net)$Pscore.random, decreasing = TRUE)
  narm <- length(order_re)
  ## get number of study
  if (iv){
    s_num <- length(unique(data$id))
  }else{
    s_num <- length(unique(data$studlab))
  }
  ## Exclude studies with narm > 20
  if (narm > 20){
    TE_prop = NA
    lower_prop = NA
    upper_prop = NA
    tau = NA
    
    prop <- list(TE_prop = TE_prop, lower_prop = lower_prop, upper_prop = upper_prop,
                 t_num = narm, s_num = s_num, tau = tau)
    ##
    class(prop) <- "propClass"
    
    return(prop)
  }
  
  ## get useful columns of original data
  dataout <- data_readin_meta(data, iv)
  
  ## Output table
  for (mm in 1:narm){
    start <- Sys.time()
    data.final = reform.fun(dataout, mm, narm, order_re)
    res = CLNMA.equal.tau(data.final, narm, correct)
    
    treatment = shift(order_re,mm)
    
    treatment_label = (cbind(1:narm, treatment))
    colnames(treatment_label)[1] = "T"
    
    result = data.frame(cbind(treatment[-narm],res$mu, res$diagV))
    names(result)=c("treatment","mu","diagV")
    result$LB = as.numeric(result$mu) - 1.96*sqrt(as.numeric(result$diagV))
    result$UB = as.numeric(result$mu) + 1.96*sqrt(as.numeric(result$diagV))
    
    if (mm == 1){
      TE_prop_temp = c(0,result$mu)
      TE_prop = TE_prop_temp
      lower_prop_temp = c(0,result$LB)
      lower_prop = lower_prop_temp
      upper_prop_temp = c(0,result$UB)
      upper_prop = upper_prop_temp
    }else if (mm == narm){
      TE_prop_temp = c(result$mu,0)
      TE_prop = c(TE_prop, TE_prop_temp)
      lower_prop_temp = c(result$LB,0)
      lower_prop = c(lower_prop, lower_prop_temp)
      upper_prop_temp = c(result$UB,0)
      upper_prop = c(upper_prop, upper_prop_temp)
    }else{
      TE_prop_temp = c(result$mu[1:(mm-1)],0,result$mu[mm:(narm-1)])
      TE_prop = c(TE_prop, TE_prop_temp)
      lower_prop_temp = c(result$LB[1:(mm-1)],0,result$LB[mm:(narm-1)])
      lower_prop = c(lower_prop, lower_prop_temp)
      upper_prop_temp = c(result$UB[1:(mm-1)],0,result$UB[mm:(narm-1)])
      upper_prop = c(upper_prop, upper_prop_temp)
    }
    delta <- Sys.time() - start
    delta
    
  }
  TE_prop = matrix(TE_prop, nrow = narm, ncol = narm)
  lower_prop = matrix(lower_prop, nrow = narm, ncol = narm)
  upper_prop = matrix(upper_prop, nrow = narm, ncol = narm)
  
  prop <- list(TE_prop = TE_prop, lower_prop = lower_prop, upper_prop = upper_prop,
               t_num = narm, s_num = s_num, tau = res$tau)
  
  class(prop) <- "propClass"
  
  return(prop)
}

data_readin_meta <- function(data_raw, iv){
  if (iv) {
    dataout = data_raw[,c("id", "t1", "t2", "effect", "se")]
    colnames(dataout) = c("ID", "t1", "t2", "TE", "seTE")
  }
  else {
    dataout = data_raw[,c(1:5)]
    colnames(dataout) = c("ID", "t1", "t2", "TE", "seTE")
  }
  return(dataout)
}

### data reform
reform.fun <- function(data.temp, mm, narm, order_re){
  data.temp$t1 = mapvalues(data.temp$t1, from = c(1:narm), to = order(shift(order_re,mm)))
  data.temp$t2 = mapvalues(data.temp$t2, from = c(1:narm), to = order(shift(order_re,mm)))
  n = dim(data.temp)[1]
  for (i in 1:n){
    t1 = data.temp$t1[i]
    t2 = data.temp$t2[i]
    if (t1>t2){
      data.temp[i,c(2,3)] = data.temp[i,c(3,2)]
      data.temp[i,4]=(-1)*data.temp$TE[i]
    }
  }
  data.temp$outcome = data.temp$TE
  data.temp$sd = data.temp$seTE
  data.temp = data.temp[,c("ID", "t1","t2", "outcome", "sd")]
  data.temp = data.temp[complete.cases(data.temp),]
  return(data.temp)
}

### shift
shift <- function(seq, k){
  return(c(seq[-k],seq[k]))
}