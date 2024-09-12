
### data readin transfer
data_readin_IOP <- function(data_raw){
  dataout = data_raw[,c("studyid", "tt1", "tt2", "md", "semd")]
  colnames(dataout) = c("ID", "t1", "t2", "TE", "seTE")
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