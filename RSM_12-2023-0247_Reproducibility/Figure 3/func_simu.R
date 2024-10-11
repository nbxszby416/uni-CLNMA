
simu = function(nab,nac,nbc,nabc,mu,betweenv,rho_w,ss1){
  dataout = gendata(nab,nac,nbc,nabc,mu,betweenv,rho_w,ss1)
  out1 = tryCatch(CLNMA.equal.tau(dataout, narm=3),error=function(e) rep(NA,5))
  out2 = tryCatch(CLNMA.equal.tau(dataout, narm=3, correct='KC'),error=function(e) rep(NA,5))
  out3 = tryCatch(CLNMA.equal.tau(dataout, narm=3, correct='MD'),error=function(e) rep(NA,5))
  return(c(out1,out2,out3))
}

generate.data = function(mu,betweenv,rho_w,ss1){
  s1 =abs(rnorm(2,ss1,0.1))
  s = s1^2
  withinv = diag(s)
  for(i in 1:1){
    for(j in (i+1):2){
      withinv[i,j] = withinv[j,i]=rho_w[i,j]*sqrt(s[i]*s[j])
    }
  }
  y = mvrnorm(1,mu,withinv+betweenv)
  return(c(y,s1))
}

gendata = function(nab,nac,nbc,nabc,mu,betweenv,rho_w,ss1){
  n = nab+nac+nbc+nabc
  data = replicate(n,generate.data(mu,betweenv,rho_w,ss1))
  
  data = data.frame(t(data))
  colnames(data) = c("Y_BA_1","Y_CA_1","S_BA_1","S_CA_1")
  
  data$Y_BC_1 = data$Y_BA_1-data$Y_CA_1
  data$S_BC_1 = sqrt(data$S_BA_1^2+data$S_CA_1^2-2*rho_w[1,2]*data$S_CA_1*data$S_BA_1)
  
  #BAarm
  dataBA = data.frame(ID = 1:nab, t1 = rep(1,nab),t2 = rep(3,nab))
  dataBA$outcome1 = data[1:nab,"Y_BA_1"]
  dataBA$sd1 = data[1:nab,"S_BA_1"]
  
  #CAarm
  dataCA = data.frame(ID = nab+1:nac, t1 = rep(2,nac),t2 = rep(3,nac))
  dataCA$outcome1 = data[nab+1:nac,"Y_CA_1"]
  dataCA$sd1 = data[nab+1:nac,"S_CA_1"]
  
  #BCarm
  dataBC = data.frame(ID = nab+nac+1:nbc, t1 = rep(1,nbc),t2 = rep(2,nbc))
  dataBC$outcome1 = data[nab+nac+1:nbc,"Y_BC_1"]
  dataBC$sd1 = data[nab+nac+1:nbc,"S_BC_1"]
  
  #ABCarm
  dataABC1 = data.frame(ID = nab+nac+nbc+1:nabc, t1 = rep(1,nabc),t2 = rep(3,nabc))
  dataABC1$outcome1 = data[nab+nac+nbc+1:nabc,"Y_BA_1"]
  dataABC1$sd1 = data[nab+nac+nbc+1:nabc,"S_BA_1"]
  
  dataABC2 = data.frame(ID = nab+nac+nbc+1:nabc, t1 = rep(2,nabc),t2 = rep(3,nabc))
  dataABC2$outcome1 = data[nab+nac+nbc+1:nabc,"Y_CA_1"]
  dataABC2$sd1 = data[nab+nac+nbc+1:nabc,"S_CA_1"]
  
  dataABC3 = data.frame(ID = nab+nac+nbc+1:nabc, t1 = rep(1,nabc),t2 = rep(2,nabc))
  dataABC3$outcome1 = data[nab+nac+nbc+1:nabc,"Y_BC_1"]
  dataABC3$sd1 = data[nab+nac+nbc+1:nabc,"S_BC_1"]
  
  dataout = rbind(dataBA,dataCA,dataBC,dataABC1,dataABC2,dataABC3)
  colnames(dataout) = c("ID", "t1", "t2", "outcome", "sd")
  return(dataout)
}


