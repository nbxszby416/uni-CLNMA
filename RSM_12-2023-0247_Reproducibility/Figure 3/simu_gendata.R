### Last updated on 10-10-2024
### Example of generating simulation data
### 3 arms

library(mvtnorm)
library(MASS)
library(Matrix)

source("func_simu.R")

set.seed(1)

mu = c(-2,-4) # mu_AB, mu_AC
tau = c(0.25,0.25) # tau_AB, tau_AC
rho = matrix(c(1,0.1,0.1,1),nrow = 2) # for between-study correlation

betweenv = diag(tau)
for(i in 1:1){
  for(j in (i+1):2){
    betweenv[i,j] =betweenv[j,i]= rho[i,j]*sqrt(tau[i]*tau[j])
  }
}
ss1 = 1
rho_w = matrix(c(1,0.2,0.2,1),nrow = 2) # for within-study correlation

nab = nac = nbc = nabc = 5

simu_data <- simu(nab,nac,nbc,nabc,mu,betweenv,rho_w,ss1)
save(simu_data, file="simu_gendata_n5.RData")

