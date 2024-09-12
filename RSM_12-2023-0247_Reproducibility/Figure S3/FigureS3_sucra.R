### Last updated on 09-11-2024
### Figure S3

setwd("~/Dropbox/Bingyu_PennCIL/00_Bingyu/Projects/CL-NMA/production/Figure S3")


######################## Code to reproduce Figure S3 ############################
library(Matrix)
library(plyr)
library(dplyr)
library(netmeta)
library(mvtnorm)
library(reshape)
library(ggplot2)
library(scales)
library(showtext)

source("CLNMA.equal.tau.R")
source("get_table_IOP.R")
source("get_table_meta.R")

### Add a font that supports Greek characters (e.g., Arial)
font_add(family = "Arial", regular = "path/to/Arial.ttf")
showtext_auto()  # Enable showtext to handle text rendering


### IOP
Nsim = 50000
set.seed(1)

treatment = c("Bimatoprost",
              "Travoprost",
              "Latanoprost",
              "Levobunolol",
              "Tafluprost",
              "Timolol",
              "Carteolol",
              "Brinzolamide",
              "Brimonidine",
              "Levobetaxolol",
              "Dorzolamide",
              "Betaxolol",
              "Apraclonidine",
              "Unoprostone",
              "Placebo/Vehicle/No treatment")

newlist = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
Treatment = treatment[newlist]

cbp2 <- c( "#D55E00", "#CC79A7","#999999", "#0072B2",
           "#F0E442", "#56B4E9", "#E69F00", "#009E73",
           "#000099", "#CC0000", "#CC6666", "#9999CC",
           "#66CC99", "#FDDBC7")

###########################################################
data_IOP <- read.csv("IOP_n125.csv")
dataout <- data_readin_IOP(data_IOP)
net <- netmeta(md, semd, tt1, tt2, studyid, data=data_IOP)
order_re <- order(netrank(net)$Pscore.random, decreasing = TRUE)
narm <- length(order_re)
data.final = reform.fun(dataout, 15, narm, order_re)
res = CLNMA.equal.tau(data.final, narm)
mu = res$mu
V = res$V

m = mu[newlist]
Varm = V[newlist, newlist]
y = rmvnorm(Nsim,m,Varm)
R3 = apply(y,1,function(x){order(x,decreasing = F)})
get.count = function(x){
  ct = rep(0,14)
  t = table(x)
  ct[as.numeric(rownames(t))] = t
  return(ct)
}
C3 = apply(R3,1,get.count)

C3 = data.frame(C3)
colnames(C3) = as.character(1:14)


new_order <- c()
datm <- melt(cbind(C3, ind = rownames(C3)), id.vars = c('ind'))
colnames(datm) = c("Treatments", "rank","Counts")
datm$Treatments = as.character(datm$Treatments)
l1 = Treatment
datm$Treatments =mapvalues(datm$Treatments, from =1:14, to=l1)
datm$Treatments = factor(datm$Treatments, levels = Treatment)
datm$Probability = datm$Counts/50000

write.csv(datm, "IOP_sucra_counts.csv")

datm2 <- data.frame(datm[,1:2],datm[,3:4])
colnames(datm2) = c("Treatments","rank","Standard NMA method", "Proposed method")
datm3 <- melt(datm2, id.vars = c("Treatments","rank"))
colnames(datm3) <- c("Treatments","rank","Method","Probability")

p <- ggplot(datm,aes(x = rank, y = Probability,fill = Treatments)) +
  geom_bar(position = "fill",stat = "identity",color='black') +
  xlab("Ranks")+
  scale_fill_manual("Treatments", values =  cbp2) +
  scale_y_continuous(labels = percent_format())+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme_classic(base_size = 15)

ggsave(p, file = "IOP_sucra_barplot.pdf", width=10, height=12)


### CP/CPPS
Nsim = 50000
set.seed(1)

treatment = c('Placebo','α-Blockers',  'Antibiotics',
              'α-Blockers + Antibiotics',
              'Anti-inflammatory')

newlist = c(3,2,4,5)
Treatment = treatment[newlist]

cbp22 <- c("#D55E00", "#CC79A7","#999999", "#0072B2")

data_479622 <- read.csv("CP_CPPS_raw.csv")
net <- netmeta(TE,seTE,treat1,treat2,studlab, data=data_479622)
dataout <- data_readin_meta(data_479622, iv=FALSE)
order_re <- order(netrank(net)$Pscore.random, decreasing = TRUE)
narm <- length(order_re)
data.final = reform.fun(dataout, 5, narm, order_re)
res = CLNMA.equal.tau(data.final, narm)
mu = res$mu
V = res$V

m = mu[1:4]
Varm = V[1:4, 1:4]
y = rmvnorm(Nsim,m,Varm)
R3 = apply(y,1,function(x){order(x,decreasing = F)})
get.count = function(x){
  ct = rep(0,4)
  t = table(x)
  ct[as.numeric(rownames(t))] = t
  return(ct)
}
C3 = apply(R3,1,get.count)

C3 = data.frame(C3)
colnames(C3) = as.character(1:4)

new_order <- c()
datm22 <- melt(cbind(C3, ind = rownames(C3)), id.vars = c('ind'))
colnames(datm22) = c("Treatments", "rank","Counts")
datm22$Treatments = as.character(datm22$Treatments)
l1 = Treatment
datm22$Treatments =mapvalues(datm22$Treatments, from =1:4, to=l1)
datm22$Treatments = factor(datm22$Treatments, levels = Treatment)
datm22$Probability = datm22$Counts/Nsim

write.csv(datm22, "CP_CPPS_sucra_counts.csv")

p_small <- ggplot(datm22,aes(x = rank, y = Probability,fill = Treatments)) +
  geom_bar(position = "fill",stat = "identity",color='black') +
  xlab("Ranks")+
  scale_fill_manual("Treatments", values =  cbp22) +
  scale_y_continuous(labels = percent_format())+
  # ggtitle("Sucra Treatment Ranking")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold", family ="Arial"))+
  theme_classic(base_size = 15)

ggsave(p_small, file = "CP_CPPS_sucra_barplot.pdf", width=6, height=8)



