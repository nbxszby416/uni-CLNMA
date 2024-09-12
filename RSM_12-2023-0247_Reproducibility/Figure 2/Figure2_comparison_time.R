### Last updated on 09-11-2024
### Figure 2

setwd("~/Dropbox/Bingyu_PennCIL/00_Bingyu/Projects/CL-NMA/production/Figure 2")

######################## Code to reproduce Figure 2 ############################
library(ggplot2)

## time data, input manually
t = c(rep('5 treatments',15), rep('10 treatments', 15), rep('15 treatments', 15))
N = c(rep(c(10,20,30,40,50), 3), rep(c(40,60,80,100,120), 3), rep(c(120,150,180,210,240), 3))
method = rep(c(rep('gemtc', 5), rep('netmeta', 5), rep('proposed', 5)), 3)

time = c(1.581, 2.399, 4.848, 5.617, 8.612,
         1.814, 2.157, 2.476, 2.884, 2.916,
         0.411, 0.385, 0.453, 0.561, 0.523,
         9.318, 17.481, 28.476, 49.044, 61.755,
         7.392, 11.721, 17.463, 40.682, 69.143,
         3.395, 4.166, 4.201, 5.177, 5.670,
         12.967, 38.263, 70.848, 95.597, 138.93,
         9.156, 35.275, 139.04, 321.474, 856.16,
         34.013, 26.756, 20.091, 24.637, 37.626)

df = data.frame(t,N,method, time)

df$t <- factor(df$t,
               levels = c('5 treatments','10 treatments','15 treatments'),
               ordered = TRUE)

breaks_fun <- function(x) {
  if (max(x) < 60) {
    seq(10, 50, 10)
  } else if (max(x) < 130){
    seq(40,120,20)
  }else {
    seq(120, 240, 30)
  }
}

p <- ggplot(data=df, aes(x=N, y=time)) +
  geom_line(aes(colour = method)) +
  geom_point(aes(colour = method)) +
  facet_wrap(~t, scales="free") +
  scale_x_continuous(breaks = breaks_fun) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Computational time (seconds)") + xlab("Total number of studies") +
  theme_bw() +
  theme(legend.title=element_blank(),
        legend.position = c(0.055, 0.885), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = "white", colour = NA))

ggsave(p, file = "time_comparison.png", height = 5, width = 10, bg = "white")


################### Example to calculate computational time #######################

##### gemtc #####
# use long data
# estimated points and variances in tbl
library(gemtc)
library(rjags) ## install rjags package from https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Mac%20OS%20X/

data.raw1 <- read.csv("time_comparison_sampling_data/sampling_5_50.csv")
data.raw1 <- data.raw1[, 1:4]
colnames(data.raw1) <- c("study", "treatment","mean", "std.err")

start <- Sys.time()

network <- mtc.network(data.raw1)
model <- mtc.model(network)
results <- mtc.run(model, thin=10)
tbl <- relative.effect.table(results)

delta <- Sys.time() - start
delta

##### netmeta #####
# use iv data; if long, use function pairwise() to transfer to long format
# estimated points and variances in league$random
library(netmeta)

data.raw2 <- read.csv("time_comparison_sampling_data/resampling_5_50.csv")

start <- Sys.time()

net <- netmeta(TE,seTE,treat1,treat2,studlab, data=data.raw2)
league <- netleague(net, fixed=FALSE, digits=2, seq=netrank(net), text.NA = "")

delta <- Sys.time() - start
delta

##### proposed ######
# use both long and iv data
#estimated points and variances in prop
library(Matrix)
library(plyr)
library(dplyr)

source("CLNMA.equal.tau.R")
source("get_table_meta.R")

data.raw3 <- read.csv("time_comparison_sampling_data/resampling_5_50.csv")

net <- netmeta(TE,seTE,treat1,treat2,studlab, data=data.raw3)

start <- Sys.time()
prop <- get_table_meta(data = data.raw3, net = net, iv = FALSE)

delta <- Sys.time() - start
delta

