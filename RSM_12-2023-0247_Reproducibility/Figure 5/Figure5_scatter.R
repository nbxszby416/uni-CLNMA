### Last updated on 09-11-2024
### Figure 5

library(ggplot2)
library(ggpubr)

######################## Code to reproduce Figure 5 ############################

### IOP
data = read.csv("IOP_full_scatter.csv", header = TRUE)

p1 = ggplot(data, aes(Z2, Z1)) +
  geom_vline(xintercept = c(-1.96, 1.96), lty = 2) +
  geom_hline(yintercept = c(-1.96, 1.96), lty = 2) +
  geom_point(shape=21, color="white", fill="slateblue4", size=3, stroke=1) +
  xlab("Z values from the proposed method") +
  ylab("Z values from standard NMA") +
  theme_bw()

ggsave(p1, file="IOP_full_scatter_plot.png", width=8, height=6)

### CP/CPPS
df = read.csv("CP_CPPS_full_scatter.csv")
p1 = ggplot(df, aes(Z2, Z1)) +
  geom_vline(xintercept = c(-1.96, 1.96), lty = 2) +
  geom_hline(yintercept = c(-1.96, 1.96), lty = 2) +
  geom_point(shape=21, color="white", fill="slateblue4", size=3, stroke=1) +
  xlab("Z values from the proposed method") +
  ylab("Z values from standard NMA") +
  theme_bw()

p2 = ggplot(df, aes(Z3, Z1)) +
  geom_vline(xintercept = c(-1.96, 1.96), lty = 2) +
  geom_hline(yintercept = c(-1.96, 1.96), lty = 2) +
  geom_point(shape=21, color="white", fill="slateblue4", size=3, stroke=1) +
  xlab("Z values from the proposed method (KC corrected)") +
  ylab("Z values from standard NMA") +
  theme_bw()

p3 = ggplot(df, aes(Z4, Z1)) +
  geom_vline(xintercept = c(-1.96, 1.96), lty = 2) +
  geom_hline(yintercept = c(-1.96, 1.96), lty = 2) +
  geom_point(shape=21, color="white", fill="slateblue4", size=3, stroke=1) +
  xlab("Z values from the proposed method (MD corrected)") +
  ylab("Z values from standard NMA") +
  theme_bw()

p = ggarrange(p1,p2,p3, ncol=3, nrow=1)

ggsave(p, file="CP_CPPS_full_scatter.png", width=18, height=4.5)
