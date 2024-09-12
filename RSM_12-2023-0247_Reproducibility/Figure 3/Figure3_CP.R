### Last updated on 09-11-2024
### Figure 3

setwd("~/Dropbox/Bingyu_PennCIL/00_Bingyu/Projects/CL-NMA/production/Figure 3")

######################## Code to reproduce Figure 3 ############################
library(ggplot2)
library(ggpubr)

table1 = read.csv("simu_table_rhow_0.2.csv")
table2 = read.csv("simu_table_rhow_0.5.csv")

table1$method <- factor(table1$method,
                        levels = c('No correction','KC-correction','MD-correction'),
                        ordered = TRUE)
table2$method <- factor(table2$method,
                        levels = c('No correction','KC-correction','MD-correction'),
                        ordered = TRUE)

p1 <- ggplot(table1, aes(x = n, y = cp_ab)) +
  geom_line(aes(colour = method)) +
  geom_point(aes(colour = method)) +
  scale_color_brewer(palette = "Dark2") +
  xlab("") +
  ylab("Coverage Probability") +
  ylim(c(0.87, 0.95)) +
  theme_bw() +
  theme(legend.title=element_blank(),
        legend.position = c(0.15, 0.86), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = "white", colour = NA))

p2 <- ggplot(table1, aes(x = n, y = cp_bc)) +
  geom_line(aes(colour = method)) +
  geom_point(aes(colour = method)) +
  scale_color_brewer(palette = "Dark2") +
  xlab("") +
  ylab("") +
  ylim(c(0.87, 0.95)) +
  theme_bw() +
  theme(legend.position = "none")

##################
p3 <- ggplot(table2, aes(x = n, y = cp_ab)) +
  geom_line(aes(colour = method)) +
  geom_point(aes(colour = method)) +
  scale_color_brewer(palette = "Dark2") +
  xlab("Number of studies (AB comparison)") + ylab("Coverage Probability") +
  ylim(c(0.87, 0.95)) +
  theme_bw() +
  theme(legend.position = "none")

p4 <- ggplot(table2, aes(x = n, y = cp_bc)) +
  geom_line(aes(colour = method)) +
  geom_point(aes(colour = method)) +
  scale_color_brewer(palette = "Dark2") +
  xlab("Number of studies (AC comparison)") + ylab("") +
  ylim(c(0.87, 0.95)) +
  theme_bw() +
  theme(legend.position = "none")

p <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, labels = c("(a)" ,"", "(b)", ""),
               align = "h")

ggsave(p, file = "simu_correct.png", height = 8, width = 11, bg = "white")
