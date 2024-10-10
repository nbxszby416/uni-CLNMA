### Last updated on 09-11-2024
### Figure 4

######################## Code to reproduce Figure 4 ############################
library(forestplot)
library(Gmisc)

### IOP-pairwise
## 1 for pairwise meta-analysis; 2 for Li's approach; 3 for the proposed method
data = read.csv("IOP_pairwise_scatter.csv", header=TRUE)
tabletext <- cbind(data$Column1,data$Column2)

p1=forestplot(tabletext,mean=cbind(data$MD1, data$MD2, data$MD3),
              lower=cbind(data$LB1, data$LB2,data$LB3),
              upper=cbind(data$UB1,data$UB2, data$UB3),
              legend=c("Pairwise meta-analysis","Standard NMA","Proposed method"),
              xticks=c(-10:4),
              boxsize=0.25,
              xlab="Mean difference",
              xt_gp = fpTxtGp(label = gpar(fontfamily = "Arial", cex=10), xlab=gpar(cex= 10),  legend=gpar(cex=10), ticks=gpar(cex=10)),
              fn.ci_norm = c(fpDrawCircleCI, fpDrawNormalCI, fpDrawDiamondCI),  lty=c(1,1,1),
              ci.vertices=TRUE, ci.vertices.height=0.05,
              line.margin=0.68,
              mar = unit(rep(8, times = 4), "mm"),
              col =fpColors(box = c("green3","red", "blue"), line=c("darkgreen","darkred","darkblue")),
              new_page=TRUE)

pdf("IOP_pairwise_forest_plot.pdf", width=8, height=10)
p1
dev.off()

### CP/CPPS-pairwise
df = read.csv("CP_CPPS_pairwise.csv")
label_list <- list(c("vs. Placebo", "vs. Placebo", "vs. Placebo", "vs. Placebo",
                     bquote("vs. \u03B1-Blockers + Antibiotics"), 
                     bquote("vs. \u03B1-Blockers + Antibiotics"), 
                     bquote("vs. \u03B1-Blockers")),
                   c("Anti-inflammatory", bquote("\u03B1-Blockers + Antibiotics"),
                     bquote("\u03B1-Blockers"), "Antibiotics", 
                     bquote("\u03B1-Blockers"), "Antibiotics", "Antibiotics"))
p=forestplot(labeltext=label_list,
             mean=cbind(df$MD0, df$MD1, df$MD2, df$MD3, df$MD4),
             lower=cbind(df$LB0, df$LB1, df$LB2, df$LB3, df$LB4),
             upper=cbind(df$UB0, df$UB1, df$UB2, df$UB3, df$UB4),
             legend=c("Pairwise meta-analysis","Standard NMA","Proposed method", "Proposed method-KC", "Proposed method-MD"),
             xticks=c(-12,-9,-6,-3,0,3,6,9),
             boxsize=0.05,
             xlab="Mean difference",
             xt_gp = fpTxtGp(label = gpar(fontfamily = "Arial", cex=10), xlab=gpar(fontfamily = "Arial",cex= 10),  legend=gpar(fontfamily = "Arial",cex=10), ticks=gpar(fontfamily = "Arial",cex=10)),
             fn.ci_norm = c(fpDrawCircleCI, fpDrawNormalCI, fpDrawDiamondCI, fpDrawPointCI, fpDrawPointCI),  lty=c(1,1,1,1,1),
             ci.vertices=TRUE, ci.vertices.height=0.05,
             line.margin=0.68,
             mar = unit(rep(5, times = 5), "mm"),
             col =fpColors(box = c("green3", "red", "blue","purple", "grey"), line=c("darkgreen","darkred","darkblue","purple","darkgrey")),
             new_page=TRUE)

png("CP_CPPS_pairwise.png", width = 1000, height = 1200, res = 120)
print(p)
dev.off()
