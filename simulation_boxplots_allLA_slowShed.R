library(ggplot2)
library(cowplot)
library(stringr)
library(ggpubr)
library(tidyr)
library(dplyr) 

#read in the data
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA05/noSS_lowBR")
setwd("~/Desktop/CWD_Modeling")

allSS.LA05.data = read.csv("allSS_LA05_medBR_slowShed_data.csv", header = TRUE)
allSS.LA1.data = read.csv("allSS_LA1_medBR_slowShed_data.csv", header = TRUE)

allSS.LA05.data = subset(allSS.LA05.data, SS.strat != "no SS")
allSS.allLA.data = rbind(allSS.LA05.data, allSS.LA1.data)

allSS.allLA.data.endemic = subset(allSS.allLA.data, final.prev>0)
allSS.allLA.data.endemic$LA[which(allSS.allLA.data.endemic$SS.strat == "no SS")] = "no SS"

allSS.allLA.data.endemic$final.prev.pcent = allSS.allLA.data.endemic$final.prev*100

allSS.allLA.data.endemic$SS.strat = factor(allSS.allLA.data.endemic$SS.strat, levels = c("no SS", "random", "edge", "priority"), 
                                  order = TRUE)

allSS.allLA.data.endemic$LA = factor(allSS.allLA.data.endemic$LA, levels = c("no SS", "0.5", "1"), 
                                           order = TRUE)

#Plots

########################
#Final prevalence
########################
final.prev.aov = aov(final.prev.pcent~SS.strat + LA, data = allSS.allLA.data.endemic)
summary(final.prev.aov)
TukeyHSD(final.prev.aov)
# summary(kruskal.test(final.prev ~ SS.strat + LA, data = allSS.allLA.data.endemic))

fp.means = aggregate(allSS.allLA.data.endemic, final.prev.pcent~SS.strat + LA, mean)
pcent.change = NA
for(i in 2:7){
  pcent.change[i] = (fp.means[i,3] - fp.means[1,3])/fp.means[1,3]*100
}

fp.means$pcent.change = pcent.change

endemic.counts = as.data.frame(table(allSS.allLA.data.endemic$SS.strat, allSS.allLA.data.endemic$LA))
names(endemic.counts) = c("SS.strat", "LA", "count")
endemic.counts = subset(endemic.counts, count > 0)

fp.plot = ggboxplot(allSS.allLA.data.endemic, x = "SS.strat", y = "final.prev.pcent", 
                    color = "black", fill = "LA", palette = "jco", width = 0.4) + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + 
  theme_cowplot() +
  ylab("final prevalence (%)") + xlab("sharpshooting strategy") + theme(legend.position = "bottom") + 
  geom_point(data = fp.means, aes(x = SS.strat, y = final.prev.pcent, fill = LA), shape = 23, size = 3, position = position_dodge(width = 0.25)) +
  geom_text(data = endemic.counts, aes(x = SS.strat, y = 15.9, col = LA, label = count), position = position_dodge(width = .75), show.legend = FALSE) +
  geom_text(data = fp.means, aes(x = SS.strat, y = 15.2, col = LA, label = round(final.prev.pcent, 3)), position = position_dodge(width = 0.75), show.legend = FALSE) +
  geom_text(data = fp.means, aes(x = SS.strat, y = 14.5, col = LA, label = round(pcent.change, 1)), position = position_dodge(width = .75), show.legend = FALSE) +
  scale_fill_manual(values = c("grey30", "#0073C2FF", "#EFC000FF")) +
  scale_color_manual(values = c("grey30", "#0073C2FF", "#EFC000FF"))

allSS.allLA.data.endemic.sub = subset(allSS.allLA.data.endemic, SS.strat != "no SS")
fp.means.sub = subset(fp.means, SS.strat != "no SS")
allSS.allLA.data.endemic.sub$SS.strat = factor(allSS.allLA.data.endemic.sub$SS.strat, levels = c("random", "edge", "priority"), 
                                              order = TRUE)
fp.plot.inset = ggboxplot(allSS.allLA.data.endemic.sub, x = "SS.strat", y = "final.prev.pcent", 
                          color = "black", fill = "LA", palette = "jco", width = 0.4) + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + 
  theme_cowplot() +
  ylab("final prevalence %") + xlab("SS strategy") + theme(legend.position = "none") + 
  geom_point(data = fp.means.sub, aes(x = SS.strat, y = final.prev.pcent, fill = LA), shape = 23, size = 3, position = position_dodge(width = 0.25)) +
  coord_cartesian(ylim = c(-0, 0.5)) +
  theme(axis.title = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(l = 5, r = -31))) +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))


fp.plot2 = fp.plot +
  draw_plot(fp.plot.inset, x = 1.35, y = 2, width = 3.35, height = 12.0)

fp.plot2
#########################
#final spread area
#########################
sa.means = aggregate(allSS.allLA.data.endemic, final.spread.area~SS.strat + LA, mean)
pcent.change = NA
for(i in 2:7){
  pcent.change[i] = (sa.means[i,3]-sa.means[1,3])/sa.means[1,3]*100
}

sa.means$pcent.change = pcent.change

sa.plot = ggboxplot(allSS.allLA.data.endemic, x = "SS.strat", y = "final.spread.area", 
                    color = "black", fill = "LA", palette = "jco", width = 0.4) + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + 
  theme_cowplot() +
  ylab(bquote('area of spread '(km^2))) + xlab("sharpshooting strategy") + theme(legend.position = "bottom") + 
  geom_point(data = sa.means, aes(x = SS.strat, y = final.spread.area, fill = LA), shape = 23, size = 3, position = position_dodge(width = 0.25)) +
  geom_text(data = sa.means, aes(x = SS.strat, y = 4950, col = LA, label = round(final.spread.area, 3)), position = position_dodge(width = 0.75), show.legend = NA) +
  geom_text(data = sa.means, aes(x = SS.strat, y = 4750, col = LA, label = round(pcent.change, 1)), position = position_dodge(width = .75), show.legend = NA) +
  scale_fill_manual(values = c("grey30", "#0073C2FF", "#EFC000FF")) +
  scale_color_manual(values = c("grey30", "#0073C2FF", "#EFC000FF"))

sa.means.sub = subset(sa.means, SS.strat != "no SS")

sa.plot.inset = ggboxplot(allSS.allLA.data.endemic.sub, x = "SS.strat", y = "final.spread.area", 
                          color = "black", fill = "LA", palette = "jco", width = 0.4) + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + 
  theme_cowplot() +
  ylab("final prevalence %") + xlab("SS strategy") + theme(legend.position = "none") + 
  geom_point(data = sa.means.sub, aes(x = SS.strat, y = final.spread.area, fill = LA), shape = 23, size = 3, position = position_dodge(width = 0.25)) +
  coord_cartesian(ylim = c(0, 1500)) +
  theme(axis.title = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(l = 5, r = -31))) +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))


sa.plot2 = sa.plot +
  draw_plot(sa.plot.inset, x = 1.3, y = 1500, width = 3.35, height = 3000.0)

sa.plot2
##################
#prev rate
##################
pr.plot = ggboxplot(allSS.allLA.data.endemic, x = "SS.strat", y = "mean.prev.rate",
                    color = "black", fill = "LA", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab(expression(Delta * " prevalence")) + xlab("SS strat") + 
  theme(legend.position = "none")

##################
#spread rate
##################
sr.plot = ggboxplot(allSS.allLA.data.endemic, x = "SS.strat", y = "spread.area.rate",
                    color = "black", fill = "LA", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab(expression(Delta * " area (km" ^ 2*")")) + xlab("SS strat") + 
  theme(legend.position = "none")

##################
#final population size
##################
finalpop.plot = ggboxplot(allSS.allLA.data.endemic, x = "SS.strat", y = "final.pop",
                          color = "black", fill = "LA", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("final population size") + xlab("SS strat") + 
  theme(legend.position = "none")

##################
#I distance
##################
ID.plot = ggboxplot(allSS.allLA.data.endemic, x = "SS.strat", y = "mean.I.dist",
                    color = "black", fill = "LA", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("maximum infected distance from center") + xlab("SS strat") + 
  theme(legend.position = "none")

ggboxplot(allSS.allLA.data.endemic, x = "SS.strat", y = "std.I.dist",
          color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("std. I distance") + xlab("SS strat") + 
  theme(legend.position = "none")

#######################
#Prop of infected cells
#######################
propinfcell.plot = ggboxplot(allSS.allLA.data.endemic, x = "SS.strat", y = "final.number.I.cells",
                             color = "black", fill = "LA", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("number of cells with infected individuals") + xlab("SS strat") + 
  theme(legend.position = "none")

########################################
#boxplots of SS individual proportions
########################################
allSS.allLA.data.endemic.tall = gather(allSS.allLA.data.endemic, key = "SS_group", value = "num_rem", final.SS.S.prop:final.SS.I.prop)

SS.plot = ggboxplot(allSS.allLA.data.endemic.tall, x = "SS.strat", y = "num_rem",
                    color = "black", fill = "LA", palette = c("#EFC000FF", "#868686FF", "#CD534CFF", "#7AA6DCFF")) + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("proportion of removed individuals") + xlab("individuals removed") + 
  theme(legend.position = "inside", legend.position.inside =  c(0.15, 0.85)) + facet_wrap(~SS_group)
SS.plot

##########################################
#Inf. Fam. Prev.
##########################################

inffam.plot = ggboxplot(allSS.allLA.data.endemic, x = "SS.strat", y = "mean.inf.fams.prev",
                        color = "black", fill = "LA", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("inf. family prev.") + xlab("SS strategy") + 
  theme(legend.position = "none")

chull.plot = ggboxplot(allSS.allLA.data.endemic, x = "SS.strat", y = "mean.chull.fams.prev",
                       color = "black", fill = "LA", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("inf. chull family prev.") + xlab("SS strategy") + 
  theme(legend.position = "none")

chullI.plot = ggboxplot(allSS.allLA.data.endemic, x = "SS.strat", y = "mean.chull.fams.prev.I",
                        color = "black", fill = "LA", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("inf. chull family I prev.") + xlab("SS strategy") + 
  theme(legend.position = "bottom")

chullI.plot


###############################################
#Relate the area and local prev.
###############################################
ggplot(allSS.allLA.data.endemic) + geom_point(aes(x = mean.chull.fams.prev.I, y = final.spread.area, color = SS.strat, pch = LA)) +
  facet_grid(LA~SS.strat) + geom_smooth(aes(x = mean.chull.fams.prev.I, y = final.spread.area, color = SS.strat, pch = LA)) + theme_cowplot()


###############################################
#Combine plots
###############################################

ggarrange(fp.plot2 + theme(axis.title.x=element_blank(),
                           axis.text.x=element_blank(),
                           axis.ticks.x=element_blank(),
                           axis.text.y = element_text(angle = 90, vjust = 0, hjust=0.5)) + 
            labs(color = "land access probability", fill = "land access probability"), 
          sa.plot2 + theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust=0.5),
                           plot.margin = unit(c(0,0,0,0), "cm")) , 
          nrow = 2, common.legend = TRUE, legend = "bottom", 
          labels = c("a", "b"))

ggarrange(pr.plot, sr.plot,
          finalpop.plot, ID.plot,
          inffam.plot, chullI.plot, nrow = 3, ncol = 2, common.legend = TRUE)


############################################
#GLM
#############################################
glm.data = allSS.allLA.data.endemic
glm.data$SS.strat = factor(glm.data$SS.strat, ordered = FALSE)

fp.glm = glm(final.prev.pcent ~ SS.strat + LA, data = glm.data, family = "Gamma")
sa.glm = glm(final.spread.area ~ SS.strat, data = glm.data, family = "Gamma")



###############################################
#Anova
###############################################
anova.data = allSS.allLA.data.endemic
levels(anova.data$SS.strat) = c(levels(anova.data$SS.strat))
fp.anova = aov(final.prev.pcent~SS.strat + LA, data = anova.data)
fp.tukey = TukeyHSD(fp.anova)

fp.tukey.df = as.data.frame(fp.tukey$SS.strat)
names(fp.tukey.df)[4] = "padj"
fp.tukey.df = cbind(fp.tukey.df, data.frame(rw = rownames(fp.tukey$SS.strat)))
fp.tukey.df = separate(fp.tukey.df, rw, into = c("var1","var2"), sep = "-", extra = "merge")
fp.tukey.df$var1 = factor(fp.tukey.df$var1, levels = c("no SS", "random", "edge",  "priority"), 
                          order = TRUE)
fp.tukey.df$var2 = factor(fp.tukey.df$var2, levels = c("no SS", "random", "edge", "priority"), 
                          order = TRUE)

fp.tukey.plot = ggplot(fp.tukey.df, aes(x = var1,y = var2, fill = padj^0.2313782))+
  geom_tile() +  scale_fill_gradientn(colours=c("firebrick","goldenrod","black","#996633"),
                                      values=c(0,0.0499,0.05,1)^ 0.2313782,
                                      na.value="white", guide="colourbar",
                                      name="P-value",limits=c(0,1),breaks=c(0,0.05,1)^ 0.2313782, 
                                      labels=c(0,0.05,1)) +
  geom_text(aes(label = round(padj, 3)), color = "white") +
  theme_cowplot() + xlab("strategy 1") + ylab("strategy 2")

fp.tukey.plot
