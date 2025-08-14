library(ggplot2)
library(cowplot)
library(stringr)
library(ggpubr)
library(tidyr)
library(dplyr) 

#read in the data
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA05/noSS_lowBR")
setwd("~/Desktop/CWD_Modeling")
noSS.data = read.csv("noSS_medBR_slowShed_output.csv")
noSS.data$SS.strat = "no SS"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA05/randomSS_LA05_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA05/randomSS_LA05_lowBR")
setwd("~/Desktop/CWD_Modeling")
randomSS.data = read.csv("randomSS_output_LA05_medBR_slowShed.csv")
randomSS.data$SS.strat = "random"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA05/edgeSS_LA05_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA05/edgeSS_LA05_lowBR")
setwd("~/Desktop/CWD_Modeling")
edgeSS.data = read.csv("edgeSS_output_LA05_medBR_slowShed.csv")
edgeSS.data$SS.strat = "edge"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA05/prioritySS_LA05_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA05/prioritySS_LA05_lowBR")
setwd("~/Desktop/CWD_Modeling")
prioritySS.data = read.csv("prioritySS_output_LA05_medBR_slowShed.csv")
prioritySS.data$SS.strat = "priority"

# allSS.LA05.data = rbind(noSS.data, randomSS.data, edgeSS.data, prioritySS.data, edge20SS.data)
allSS.LA05.data = rbind(noSS.data,  edgeSS.data, prioritySS.data, randomSS.data)
allSS.LA05.data$SS.strat = factor(allSS.LA05.data$SS.strat, levels = c("no SS", "random", "edge", "priority"), 
                                 order = TRUE)
allSS.LA05.data$LA = 0.5
allSS.LA05.data.endemic = subset(allSS.LA05.data, final.prev>0)

allSS.LA05.data.endemic$final.prev.pcent = allSS.LA05.data.endemic$final.prev*100

setwd("~/Desktop/CWD_Modeling")
write.csv(allSS.LA05.data, "allSS_LA05_medBR_slowShed_data.csv", row.names = FALSE)

#Plots

########################
#Final prevalence
########################
final.prev.aov = aov(final.prev.pcent~SS.strat, data = allSS.LA05.data.endemic)
summary(final.prev.aov)
TukeyHSD(final.prev.aov)
summary(kruskal.test(final.prev ~ SS.strat, data = allSS.LA05.data.endemic))

fp.means = aggregate(allSS.LA05.data.endemic, final.prev.pcent~SS.strat, mean)
pcent.change = NA
for(i in 2:4){
  pcent.change[i] = (fp.means[i,2] - fp.means[1,2])/fp.means[1,2]*100
}

fp.means$pcent.change = pcent.change

endemic.counts = as.data.frame(table(allSS.LA05.data.endemic$SS.strat))
names(endemic.counts) = c("SS.strat","count")

fp.plot = ggboxplot(allSS.LA05.data.endemic, x = "SS.strat", y = "final.prev.pcent", 
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + 
  theme_cowplot() +
  ylab("final prevalence %") + xlab("SS strategy") + theme(legend.position = "none") + 
  geom_point(data = fp.means, aes(x = SS.strat, y = final.prev.pcent), shape = 3, size = 3, position = position_dodge(width = 0.75)) +
  geom_text(data = endemic.counts, aes(x = SS.strat, y = 15.7, col = SS.strat, label = count), position = position_dodge(width = .75), show.legend = FALSE) +
  geom_text(data = fp.means, aes(x = SS.strat, y = 15.4, col = SS.strat, label = round(final.prev.pcent, 3)), position = position_dodge(width = 0.75), show.legend = FALSE) +
  geom_text(data = fp.means, aes(x = SS.strat, y = 15.1, col = SS.strat, label = round(pcent.change, 1)), position = position_dodge(width = .75), show.legend = FALSE) 

allSS.LA05.data.endemic.sub = subset(allSS.LA05.data.endemic, SS.strat != "no SS")
fp.means.sub = subset(fp.means, SS.strat != "no SS")
allSS.LA05.data.endemic.sub$SS.strat = factor(allSS.LA05.data.endemic.sub$SS.strat, levels = c("random", "edge", "priority"), 
                                             order = TRUE)
fp.plot.inset = ggboxplot(allSS.LA05.data.endemic.sub, x = "SS.strat", y = "final.prev.pcent", 
                          color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + 
  theme_cowplot() +
  ylab("final prevalence %") + xlab("SS strategy") + theme(legend.position = "none") + 
  geom_point(data = fp.means.sub, aes(x = SS.strat, y = final.prev.pcent), shape = 3, size = 3, position = position_dodge(width = 0.75)) +
  coord_cartesian(ylim = c(-0, 0.15)) +
  theme(axis.title = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(l = 5, r = -31))) +
  scale_fill_manual(values = c("#EFC000FF", "#868686FF", "#CD534CFF")) +
  scale_color_manual(values = c("#EFC000FF", "#868686FF", "#CD534CFF"))


fp.plot2 = fp.plot +
  draw_plot(fp.plot.inset, x = 1.35, y = 2, width = 3.35, height = 12.0)
#########################
#final spread area
#########################
sa.means = aggregate(allSS.LA05.data.endemic, final.spread.area~SS.strat, mean)
pcent.change = NA
for(i in 2:4){
  pcent.change[i] = (sa.means[i,2]-sa.means[1,2])/sa.means[1,2]*100
}

sa.means$pcent.change = pcent.change

sa.plot = ggboxplot(allSS.LA05.data.endemic, x = "SS.strat", y = "final.spread.area", 
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + 
  theme_cowplot() +
  ylab("area of spread (km2)") + xlab("SS strategy") + theme(legend.position = "none") + 
  geom_point(data = sa.means, aes(x = SS.strat, y = final.spread.area), shape = 3, size = 3, position = position_dodge(width = 0.75)) +
  geom_text(data = sa.means, aes(x = SS.strat, y = 4950, col = SS.strat, label = round(final.spread.area, 3)), position = position_dodge(width = 0.75), show.legend = FALSE) +
  geom_text(data = sa.means, aes(x = SS.strat, y = 4850, col = SS.strat, label = round(pcent.change, 1)), position = position_dodge(width = .75), show.legend = FALSE) 

sa.means.sub = subset(sa.means, SS.strat != "no SS")

sa.plot.inset = ggboxplot(allSS.LA05.data.endemic.sub, x = "SS.strat", y = "final.spread.area", 
                          color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + 
  theme_cowplot() +
  ylab("final prevalence %") + xlab("SS strategy") + theme(legend.position = "none") + 
  geom_point(data = sa.means.sub, aes(x = SS.strat, y = final.spread.area), shape = 3, size = 3, position = position_dodge(width = 0.75)) +
  coord_cartesian(ylim = c(0, 1500)) +
  theme(axis.title = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(l = 5, r = -31))) +
  scale_fill_manual(values = c("#EFC000FF", "#868686FF", "#CD534CFF")) +
  scale_color_manual(values = c("#EFC000FF", "#868686FF", "#CD534CFF"))


sa.plot2 = sa.plot +
  draw_plot(sa.plot.inset, x = 1.3, y = 1500, width = 3.35, height = 3000.0)
##################
#prev rate
##################
pr.plot = ggboxplot(allSS.LA05.data.endemic, x = "SS.strat", y = "mean.prev.rate",
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab(expression(Delta * " prevalence")) + xlab("SS strat") + 
  theme(legend.position = "none")

##################
#spread rate
##################
sr.plot = ggboxplot(allSS.LA05.data.endemic, x = "SS.strat", y = "spread.area.rate",
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab(expression(Delta * " area (km" ^ 2*")")) + xlab("SS strat") + 
  theme(legend.position = "none")

##################
#final population size
##################
finalpop.plot = ggboxplot(allSS.LA05.data.endemic, x = "SS.strat", y = "final.pop",
                          color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("final population size") + xlab("SS strat") + 
  theme(legend.position = "none")

##################
#I distance
##################
ID.plot = ggboxplot(allSS.LA05.data.endemic, x = "SS.strat", y = "mean.I.dist",
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("maximum infected distance from center") + xlab("SS strat") + 
  theme(legend.position = "none")

ggboxplot(allSS.LA05.data.endemic, x = "SS.strat", y = "std.I.dist",
          color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("final population size") + xlab("SS strat") + 
  theme(legend.position = "none")

#######################
#Prop of infected cells
#######################
propinfcell.plot = ggboxplot(allSS.LA05.data.endemic, x = "SS strat", y = "final.number.I.cells",
                             color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("number of cells with infected individuals") + xlab("SS strat") + 
  theme(legend.position = "none")

########################################
#boxplots of SS individual proportions
########################################
allSS.LA05.data.endemic.tall = gather(allSS.LA05.data.endemic, key = "SS_group", value = "num_rem", final.SS.S.prop:final.SS.I.prop)

SS.plot = ggboxplot(allSS.LA05.data.endemic.tall, x = "SS_group", y = "num_rem",
                    color = "black", fill = "SS.strat", palette = c("#EFC000FF", "#868686FF", "#CD534CFF", "#7AA6DCFF")) + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("proportion of removed individuals") + xlab("individuals removed") + 
  theme(legend.position = "inside", legend.position.inside =  c(0.85, 0.85)) +
  scale_x_discrete(labels= c("susceptible", "exposed", "infectious")) 
SS.plot

##########################################
#Inf. Fam. Prev.
##########################################

inffam.plot = ggboxplot(allSS.LA05.data.endemic, x = "SS.strat", y = "mean.inf.fams.prev",
                        color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("inf. family prev.") + xlab("SS strategy") + 
  theme(legend.position = "none")

chull.plot = ggboxplot(allSS.LA05.data.endemic, x = "SS.strat", y = "mean.chull.fams.prev",
                       color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("inf. chull family prev.") + xlab("SS strategy") + 
  theme(legend.position = "none")

chullI.plot = ggboxplot(allSS.LA05.data.endemic, x = "SS.strat", y = "mean.chull.fams.prev.I",
                        color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("inf. chull family prev.") + xlab("SS strategy") + 
  theme(legend.position = "none")

chullI.plot
###############################################
#Combine plots
###############################################

ggarrange(fp.plot2, sa.plot2, nrow = 2, common.legend = TRUE)

ggarrange(pr.plot, sr.plot,
          finalpop.plot, ID.plot,
          inffam.plot, chull.plot, nrow = 3, ncol = 2, common.legend = TRUE)


############################################
#GLM
#############################################
glm.data = allSS.LA05.data.endemic
glm.data$SS.strat = factor(glm.data$SS.strat, ordered = FALSE)

fp.glm = glm(final.prev.pcent ~ SS.strat, data = glm.data, family = "Gamma")
sa.glm = glm(final.spread.area ~ SS.strat, data = glm.data, family = "Gamma")



###############################################
#Anova
###############################################
anova.data = allSS.LA05.data.endemic
levels(anova.data$SS.strat) = c(levels(anova.data$SS.strat))
fp.anova = aov(final.prev.pcent~SS.strat, data = anova.data)
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
