library(ggplot2)
library(cowplot)
library(stringr)
library(ggpubr)
library(tidyr)
library(dplyr) 

#read in the data
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/noSS_lowBR")
setwd("~/Desktop/CWD_Modeling")
noSS.data = read.csv("noSS_medBR_slowShed_output.csv")
noSS.data$delay = "no SS"
noSS.data$SS.strat = "no SS"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/randomSS_LA1_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/randomSS_LA1_lowBR")
setwd("~/Desktop/CWD_Modeling")
delay5.data = read.csv("prioritySS_output_LA1_medBR_delay5_slowShed.csv")
delay5.data$delay = "delay 5"
delay5.data$SS.strat = "priority"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/edgeSS_LA1_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/edgeSS_LA1_lowBR")
setwd("~/Desktop/CWD_Modeling")
delay10.data = read.csv("prioritySS_output_LA1_medBR_delay10_slowShed.csv")
delay10.data$delay = "delay 10"
delay10.data$SS.strat = "priority"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/edgeSS_LA1_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/edgeSS_LA1_lowBR")
setwd("~/Desktop/CWD_Modeling")
delay15.data = read.csv("prioritySS_output_LA1_medBR_delay15_slowShed.csv")
delay15.data$delay = "delay 15"
delay15.data$SS.strat = "priority"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/prioritySS_LA1_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/prioritySS_LA1_lowBR")
setwd("~/Desktop/CWD_Modeling")
prioritySS.data = read.csv("prioritySS_output_LA1_medBR_slowShed.csv")
prioritySS.data$delay = "all years"
prioritySS.data$SS.strat = "priority"

#EDGE
setwd("~/Desktop/CWD_Modeling")
delay5.edge.data = read.csv("edgeSS_output_LA1_medBR_delay5_slowShed.csv")
delay5.edge.data$delay = "delay 5"
delay5.edge.data$SS.strat = "edge"

setwd("~/Desktop/CWD_Modeling")
delay10.edge.data = read.csv("edgeSS_output_LA1_medBR_delay10_slowShed.csv")
delay10.edge.data$delay = "delay 10"
delay10.edge.data$SS.strat = "edge"

setwd("~/Desktop/CWD_Modeling")
delay15.edge.data = read.csv("edgeSS_output_LA1_medBR_delay15_slowShed.csv")
delay15.edge.data$delay = "delay 15"
delay15.edge.data$SS.strat = "edge"

setwd("~/Desktop/CWD_Modeling")
edgeSS.data = read.csv("edgeSS_output_LA1_medBR_slowShed.csv")
edgeSS.data$delay = "all years"
edgeSS.data$SS.strat = "edge"


# allSS.LA1.data = rbind(noSS.data, randomSS.data, edgeSS.data, prioritySS.data, edge20SS.data)
allSS.LA1.data = rbind(noSS.data,  
                       delay5.data, delay10.data, delay15.data, prioritySS.data, 
                       delay5.edge.data, delay10.edge.data, delay15.edge.data, edgeSS.data)
allSS.LA1.data$delay = factor(allSS.LA1.data$delay, levels = c("no SS", "delay 15", "delay 10", "delay 5", "all years"), 
                                  order = TRUE)
allSS.LA1.data$SS.strat = factor(allSS.LA1.data$SS.strat, levels = c("no SS", "priority", "edge"), 
                              order = TRUE)
allSS.LA1.data$LA = 1.0
allSS.LA1.data.endemic = subset(allSS.LA1.data, final.prev>0)

allSS.LA1.data.endemic$final.prev.pcent = allSS.LA1.data.endemic$final.prev*100

setwd("~/Desktop/CWD_Modeling")
write.csv(allSS.LA1.data, "allSS_LA1_medBR_slowShed_delaySS_data.csv", row.names = FALSE)

#Plots

########################
#Final prevalence
########################
final.prev.aov = aov(final.prev.pcent~delay + SS.strat, data = allSS.LA1.data.endemic)
summary(final.prev.aov)
TukeyHSD(final.prev.aov)
summary(kruskal.test(final.prev ~ delay, data = allSS.LA1.data.endemic))

fp.means = aggregate(allSS.LA1.data.endemic, final.prev.pcent~delay + SS.strat, mean)
pcent.change = rep(NA, 9)
for(i in 2:9){
  pcent.change[i] = (fp.means[i,3] - fp.means[1,3])/fp.means[1,3]*100
}

fp.means$pcent.change = pcent.change

endemic.counts = as.data.frame(table(allSS.LA1.data.endemic$delay, allSS.LA1.data.endemic$SS.strat))
names(endemic.counts) = c("delay", "SS.strat", "count")
endemic.counts = subset(endemic.counts, count > 0)

fp.plot = ggboxplot(allSS.LA1.data.endemic, x = "delay", y = "final.prev.pcent", 
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + 
  theme_cowplot() +
  ylab("final prevalence (%)") + xlab("number of years before sharpshooting was implemented") + theme(legend.position = "none") +   geom_point(data = fp.means, aes(x = delay, y = final.prev.pcent, fill = SS.strat), shape = 23, size = 3, position = position_dodge(width = 0.75)) +
  geom_label(data = fp.means, aes(x = delay, y = 14.3, col = SS.strat, label = round(pcent.change, 1)), position = position_dodge(width = .75), show.legend = FALSE, label.size = NA) +
  geom_label(data = fp.means, aes(x = delay, y = 15.2, col = SS.strat, label = round(final.prev.pcent, 3)), position = position_dodge(width = 0.75), show.legend = FALSE, label.size = NA) +
  geom_label(data = endemic.counts, aes(x = delay, y = 16.1, col = SS.strat, label = count), position = position_dodge(width = .75), show.legend = FALSE, label.size = NA) +
  scale_fill_manual(values = c("grey30", "#0073C2FF", "#EFC000FF")) +
  scale_color_manual(values = c("grey30", "#0073C2FF", "#EFC000FF"))


fp.plot 
fp.plot.delay = fp.plot
#########################
#final spread area
#########################
sa.means = aggregate(allSS.LA1.data.endemic, final.spread.area ~ delay + SS.strat, mean)
pcent.change = rep(NA, 6)
for(i in 2:9){
  pcent.change[i] = (sa.means[i,3]-sa.means[1,3])/sa.means[1,3]*100
}

sa.means$pcent.change = pcent.change

sa.plot = ggboxplot(allSS.LA1.data.endemic, x = "delay", y = "final.spread.area", 
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + 
  theme_cowplot() +
  ylab(bquote('area of spread '(km^2))) + xlab("number of years before sharpshooting was implemented") + theme(legend.position = "none") + 
  geom_point(data = sa.means, aes(x = delay, y = final.spread.area, fill = SS.strat), shape = 23, size = 3, position = position_dodge(width = 0.75)) +
  geom_label(data = sa.means, aes(x = delay, y = 5600, col = SS.strat, label = round(pcent.change, 1)), position = position_dodge(width = 0.75), show.legend = FALSE, label.size = NA) +
  geom_label(data = sa.means, aes(x = delay, y = 5950, col = SS.strat, label = round(final.spread.area, 3)), position = position_dodge(width = 0.75), show.legend = FALSE, label.size = NA) +
  scale_fill_manual(values = c("grey30", "#0073C2FF", "#EFC000FF")) +
  scale_color_manual(values = c("grey30", "#0073C2FF", "#EFC000FF"))

sa.plot

sa.plot.delay = sa.plot
##################
#prev rate
##################
pr.plot = ggboxplot(allSS.LA1.data.endemic, x = "delay", y = "mean.prev.rate",
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab(expression(Delta * " prevalence")) + xlab("delay") + 
  theme(legend.position = "none")

##################
#spread rate
##################
sr.plot = ggboxplot(allSS.LA1.data.endemic, x = "delay", y = "spread.area.rate",
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab(expression(Delta * " area (km" ^ 2*")")) + xlab("delay") + 
  theme(legend.position = "none")

##################
#final population size
##################
finalpop.plot = ggboxplot(allSS.LA1.data.endemic, x = "delay", y = "final.pop",
                          color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("final population size") + xlab("delay") + 
  theme(legend.position = "none")

##################
#I distance
##################
ID.plot = ggboxplot(allSS.LA1.data.endemic, x = "delay", y = "mean.I.dist",
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("maximum infected distance from center") + xlab("delay") + 
  theme(legend.position = "none")

ID.plot

#######################
#Prop of infected cells
#######################
propinfcell.plot = ggboxplot(allSS.LA1.data.endemic, x = "delay", y = "final.number.I.cells",
                             color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("number of cells with infected individuals") + xlab("delay") + 
  theme(legend.position = "none")

propinfcell.plot
########################################
#boxplots of SS individual proportions
########################################
allSS.LA1.data.endemic.tall = gather(allSS.LA1.data.endemic, key = "SS_group", value = "num_rem", final.SS.S.prop:final.SS.I.prop)
allSS.LA1.data.endemic.tall = subset(allSS.LA1.data.endemic.tall, SS.strat != 'no SS')
allSS.LA1.data.endemic.tall$SS.strat = factor(allSS.LA1.data.endemic.tall$SS.strat, levels = c("priority", "edge"), 
                                 order = TRUE)
allSS.LA1.data.endemic.tall$delay = factor(allSS.LA1.data.endemic.tall$delay, levels = c("all years", "delay 5", "delay 10", "delay 15"),
order = TRUE)

SS.plot = ggboxplot(allSS.LA1.data.endemic.tall, x = "SS_group", y = "num_rem",
                    color = "black", fill = "SS_group", palette = c("#EFC000FF", "#868686FF", "#CD534CFF", "#7AA6DCFF")) + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("proportion of removed individuals") + xlab("individuals removed") + 
  theme(legend.position = "none", legend.position.inside =  c(0.85, 0.85)) +
  scale_x_discrete(labels= c("susceptible", "exposed", "infectious")) + facet_grid(SS.strat~delay)
SS.plot

##########################################
#Inf. Fam. Prev.
##########################################

inffam.plot = ggboxplot(allSS.LA1.data.endemic, x = "delay", y = "mean.inf.fams.prev",
                        color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("inf. family prev.") + xlab("delay") + 
  theme(legend.position = "none")

chull.plot = ggboxplot(allSS.LA1.data.endemic, x = "delay", y = "mean.chull.fams.prev",
                       color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("inf. chull family prev.") + xlab("delay") + 
  theme(legend.position = "none")

chullI.plot = ggboxplot(allSS.LA1.data.endemic, x = "delay", y = "mean.chull.fams.prev.I",
                        color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("inf. chull family prev.") + xlab("delay") + 
  theme(legend.position = "none")

chullI.plot


#################
#Relating Max and Final Area and Prev
#################
ggplot(allSS.LA1.data.endemic) + geom_point(aes(x = max.spread.area, y = final.spread.area, col = SS.strat)) + 
  facet_grid(SS.strat~delay)

ggplot(allSS.LA1.data.endemic) + geom_point(aes(x = max.prev, y = final.prev, col = SS.strat)) + 
  facet_grid(SS.strat~delay)

allSS.LA1.data.endemic$max.final.prev.diff = allSS.LA1.data.endemic$max.prev - allSS.LA1.data.endemic$final.prev
allSS.LA1.data.endemic$max.final.area.diff = allSS.LA1.data.endemic$max.spread.area - allSS.LA1.data.endemic$final.spread.area

ggplot(allSS.LA1.data.endemic) + geom_point(aes(x = max.final.prev.diff, y = max.final.area.diff, col = SS.strat)) + 
  facet_grid(SS.strat~delay)

###############################################
#Combine plots
###############################################

ggarrange(fp.plot, sa.plot, nrow = 2, common.legend = TRUE)

ggarrange(pr.plot, sr.plot,
          finalpop.plot, ID.plot,
          inffam.plot, chull.plot, nrow = 3, ncol = 2, common.legend = TRUE)


############################################
#GLM
#############################################
glm.data = allSS.LA1.data.endemic
glm.data$SS.strat = factor(glm.data$SS.strat, ordered = FALSE)
glm.data$delay = factor(glm.data$delay, ordered = FALSE)

fp.glm = glm(final.prev.pcent ~ SS.strat + delay, data = glm.data, family = "Gamma")
summary(fp.glm)


###############################################
#Anova
###############################################
anova.data = allSS.LA1.data.endemic
levels(anova.data$SS.strat) = c(levels(anova.data$SS.strat))
levels(anova.data$delay) = c(levels(anova.data$delay))
fp.anova = aov(final.prev.pcent~SS.strat + delay, data = anova.data)
fp.tukey = TukeyHSD(fp.anova)

fp.tukey.df = as.data.frame(fp.tukey$SS.strat)
names(fp.tukey.df)[4] = "padj"
fp.tukey.df = cbind(fp.tukey.df, data.frame(rw = rownames(fp.tukey$SS.strat)))
fp.tukey.df = separate(fp.tukey.df, rw, into = c("var1","var2"), sep = "-", extra = "merge")
fp.tukey.df$var1 = factor(fp.tukey.df$var1, levels = c("no SS", "random", "edge",  "priority"), 
                          order = TRUE)
fp.tukey.df$var2 = factor(fp.tukey.df$var2, levels = c("no SS", "random", "edge", "priority"), 
                          order = TRUE)

fp.tukey.plot.SSstrat = ggplot(fp.tukey.df, aes(x = var1,y = var2, fill = padj^0.2313782))+
  geom_tile() +  scale_fill_gradientn(colours=c("firebrick","goldenrod","black","#996633"),
                                      values=c(0,0.0499,0.05,1)^ 0.2313782,
                                      na.value="white", guide="colourbar",
                                      name="P-value",limits=c(0,1),breaks=c(0,0.05,1)^ 0.2313782, 
                                      labels=c(0,0.05,1)) +
  geom_text(aes(label = round(padj, 3)), color = "white") +
  theme_cowplot() + xlab("strategy 1") + ylab("strategy 2")

fp.tukey.plot.SSstrat
