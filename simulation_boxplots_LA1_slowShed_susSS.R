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
noSS.data$sus = "no SS"
noSS.data$SS.strat = "no SS"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/randomSS_LA1_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/randomSS_LA1_lowBR")
setwd("~/Desktop/CWD_Modeling")
SS5years.data = read.csv("prioritySS_LA1_medBR_SS5years_lowShed.csv")
SS5years.data$sus = "sus 5"
SS5years.data$SS.strat = "priority"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/edgeSS_LA1_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/edgeSS_LA1_lowBR")
setwd("~/Desktop/CWD_Modeling")
SS10years.data = read.csv("prioritySS_LA1_medBR_SS10years_slowShed.csv")
SS10years.data$sus = "sus 10"
SS10years.data$SS.strat = "priority"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/edgeSS_LA1_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/edgeSS_LA1_lowBR")
setwd("~/Desktop/CWD_Modeling")
SS15years.data = read.csv("prioritySS_LA1_medBR_SS15years_slowShed.csv")
SS15years.data$sus = "sus 15"
SS15years.data$SS.strat = "priority"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/prioritySS_LA1_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/prioritySS_LA1_lowBR")
setwd("~/Desktop/CWD_Modeling")
prioritySS.data = read.csv("prioritySS_output_LA1_medBR_slowShed.csv")
prioritySS.data$sus = "all years"
prioritySS.data$SS.strat = "priority"

#EDGE
setwd("~/Desktop/CWD_Modeling")
edge.SS5years.data = read.csv("edgeSS_LA1_medBR_SS5years_lowShed.csv")
edge.SS5years.data$sus = "sus 5"
edge.SS5years.data$SS.strat = "edge"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/edgeSS_LA1_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/edgeSS_LA1_lowBR")
setwd("~/Desktop/CWD_Modeling")
edge.SS10years.data = read.csv("edgeSS_LA1_medBR_SS10years_slowShed.csv")
edge.SS10years.data$sus = "sus 10"
edge.SS10years.data$SS.strat = "edge"

# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/edgeSS_LA1_lowBR")
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/LA1/edgeSS_LA1_lowBR")
setwd("~/Desktop/CWD_Modeling")
edge.SS15years.data = read.csv("edgeSS_LA1_medBR_SS15years_slowShed.csv")
edge.SS15years.data$sus = "sus 15"
edge.SS15years.data$SS.strat = "edge"

setwd("~/Desktop/CWD_Modeling")
edgeSS.data = read.csv("edgeSS_output_LA1_medBR_slowShed.csv")
edgeSS.data$sus = "all years"
edgeSS.data$SS.strat = "edge"

# allSS.LA1.data = rbind(noSS.data, randomSS.data, edgeSS.data, prioritySS.data, edge20SS.data)
allSS.LA1.data = rbind(noSS.data, SS5years.data, SS10years.data, SS15years.data, prioritySS.data, 
                       edge.SS5years.data, edge.SS10years.data, edge.SS15years.data, edgeSS.data, edgeSS.data)

allSS.LA1.data$sus = factor(allSS.LA1.data$sus, levels = c("no SS", "sus 5", "sus 10", "sus 15", "all years"), 
                              order = TRUE)
allSS.LA1.data$SS.strat = factor(allSS.LA1.data$SS.strat, levels = c("no SS", "priority", "edge"), 
                                 order = TRUE)
allSS.LA1.data$LA = 1.0
allSS.LA1.data.endemic = subset(allSS.LA1.data, final.prev>0)

allSS.LA1.data.endemic$final.prev.pcent = allSS.LA1.data.endemic$final.prev*100

setwd("~/Desktop/CWD_Modeling")
write.csv(allSS.LA1.data, "allSS_LA1_medBR_slowShed_susSS_data.csv", row.names = FALSE)

#Plots

########################
#Final prevalence
########################
final.prev.aov = aov(final.prev.pcent~sus + SS.strat, data = allSS.LA1.data.endemic)
summary(final.prev.aov)
TukeyHSD(final.prev.aov)
summary(kruskal.test(final.prev ~ sus, data = allSS.LA1.data.endemic))

fp.means = aggregate(allSS.LA1.data.endemic, final.prev.pcent~sus + SS.strat, mean)
pcent.change = rep(NA, 9)
for(i in 2:9){
  pcent.change[i] = (fp.means[i,3] - fp.means[1,3])/fp.means[1,3]*100
}

fp.means$pcent.change = pcent.change

endemic.counts = as.data.frame(table(allSS.LA1.data.endemic$sus, allSS.LA1.data.endemic$SS.strat))
names(endemic.counts) = c("sus", "SS.strat", "count")
endemic.counts = subset(endemic.counts, count > 0)

fp.plot = ggboxplot(allSS.LA1.data.endemic, x = "sus", y = "final.prev.pcent", 
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + 
  theme_cowplot() +
  ylab("final prevalence (%)") + xlab("number of years before sharpshooting was suspended") + theme(legend.position = "none") + 
  geom_point(data = fp.means, aes(x = sus, y = final.prev.pcent, fill = SS.strat), shape = 23, size = 3, position = position_dodge(width = 0.75)) +
  geom_label(data = fp.means, aes(x = sus, y = 14.3, col = SS.strat, label = round(pcent.change, 1)), position = position_dodge(width = .75), show.legend = FALSE, label.size = NA) +
  geom_label(data = fp.means, aes(x = sus, y = 15.2, col = SS.strat, label = round(final.prev.pcent, 3)), position = position_dodge(width = 0.75), show.legend = FALSE, label.size = NA) +
  geom_label(data = endemic.counts, aes(x = sus, y = 16.1, col = SS.strat, label = count), position = position_dodge(width = .75), show.legend = FALSE, label.size = NA) +
  scale_fill_manual(values = c("grey30", "#0073C2FF", "#EFC000FF")) +
  scale_color_manual(values = c("grey30", "#0073C2FF", "#EFC000FF"))

fp.plot

fp.plot.sus  = fp.plot


########################
#final spread area
#########################
sa.means = aggregate(allSS.LA1.data.endemic, final.spread.area ~ sus + SS.strat, mean)
pcent.change = rep(NA, 9)
for(i in 2:9){
  pcent.change[i] = (sa.means[i,3]-sa.means[1,3])/sa.means[1,3]*100
}

sa.means$pcent.change = pcent.change

sa.plot = ggboxplot(allSS.LA1.data.endemic, x = "sus", y = "final.spread.area", 
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + 
  theme_cowplot() +
  ylab(bquote('area of spread '(km^2))) + xlab("number of years before sharpshooting was suspended") + theme(legend.position = "none") + 
  geom_point(data = sa.means, aes(x = sus, y = final.spread.area, fill = SS.strat), shape = 23, size = 3, position = position_dodge(width = 0.75)) +
  geom_label(data = sa.means, aes(x = sus, y = 4650, col = SS.strat, label = round(pcent.change, 1)), position = position_dodge(width = .75), show.legend = FALSE, label.size = NA) +
  geom_label(data = sa.means, aes(x = sus, y = 4950, col = SS.strat, label = round(final.spread.area, 3)), position = position_dodge(width = 0.75), show.legend = FALSE, label.size = NA) +
  scale_fill_manual(values = c("grey30", "#0073C2FF", "#EFC000FF")) +
  scale_color_manual(values = c("grey30", "#0073C2FF", "#EFC000FF"))

sa.plot.sus = sa.plot
##################
#prev rate
##################
pr.plot = ggboxplot(allSS.LA1.data.endemic, x = "sus", y = "mean.prev.rate",
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab(expression(Delta * " prevalence")) + xlab("sus") + 
  theme(legend.position = "none")

##################
#spread rate
##################
sr.plot = ggboxplot(allSS.LA1.data.endemic, x = "sus", y = "spread.area.rate",
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab(expression(Delta * " area (km" ^ 2*")")) + xlab("sus") + 
  theme(legend.position = "none")

##################
#final population size
##################
finalpop.plot = ggboxplot(allSS.LA1.data.endemic, x = "sus", y = "final.pop",
                          color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("final population size") + xlab("sus") + 
  theme(legend.position = "none")

##################
#I distance
##################
ID.plot = ggboxplot(allSS.LA1.data.endemic, x = "sus", y = "mean.I.dist",
                    color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("maximum infected distance from center") + xlab("sus") + 
  theme(legend.position = "none")

ID.plot

#######################
#Prop of infected cells
#######################
propinfcell.plot = ggboxplot(allSS.LA1.data.endemic, x = "sus", y = "final.number.I.cells",
                             color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("number of cells with infected individuals") + xlab("sus") + 
  theme(legend.position = "none")

propinfcell.plot
########################################
#boxplots of SS individual proportions
########################################
allSS.LA1.data.endemic.tall = gather(allSS.LA1.data.endemic, key = "SS_group", value = "num_rem", final.SS.S.prop:final.SS.I.prop)

SS.plot = ggboxplot(allSS.LA1.data.endemic.tall, x = "SS_group", y = "num_rem",
                    color = "black", fill = "SS.strat", palette = c("#EFC000FF", "#868686FF", "#CD534CFF", "#7AA6DCFF")) + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("proportion of removed individuals") + xlab("individuals removed") + 
  theme(legend.position = "inside", legend.position.inside =  c(0.85, 0.85)) +
  scale_x_discrete(labels= c("susceptible", "exposed", "infectious")) + facet_wrap(~sus)
SS.plot

##########################################
#Inf. Fam. Prev.
##########################################

inffam.plot = ggboxplot(allSS.LA1.data.endemic, x = "sus", y = "mean.inf.fams.prev",
                        color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("inf. family prev.") + xlab("sus") + 
  theme(legend.position = "none")

chull.plot = ggboxplot(allSS.LA1.data.endemic, x = "sus", y = "mean.chull.fams.prev",
                       color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("inf. chull family prev.") + xlab("sus") + 
  theme(legend.position = "none")

chullI.plot = ggboxplot(allSS.LA1.data.endemic, x = "sus", y = "mean.chull.fams.prev.I",
                        color = "black", fill = "SS.strat", palette = "jco") + 
  # stat_compare_means(comparisons = my_comparisons, label="p.signif") + theme_cowplot() +
  ylab("inf. chull family prev.") + xlab("sus") + 
  theme(legend.position = "none")

chullI.plot
###############################################
#Combine plots
###############################################

ggarrange(fp.plot.sus, sa.plot.sus, nrow = 2, common.legend = TRUE)

ggarrange(pr.plot, sr.plot,
          finalpop.plot, ID.plot,
          inffam.plot, chull.plot, nrow = 3, ncol = 2, common.legend = TRUE)


############################################
#GLM
#############################################
glm.data = allSS.LA1.data.endemic
glm.data$SS.strat = factor(glm.data$SS.strat, ordered = FALSE)
glm.data$sus = factor(glm.data$sus, ordered = FALSE)

fp.glm = glm(final.prev.pcent ~ SS.strat + sus, data = glm.data, family = "Gamma")
summary(fp.glm)


###############################################
#Anova
###############################################
anova.data = allSS.LA1.data.endemic
levels(anova.data$SS.strat) = c(levels(anova.data$SS.strat))
levels(anova.data$sus) = c(levels(anova.data$sus))
fp.anova = aov(final.prev.pcent~SS.strat + sus, data = anova.data)
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
