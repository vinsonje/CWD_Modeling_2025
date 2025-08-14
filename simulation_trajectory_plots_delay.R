#code to look at prevalence trajectory
library(ggplot2)
library(cowplot)
library(stringr)
library(tidyr)
library(ggpubr)

#laod in the PRIORITY and DELAY 5 dynamics
setwd("~/Desktop/CWD_Modeling/prioritySS_LA1_medBR_delay5_slowShed") #for my PC
all.files = list.files(path = getwd(), all.files = T, recursive = T) #get all the files in the folder
all.files.dynamics = all.files[str_detect(all.files, "dynamics")]

dynamics.out = NULL
for(i in 1:length(all.files.dynamics)){
  print(i)
  dynamics.temp = read.csv(all.files.dynamics[i])
  names(dynamics.temp) = c("S", "E", "I")
  dynamics.temp$time = 1:dim(dynamics.temp)[1]
  dynamics.temp$sim = i
  dynamics.temp$prev = dynamics.temp$I/rowSums(dynamics.temp[,1:3])
  dynamics.temp$prev.pcent = dynamics.temp$prev*100
  dynamics.temp$endemic = 0
  if(tail(dynamics.temp$prev.pcent,1)>0){dynamics.temp$endemic = 1}
  dynamics.out = rbind(dynamics.out, dynamics.temp)
}

prioritySS.d5.dynamics = dynamics.out
prioritySS.d5.dynamics$delay = "delay 5"
prioritySS.d5.dynamics$SS.strat = "priority"

#laod in the PRIORITY and DELAY 10 dynamics
setwd("~/Desktop/CWD_Modeling/prioritySS_LA1_medBR_delay10_slowShed") #for my PC
all.files = list.files(path = getwd(), all.files = T, recursive = T) #get all the files in the folder
all.files.dynamics = all.files[str_detect(all.files, "dynamics")]

dynamics.out = NULL
for(i in 1:length(all.files.dynamics)){
  print(i)
  dynamics.temp = read.csv(all.files.dynamics[i])
  names(dynamics.temp) = c("S", "E", "I")
  dynamics.temp$time = 1:dim(dynamics.temp)[1]
  dynamics.temp$sim = i
  dynamics.temp$prev = dynamics.temp$I/rowSums(dynamics.temp[,1:3])
  dynamics.temp$prev.pcent = dynamics.temp$prev*100
  dynamics.temp$endemic = 0
  if(tail(dynamics.temp$prev.pcent,1)>0){dynamics.temp$endemic = 1}
  dynamics.out = rbind(dynamics.out, dynamics.temp)
}

prioritySS.d10.dynamics = dynamics.out
prioritySS.d10.dynamics$delay = "delay 10"
prioritySS.d10.dynamics$SS.strat = "priority"

#laod in the PRIORITY and DELAY 15 dynamics
setwd("~/Desktop/CWD_Modeling/prioritySS_LA1_medBR_delay15_slowShed") #for my PC
all.files = list.files(path = getwd(), all.files = T, recursive = T) #get all the files in the folder
all.files.dynamics = all.files[str_detect(all.files, "dynamics")]

dynamics.out = NULL
for(i in 1:length(all.files.dynamics)){
  print(i)
  dynamics.temp = read.csv(all.files.dynamics[i])
  names(dynamics.temp) = c("S", "E", "I")
  dynamics.temp$time = 1:dim(dynamics.temp)[1]
  dynamics.temp$sim = i
  dynamics.temp$prev = dynamics.temp$I/rowSums(dynamics.temp[,1:3])
  dynamics.temp$prev.pcent = dynamics.temp$prev*100
  dynamics.temp$endemic = 0
  if(tail(dynamics.temp$prev.pcent,1)>0){dynamics.temp$endemic = 1}
  dynamics.out = rbind(dynamics.out, dynamics.temp)
}

prioritySS.d15.dynamics = dynamics.out
prioritySS.d15.dynamics$delay = "delay 15"
prioritySS.d15.dynamics$SS.strat = "priority"

#laod in the EDGE and DELAY 5 dynamics
setwd("~/Desktop/CWD_Modeling/edgeSS_LA1_medBR_delay5_slowShed") #for my PC
all.files = list.files(path = getwd(), all.files = T, recursive = T) #get all the files in the folder
all.files.dynamics = all.files[str_detect(all.files, "dynamics")]

dynamics.out = NULL
for(i in 1:length(all.files.dynamics)){
  print(i)
  dynamics.temp = read.csv(all.files.dynamics[i])
  names(dynamics.temp) = c("S", "E", "I")
  dynamics.temp$time = 1:dim(dynamics.temp)[1]
  dynamics.temp$sim = i
  dynamics.temp$prev = dynamics.temp$I/rowSums(dynamics.temp[,1:3])
  dynamics.temp$prev.pcent = dynamics.temp$prev*100
  dynamics.temp$endemic = 0
  if(tail(dynamics.temp$prev.pcent,1)>0){dynamics.temp$endemic = 1}
  dynamics.out = rbind(dynamics.out, dynamics.temp)
}

edgeSS.d5.dynamics = dynamics.out
edgeSS.d5.dynamics$delay = "delay 5"
edgeSS.d5.dynamics$SS.strat = "edge"

#laod in the EDGE and DELAY 10 dynamics
setwd("~/Desktop/CWD_Modeling/edgeSS_LA1_medBR_delay10_slowShed") #for my PC
all.files = list.files(path = getwd(), all.files = T, recursive = T) #get all the files in the folder
all.files.dynamics = all.files[str_detect(all.files, "dynamics")]

dynamics.out = NULL
for(i in 1:length(all.files.dynamics)){
  print(i)
  dynamics.temp = read.csv(all.files.dynamics[i])
  names(dynamics.temp) = c("S", "E", "I")
  dynamics.temp$time = 1:dim(dynamics.temp)[1]
  dynamics.temp$sim = i
  dynamics.temp$prev = dynamics.temp$I/rowSums(dynamics.temp[,1:3])
  dynamics.temp$prev.pcent = dynamics.temp$prev*100
  dynamics.temp$endemic = 0
  if(tail(dynamics.temp$prev.pcent,1)>0){dynamics.temp$endemic = 1}
  dynamics.out = rbind(dynamics.out, dynamics.temp)
}

edgeSS.d10.dynamics = dynamics.out
edgeSS.d10.dynamics$delay = "delay 10"
edgeSS.d10.dynamics$SS.strat = "edge"

#laod in the EDGE and DELAY 15 dynamics
setwd("~/Desktop/CWD_Modeling/edgeSS_LA1_medBR_delay15_slowShed") #for my PC
all.files = list.files(path = getwd(), all.files = T, recursive = T) #get all the files in the folder
all.files.dynamics = all.files[str_detect(all.files, "dynamics")]

dynamics.out = NULL
for(i in 1:length(all.files.dynamics)){
  print(i)
  dynamics.temp = read.csv(all.files.dynamics[i])
  names(dynamics.temp) = c("S", "E", "I")
  dynamics.temp$time = 1:dim(dynamics.temp)[1]
  dynamics.temp$sim = i
  dynamics.temp$prev = dynamics.temp$I/rowSums(dynamics.temp[,1:3])
  dynamics.temp$prev.pcent = dynamics.temp$prev*100
  dynamics.temp$endemic = 0
  if(tail(dynamics.temp$prev.pcent,1)>0){dynamics.temp$endemic = 1}
  dynamics.out = rbind(dynamics.out, dynamics.temp)
}

edgeSS.d15.dynamics = dynamics.out
edgeSS.d15.dynamics$delay = "delay 15"
edgeSS.d15.dynamics$SS.strat = "edge"

#laod in the PRIORITY and all years dynamics
setwd("~/Desktop/CWD_Modeling/prioritySS_LA1_medBR_slowShed") #for my PC
all.files = list.files(path = getwd(), all.files = T, recursive = T) #get all the files in the folder
all.files.dynamics = all.files[str_detect(all.files, "dynamics")]

dynamics.out = NULL
for(i in 1:length(all.files.dynamics)){
  print(i)
  dynamics.temp = read.csv(all.files.dynamics[i])
  names(dynamics.temp) = c("S", "E", "I")
  dynamics.temp$time = 1:dim(dynamics.temp)[1]
  dynamics.temp$sim = i
  dynamics.temp$prev = dynamics.temp$I/rowSums(dynamics.temp[,1:3])
  dynamics.temp$prev.pcent = dynamics.temp$prev*100
  dynamics.temp$endemic = 0
  if(tail(dynamics.temp$prev.pcent,1)>0){dynamics.temp$endemic = 1}
  dynamics.out = rbind(dynamics.out, dynamics.temp)
}

prioritySS.all.dynamics = dynamics.out
prioritySS.all.dynamics$delay = "all years"
prioritySS.all.dynamics$SS.strat = "priority"

#laod in the EDGE and ALL YEARS dynamics
setwd("~/Desktop/CWD_Modeling/edgeSS_LA1_medBR_slowShed") #for my PC
all.files = list.files(path = getwd(), all.files = T, recursive = T) #get all the files in the folder
all.files.dynamics = all.files[str_detect(all.files, "dynamics")]

dynamics.out = NULL
for(i in 1:length(all.files.dynamics)){
  print(i)
  dynamics.temp = read.csv(all.files.dynamics[i])
  names(dynamics.temp) = c("S", "E", "I")
  dynamics.temp$time = 1:dim(dynamics.temp)[1]
  dynamics.temp$sim = i
  dynamics.temp$prev = dynamics.temp$I/rowSums(dynamics.temp[,1:3])
  dynamics.temp$prev.pcent = dynamics.temp$prev*100
  dynamics.temp$endemic = 0
  if(tail(dynamics.temp$prev.pcent,1)>0){dynamics.temp$endemic = 1}
  dynamics.out = rbind(dynamics.out, dynamics.temp)
}

edgeSS.all.dynamics = dynamics.out
edgeSS.all.dynamics$delay = "all years"
edgeSS.all.dynamics$SS.strat = "edge"

#laod in the no SS dynamics
setwd("~/Desktop/CWD_Modeling/noSS_medBR_slowShed_noSS") #for my PC
all.files = list.files(path = getwd(), all.files = T, recursive = T) #get all the files in the folder
all.files.dynamics = all.files[str_detect(all.files, "dynamics")]

dynamics.out = NULL
for(i in 1:length(all.files.dynamics)){
  print(i)
  dynamics.temp = read.csv(all.files.dynamics[i])
  names(dynamics.temp) = c("S", "E", "I")
  dynamics.temp$time = 1:dim(dynamics.temp)[1]
  dynamics.temp$sim = i
  dynamics.temp$prev = dynamics.temp$I/rowSums(dynamics.temp[,1:3])
  dynamics.temp$prev.pcent = dynamics.temp$prev*100
  dynamics.temp$endemic = 0
  if(tail(dynamics.temp$prev.pcent,1)>0){dynamics.temp$endemic = 1}
  dynamics.out = rbind(dynamics.out, dynamics.temp)
}

noSS.dynamics = dynamics.out
noSS.dynamics$delay = "no SS"
noSS.dynamics$SS.strat = "no SS"

#combine them

allSS.data = rbind(prioritySS.d5.dynamics, prioritySS.d10.dynamics,
                   prioritySS.d15.dynamics, prioritySS.all.dynamics,
                   edgeSS.d5.dynamics, edgeSS.d10.dynamics, edgeSS.d15.dynamics,
                   edgeSS.all.dynamics,
                   noSS.dynamics
                   )
allSS.data$delay = factor(allSS.data$delay, levels = c("no SS", "delay 15", "delay 10", "delay 5", "all years"), 
                              order = TRUE)

allSS.data$SS.strat = factor(allSS.data$SS.strat, levels = c("edge", "no SS", "priority"), 
                                 order = TRUE)

allSS.dynamics.end = subset(allSS.data, endemic == 1)
allSS.dynamics.end.agg = aggregate(allSS.dynamics.end, prev.pcent ~ SS.strat + time + delay, "mean")
mainplot = ggplot(allSS.dynamics.end, aes(x = time, y = prev.pcent)) + 
  # geom_line(aes(group = grouping, colour = strat), alpha = 0.1) + 
  geom_line(data = allSS.dynamics.end.agg, aes(x = time, y = prev.pcent, color = delay), lwd = 1.5) +
  # geom_ribbon(data = allSS.dynamics.end.agg, aes(ymin = lower.b, ymax = upper.b, colour = strat, fill = strat), alpha = 0.25, lty = "dashed") +
  theme_cowplot() +
  facet_wrap(~SS.strat, nrow = 3) +
  scale_linetype_manual(values = c("dotted", "solid", "longdash")) +
  ylab("prevalence (%)") + 
  theme(legend.position = "bottom") + 
  geom_vline(aes(xintercept = 5*12), lty = 'dashed', color = "grey50") + 
  annotate(geom = "text", x = 5*12+3, y = 4, label = "5 years", color = "grey50", angle = 90) +
  geom_vline(aes(xintercept = 10*12), lty = 'dashed', color = "grey50") + 
  annotate(geom = "text", x = 10*12+3, y = 4, label = "10 years", color = "grey50", angle = 90) +
  geom_vline(aes(xintercept = 15*12), lty = 'dashed', color = "grey50") + 
  annotate(geom = "text", x = 15*12+3, y = 4, label = "15 years", color = "grey50", angle = 90) +
  scale_color_manual(values = c("grey30", "#CC79A7", "#D55E00", "#0072B2", "#009E73"))

mainplot.delay = mainplot

allyears.data = subset(allSS.dynamics.end.agg, delay %in% c("no SS", "all years"))

mainplot.allyears = ggplot(allyears.data, aes(x = time, y = prev.pcent)) + 
  # geom_line(aes(group = grouping, colour = strat), alpha = 0.1) + 
  geom_line(data = allyears.data, aes(x = time, y = prev.pcent, col = SS.strat), lwd = 1.5) +
  # geom_ribbon(data = allSS.dynamics.end.agg, aes(ymin = lower.b, ymax = upper.b, colour = strat, fill = strat), alpha = 0.25, lty = "dashed") +
  theme_cowplot()  +
  scale_linetype_manual(values = c("dotted", "solid", "longdash")) +
  ylab("prevalence (%)") + 
  theme(legend.position = "bottom") + 
  geom_vline(aes(xintercept = 5*12), lty = 'dashed', color = "grey50") + 
  annotate(geom = "text", x = 5*12+3, y = 4, label = "5 years", color = "grey50", angle = 90) +
  geom_vline(aes(xintercept = 10*12), lty = 'dashed', color = "grey50") + 
  annotate(geom = "text", x = 10*12+3, y = 4, label = "10 years", color = "grey50", angle = 90) +
  geom_vline(aes(xintercept = 15*12), lty = 'dashed', color = "grey50") + 
  annotate(geom = "text", x = 15*12+3, y = 4, label = "15 years", color = "grey50", angle = 90) +
  scale_color_manual("sharpshooting strategy", values = c("#EFC000FF", "grey30", "#0073C2FF"))


mainplot.allyears.small = ggplot(allyears.data, aes(x = time, y = prev.pcent)) + 
  # geom_line(aes(group = grouping, colour = strat), alpha = 0.1) + 
  geom_line(data = allyears.data, aes(x = time, y = prev.pcent, col = SS.strat), lwd = 1.5) +
  # geom_ribbon(data = allSS.dynamics.end.agg, aes(ymin = lower.b, ymax = upper.b, colour = strat, fill = strat), alpha = 0.25, lty = "dashed") +
  theme_cowplot()  +
  scale_linetype_manual(values = c("dotted", "solid", "longdash")) +
  ylab("prevalence (%)") + 
  theme(legend.position = "bottom") + 
  geom_vline(aes(xintercept = 5*12), lty = 'dashed', color = "grey50") + 
  annotate(geom = "text", x = 5*12+3, y = 4, label = "5 years", color = "grey50", angle = 90) +
  geom_vline(aes(xintercept = 10*12), lty = 'dashed', color = "grey50") + 
  annotate(geom = "text", x = 10*12+3, y = 4, label = "10 years", color = "grey50", angle = 90) +
  geom_vline(aes(xintercept = 15*12), lty = 'dashed', color = "grey50") + 
  annotate(geom = "text", x = 15*12+3, y = 4, label = "15 years", color = "grey50", angle = 90) +
  scale_color_manual("sharpshooting strategy", values = c("#EFC000FF", "grey30", "#0073C2FF")) + 
  ylim(0, 0.075)

mainplot.allyears.small



ggarrange(mainplot.allyears + xlab("") + theme(axis.text.x = element_blank()), 
          mainplot.allyears.small+ theme(axis.text.y = element_text(angle=90, vjust = 0.1, hjust=0.5, size = 10.0), 
                                         axis.title.y = element_text(size = 11.0)), 
          heights = c(1, 0.5),
          nrow = 2, common.legend = T, legend = "bottom",
          labels = c("a", "b"))











#####################
#Colored one
#####################

allSS.dynamics.end.agg2 = subset(allSS.dynamics.end, SS.strat != "noSS")
zoomed.in = ggplot(allSS.dynamics.end, aes(x = time, y = prev.pcent)) + 
  # geom_line(aes(group = grouping, colour = strat), alpha = 0.1) + 
  geom_line(data = allSS.dynamics.end.agg2, aes(x = time, y = prev.pcent, colour = SS.strat)) +
  # geom_ribbon(data = allSS.dynamics.end.agg2, aes(ymin = lower.b, ymax = upper.b, colour = strat, fill = strat), alpha = 0.15, lty = "dashed") +
  theme_cowplot() + 
  coord_cartesian(ylim = c(-0, 1.0), xlim = c(0,240)) + 
  theme(axis.title.y = element_blank())
zoomed.in

ggarrange(mainplot, zoomed.in, nrow = 1, common.legend = TRUE, legend = "bottom")

xstart = seq(0, 239, 1)
xend = seq(1, 240, 1)
month = rep(format(ISOdatetime(2000,1:12,1,0,0,0),"%b"), 20)
xmid = seq(0.5, 239.5, 1)

rects = data.frame(xstart = xstart, xend = xend, xmid = xmid, month = month)

xevents = c(120.5, 121.5, 125.5, 130.5)
yevents = c(0.1, 0.07, 0.075, 0.075) + 1.4
events.lab = c("harv.", "s h a r p s h o o t i n g", "births", "h   a   r   v   e   s   t")
events = data.frame(xevents = xevents, yevents = yevents, labs = events.lab)

allSS.dynamics.end.sub = subset(allSS.dynamics.end.agg, delay %in% c("no SS", "delay 10", "delay 5"))
example.plot = ggplot(allSS.dynamics.end.sub) + 
  # geom_line(aes(x = time, y = prev.pcent, lty = delay, colour = SS.strat), alpha = 0.1) +
  theme_cowplot() +  coord_cartesian(ylim = c(0, 1.5)) +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = month), alpha = 0.4, inherit.aes = FALSE, show.legend = FALSE) +
  geom_text(data = rects, aes(x = xmid, y = -0.03, label = month), inherit.aes = FALSE) +
  geom_text(data = events, aes(x = xevents, y = yevents, label = labs), inherit.aes = FALSE, label.size = 0.5) +
  geom_line(data = allSS.dynamics.end.sub, aes(x = time, y = prev.pcent, lty = delay, color = SS.strat), lwd = 2.0) +
  scale_x_continuous(breaks=xstart, limits = c(120, 132)) +
  scale_fill_manual(values = c("#72874EFF", "#72874EFF", "#72874EFF",
                               "#A4BED5FF", "#A4BED5FF", "#FF9898FF",
                               "#A4BED5FF", "#A4BED5FF", "#A4BED5FF",
                               "#FED789FF", "#FED789FF", "#FED789FF"
  ), limits = rects$month[1:12]) +
  theme(legend.position = "bottom") + 
  ylab("prevalence (%)") + xlab("month") +
  scale_color_manual("sharpshooting strategy", values = c("#EFC000FF", "grey30", "#0073C2FF")) +
  scale_linetype_manual("delay", values = c("solid", "dotdash", "dotted"))

example.plot

