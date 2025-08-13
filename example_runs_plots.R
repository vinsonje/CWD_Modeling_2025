#create animations that show each SS strategy
rm(list = ls())
gc()
setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
source(paste(getwd(), "/CWD_plots20.R", sep = ''))

######################################
#random base
######################################
set.seed(105)

setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
source(paste(getwd(), "/CWDSourcer.R", sep = ''))
setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
source(paste(getwd(), "/CWDparms_50x50_random_medBR.R", sep = ''))

start.time = Sys.time()

init.model = init.CWD()

pop = init.model[[1]]

landscape.prions = init.model[[2]]

centroids = init.model[[3]]

onerun = SimulateOneRunCWD(pop, landscape.prions, centroids, track.pop = TRUE)
plot.landscape.meta20(onerun, grid.xmax, grid.ymax, save = TRUE, title = "random_base.mp4")

####################################
#priority base
####################################
set.seed(105)

setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
source(paste(getwd(), "/CWDSourcer.R", sep = ''))
setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
source(paste(getwd(), "/CWDparms_50x50_priority_medBR.R", sep = ''))

start.time = Sys.time()

init.model = init.CWD()

pop = init.model[[1]]

landscape.prions = init.model[[2]]

centroids = init.model[[3]]

onerun = SimulateOneRunCWD(pop, landscape.prions, centroids, track.pop = TRUE)

plot.landscape.meta20(onerun, grid.xmax, grid.ymax, save = TRUE, title = "priority_base.mp4")

####################################
#edge base
####################################

set.seed(105)

setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
source(paste(getwd(), "/CWDSourcer.R", sep = ''))
setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
source(paste(getwd(), "/CWDparms_50x50_edge_medBR.R", sep = ''))

start.time = Sys.time()

init.model = init.CWD()

pop = init.model[[1]]

landscape.prions = init.model[[2]]

centroids = init.model[[3]]

onerun = SimulateOneRunCWD(pop, landscape.prions, centroids, track.pop = TRUE)

plot.landscape.meta20(onerun, grid.xmax, grid.ymax, save = TRUE, title = "edge_base.mp4")

####################################
#edge perfect detection
####################################

set.seed(105)

setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
source(paste(getwd(), "/CWDSourcer.R", sep = ''))
setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
source(paste(getwd(), "/CWDparms_50x50_edge_medBR_pdetect.R", sep = ''))

start.time = Sys.time()

init.model = init.CWD()

pop = init.model[[1]]

landscape.prions = init.model[[2]]

centroids = init.model[[3]]

onerun = SimulateOneRunCWD(pop, landscape.prions, centroids, track.pop = TRUE)

plot.landscape.meta20(onerun, grid.xmax, grid.ymax, save = TRUE, title = "edge_pdetect.mp4")

####################################
#priority perfect detection
####################################

set.seed(105)

setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
source(paste(getwd(), "/CWDSourcer.R", sep = ''))
setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
source(paste(getwd(), "/CWDparms_50x50_priority_medBR_pdetect.R", sep = ''))

start.time = Sys.time()

init.model = init.CWD()

pop = init.model[[1]]

landscape.prions = init.model[[2]]

centroids = init.model[[3]]

onerun = SimulateOneRunCWD(pop, landscape.prions, centroids, track.pop = TRUE)

plot.landscape.meta20(onerun, grid.xmax, grid.ymax, save = TRUE, title = "priority_pdetect.mp4")
