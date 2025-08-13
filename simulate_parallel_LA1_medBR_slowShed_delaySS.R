#Script to run all parallel simulations
rm(list = ls())
gc()
set.seed(103)

library(doParallel)
library(foreach)
library(ggplot2)
library(tidyr)
library(doSNOW)
library(tcltk)

time.taken.list = list()

nsim = 1000

cluster = makeCluster(15, outfile ="")
registerDoSNOW(cluster)

pb <- tkProgressBar(max=nsim)
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress=progress)

total.time.start = Sys.time()


print("STARTING 5 YEAR DELAY SHARPSHOOTING SIMULATIONS")
prioritySS.output = foreach(z1 = 1:nsim, .errorhandling = "pass", .verbose = TRUE, .options.snow=opts) %dopar% {

  setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
  source(paste(getwd(), "/CWDSourcer.R", sep = ''))
  setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
  source(paste(getwd(), "/CWDparms_100x100_prioritySS_medBR_delay5_slowShed.R", sep = ''))

  start.time = Sys.time()

  init.model = init.CWD()

  pop = init.model[[1]]

  landscape.prions = init.model[[2]]

  centroids = init.model[[3]]

  onerun = SimulateOneRunCWD(pop, landscape.prions, centroids, track.pop = FALSE)

  output = extract_metrics(onerun, centroids)

  setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/prioritySS_LA1_medBR_delay5_slowShed") #for my PC

  file.name = paste("output_", z1, ".csv", sep = "")
  write.csv(output, file.name, row.names = FALSE)

  dynamics = cbind(onerun[[2]], onerun[[3]], onerun[[4]])
  file.name2 = paste("dynamics_", z1, ".csv", sep = "")
  write.csv(dynamics, file.name2, row.names = FALSE)

  output
}

prioritySS.output = as.data.frame(do.call(rbind, prioritySS.output)) #convert the list to a dataframe

names(prioritySS.output) = c("max.pop", "max.pop.time",
                           "max.I", "max.I.time", "last.I.time", "max.prev",
                           "total.I", "max.inc", "time.max.inc",
                           "max.spread", "max.number.I.cells", "max.spread.area",
                           "final.avg.prion.density",
                           "final.I", "final.pop", "final.prev",
                           "final.spread", "final.number.I.cells", "final.spread.area", "final.max.sp.cent",
                           "final.E", "final.I.cells", "final.E.cells",
                           "final.surv.prev",
                           "final.prions.area", "final.prop.inf.cells", "final.prop.ID50.cells",
                           "prop.ID50.infcells", "prions.gamma.shape", "prions.gamma.rate",
                           "final.SS.S.prop", "final.SS.E.prop", "final.SS.I.prop",
                           "spread.area.rate", "spread.dist.rate", "spread.distcent.rate",
                           "mean.E.dist", "std.E.dist", "mean.I.dist", "std.I.dist", "mean.prev.rate",
                           "mean.inf.fams.prev", "mean.chull.fams.prev", "mean.inf.fams.prev.I", "mean.chull.fams.prev.I")

setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/prioritySS_LA1_medBR_delay5_slowShed") #for my PC
write.csv(prioritySS.output, "prioritySS_output_LA1_medBR_delay5_slowShed.csv", row.names = FALSE)

# print("STARTING 10 YEAR DELAY SHARPSHOOTING SIMULATIONS")
# prioritySS.output = foreach(z1 = 1:nsim, .errorhandling = "pass", .verbose = TRUE, .options.snow=opts) %dopar% {
# 
#   setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
#   source(paste(getwd(), "/CWDSourcer.R", sep = ''))
#   setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
#   source(paste(getwd(), "/CWDparms_100x100_prioritySS_medBR_delay10_slowShed.R", sep = ''))
#   
#   start.time = Sys.time()
# 
#   init.model = init.CWD()
# 
#   pop = init.model[[1]]
# 
#   landscape.prions = init.model[[2]]
# 
#   centroids = init.model[[3]]
# 
#   onerun = SimulateOneRunCWD(pop, landscape.prions, centroids, track.pop = FALSE)
# 
#   output = extract_metrics(onerun, centroids)
# 
#   setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/prioritySS_LA1_medBR_delay10_slowShed") #for my PC
# 
#   file.name = paste("output_", z1, ".csv", sep = "")
#   write.csv(output, file.name, row.names = FALSE)
# 
#   dynamics = cbind(onerun[[2]], onerun[[3]], onerun[[4]])
#   file.name2 = paste("dynamics_", z1, ".csv", sep = "")
#   write.csv(dynamics, file.name2, row.names = FALSE)
# 
#   output
# }
# 
# prioritySS.output = as.data.frame(do.call(rbind, prioritySS.output)) #convert the list to a dataframe
# 
# names(prioritySS.output) = c("max.pop", "max.pop.time",
#                              "max.I", "max.I.time", "last.I.time", "max.prev",
#                              "total.I", "max.inc", "time.max.inc",
#                              "max.spread", "max.number.I.cells", "max.spread.area",
#                              "final.avg.prion.density",
#                              "final.I", "final.pop", "final.prev",
#                              "final.spread", "final.number.I.cells", "final.spread.area", "final.max.sp.cent",
#                              "final.E", "final.I.cells", "final.E.cells",
#                              "final.surv.prev",
#                              "final.prions.area", "final.prop.inf.cells", "final.prop.ID50.cells",
#                              "prop.ID50.infcells", "prions.gamma.shape", "prions.gamma.rate",
#                              "final.SS.S.prop", "final.SS.E.prop", "final.SS.I.prop",
#                              "spread.area.rate", "spread.dist.rate", "spread.distcent.rate",
#                              "mean.E.dist", "std.E.dist", "mean.I.dist", "std.I.dist", "mean.prev.rate",
#                              "mean.inf.fams.prev", "mean.chull.fams.prev", "mean.inf.fams.prev.I", "mean.chull.fams.prev.I")
# 
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/prioritySS_LA1_medBR_delay10_slowShed") #for my PC
# write.csv(prioritySS.output, "prioritySS_output_LA1_medBR_delay10_slowShed.csv", row.names = FALSE)
# 
# print("STARTING 15 YEAR DELAY SHARPSHOOTING SIMULATIONS")
# prioritySS.output = foreach(z1 = 1:nsim, .errorhandling = "pass", .verbose = TRUE, .options.snow=opts) %dopar% {
# 
#   setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
#   source(paste(getwd(), "/CWDSourcer.R", sep = ''))
#   setwd("E:/CWD_Modeling/Metapop_Model/Scripts") #for my PC
#   source(paste(getwd(), "/CWDparms_100x100_prioritySS_medBR_delay15_slowShed.R", sep = ''))
#   
#   start.time = Sys.time()
# 
#   init.model = init.CWD()
# 
#   pop = init.model[[1]]
# 
#   landscape.prions = init.model[[2]]
# 
#   centroids = init.model[[3]]
# 
#   onerun = SimulateOneRunCWD(pop, landscape.prions, centroids, track.pop = FALSE)
# 
#   output = extract_metrics(onerun, centroids)
# 
#   setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/prioritySS_LA1_medBR_delay15_slowShed") #for my PC
# 
#   file.name = paste("output_", z1, ".csv", sep = "")
#   write.csv(output, file.name, row.names = FALSE)
# 
#   dynamics = cbind(onerun[[2]], onerun[[3]], onerun[[4]])
#   file.name2 = paste("dynamics_", z1, ".csv", sep = "")
#   write.csv(dynamics, file.name2, row.names = FALSE)
# 
#   output
# }
# 
# prioritySS.output = as.data.frame(do.call(rbind, prioritySS.output)) #convert the list to a dataframe
# 
# names(prioritySS.output) = c("max.pop", "max.pop.time",
#                          "max.I", "max.I.time", "last.I.time", "max.prev",
#                          "total.I", "max.inc", "time.max.inc",
#                          "max.spread", "max.number.I.cells", "max.spread.area",
#                          "final.avg.prion.density",
#                          "final.I", "final.pop", "final.prev",
#                          "final.spread", "final.number.I.cells", "final.spread.area", "final.max.sp.cent",
#                          "final.E", "final.I.cells", "final.E.cells",
#                          "final.surv.prev",
#                          "final.prions.area", "final.prop.inf.cells", "final.prop.ID50.cells",
#                          "prop.ID50.infcells", "prions.gamma.shape", "prions.gamma.rate",
#                          "final.SS.S.prop", "final.SS.E.prop", "final.SS.I.prop",
#                          "spread.area.rate", "spread.dist.rate", "spread.distcent.rate",
#                          "mean.E.dist", "std.E.dist", "mean.I.dist", "std.I.dist", "mean.prev.rate",
#                          "mean.inf.fams.prev", "mean.chull.fams.prev", "mean.inf.fams.prev.I", "mean.chull.fams.prev.I")
# 
# setwd("E:/CWD_Modeling/Metapop_Model/Simulation_outputs/prioritySS_LA1_medBR_delay15_slowShed") #for my PC
# write.csv(prioritySS.output, "prioritySS_output_LA1_medBR_delay15_slowShed.csv", row.names = FALSE)

stopCluster(cl = cluster)

