#load in libraries
library(profvis)
library(pracma)
library(rdist)
library(tidyverse)
library(microbenchmark)
library(cowplot)
library(ggplot2)
library(gganimate)
library(gifski)
library(cowplot)
library(gridExtra)
library(magick)
library(varhandle)
library(fitdistrplus)
library(geometry)
library(sp)
library(terra)
library(sf)

#Source File
#Initialize
source(paste(getwd(), "/CreateGrid.R", sep = ''))
source(paste(getwd(), "/InitializeFamilies.R", sep = ''))
source(paste(getwd(), "/SimulateOneRunCWD.R", sep = ''))
source(paste(getwd(), "/InitializeCWDModel.R", sep = ''))

#Individual Processes
source(paste(getwd(), "/FastMovementCWD.R", sep = ''))
source(paste(getwd(), "/FOICWD.R", sep = ''))
source(paste(getwd(), "/StateChangesCWD.R", sep = ''))
source(paste(getwd(), "/sharpshootingCWD.R", sep = ''))
source(paste(getwd(), "/movementCWD.R", sep = ''))
source(paste(getwd(), "/shedCWD.R", sep = ''))
source(paste(getwd(), "/corpse_burstCWD.R", sep = ''))
source(paste(getwd(), "/prion_removalCWD.R", sep = ''))
source(paste(getwd(), "/harvestingCWD_simple.R", sep = ''))
source(paste(getwd(), "/surveillanceCWD.R", sep = ''))
source(paste(getwd(), "/dispersalCWD.R", sep = ''))
source(paste(getwd(), "/find_pt_dist_chull.R", sep = ''))

#Summary
source(paste(getwd(), "/areaOfInfectionCWD.R", sep = ''))
source(paste(getwd(), "/summarizeCWD.R", sep = ''))
source(paste(getwd(), "/extract_metrics.R", sep = ''))

#Plots
source(paste(getwd(), "/CWD_plots_static.R", sep = ''))
source(paste(getwd(), "/CWD_plots.R", sep = ''))



